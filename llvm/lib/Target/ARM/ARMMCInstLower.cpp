//===-- ARMMCInstLower.cpp - Convert ARM MachineInstr to an MCInst --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower ARM MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "ARM.h"
#include "ARMAsmPrinter.h"
#include "ARMBaseInstrInfo.h"
#include "ARMMachineFunctionInfo.h"
#include "ARMSubtarget.h"
#include "MCTargetDesc/ARMAddressingModes.h"
#include "MCTargetDesc/ARMBaseInfo.h"
#include "MCTargetDesc/ARMMCExpr.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/IR/Constants.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>
#include <cstdint>

using namespace llvm;

extern cl::opt<std::string> EnableFPInstrumentation;
extern cl::opt<unsigned> FPInstrumentationMask;
extern cl::opt<bool> FPInstrumentationOpt;

MCOperand ARMAsmPrinter::GetSymbolRef(const MachineOperand &MO,
                                      const MCSymbol *Symbol) {
  MCSymbolRefExpr::VariantKind SymbolVariant = MCSymbolRefExpr::VK_None;
  if (MO.getTargetFlags() & ARMII::MO_SBREL)
    SymbolVariant = MCSymbolRefExpr::VK_ARM_SBREL;

  const MCExpr *Expr =
      MCSymbolRefExpr::create(Symbol, SymbolVariant, OutContext);
  switch (MO.getTargetFlags() & ARMII::MO_OPTION_MASK) {
  default:
    llvm_unreachable("Unknown target flag on symbol operand");
  case ARMII::MO_NO_FLAG:
    break;
  case ARMII::MO_LO16:
    Expr =
        MCSymbolRefExpr::create(Symbol, SymbolVariant, OutContext);
    Expr = ARMMCExpr::createLower16(Expr, OutContext);
    break;
  case ARMII::MO_HI16:
    Expr =
        MCSymbolRefExpr::create(Symbol, SymbolVariant, OutContext);
    Expr = ARMMCExpr::createUpper16(Expr, OutContext);
    break;
  case ARMII::MO_LO_0_7:
    Expr = MCSymbolRefExpr::create(Symbol, SymbolVariant, OutContext);
    Expr = ARMMCExpr::createLower0_7(Expr, OutContext);
    break;
  case ARMII::MO_LO_8_15:
    Expr = MCSymbolRefExpr::create(Symbol, SymbolVariant, OutContext);
    Expr = ARMMCExpr::createLower8_15(Expr, OutContext);
    break;
  case ARMII::MO_HI_0_7:
    Expr = MCSymbolRefExpr::create(Symbol, SymbolVariant, OutContext);
    Expr = ARMMCExpr::createUpper0_7(Expr, OutContext);
    break;
  case ARMII::MO_HI_8_15:
    Expr = MCSymbolRefExpr::create(Symbol, SymbolVariant, OutContext);
    Expr = ARMMCExpr::createUpper8_15(Expr, OutContext);
    break;
  }

  if (!MO.isJTI() && MO.getOffset())
    Expr = MCBinaryExpr::createAdd(Expr,
                                   MCConstantExpr::create(MO.getOffset(),
                                                          OutContext),
                                   OutContext);
  return MCOperand::createExpr(Expr);

}

bool ARMAsmPrinter::lowerOperand(const MachineOperand &MO,
                                 MCOperand &MCOp) {
  switch (MO.getType()) {
  default: llvm_unreachable("unknown operand type");
  case MachineOperand::MO_Register:
    // Ignore all implicit register operands.
    if (MO.isImplicit())
      return false;
    assert(!MO.getSubReg() && "Subregs should be eliminated!");
    MCOp = MCOperand::createReg(MO.getReg());
    break;
  case MachineOperand::MO_Immediate:
    MCOp = MCOperand::createImm(MO.getImm());
    break;
  case MachineOperand::MO_MachineBasicBlock:
    MCOp = MCOperand::createExpr(MCSymbolRefExpr::create(
        MO.getMBB()->getSymbol(), OutContext));
    break;
  case MachineOperand::MO_GlobalAddress:
    MCOp = GetSymbolRef(MO,
                        GetARMGVSymbol(MO.getGlobal(), MO.getTargetFlags()));
    break;
  case MachineOperand::MO_ExternalSymbol:
    MCOp = GetSymbolRef(MO,
                        GetExternalSymbolSymbol(MO.getSymbolName()));
    break;
  case MachineOperand::MO_JumpTableIndex:
    MCOp = GetSymbolRef(MO, GetJTISymbol(MO.getIndex()));
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    if (Subtarget->genExecuteOnly())
      llvm_unreachable("execute-only should not generate constant pools");
    MCOp = GetSymbolRef(MO, GetCPISymbol(MO.getIndex()));
    break;
  case MachineOperand::MO_BlockAddress:
    MCOp = GetSymbolRef(MO, GetBlockAddressSymbol(MO.getBlockAddress()));
    break;
  case MachineOperand::MO_FPImmediate: {
    APFloat Val = MO.getFPImm()->getValueAPF();
    bool ignored;
    Val.convert(APFloat::IEEEdouble(), APFloat::rmTowardZero, &ignored);
    MCOp = MCOperand::createDFPImm(bit_cast<uint64_t>(Val.convertToDouble()));
    break;
  }
  case MachineOperand::MO_RegisterMask:
    // Ignore call clobbers.
    return false;
  }
  return true;
}

void llvm::LowerARMMachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI,
                                        ARMAsmPrinter &AP) {
  OutMI.setOpcode(MI->getOpcode());

  // In the MC layer, we keep modified immediates in their encoded form
  bool EncodeImms = false;
  switch (MI->getOpcode()) {
  default: break;
  case ARM::MOVi:
  case ARM::MVNi:
  case ARM::CMPri:
  case ARM::CMNri:
  case ARM::TSTri:
  case ARM::TEQri:
  case ARM::MSRi:
  case ARM::ADCri:
  case ARM::ADDri:
  case ARM::ADDSri:
  case ARM::SBCri:
  case ARM::SUBri:
  case ARM::SUBSri:
  case ARM::ANDri:
  case ARM::ORRri:
  case ARM::EORri:
  case ARM::BICri:
  case ARM::RSBri:
  case ARM::RSBSri:
  case ARM::RSCri:
    EncodeImms = true;
    break;
  }

  for (const MachineOperand &MO : MI->operands()) {
    MCOperand MCOp;
    if (AP.lowerOperand(MO, MCOp)) {
      if (MCOp.isImm() && EncodeImms) {
        int32_t Enc = ARM_AM::getSOImmVal(MCOp.getImm());
        if (Enc != -1)
          MCOp.setImm(Enc);
      }
      OutMI.addOperand(MCOp);
    }
  }
}

void ARMAsmPrinter::EmitSled(const MachineInstr &MI, SledKind Kind)
{
  if (MI.getParent()->getParent()->getInfo<ARMFunctionInfo>()
    ->isThumbFunction())
  {
    MI.emitError("An attempt to perform XRay instrumentation for a"
      " Thumb function (not supported). Detected when emitting a sled.");
    return;
  }
  static const int8_t NoopsInSledCount = 6;
  // We want to emit the following pattern:
  //
  // .Lxray_sled_N:
  //   ALIGN
  //   B #20
  //   ; 6 NOP instructions (24 bytes)
  // .tmpN
  //
  // We need the 24 bytes (6 instructions) because at runtime, we'd be patching
  // over the full 28 bytes (7 instructions) with the following pattern:
  //
  //   PUSH{ r0, lr }
  //   MOVW r0, #<lower 16 bits of function ID>
  //   MOVT r0, #<higher 16 bits of function ID>
  //   MOVW ip, #<lower 16 bits of address of __xray_FunctionEntry/Exit>
  //   MOVT ip, #<higher 16 bits of address of __xray_FunctionEntry/Exit>
  //   BLX ip
  //   POP{ r0, lr }
  //
  OutStreamer->emitCodeAlignment(Align(4), &getSubtargetInfo());
  auto CurSled = OutContext.createTempSymbol("xray_sled_", true);
  OutStreamer->emitLabel(CurSled);
  auto Target = OutContext.createTempSymbol();

  // Emit "B #20" instruction, which jumps over the next 24 bytes (because
  // register pc is 8 bytes ahead of the jump instruction by the moment CPU
  // is executing it).
  // By analogy to ARMAsmPrinter::emitPseudoExpansionLowering() |case ARM::B|.
  // It is not clear why |addReg(0)| is needed (the last operand).
  EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::Bcc).addImm(20)
    .addImm(ARMCC::AL).addReg(0));

  emitNops(NoopsInSledCount);

  OutStreamer->emitLabel(Target);
  recordSled(CurSled, MI, Kind, 2);
}

void ARMAsmPrinter::LowerPATCHABLE_FUNCTION_ENTER(const MachineInstr &MI)
{
  EmitSled(MI, SledKind::FUNCTION_ENTER);
}

void ARMAsmPrinter::LowerPATCHABLE_FUNCTION_EXIT(const MachineInstr &MI)
{
  EmitSled(MI, SledKind::FUNCTION_EXIT);
}

void ARMAsmPrinter::LowerPATCHABLE_TAIL_CALL(const MachineInstr &MI)
{
  EmitSled(MI, SledKind::TAIL_CALL);
}

void ARMAsmPrinter::SetupFPInstrumentationRegisterMap(MachineFunction &MF) {
    /*
     * Note, we have to backport the following change from LLVM10 to LLVM7 to
     * perform the optimization using registers if possible.
     *
     *   [ARM] Preserve liveness in ARMConstantIslands.
     *   https://reviews.llvm.org/D66319
     *   commit: eaff844fe9584be189ce66214efdf8ce1eceeb5b
     */
    if (EnableFPInstrumentation.empty() ||
        !FPInstrumentationOpt ||
        !MF.getRegInfo().tracksLiveness()) return;

    for (MachineBasicBlock &MBB : MF) {
        LiveRegUnits LRU(*Subtarget->getRegisterInfo());

        // Collect defs, uses and regmask clobbers within the basic block
        std::for_each(MBB.rbegin(), MBB.rend(),
                      [&LRU](MachineInstr &MI) { LRU.accumulate(MI); });

        // Now, add the live outs to the set.
        LRU.addLiveOuts(MBB);

        // If the register is available in the MBB, but also not a live out of
        // the block, then we know it's safe to use it without save/restore.
        FPInstrumentationRegisterMap[&MBB] =
        std::tuple<bool, bool, bool>(
             LRU.available(ARM::R11), // callee-saved register
             LRU.available(ARM::R12), // Intra-procedural-call scratch register
             LRU.available(ARM::CPSR));
    }
}

void ARMAsmPrinter::LowerFP_INSTRUMENT_FUNCTION_ENTER(const MachineInstr &MI)
{
  if (!FPInstrumentationOpt) {
    EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::STMDB_UPD)
         // Add predicate operands.
        .addReg(ARM::SP)
        .addReg(ARM::SP)
        .addImm(ARMCC::AL)
        .addReg(0)
        .addReg(ARM::R12));
  }

  // #clear fpscr
  // vmrs r12, fpscr
  // bic r12, r12, #FPInstrumentationMask
  // vmsr fpscr, r12
  EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::VMRS)
      .addReg(ARM::R12)
       // Add predicate operands.
      .addImm(ARMCC::AL)
      .addReg(0)
      .addReg(ARM::FPSCR));

  EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::BICri)
      .addReg(ARM::R12)
      .addReg(ARM::R12)
      .addImm(FPInstrumentationMask)
       // Add predicate operands.
      .addImm(ARMCC::AL)
      .addReg(0)
       // 's' bit operand (always reg0 for this).
      .addReg(0));

  EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::VMSR)
      .addReg(ARM::R12)
       // Add predicate operands.
      .addImm(ARMCC::AL)
      .addReg(0)
      .addReg(ARM::FPSCR));

  if (!FPInstrumentationOpt) {
    EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::LDMIA_UPD)
         // Add predicate operands.
        .addReg(ARM::SP)
        .addReg(ARM::SP)
        .addImm(ARMCC::AL)
        .addReg(0)
        .addReg(ARM::R12));
  }
}

void ARMAsmPrinter::LowerFP_INSTRUMENT_OP(const MachineInstr &MI)
{
  bool R11AvailableInBlock = false;
  bool R12AvailableInBlock = false;
  bool CPSRAvailableInBlock = false;

  if (FPInstrumentationOpt) {
    auto iter = FPInstrumentationRegisterMap.find(MI.getParent());
    if (iter != FPInstrumentationRegisterMap.end()) {
      std::tuple<bool, bool, bool> &AvailableR11R12CPSR = iter->second;
      R11AvailableInBlock = std::get<0>(AvailableR11R12CPSR);
      R12AvailableInBlock = std::get<1>(AvailableR11R12CPSR);
      CPSRAvailableInBlock = std::get<2>(AvailableR11R12CPSR);
    }
  }

  // save r12
  if (!R12AvailableInBlock) {
    EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::STMDB_UPD)
         // Add predicate operands.
        .addReg(ARM::SP)
        .addReg(ARM::SP)
        .addImm(ARMCC::AL)
        .addReg(0)
        .addReg(ARM::R12));
  }

  // save CPSR
  if (!CPSRAvailableInBlock) {
    if (!R11AvailableInBlock) {
      EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::MRS)
          .addReg(ARM::R12)
           // Add predicate operands.
          .addImm(ARMCC::AL)
          .addReg(0)
          .addReg(ARM::CPSR));

      EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::STMDB_UPD)
           // Add predicate operands.
          .addReg(ARM::SP)
          .addReg(ARM::SP)
          .addImm(ARMCC::AL)
          .addReg(0)
          .addReg(ARM::R12));
      } else {
      EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::MRS)
          .addReg(ARM::R11)
           // Add predicate operands.
          .addImm(ARMCC::AL)
          .addReg(0)
          .addReg(ARM::CPSR));
    }
  }

  // #check
  // vmrs r12, fpscr
  // tst r12, #FPInstrumentationMask
  // bne wr_fixed_addr
  EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::VMRS)
      .addReg(ARM::R12)
       // Add predicate operands.
      .addImm(ARMCC::AL)
      .addReg(0)
      .addReg(ARM::FPSCR));

  EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::TSTri)
      .addReg(ARM::R12)
      .addImm(FPInstrumentationMask)
       // Add predicate operands.
      .addImm(ARMCC::AL)
      .addReg(0));

  // Existing tests can pass with -mllvm --enable-fp-instrumentation=nop even
  // when there is any floating point exception.
  if (EnableFPInstrumentation == ".nop") {
    emitNops(1);
  } else {
    MCSymbol *Label = OutContext.getOrCreateSymbol(EnableFPInstrumentation);
    const MCExpr *SymbolExpr = MCSymbolRefExpr::create(Label, OutContext);
    EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::Bcc)
         .addExpr(SymbolExpr)
         .addImm(ARMCC::NE));
  }

  // restore CPSR
  if (!CPSRAvailableInBlock) {
    if (!R11AvailableInBlock) {
      EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::LDMIA_UPD)
           // Add predicate operands.
          .addReg(ARM::SP)
          .addReg(ARM::SP)
          .addImm(ARMCC::AL)
          .addReg(0)
          .addReg(ARM::R12));

      EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::MSR)
          .addImm(8)
          .addReg(ARM::R12)
           // Add predicate operands.
          .addImm(ARMCC::AL)
          .addReg(0));
    } else {
      EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::MSR)
          .addImm(8)
          .addReg(ARM::R11)
           // Add predicate operands.
          .addImm(ARMCC::AL)
          .addReg(0));
    }
  }

  // restore r12
  if (!R12AvailableInBlock) {
    EmitToStreamer(*OutStreamer, MCInstBuilder(ARM::LDMIA_UPD)
         // Add predicate operands.
        .addReg(ARM::SP)
        .addReg(ARM::SP)
        .addImm(ARMCC::AL)
        .addReg(0)
        .addReg(ARM::R12));
  }
}
