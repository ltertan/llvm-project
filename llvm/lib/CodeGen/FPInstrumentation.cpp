//===- FPInstrumentation.cpp - Adds FP instrumentation to functions. --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements a MachineFunctionPass that inserts the appropriate
// instrumentation instructions to track floating-point exceptions.
//
//===---------------------------------------------------------------------===//

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/CodeGen/TargetLowering.h"

using namespace llvm;

cl::opt<std::string> EnableFPInstrumentation(
  "enable-fp-instrumentation", cl::Hidden, cl::init(""),
  cl::desc("Insert instructions that check FPSCR/FPCR after each unsafe "
           "floating point instruction. Jump to the specified exception "
           "handler if an exception occurs. Also insert instructions that "
           "initialize FPSCR/FPCR at the start of the examined function."));

cl::opt<bool> EnableFPInstrumentationCall(
  "enable-fp-instrumentation-call", cl::Hidden, cl::init(false),
  cl::desc("Insert a function call that checks FPSCR/FPCR after "
           "each unsafe floating point instruction. "
           "Also insert a function call that initializes FPSCR/FPCR."));

cl::opt<std::string> FPInstrumentationCheck(
  "fp-instrumentation-check", cl::Hidden,
  cl::init("__wr_fp_instrumentation_check"),
  cl::desc("Specify FPSCR/FPCR checking function to be called after each "
           "unsafe floating point instruction."));

cl::opt<std::string> FPInstrumentationInitialize(
  "fp-instrumentation-initialize", cl::Hidden,
  cl::init("__wr_fp_instrumentation_initialize"),
  cl::desc("Specify FPSCR/FPCR initialization function to be called."));

cl::opt<unsigned> FPInstrumentationMask(
  "fp-instrumentation-mask", cl::Hidden, cl::init(0x87),
  cl::desc("Set exception bits to be checked. (Default: 0x87)"));

cl::opt<bool> FPInstrumentationOpt(
  "fp-instrumentation-opt", cl::Hidden, cl::init(false),
  cl::desc("Use fewer instrumentation instructions if possible"));

cl::opt<bool> FPInstrumentationSpareLRSaveRestore(
  "fp-instrumentation-spare-lr-save-restore", cl::Hidden, cl::init(true),
  cl::desc("Don't insert LR save/restore around instrumentation calls if "
           "LR is already saved by stack frame setup."));

cl::opt<bool> FPInstrumentationMergeLRSaveRestore(
  "fp-instrumentation-merge-lr-save-restore", cl::Hidden, cl::init(true),
  cl::desc("Use a single pair of LR save/restore for multiple "
           "instrumentation calls."));

namespace {

enum LR_SAVED_ON_STACK_STATUS {
  LR_SAVED_ON_STACK_STATUS_UNKWON,
  LR_SAVED_ON_STACK_STATUS_YES,
  LR_SAVED_ON_STACK_STATUS_NO
};

struct FPInstrumentation : public MachineFunctionPass {
  static char ID;

  FPInstrumentation() : MachineFunctionPass(ID) {
    initializeFPInstrumentationPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

private:
  void InsertFPUCheckingAfterFPInstruction(MachineFunction &MF);
  void InsertFPUCheckingCallAfterFPInstruction(MachineFunction &MF);
  void InsertFPUInitializationAtFunctionStart(MachineFunction &MF);
  void InsertFPUInitializationCallAtFunctionStart(MachineFunction &MF);
  void WrapInstrumentationCallWithLRSaveRestore(MachineFunction &MF);
  bool AccessSPOrLR(const MachineInstr &MI, const TargetLowering *TLI);
  void FindNextLRSaveRestoreRange(MachineBasicBlock::iterator &Start,
                                  const MachineBasicBlock::iterator &End,
                                  MachineBasicBlock::iterator &RStart,
                                  MachineBasicBlock::iterator &REnd,
                                  const TargetLowering *TLI);
  LR_SAVED_ON_STACK_STATUS GetLRSavedOnStackStatus(MachineFunction &MF);
  const TargetInstrInfo *TII;
  bool Changed;
  bool AnyInstrumentationCallNeedLRSaveRestore;
  LR_SAVED_ON_STACK_STATUS LRSavedOnStackStatus;
};

} // end anonymous namespace

LR_SAVED_ON_STACK_STATUS FPInstrumentation::GetLRSavedOnStackStatus(
                                                          MachineFunction &MF) {
  // Already computed
  if (LRSavedOnStackStatus != LR_SAVED_ON_STACK_STATUS_UNKWON)
    return LRSavedOnStackStatus;

  // If the optimization is disabled, we can simply treat it as
  // LR_SAVED_ON_STACK_STATUS_NO.
  if (!FPInstrumentationSpareLRSaveRestore) {
    LRSavedOnStackStatus = LR_SAVED_ON_STACK_STATUS_NO;
    return LRSavedOnStackStatus;
  }

  const MachineBasicBlock &FirstMBB = *MF.begin();
  const TargetLowering *TLI = MF.getSubtarget().getTargetLowering();
  unsigned LinkRegister = TLI->getLinkRegister();
  for (auto MI = FirstMBB.begin(), ME = FirstMBB.end(); MI != ME; ++MI) {
    if (!MI->getFlag(MachineInstr::FrameSetup)) continue;

    for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
      const MachineOperand &MO = MI->getOperand(i);
      if (MO.isReg() &&
          MO.getReg() == LinkRegister &&
          MO.isKill()) {
        LRSavedOnStackStatus = LR_SAVED_ON_STACK_STATUS_YES;
        return LRSavedOnStackStatus;
      }
    }
  }
  LRSavedOnStackStatus = LR_SAVED_ON_STACK_STATUS_NO;
  return LRSavedOnStackStatus;
}

static bool FPInstrumentationCallNeedLRSaveRestore(
                                                MachineBasicBlock::iterator I) {
  auto Op = I->getOpcode();
  if(Op == TargetOpcode::FP_INSTRUMENT_CALL_OP ||
     Op == TargetOpcode::FP_INSTRUMENT_CALL_FUNCTION_ENTER) {
    MachineOperand& NeedLRSaveRestore = I->getOperand(0);
    if (NeedLRSaveRestore.getImm() == 1)
      return true;
  }

  return false;
}

bool FPInstrumentation::AccessSPOrLR(const MachineInstr &MI,
                                     const TargetLowering *TLI) {
    unsigned SP = TLI->getStackPointerRegisterToSaveRestore();
    unsigned LR = TLI->getLinkRegister();
    for (auto MO: MI.operands()) {
        if (!MO.isReg()) continue;
        if (MO.getReg() == SP ||
            MO.getReg() == LR) return true;
    }
    return false;
}

void FPInstrumentation::FindNextLRSaveRestoreRange(
                                MachineBasicBlock::iterator &Start,
                                const MachineBasicBlock::iterator &End,
                                MachineBasicBlock::iterator &RStart,
                                MachineBasicBlock::iterator &REnd,
                                const TargetLowering *TLI) {
  // Find first FP_INSTRUMENT_CALL_OP or FP_INSTRUMENT_CALL_FUNCTION_ENTER
  // which needs LR save/restore
  RStart = Start;
  while (RStart != End && !FPInstrumentationCallNeedLRSaveRestore(RStart))
    ++RStart;

  if (RStart == End) {
    REnd = Start = End;
    return;
  }

  REnd = RStart;
  ++REnd;

  // If no optimization, wrap each call with LR save/restore
  if (!FPInstrumentationOpt || !FPInstrumentationMergeLRSaveRestore) {
    Start = REnd;
    return;
  }

  // Try to gather multiple calls and use a single pair of LR save/restore
  auto Next = REnd;
  while (true) {
    for (; Next != End; Next++) {
      // LR and SP should not be accessed within [RStart, REnd) as the
      // instrumentation modifies both SP and LR during
      // saving/restoring LR to/from stack and calling the instrumentation
      // function.
      if (Next->getFlag(MachineInstr::FrameSetup)) break;
      if (Next->getFlag(MachineInstr::FrameDestroy)) break;
      if (FPInstrumentationCallNeedLRSaveRestore(Next)) break;
      if (AccessSPOrLR(*Next, TLI)) break;
    }

    if (Next != End && FPInstrumentationCallNeedLRSaveRestore(Next))
      REnd = ++Next;
    else break;
  }
  Start = REnd;
}

void FPInstrumentation::WrapInstrumentationCallWithLRSaveRestore(
                                                    MachineFunction &MF) {
  const TargetLowering *TLI = MF.getSubtarget().getTargetLowering();
  for (MachineBasicBlock &MBB : MF) {
    MachineBasicBlock::iterator RStart, REnd;
    auto Start = MBB.begin();
    auto End = MBB.end();
    while (true) {
      FindNextLRSaveRestoreRange(Start, End, RStart, REnd, TLI);
      if (RStart != End) {
        BuildMI(MBB, RStart, DebugLoc(),
                             TII->get(TargetOpcode::FP_INSTRUMENT_SAVE_LR));
        BuildMI(MBB, REnd, DebugLoc(),
                             TII->get(TargetOpcode::FP_INSTRUMENT_RESTORE_LR));
      } else  break;
    }
  }
}

void FPInstrumentation::InsertFPUCheckingAfterFPInstruction(
                                                          MachineFunction &MF) {
  for (MachineBasicBlock &MBB : MF) {
    for (MachineBasicBlock::iterator I = MBB.begin(), E = MBB.end(); I != E; ) {
      MachineInstr *MI = &*I;
      auto Next = ++I;

      if (!MI->isCandidateForFPInstrumentation()) continue;

      Changed = true;

      BuildMI(MBB, Next, DebugLoc(),
              TII->get(TargetOpcode::FP_INSTRUMENT_OP));
    }
  }
}

void FPInstrumentation::InsertFPUCheckingCallAfterFPInstruction(
                                                          MachineFunction &MF) {
  bool IsEntry = true;
  for (MachineBasicBlock &MBB : MF) {
    auto MB = MBB.begin();
    auto ME = MBB.end();
    bool HasEncounteredLastFrameSetup;
    bool HasEncounteredFirstFrameDestroy = false;
    MachineBasicBlock::iterator LastFrameSetup;
    if (IsEntry) {
      LastFrameSetup = ME;

      // Find last frame setup
      for (MachineBasicBlock::iterator MI = MB; MI != ME; ++MI) {
        if (MI->getFlag(MachineInstr::FrameSetup)) {
          LastFrameSetup = MI;
        }
      }
      HasEncounteredLastFrameSetup = LastFrameSetup == ME;
    } else {
      HasEncounteredLastFrameSetup = true;
    }

    for (MachineBasicBlock::iterator MI = MB; MI != ME; ++MI) {
      if (!HasEncounteredLastFrameSetup && MI == LastFrameSetup) {
        HasEncounteredLastFrameSetup = true;
        continue;
      }

      if (MI->getFlag(MachineInstr::FrameDestroy)) {
        HasEncounteredFirstFrameDestroy = true;
        continue;
      }

      if (!MI->isCandidateForFPInstrumentation()) continue;

      Changed = true;

      int NeedLRSaveRestore;
      /*
       * To spare LR save/restore, it needs to satisfy the following conditions.
       *   1. the optimization is on
       *   2. the unsafe instruction is within
       *      (LastFrameSetup, FirstFrameDestroy)
       *   3. LR is already saved during stack frame setup
       */
      if (FPInstrumentationOpt &&
          HasEncounteredLastFrameSetup &&
          !HasEncounteredFirstFrameDestroy &&
          GetLRSavedOnStackStatus(MF) == LR_SAVED_ON_STACK_STATUS_YES) {
        NeedLRSaveRestore = 0;
      } else {
        NeedLRSaveRestore = 1;
        AnyInstrumentationCallNeedLRSaveRestore = true;
      }

      auto Next = MI;
      ++Next;

      BuildMI(MBB, Next, DebugLoc(),
              TII->get(TargetOpcode::FP_INSTRUMENT_CALL_OP))
        .addImm(NeedLRSaveRestore);
    }
    IsEntry = false;
  }
}

void FPInstrumentation::InsertFPUInitializationAtFunctionStart(
                                                          MachineFunction &MF) {
  MachineBasicBlock &FirstMBB = *MF.begin();
  BuildMI(FirstMBB, FirstMBB.begin(), DebugLoc(),
          TII->get(TargetOpcode::FP_INSTRUMENT_FUNCTION_ENTER));
}

void FPInstrumentation::InsertFPUInitializationCallAtFunctionStart(
                                                          MachineFunction &MF) {
  MachineBasicBlock &FirstMBB = *MF.begin();

  int NeedLRSaveRestore = 1;

  // If no optimization, simply put it at function entrance.
  auto B = FirstMBB.begin();
  auto E = FirstMBB.end();
  auto Pos = B;

  if (FPInstrumentationOpt) {
    // Otherwise, put it right before the first unsafe instruction.
    // We put the initialization near to the first unsafe instruction so that
    // we may have a chance to use only a single pair of LR save/restore for
    // more than one instrumentation call.
    while (Pos != E) {
      if (Pos->isCandidateForFPInstrumentation()) break;
      if (Pos->isBranch()) break;
      if (Pos->isReturn()) break;
      ++Pos;
    }

    if (GetLRSavedOnStackStatus(MF) == LR_SAVED_ON_STACK_STATUS_YES) {
      NeedLRSaveRestore = 0;
      for (auto MI = Pos; MI != E; ++MI) {
        if (MI->getFlag(MachineInstr::FrameSetup)) {
          NeedLRSaveRestore = 1;
          break;
        }
      }
    }
  }

  if (NeedLRSaveRestore) AnyInstrumentationCallNeedLRSaveRestore = true;

  BuildMI(FirstMBB, Pos, DebugLoc(),
          TII->get(TargetOpcode::FP_INSTRUMENT_CALL_FUNCTION_ENTER))
    .addImm(NeedLRSaveRestore);
}

bool FPInstrumentation::runOnMachineFunction(MachineFunction &MF) {
  TII = MF.getSubtarget().getInstrInfo();

  AnyInstrumentationCallNeedLRSaveRestore = false;
  LRSavedOnStackStatus = LR_SAVED_ON_STACK_STATUS_UNKWON;
  Changed = false;

  if (!EnableFPInstrumentationCall &&
      EnableFPInstrumentation.empty())
    return Changed;

  // EnableFPInstrumentationCall has a higher priority
  // than EnableFPInstrumentation.
  if (EnableFPInstrumentationCall) {
    InsertFPUCheckingCallAfterFPInstruction(MF);
  } else {
    InsertFPUCheckingAfterFPInstruction(MF);
  }

  if (Changed) {
    if (EnableFPInstrumentationCall) {
      InsertFPUInitializationCallAtFunctionStart(MF);
    } else {
      InsertFPUInitializationAtFunctionStart(MF);
    }

    if (AnyInstrumentationCallNeedLRSaveRestore) {
      WrapInstrumentationCallWithLRSaveRestore(MF);
    }
  }

  return Changed;
}

char FPInstrumentation::ID = 0;
char &llvm::FPInstrumentationID = FPInstrumentation::ID;
INITIALIZE_PASS(FPInstrumentation, "fp-instrumentation",
                                   "Insert FP tracking ops", false, false)
