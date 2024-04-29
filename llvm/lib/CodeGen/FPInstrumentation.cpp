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
#include "llvm/ADT/Triple.h"
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

using namespace llvm;

cl::opt<std::string> EnableFPInstrumentation(
  "enable-fp-instrumentation", cl::Hidden, cl::init(""),
  cl::desc("Enable floating point instrumentation and set the function "
           "to be called when an exception occurs"));

cl::opt<unsigned> FPInstrumentationMask(
  "fp-instrumentation-mask", cl::Hidden, cl::init(0x87),
  cl::desc("Set exception bits to be checked. (Default: 0x87)"));

cl::opt<bool> FPInstrumentationOpt(
  "fp-instrumentation-opt", cl::Hidden, cl::init(false),
  cl::desc("Use fewer stack slots if possible"));

namespace {

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
  void InsertFPSCRCheckingAfterFPInstruction(MachineBasicBlock &MBB);
  void InsertFPSCRInitializationAtFunctionStart(MachineFunction &MF);

  const TargetInstrInfo *TII;
  bool Changed;
};

} // end anonymous namespace

void FPInstrumentation::InsertFPSCRCheckingAfterFPInstruction(
                                                      MachineBasicBlock &MBB) {

  for (MachineBasicBlock::iterator I = MBB.begin(), E = MBB.end(); I != E; ) {
    MachineInstr *MI = &*I;
    auto Cur = I;
    auto Next = ++I;

    if (!MI->isCandidateForFPInstrumentation()) continue;

    Changed = true;

    BuildMI(MBB, Next, Cur->getDebugLoc(), TII->get(TargetOpcode::FP_INSTRUMENT_OP));
  }
}

void FPInstrumentation::InsertFPSCRInitializationAtFunctionStart(
                                                          MachineFunction &MF) {
  MachineBasicBlock &FirstMBB = *MF.begin();
  BuildMI(FirstMBB, FirstMBB.begin(), DebugLoc(),
          TII->get(TargetOpcode::FP_INSTRUMENT_FUNCTION_ENTER));
}

bool FPInstrumentation::runOnMachineFunction(MachineFunction &MF) {
  TII = MF.getSubtarget().getInstrInfo();

  Changed = false;

  if (EnableFPInstrumentation.empty())
    return Changed;

  for (MachineBasicBlock &MBB : MF) {
    InsertFPSCRCheckingAfterFPInstruction(MBB);
  }

  if (Changed) {
    InsertFPSCRInitializationAtFunctionStart(MF);
  }

  return Changed;
}

char FPInstrumentation::ID = 0;
char &llvm::FPInstrumentationID = FPInstrumentation::ID;
INITIALIZE_PASS(FPInstrumentation, "fp-instrumentation",
                                   "Insert FP tracking ops", false, false)
