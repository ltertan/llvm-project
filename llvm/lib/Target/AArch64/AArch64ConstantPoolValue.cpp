//===- AArch64ConstantPoolValue.cpp - ARM constantpool value
//------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the AArch64 specific constantpool value class.
//
//===----------------------------------------------------------------------===//

#include "AArch64ConstantPoolValue.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

//===----------------------------------------------------------------------===//
// AArch64ConstantPoolValue
//===----------------------------------------------------------------------===//

AArch64ConstantPoolValue::AArch64ConstantPoolValue(
    Type *Ty, unsigned id, AArch64CP::AArch64CPKind kind,
    AArch64CP::AArch64CPModifier modifier, bool addCurrentAddress)
    : MachineConstantPoolValue(Ty), LabelId(id), Kind(kind), Modifier(modifier),
      AddCurrentAddress(addCurrentAddress) {}

AArch64ConstantPoolValue::~AArch64ConstantPoolValue() = default;

StringRef AArch64ConstantPoolValue::getModifierText() const {
  switch (Modifier) {
  case AArch64CP::no_modifier:
    return "none";
  }
  llvm_unreachable("Unknown modifier!");
}

LLVM_DUMP_METHOD void AArch64ConstantPoolValue::dump() const {
  errs() << "  " << *this;
}

void AArch64ConstantPoolValue::print(raw_ostream &O) const {
  if (Modifier) {
    O << "(" << getModifierText() << ")";
    if (AddCurrentAddress)
      O << "-.";
    O << ")";
  }
}

int AArch64ConstantPoolValue::getExistingMachineCPValue(MachineConstantPool *CP,
                                                        Align Alignment) {
  llvm_unreachable("Shouldn't be calling this directly!");
}

void AArch64ConstantPoolValue::addSelectionDAGCSEId(FoldingSetNodeID &ID) {
  ID.AddInteger(LabelId);
}

AArch64ConstantPoolConstant::AArch64ConstantPoolConstant(
    Type *Ty, const GlobalValue *C, unsigned ID, AArch64CP::AArch64CPKind Kind,
    AArch64CP::AArch64CPModifier Modifier, bool AddCurrentAddress)
    : AArch64ConstantPoolValue(Ty, ID, Kind, Modifier, AddCurrentAddress),
      CVal(C) {}

AArch64ConstantPoolConstant *
AArch64ConstantPoolConstant::Create(const GlobalValue *GV) {
  return new AArch64ConstantPoolConstant(
      (Type *)Type::getInt64Ty(GV->getContext()), GV, 0, AArch64CP::CPValue,
      AArch64CP::no_modifier, false);
}

int AArch64ConstantPoolConstant::getExistingMachineCPValue(
    MachineConstantPool *CP, Align Alignment) {
  int index =
      getExistingMachineCPValueImpl<AArch64ConstantPoolConstant>(CP, Alignment);

  return index;
}

void AArch64ConstantPoolConstant::addSelectionDAGCSEId(FoldingSetNodeID &ID) {
  ID.AddPointer(CVal);
  AArch64ConstantPoolValue::addSelectionDAGCSEId(ID);
}

const GlobalValue *AArch64ConstantPoolConstant::getGV() const {
  return dyn_cast_or_null<GlobalValue>(CVal);
}

void AArch64ConstantPoolConstant::print(raw_ostream &O) const {
  O << CVal->getName();
  AArch64ConstantPoolValue::print(O);
}
