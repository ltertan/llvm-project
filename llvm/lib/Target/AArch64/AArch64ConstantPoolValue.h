#ifndef LLVM_LIB_TARGET_AArch64_ARMCONSTANTPOOLVALUE_H
#define LLVM_LIB_TARGET_AArch64_ARMCONSTANTPOOLVALUE_H

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/Support/Casting.h"
#include <string>
#include <vector>

namespace llvm {

class GlobalValue;
class GlobalVariable;
class LLVMContext;
class MachineBasicBlock;
class raw_ostream;
class Type;

namespace AArch64CP {

enum AArch64CPKind {
  CPValue /// TODO : Handling only GlobalValue (-mlong-calls).
};

enum AArch64CPModifier {
  no_modifier /// TODO : None
};

} // end namespace AArch64CP

// AArch6C4onstantPoolValue - ARM specific constantpool value.
class AArch64ConstantPoolValue : public MachineConstantPoolValue {

  unsigned LabelId;                      // Label id of the load.
  AArch64CP::AArch64CPKind Kind;         // Kind of constant.
  AArch64CP::AArch64CPModifier Modifier; // GV modifier
  bool AddCurrentAddress;                // To Debug

protected:
  AArch64ConstantPoolValue(Type *Ty, unsigned id, AArch64CP::AArch64CPKind Kind,
                           AArch64CP::AArch64CPModifier Modifier,
                           bool AddCurrentAddress);

  template <typename Derived>
  int getExistingMachineCPValueImpl(MachineConstantPool *CP, Align Alignment) {
    const std::vector<MachineConstantPoolEntry> &Constants = CP->getConstants();
    for (unsigned i = 0, e = Constants.size(); i != e; ++i) {
      if (Constants[i].isMachineConstantPoolEntry() &&
          Constants[i].getAlign() >= Alignment) {
        auto *CPV = static_cast<AArch64ConstantPoolValue *>(
            Constants[i].Val.MachineCPVal);
        if (Derived *APC = dyn_cast<Derived>(CPV))
          if (cast<Derived>(this)->equals(APC))
            return i;
      }
    }

    return -1;
  }

public:
  ~AArch64ConstantPoolValue() override;

  AArch64CP::AArch64CPModifier getModifier() const { return Modifier; }
  StringRef getModifierText() const;

  bool mustAddCurrentAddress() const { return AddCurrentAddress; }

  bool isGlobalValue() const { return Kind == AArch64CP::CPValue; }

  unsigned getLabelId() const { return LabelId; }

  int getExistingMachineCPValue(MachineConstantPool *CP,
                                Align Alignment) override;

  void addSelectionDAGCSEId(FoldingSetNodeID &ID) override;

  void print(raw_ostream &O) const override;
  void print(raw_ostream *O) const {
    if (O)
      print(*O);
  }
  void dump() const;
};

inline raw_ostream &operator<<(raw_ostream &O,
                               const AArch64ConstantPoolValue &V) {
  V.print(O);
  return O;
}

class AArch64ConstantPoolConstant : public AArch64ConstantPoolValue {
  const GlobalValue *CVal;

  AArch64ConstantPoolConstant(Type *Ty, const GlobalValue *C, unsigned ID,
                              AArch64CP::AArch64CPKind Kind,
                              AArch64CP::AArch64CPModifier Modifier,
                              bool AddCurrentAddress);

public:
  static AArch64ConstantPoolConstant *Create(const GlobalValue *GV);
  const GlobalValue *getGV() const;

  void print(raw_ostream &O) const override;

  static bool classof(const AArch64ConstantPoolValue *APV) {
    return APV->isGlobalValue();
  }

  bool equals(const AArch64ConstantPoolConstant *A) const {
    return CVal == A->CVal;
  }

  int getExistingMachineCPValue(MachineConstantPool *CP,
                                Align Alignment) override;

  void addSelectionDAGCSEId(FoldingSetNodeID &ID) override;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_AArch64_ARMCONSTANTPOOLVALUE_H
