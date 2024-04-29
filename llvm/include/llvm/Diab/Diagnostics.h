#ifndef LLVM_LIB_DIAB_DIAGNOSTICS_H
#define LLVM_LIB_DIAB_DIAGNOSTICS_H

#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

/*
 * To save trouble, we don't show the version.
 * Todo: Automatically detect Diab version.
 */

// #define DIAB_LLVM_VERSION "Diab 7.0.2.0"
#define DIAB_LLVM_VERSION "Diab"

namespace llvm {

enum DiabIssue {
  DIAB_ISSUE_TCDIAB_17015
};

void PrintDiabDiagnostic(const StringRef &FuncName,
                         const DebugLoc &DbgLoc,
                         DiabIssue Issue,
                         raw_ostream &OS);
}

#endif // LLVM_LIB_DIAB_DIAGNOSTICS_H
