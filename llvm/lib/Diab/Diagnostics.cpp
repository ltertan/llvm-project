#include "llvm/Diab/Diagnostics.h"

/*
 * There's a standard format we use for diagnostics. Although
 * the exact format will be tool specific (dcc, llopt, llvm),
 * we should do our best to be consistent.
 *
 * Here's an existing diagnostic.
 *   "TCDIAB-14878-1.c", line 19365: warning (dcc:1826):
 *   Diab 5.9.4.8 diagnostic: possibly impacted by TCDIAB-14878
 *   in function Ccdd_main
 */

namespace llvm {

static StringRef const DiabIssueTable[] = {
  "TCDIAB-17015"
};

void PrintDiabDiagnostic(const StringRef &FuncName,
                         const DebugLoc &DbgLoc,
                         DiabIssue Issue,
                         raw_ostream &OS) {
  // Should inform Customer to use "-g" to show the diagnostics.
  if (!DbgLoc) return;

  // Print source line info.
  auto *Scope = cast<DIScope>(DbgLoc.getScope());
  OS << "\""
     << Scope->getFilename()
     << "\", line "
     << DbgLoc.getLine()
     << ": warning: "
     << DIAB_LLVM_VERSION
     << " diagnostic: possibly impacted by "
     << DiabIssueTable[Issue]
     << " in function "
     << FuncName.str()
     << "\n";
}
}
