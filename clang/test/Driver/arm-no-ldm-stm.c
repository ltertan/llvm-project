// RUN: %clang -target arm-none-gnueabi -### %s 2>&1 | FileCheck %s --check-prefix=CHECK-DEFAULT
// RUN: %clang -target arm-none-gnueabi -mno-ldm-stm -### %s 2>&1 | FileCheck %s

// CHECK: "-target-feature" "+no-ldm-stm"
// CHECK-DEFAULT-NOT: "+no-ldm-stm"
