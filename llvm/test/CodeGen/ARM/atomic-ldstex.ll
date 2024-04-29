; RUN: llc -verify-machineinstrs -mtriple=thumbv7m-none-eabi -mcpu=cortex-m4 %s -o - | FileCheck %s
; RUN: llc -verify-machineinstrs -mtriple=thumbv7m-none-eabi -mcpu=cortex-m4 -mattr=+no-ldstex %s -o - | FileCheck %s --check-prefix=CHECK-T1
; RUN: llc -verify-machineinstrs -mtriple=thumbv7m-none-eabi -mcpu=cortex-m4 -mattr=+no-ldstex -mattr=-no-ldstex %s -o - | FileCheck %s

; CHECK: dmb
; CHECK: __sync_fetch_and_add_8
; CHECK: dmb
; CHECK: ldrex
; CHECK: strex
; CHECK-NOT: __sync_fetch_and_add_4
; CHECK: dmb
; CHECK: ldrexh
; CHECK: strexh
; CHECK-NOT: __sync_fetch_and_add_2
; CHECK: dmb
; CHECK: ldrexb
; CHECK: strexb
; CHECK-NOT: __sync_fetch_and_add_1

; CHECK-T1: dmb
; CHECK-T1: __sync_fetch_and_add_8
; CHECK-T1: dmb
; CHECK-T1-NOT: ldrex
; CHECK-T1-NOT: strex
; CHECK-T1: __sync_fetch_and_add_4
; CHECK-T1: dmb
; CHECK-T1-NOT: ldrexh
; CHECK-T1-NOT: strexh
; CHECK-T1: __sync_fetch_and_add_2
; CHECK-T1: dmb
; CHECK-T1-NOT: ldrexb
; CHECK-T1-NOT: strexb
; CHECK-T1: __sync_fetch_and_add_1

define dso_local void @atomic_function_example_64() #0 {
entry:
  %v = alloca i64, align 4
  store i64 0, i64* %v, align 4
  %0 = atomicrmw add i64* %v, i64 1 seq_cst
  %1 = add i64 %0, 1
  ret void
}

define dso_local void @atomic_function_example_32() #0 {
entry:
  %v = alloca i32, align 4
  store i32 0, i32* %v, align 4
  %0 = atomicrmw add i32* %v, i32 1 seq_cst
  %1 = add i32 %0, 1
  ret void
}

define dso_local void @atomic_function_example_16() #0 {
entry:
  %v = alloca i16, align 4
  store i16 0, i16* %v, align 4
  %0 = atomicrmw add i16* %v, i16 1 seq_cst
  %1 = add i16 %0, 1
  ret void
}


define dso_local void @atomic_function_example_8() #0 {
entry:
  %v = alloca i8, align 4
  store i8 0, i8* %v, align 4
  %0 = atomicrmw add i8* %v, i8 1 seq_cst
  %1 = add i8 %0, 1
  ret void
}
