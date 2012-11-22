#
# Test compiling a bit more complex example...
# 

set -x

CLAMP_POINTERS_PASS=../../../llvm-trunk-build/Debug+Asserts/lib/ClampPointers.dylib

# currently -O0 causes pass to infinite loop -O1 just crash
clang -O1 -c examples/check_dce_optimization.c -emit-llvm -o dce.bc &&\
  opt -load $CLAMP_POINTERS_PASS -clamp-pointers dce.bc -o dce_clamped.bc &&\
  opt -Os dce_clamped.bc -S -o dce_optimized.ll 

# afterwards check bytecode from dce_optimized.ll
# and run it eg. with command: 
# lli dce_optimized.ll 128 256
