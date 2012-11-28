llvm-ir-memprotect
==================

LLVM IR instrumentation for protection against out-of-bounds memory accesses

COMPILING:

1. copy this directory to llvm/lib/llvm-ir-memprotect
2. add directory to llvm/lib/Makefile
3. configure and build llvm as usual
4. loadable module can be found under build directory llvm-build/Debug+Asserts/lib/ClampPointers.so or .dylib

RUNNING TESTS:

1. go to tests directory
2. ./run_tests.sh

