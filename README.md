LLVM-IR-MemProtect
==================

This is an LLVM module for adding run-time protection against
out-of-bounds memory accesses into LLVM IR bitcode.

Instructions
============

1. Checkout LLVM source code:

   cd ~
   
   svn co http://llvm.org/svn/llvm-project/llvm/trunk llvm

2. Checkout Clang source code:

   cd ~/llvm/tools
   
   svn co http://llvm.org/svn/llvm-project/cfe/trunk clang

3. Build LLVM and Clang:

   cd ~/llvm
   
   ./configure
   
   make

4. Checkout LLVM-IR-MemProtect source code:

   cd ~/llvm
   
   git clone https://github.com/toaarnio/llvm-ir-memprotect.git

5. Build LLVM-IR-MemProtect:

   cd ~/llvm/passes/ClampPointers
   
   make

6. Start experimenting:

   cd ~/llvm/passes
   
   clang -c -emit-llvm -O1 examples/array.c -o array.bc
   
   llvm-dis < array.bc

   opt -load ClampPointers.so -clamp-pointers array.bc -o array_clamped.bc
   
   lli array_clamped.bc

7. See how clang, llvm-dis, opt, lli are used in tests

## Alternative instructions (LLVM trunk does not have passes subdir, but all the passes are under llvm/lib):

### COMPILING ClampPointers pass with llvm:

1. copy this directory to llvm/lib/ClampPointers
2. add directory to llvm/lib/Makefile
2.5 TODO: tell what else should be done to include pass to opt wihtout need to use -load switch (IIRC it was just one or couple of lines somewhere)
3. configure and build llvm as usual
4. loadable module can be found under build directory llvm-build/Debug+Asserts/lib/ClampPointers.so or .dylib

### RUNNING TESTS:

1. go to tests directory
2. set absolute path of the ClampPointers.so to CLAMP_PLUGIN environment variable
3. ./run_tests.sh 
4. read tests/README