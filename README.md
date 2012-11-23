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
   
   ./compile-c.sh examples/array.c
   
   ./disasm.sh
   
   ./clamp-pointers.sh
