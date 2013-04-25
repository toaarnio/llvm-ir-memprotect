LLVM-IR-MemProtect
==================

This is an LLVM module for adding run-time protection against
out-of-bounds memory accesses into LLVM IR bitcode.

Compiling annotated source documents:
    
       docco -c docs/custom.css ClampPointers.cpp

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
   
   git clone https://github.com/toaarnio/llvm-ir-memprotect.git passes/ClampPointers

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


# Implemented feateures:

* Generating safe pointers for allocas
* Generating safe pointers for fully linked global values (externals not allowed)
* Generating safe pointers to base types, base type pointers, arrays of base types and structs of base types
* Normalize argument passing to make sure that functions are ready for safe pointers to be generated
* Fixing pointer store instructions
* Fixing all store instructions to keep .Cur in smart pointer up-to-date
* Resolving from memory access address operand limits to respect
* Generationg boundary checks
* Converting internal functions to pass pointers as safe pointers
* Convert all calls to pass pointers as safe pointers
* Create map for each instruction / global which limits it respects
* Creates boundary checks to every load/store whose address might point to invalid area 
* Generate function signature for kernels which always has size parameter after passed pointer.
* Allow only calling builtins
* Convert builtin calls to safe versions

# TODO:

## Unimplemented cases: 

*Struct support*: Structs without pointers inside are already supported partly, but there are still some
serious bugs.

*Array of pointers*: Array of pointers sounds possible with reasonable work. 
It will have greater memory overhead than normal smart pointers/tables. 
            
*Pointer inside struct*: Need quite a lot of work. e.g. To be able to pass them as parameters we need to 
create smart pointer, which can have other smart pointer structs as an element and write unpack algorithm
to restore original structure after passing it. How about pointer array inside struct? Resolve what kind
of structures exactly will be supported.

*pointer to pointer*: Not sure how this can be implemented specially resolving part wil be hard. We would 
need a way to resolve dynamically where to find limits for pointed pointer which is currently impossible 
and might have serious consequences to performance. Probably we would have to write some bookkeeping, 
which maps pointer limits run time or change all the code to use safe pointers in everywhere and get rid of 
original pointers completely. We could have some limititations that pointer to pointer must always respect 
limits of original pointer which would prevent at least some of the problems, but might not be really useful... 

## Improvements:

Refactor all safe cases to use safe mapping, where certain instructions are painted to be safe.

Refactor global variable handling to first collect all globals in analyze phase and then create
safe pointers in transformation pass.

Check if safe pointer creation/bookkeeping can be refactored to separate module. Might make system 
easier to read and to reuse.

Run analysis passes before creating boundary checks to be able to check better if checks are needed
in each place. E.g. in case where we know all range that pointer is assigned or if there is fixed
length loop. Or any other loop whose boundaries can be verified without going to every iteration.

