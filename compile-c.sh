clang -O0 -Wno-array-bounds -Wno-int-conversion -emit-llvm $1 -c -o output.bc
#clang -O3 -emit-llvm $1.c -c -o $1.bc
