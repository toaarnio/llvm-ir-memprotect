//
// NOTE: input must be -O0 otherwise llvm does inlining, which ruins purpose of this test
//
// RUN: clang -c $TEST_SRC -O0 -emit-llvm -o $OUT_FILE.bc &&
// DONT RUN: llvm-dis < $OUT_FILE.bc && echo "-------- end of compiler out ----------------" &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: echo "------- Compiled fine. ---------" && 
// RUN: opt -internalize -internalize-public-api-list=main -O3 -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.optimized.ll && 
// RUN: opt -internalize -internalize-public-api-list=main -O3  -S $OUT_FILE.bc -o $OUT_FILE.optimized.ll &&
// RUN: echo "Everything compiled fine. Linking externals in and running the program" &&
// RUN: echo "int hit = 1, ext_index = 1; int ext_table[4];" > $OUT_FILE.externals.ok.c &&
// RUN: echo "int hit = 1, ext_index = 2; int ext_table[4];" > $OUT_FILE.externals.fail.c &&
// RUN: clang -c $OUT_FILE.externals.ok.c -O0 -emit-llvm -o $OUT_FILE.externals.ok.bc &&
// RUN: clang -c $OUT_FILE.externals.fail.c -O0 -emit-llvm -o $OUT_FILE.externals.fail.bc &&
// RUN: llvm-link $OUT_FILE.optimized.ll $OUT_FILE.externals.ok.bc -o $OUT_FILE.linked.ok.bc &&
// RUN: llvm-link $OUT_FILE.clamped.optimized.ll $OUT_FILE.externals.ok.bc -o $OUT_FILE.clamped.linked.ok.bc &&
// RUN: llvm-link $OUT_FILE.clamped.optimized.ll $OUT_FILE.externals.fail.bc -o $OUT_FILE.clamped.linked.fail.bc &&
// RUN: echo "Testing to run without clamp pass:" && 
// RUN: lli $OUT_FILE.linked.ok.bc && 
// RUN: echo "Testing to run with boundary checks without crossing boundaries:" && 
// RUN: lli $OUT_FILE.clamped.linked.ok.bc && 
// RUN: echo "Testing to run with boundary checks writing out of boundaries:" && 
// RUN: ( lli $OUT_FILE.clamped.linked.fail.bc; 
// RUN:   ( [ $? != 0 ] && echo "OK: running code did abort as expected.") ||
// RUN:   ( echo "FAIL: execution should have aborted, because accessing table out of bounds" && false )
// RUN: )

extern int hit, ext_index;

// allowing this might be dangerous.. 
// if this allows to link with any external symbol and treat it as table...
// probably we should only allow fully linked stuff in future.
extern int ext_table[4];

int different_types_of_parameters(int* local_alloca, 
                                  int* passing_from_function_arg, 
                                  int* table, 
                                  int* reference_from_param,
                                  int* reference_from_local_int) {
  
  return ((*local_alloca) + 
          (*passing_from_function_arg) + 
          table[1] + 
          (*reference_from_param) + 
          (*reference_from_local_int));
}

int foo(int int_param, int* int_ptr_param) {
  int* local_ptr = &ext_index;
  int local_table[8] = {0};
  int local_int = hit;
  return different_types_of_parameters(local_ptr, int_ptr_param, local_table, &int_param, &local_int);
}

int main(void) {
  int *alloca_i32ptr = ext_table;
  int int_table[8];
  *(alloca_i32ptr+3) = 1507; 
  int_table[6] = foo(hit, &alloca_i32ptr[3]);
  alloca_i32ptr = &(int_table[6]);
  *(alloca_i32ptr+ext_index) = 5071 * (int)alloca_i32ptr;
  return *(alloca_i32ptr+ext_index) == 0;
}
