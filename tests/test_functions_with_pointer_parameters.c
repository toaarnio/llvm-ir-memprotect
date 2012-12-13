// RUN: clang -c $TEST_SRC -O0 -emit-llvm -o $OUT_FILE.bc &&
// DONT RUN: llvm-dis < $OUT_FILE.bc && echo "-------- end of compiler out ----------------" &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: echo "------- Compiled fine. ---------" && 
// DONT RUN: cat $OUT_FILE.clamped.ll &&
// RUN: echo "----- clamped optimized -----" &&
// RUN: opt -internalize -internalize-public-api-list=main -Oz -S < $OUT_FILE.clamped.ll && 
// RUN: echo "----- original optimized -----" &&
// RUN: opt -internalize -internalize-public-api-list=main -Oz  -S < $OUT_FILE.bc &&
// RUN: echo "Automatic check not implemented yet !" && false

extern int hit, other;

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
  int* local_ptr = &other;
  int local_table[8] = {0};
  int local_int = hit;
  // TODO: add passing pointer to middle of table
  return different_types_of_parameters(local_ptr, int_ptr_param, local_table, &int_param, &local_int);
}

int main(void) {
  int *alloca_i32ptr = ext_table;
  int int_table[8];
  *(alloca_i32ptr+3) = 1507; 
  int_table[7] = foo(hit, &alloca_i32ptr[3]);
  alloca_i32ptr = &(int_table[7]);
  *(alloca_i32ptr+3) = 5071 * (int)alloca_i32ptr; 
  return *(alloca_i32ptr+3);
}


