// RUN: $OCLANG -c $TEST_SRC -S -O0 -emit-llvm -o $OUT_FILE.ll &&
// RUN: echo "Check that supported types compile fine." &&
// RUN: opt -debug -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: $RUN_KERNEL $OUT_FILE.clamped.ll test_kernel 1 "(char,{10,10,10,10,10,10,10,10}):(int,8):(short,{20,20,20,20,20,20,20,20}):(int,8):(int,{30,30,30,30,30,30,30,30}):(int,8):(long,{40,40,40,40,40,40,40,40}):(int,8):(float,{50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0}):(int,8):(double,{70.0,70.0,70.0,70.0,70.0,70.0,70.0,70.0}):(int,8):(char,1):(short,2):(int,3):(long,4):(float,10.5):(double,100.01)" &&
// RUN: echo "TEST THAT RESULT IS 48 OR SOMETHING LIKE THAT..." && false

typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long int64_t;

int use_types(int8_t i8, int16_t i16, int32_t i32, int64_t i64, 
              float fs, double fd,
              int8_t *i8_ptr, int16_t *i16_ptr, int32_t *i32_ptr, int64_t *i64_ptr,
              float *fs_ptr, double *fd_ptr,
              int8_t i8_table[], int16_t i16_table[], int32_t i32_table[], int64_t i64_table[],
              float fs_table[], double fd_table[]) {
  return 
    i8 + i16 + i32 + i64 + fs + fd + 
    *i8_ptr + *i16_ptr + *i32_ptr + *i64_ptr + *fs_ptr + *fd_ptr +
    i8_table[0] + i16_table[0] + i32_table[0] + i64_table[0] + fs_table[0] + fd_table[0]; 
}

__kernel void test_kernel(
  __global int8_t *e_i8_table, 
  __global int16_t *e_i16_table, 
  __global int32_t *e_i32_table, 
  __global int64_t *e_i64_table,
  __global float *e_fs_table,
  __global double *e_fd_table,
  int8_t e_i8,
  int16_t e_i16,
  int32_t e_i32,
  int64_t e_i64,
  float e_fs,
  double e_fd) { 

  int8_t i8 = e_i8; 
  int16_t i16 = e_i16;
  int32_t i32 = e_i32;
  int64_t i64 = e_i64;
  float fs = e_fs;
  double fd = e_fd;

  int8_t i8_table[8];
  i8_table[0] = e_i8_table[0];
  int16_t i16_table[8];
  i16_table[0] = e_i16_table[0];
  int32_t i32_table[8];
  i32_table[0] = e_i32_table[0];
  int64_t i64_table[8];
  i64_table[0] = e_i64_table[0];
  float fs_table[8];
  fs_table[0] = e_fs_table[0];
  double fd_table[8];
  fd_table[0] = e_fd_table[0]; 

  // external data is not in address space struct and
  // we don't care since we do not support externals in strict mode....
  int8_t *i8_ptr = i8_table;
  int16_t *i16_ptr = i16_table;
  int32_t *i32_ptr = i32_table;
  int64_t *i64_ptr = i64_table;
  float *fs_ptr = fs_table;
  double *fd_ptr = fd_table;

  printf("%i", use_types(i8, i16, i32, i64, fs, fd, i8_ptr, i16_ptr, i32_ptr, i64_ptr, fs_ptr, fd_ptr, i8_table, i16_table, i32_table, i64_table, fs_table, fd_table) );
}
