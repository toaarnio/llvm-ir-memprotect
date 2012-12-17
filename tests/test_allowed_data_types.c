// RUN: clang -c $TEST_SRC -O0 -emit-llvm -o $OUT_FILE.bc &&
// RUN: echo "Check that supported types compile fine." &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: echo "Creating init for external variables" &&
// RUN: echo "#include <stdint.h>" >> $OUT_FILE.externals.ok.c &&
// RUN: echo "int8_t e_i8 = 1; int16_t e_i16 = 2; int32_t e_i32 = 3;" >> $OUT_FILE.externals.ok.c &&
// RUN: echo "int64_t e_i64 = 4; float e_fs = 10.5; double e_fd = 100.01;" >> $OUT_FILE.externals.ok.c &&
// RUN: echo "int8_t *e_i8_ptr = &e_i8;  int16_t *e_i16_ptr = &e_i16;" >> $OUT_FILE.externals.ok.c && 
// RUN: echo "int32_t *e_i32_ptr = &e_i32;  " >> $OUT_FILE.externals.ok.c &&
// RUN: echo "int64_t *e_i64_ptr = &e_i64;  float *e_fs_ptr = &e_fs;" >> $OUT_FILE.externals.ok.c &&
// RUN: echo "double *e_fd_ptr = &e_fd; " >> $OUT_FILE.externals.ok.c &&
// RUN: echo "int8_t e_i8_table[8] = {10}; int16_t e_i16_table[8] = {20};" >> $OUT_FILE.externals.ok.c &&
// RUN: echo "int32_t e_i32_table[8] = {30}; " >> $OUT_FILE.externals.ok.c &&
// RUN: echo "int64_t e_i64_table[8] = {40}; float e_fs_table[8] = {50.0};" >> $OUT_FILE.externals.ok.c &&
// RUN: echo "double e_fd_table[8] = {70.0}; " >> $OUT_FILE.externals.ok.c &&
//
// RUN: clang -c $OUT_FILE.externals.ok.c -O0 -emit-llvm -o $OUT_FILE.externals.ok.bc &&
// RUN: llvm-link $OUT_FILE.clamped.ll $OUT_FILE.externals.ok.bc -o  $OUT_FILE.clamped.linked.bc &&
// RUN: ( lli $OUT_FILE.clamped.linked.bc;
// RUN:   ( [ $? == 205 ] && echo "OK: running code did result 205 as expected.") ||
// RUN:   ( echo "FAIL: Unexpected return value" && false )
// RUN: )

#include <stdint.h>

extern int8_t e_i8;
extern int16_t e_i16;
extern int32_t e_i32;
extern int64_t e_i64;
extern float e_fs;
extern double e_fd;
extern int8_t *e_i8_ptr;
extern int16_t *e_i16_ptr;
extern int32_t *e_i32_ptr;
extern int64_t *e_i64_ptr;
extern float *e_fs_ptr;
extern double *e_fd_ptr;
extern int8_t e_i8_table[8];
extern int16_t e_i16_table[8];
extern int32_t e_i32_table[8];
extern int64_t e_i64_table[8];
extern float e_fs_table[8];
extern double e_fd_table[8];

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

int main(void) {
  int8_t i8 = e_i8; 
  int16_t i16 = e_i16;
  int32_t i32 = e_i32;
  int64_t i64 = e_i64;
  float fs = e_fs;
  double fd = e_fd;

  int8_t *i8_ptr = e_i8_ptr;
  int16_t *i16_ptr = e_i16_ptr;
  int32_t *i32_ptr = e_i32_ptr;
  int64_t *i64_ptr = e_i64_ptr;
  float *fs_ptr = e_fs_ptr;
  double *fd_ptr = e_fd_ptr;

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
  
  return use_types(i8, i16, i32, i64, fs, fd, 
                   i8_ptr, i16_ptr, i32_ptr, i64_ptr, fs_ptr, fd_ptr, 
                   i8_table, i16_table, i32_table, i64_table, fs_table, fd_table);
}
