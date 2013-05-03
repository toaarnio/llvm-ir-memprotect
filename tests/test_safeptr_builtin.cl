// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Checking that call is made to safe signature" &&
// RUN: grep "call.*_Z6vload4lPfS_S_"  $OUT_FILE.clamped.ll && grep "call.*_Z7vstore4Dv4_flPfS0_S0_"  $OUT_FILE.clamped.ll

#define IMPLEMENT_SAFE_VLOAD(TYPE, N, DEFAULT)                          \
  TYPE##N _cl_overloadable vload##N(long i, TYPE* cur, TYPE* begin, TYPE* end) { \
    TYPE *ptr = cur + i * N;                                            \
    if (ptr < begin) return (TYPE##N) DEFAULT;                          \
    if (ptr + N > end) return (TYPE##N) DEFAULT;                        \
    return vload##N(i, cur);                                            \
  }

#define IMPLEMENT_SAFE_VSTORE(TYPE, N)                                  \
  void _cl_overloadable vstore##N(TYPE##N x, long i, TYPE* cur, TYPE* begin, TYPE* end) { \
    TYPE *ptr = cur + i * N;                                            \
    if (ptr < begin) return;                                            \
    if (ptr + N > end) return;                                          \
    return vstore##N(x, i, cur);                                        \
  }

#define IMPLEMENT_SAFE_VLOAD_N(N, DEFAULT)                             \
  IMPLEMENT_SAFE_VLOAD (char, N, DEFAULT)                              \
  IMPLEMENT_SAFE_VLOAD (uchar, N, DEFAULT)                             \
  IMPLEMENT_SAFE_VLOAD (short, N, DEFAULT)                             \
  IMPLEMENT_SAFE_VLOAD (ushort, N, DEFAULT)                            \
  IMPLEMENT_SAFE_VLOAD (int, N, DEFAULT)                               \
  IMPLEMENT_SAFE_VLOAD (uint, N, DEFAULT)                              \
  IMPLEMENT_SAFE_VLOAD (long, N, DEFAULT)                              \
  IMPLEMENT_SAFE_VLOAD (ulong, N, DEFAULT)                             \
  IMPLEMENT_SAFE_VLOAD (float, N, DEFAULT) 

#define IMPLEMENT_SAFE_VSTORE_N(N)                             \
  IMPLEMENT_SAFE_VSTORE (char, N)                              \
  IMPLEMENT_SAFE_VSTORE (uchar, N)                             \
  IMPLEMENT_SAFE_VSTORE (short, N)                             \
  IMPLEMENT_SAFE_VSTORE (ushort, N)                            \
  IMPLEMENT_SAFE_VSTORE (int, N)                               \
  IMPLEMENT_SAFE_VSTORE (uint, N)                              \
  IMPLEMENT_SAFE_VSTORE (long, N)                              \
  IMPLEMENT_SAFE_VSTORE (ulong, N)                             \
  IMPLEMENT_SAFE_VSTORE (float, N) 

IMPLEMENT_SAFE_VLOAD_N (2, (0, 0))
IMPLEMENT_SAFE_VLOAD_N (3, (0, 0, 0))
IMPLEMENT_SAFE_VLOAD_N (4, (0, 0, 0, 0))
IMPLEMENT_SAFE_VLOAD_N (8, (0, 0, 0, 0, 0, 0, 0, 0))
IMPLEMENT_SAFE_VLOAD_N (16, (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

IMPLEMENT_SAFE_VSTORE_N (2)
IMPLEMENT_SAFE_VSTORE_N (3)
IMPLEMENT_SAFE_VSTORE_N (4)
IMPLEMENT_SAFE_VSTORE_N (8)
IMPLEMENT_SAFE_VSTORE_N (16)

__kernel void test_kernel(__global float* in, __global float* out) {
  int i = get_global_id(0);
  float4 loaded = vload4(i, in);
  vstore4(loaded, i, out);
}
