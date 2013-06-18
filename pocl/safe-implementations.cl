#include "spir_kernel.h"

/* //IMPLEMENT_SAFE_ASYNC_COPY_FUNCS_ALL() */
IMPLEMENT_FOR_DUMP_ADDRESS()

#ifndef FAKECL
IMPLEMENT_FOR_FLOAT_TYPES_VF_VFVFP(fract)
IMPLEMENT_FOR_FLOAT_TYPES_VF_VFVFP(modf)
#endif
IMPLEMENT_FOR_FLOAT_TYPES_VF_VFVFP(sincos)

// Tn frexp(T x, int *exp)
// Tn frexp(T x, intn *exp)
// T remquo(T x, int T *iptr)
// T remquo(T x, intn T *iptr)
#ifndef FAKECL
IMPLEMENT_FOR_FLOAT_TYPES_VF_VFVIP(frexp)
IMPLEMENT_FOR_FLOAT_TYPES_VF_VFVIP(remquo)
#endif

// Ts lgamma_r(T x, Q int *signp)
// Tn lgamma_r(T x, Q intn *signp)
#ifndef FAKECL
IMPLEMENT_FOR_FLOAT_TYPES_VF_VFVIP(lgamma_r)
#endif

// Ts remquo(T x, T y, int *quo) { }
// Tn remquo(T x, T y, intn *quo) { }
#ifndef FAKECL
IMPLEMENT_FOR_FLOAT_TYPES_VF_VFVFVIP(remquo)
#endif

// Tn vloadn(long, Q T* p)
#ifndef FAKECL
IMPLEMENT_SAFE_VLOAD_N(2, (-42, -42))
IMPLEMENT_SAFE_VLOAD_N(3, (-42, -42, -42))
IMPLEMENT_SAFE_VLOAD_N(4, (-42, -42, -42, -42))
IMPLEMENT_SAFE_VLOAD_N(8, (-42, -42, -42, -42, -42, -42, -42, -42))
IMPLEMENT_SAFE_VLOAD_N(16, (-42, -42, -42, -42, -42, -42, -42, -42, -42, -42, -42, -42, -42, -42, -42, -42))
#endif

// void vstoren(T x, long, Q T* p)
#ifndef FAKECL
IMPLEMENT_SAFE_VSTORE_N(2)
IMPLEMENT_SAFE_VSTORE_N(3)
IMPLEMENT_SAFE_VSTORE_N(4)
IMPLEMENT_SAFE_VSTORE_N(8)
IMPLEMENT_SAFE_VSTORE_N(16)
#endif

IMPLEMENT_SAFE_ATOMIC(int, __global)
IMPLEMENT_SAFE_ATOMIC(uint, __global)
IMPLEMENT_SAFE_ATOMIC(int, __local )
IMPLEMENT_SAFE_ATOMIC(uint, __local)

IMPLEMENT_FOR_T_TYPES_VT_VOLAPTT(float, __global, atomic_xchg);
IMPLEMENT_FOR_T_TYPES_VT_VOLAPTT(float, __local, atomic_xchg);

