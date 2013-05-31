// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: ../clean_unused_code.sh $OUT_FILE.ll && 
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll.cleaned.ll -o $OUT_FILE.clamped.ll &&
// RUN: opt -Oz -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.optimized.ll &&
// RUN: echo "Running optimized webcl kernel with overindexing work group by one" &&
// RUN: $RUN_KERNEL $OUT_FILE.clamped.optimized.ll vector_square 3 "(float4,{(float4)(1.0f,2.0f,3.0f,4.0f),(float4)(5.0f,6.0f,7.0f,8.0f),(float4)(9.0f,10.0f,11.0f,12.0f)}):(int,2):(float4,{(float4)(0,0,0,0),(float4)(0,0,0,0),(float4)(0,0,0,0)}):(int,2)" | grep "1.0,4.0,9.0,16.0,25.0,36.0,49.0,64.0,0.0,0.0,0.0,0.0,"

// this should work, because we are reading values from dynamic limits
// with constant limits we couldnt be 100% sure that 0 is returned when overindexing
__kernel void vector_square(__global float4* input,  __global float4* output) {
  int i = get_global_id(0);
  output[i] = input[i]*input[i];
  float4 f4 = output[i];
  printf("%0.1f,%0.1f,%0.1f,%0.1f,", f4.x, f4.y, f4.z, f4.w);
}

