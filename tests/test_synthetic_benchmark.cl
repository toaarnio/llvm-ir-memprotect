// RUN: $OCLANG -DOUTPUT_RESULTS -S -c $TEST_SRC -O0 -emit-llvm -S -o $OUT_FILE.O0.ll &&
// RUN: echo "Running and verifying: synthetic test case on spir target." &&
// RUN: opt -S -O3 $OUT_FILE.O0.ll -o $OUT_FILE.O3.ll &&
// RUN: opt -S -load $CLAMP_PLUGIN -clamp-pointers $OUT_FILE.O0.ll -o $OUT_FILE.O0.clamped.ll &&
// RUN: opt -S -load $CLAMP_PLUGIN -clamp-pointers $OUT_FILE.O3.ll -o $OUT_FILE.O3.clamped.ll &&
// RUN: opt -S -O3 $OUT_FILE.O0.clamped.ll -o $OUT_FILE.O0.clamped.O3.ll &&
// RUN: opt -S -O3 $OUT_FILE.O3.clamped.ll -o $OUT_FILE.O3.clamped.O3.ll &&
// RUN: echo "Validating output:" &&
// RUN: [ $($RUN_KERNEL $OUT_FILE.O0.ll square 1 "(float,{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64}):(int,64):(float,{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64}):(int,64)") == $($RUN_KERNEL $OUT_FILE.O0.clamped.ll square 1 "(float,{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64}):(int,64):(int,64):(float,{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64}):(int,64):(int,64)") ] &&
// RUN: echo "Outputs matched." 

// COMPILE TO PTX: 
// clang -Dcl_clang_storage_class_specifiers -isystem ../../../../libclc/generic/include -include clc/clc.h -target nvptx -xcl test_synthetic_benchmark.cl -emit-llvm -S -O3 -o synthetic_case.ll; llvm-link ../../../../libclc/nvptx--nvidiacl/lib/builtins.bc synthetic_case.ll -o synthetic_case.linked.bc; clang -target nvptx -O3 synthetic_case.linked.bc -S -o synthetic_case.nvptx.s

#define KERNEL_LOOP_COUNT 64

#define PRIVATE_BUFFER_SIZE 128

__kernel void square(__global float* input, const unsigned int input_size,
                     __global float* output, const unsigned int output_size) {

    int range_start = get_global_id(0)*KERNEL_LOOP_COUNT;
    int range_end = range_start+KERNEL_LOOP_COUNT;

    __private float private_buffer[PRIVATE_BUFFER_SIZE];

    for (int i = range_start; i < range_end; i++) {
        __private float orig_input = input[i];
        
        // fill input buffer with orig value related value
        for (int j = 0; j < PRIVATE_BUFFER_SIZE; j++) {
            private_buffer[j] = orig_input + j;
        }

        __private float* forward_iter = &private_buffer[0];
        __private float* reverse_iter = &private_buffer[PRIVATE_BUFFER_SIZE-1]; 
        __private float result = 0;
        __private float temp = 0;
        for (int j = 0; j < PRIVATE_BUFFER_SIZE; j++) {
            temp = *forward_iter;
            result += temp;
            temp = *reverse_iter;
            result -= temp;
            forward_iter++;
            reverse_iter--;
        }

        // after the loop result should be still ~0
        // libclc builtin support was pretty limited...
        // poor man's trunc
        result = (float)((int)(result > 0 ? result : -result));
        result += orig_input;
        output[i] = result*result;
#ifdef OUTPUT_RESULTS
        printf("%f,", output[i]);
#endif
    }
}