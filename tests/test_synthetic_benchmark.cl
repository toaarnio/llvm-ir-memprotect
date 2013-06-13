// RUN: echo "Requires test scripts. And clamp seems to fail if input is compiled with O0." && false &&
// RUN: clang -Dcl_clang_storage_class_specifiers -isystem ../../../../libclc/generic/include -include clc/clc.h -target nvptx -xcl test_synthetic_benchmark.cl -emit-llvm -S -O3 -o synthetic_case.ll; llvm-link ../../../../libclc/nvptx--nvidiacl/lib/builtins.bc synthetic_case.ll -o synthetic_case.linked.bc; clang -target nvptx -O3 synthetic_case.linked.bc -S -o synthetic_case.nvptx.s

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
    }
}