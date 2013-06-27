
__constant float4 baseFactor = (float4)(1.0f,2.0f,3.0f,4.0f);

void init_scratch(
    size_t gid, size_t wgid,
    __global float4* input, __constant float4* factors, __local float4* scratch) {
    scratch[wgid] = input[gid]*factors[gid];
} 

__local float4* flip_to_awesomeness(size_t wgid, size_t wgsize, __local float4* scratch) {
    float4 index_vec = scratch[wgid];
    float index_float = index_vec.x + index_vec.y + index_vec.z + index_vec.w;
    int index = (int)(index_float) % wgsize;
    return &scratch[index];
}

__kernel void awesomize(
    __global float4* input,  
    __global float4* output,
    __constant float4* factors,
    __local float4* scratch) {

    size_t gid = get_global_id(0);
    size_t wgid = get_local_id(0);
    size_t wgsize = get_local_size (0);

    __local size_t lotteryWinner;

    init_scratch(gid, wgid, wgsize, input, factors, scratch);
    lotteryWinner = gid;
    barrier(CLK_LOCAL_MEM_FENCE);

    if (gid == 0) {
        output[0] = lotteryWinner;
    } else {
        output[gid] = (*flip_to_awesomeness(wgid, wgsize, scratch))*baseFactor;
    }
}

// FULL Translated ----------> 

typedef struct {
    float4 baseFactor;
} ConstantAllocations;

typedef struct {
    size_t lotteryWinner;
} LocalAllocations;

typedef struct {
    size_t awesomize__gid;
    size_t awesomize__wgid;
    size_t awesomize__wgsize;
    float4 flip_to_awesomeness__index_vec;
    float  flip_to_awesomeness__index_float;
    int    flip_to_awesomeness__index;
} PrivateAllocations;

// TODO: fix representation to be correct
typedef struct {
    // probably this is not needed here.. we can refer directly to struct 
    // where ever it is used
    __constant ConstantAllocations *constantAllocations_min, *constantAllocations_max;
    __constant float4 *factors_min, *factors_max;
} ConstantLimits;

typedef struct {
    __global float4 *input_min, *input_max;
    __global float4 *output_min, *output_max;
} GlobalLimits;

typedef struct {
    __local LocalAllocations *localAllocations_min, *localAllocations_max;
    __local float4 *scratch_min, *scratch_max;
} LocalLimits;
 
typedef struct {
    ConstantLimits     constantLimits;
    GlobalLimits       globalLimits;
    LocalLimits        localLimits;
    // this is used also to actually store private memory since 
    // that is always unfragmented
    PrivateAllocations privateAllocations;
} ProgramAllocations;

// all initialized values in global scope must be in constant address space
__constant ConstantAllocations constantAllocations = {
    (float4)(1.0f,2.0f,3.0f,4.0f)
};

// local address space is here because in LLVM IR variables in local address
// space are promoted to global scope
__local LocalAllocations localAllocations = {0};

// startup code to collect all limits and allocate private memory
__kernel void awesomize(
    __global float4* input, size_t input_count, 
    __global float4* output, size_t output_count,
    __constant float4* factors, size_t factors_count,
    __local float4* scratch, size_t scratch_count) {

    ProgramAllocations pa;
    pa.constantLimits.constantAllocations_min = &constantAllocations;
    pa.constantLimits.constantAllocations_max = &constantAllocations + 1;
    pa.constantLimits.factors_min = &factors[0];
    pa.constantLimits.factors_max = &factors[factors_count];

    pa.globalLimits.input_min = &input[0];
    pa.globalLimits.input_max = &input[input_count];
    pa.globalLimits.output_min = &output[0];
    pa.globalLimits.output_max = &output[output_count];

    pa.localLimits.localAllocations_min = &localAllocations;
    pa.localLimits.localAllocations_max = &localAllocations + 1;
    pa.localLimits.scratch_min = &scratch[0];
    pa.localLimits.scratch_max = &scratch[scratch_count];

    // syntax simplified to prevent too verbose struct definition and creation
    awesomize_original_kernel(
        &pa, 
        {input, pa.globalLimits.input_min, pa.globalLimits.input_max}, 
        {output, pa.globalLimits.output_min, pa.globalLimits.output_max},
        {factors, pa.constantLimits.factors_min, pa.constantLimits.factors_max},
        {scratch, pa.localLimits.scratch_min, pa.localLimits.scratch_max}
    );
}

void init_scratch(
    ProgramAllocations *pa,
    size_t gid, size_t wgid,
    SafeGlobalFloat4 input, SafeConstantFloat4 factors, SafeLocalFloat4 scratch) {

    // original scratch[wgid] = input[gid]*factors[gid];

    write_safely(scratch, wgid, scratch.min, scratch.max, 
        read_safely(input.ptr, gid, input.min, input.max) *
        read_safely(factors.ptr, gid, factors.min, factors.max);
} 

__local float4* flip_to_awesomeness(
    ProgramAllocations *pa,
    size_t wgid, size_t wgsize, SafeLocalFloat4 scratch) {
    
    pa->privateAllocations.flip_to_awesomeness__index_vec = 
        read_safely(scratch.ptr, wgid, scratch.min, scratch.max);

    pa->privateAllocations.flip_to_awesomeness__index_float = 
        pa->privateAllocations.flip_to_awesomeness__index_vec.x + 
        pa->privateAllocations.flip_to_awesomeness__index_vec.y + 
        pa->privateAllocations.flip_to_awesomeness__index_vec.z + 
        pa->privateAllocations.flip_to_awesomeness__index_vec.w;

    pa->privateAllocations.flip_to_awesomeness__index = 
        (int)(pa->privateAllocations.flip_to_awesomeness__index_float) % wgsize;

    // no check needed because we are not accessing memory yet, just getting address
    return &scratch.ptr[index];
}

void awesomize_original_kernel(
    ProgramAllocations *pa,
    SafeGlobalFloat4   input,
    SafeGlobalFloat4   output,
    SafeConstantFloat4 factors,
    SafeLocalFloat4    scratch) {

    pa->privateAllocations.awesomize__gid = get_global_id(0);
    pa->privateAllocations.awesomize__wgid = get_local_id(0);
    pa->privateAllocations.awesomize__wgsize = get_local_size(0);
   
    init_scratch(
        pa->privateAllocations.awesomize__gid, 
        pa->privateAllocations.awesomize__wgid, 
        pa->privateAllocations.awesomize__wgsize, 
        input, factors, scratch);

    // for local memory replacement is done directly against struct in global scope 
    localAllocations.lotteryWinner = pa->privateAllocations.awesomize__gid;
    barrier(CLK_LOCAL_MEM_FENCE);

    if (pa->privateAllocations.awesomize__gid == 0) {
        write_safely(output.ptr, 0, output.min, output.max, localAllocations.lotteryWinner);
    } else {
        // here we were not able to know exact limit to which fragment pointer belongs, so for read
        // we need to compare all limits
        write_safely(output.ptr, gid, output.min, output.max, 
            read_safely(flip_to_awesomeness(wgid, wgsize, scratch), 0, pa->localLimits) * 
            constantAllocations.baseFactor);
    }
}


// OPTIMIZED translation -------->

typedef struct {
    __constant float4 *factors_min, *factors_max;
} ConstantLimits;

typedef struct {
    __global float4 *input_min, *input_max;
    __global float4 *output_min, *output_max;
} GlobalLimits;

typedef struct {
    __local float4 *scratch_min, *scratch_max;
} LocalLimits;
 
typedef struct {
    ConstantLimits     constantLimits;
    GlobalLimits       globalLimits;
    LocalLimits        localLimits;
} ProgramAllocations;

__constant float4 baseFactor = (float4)(1.0f,2.0f,3.0f,4.0f);

// startup code to collect all limits and allocate private memory
__kernel void awesomize(
    __global float4* input, size_t input_count, 
    __global float4* output, size_t output_count,
    __constant float4* factors, size_t factors_count,
    __local float4* scratch, size_t scratch_count) {

    ProgramAllocations pa;
    pa.constantLimits.factors_min = &factors[0];
    pa.constantLimits.factors_max = &factors[factors_count];
    pa.globalLimits.input_min = &input[0];
    pa.globalLimits.input_max = &input[input_count];
    pa.globalLimits.output_min = &output[0];
    pa.globalLimits.output_max = &output[output_count];
    pa.localLimits.scratch_min = &scratch[0];
    pa.localLimits.scratch_max = &scratch[scratch_count];

    // syntax simplified to prevent too verbose struct definition and creation
    awesomize_original_kernel(
        &pa, 
        {input, pa.globalLimits.input_min, pa.globalLimits.input_max}, 
        {output, pa.globalLimits.output_min, pa.globalLimits.output_max},
        {factors, pa.constantLimits.factors_min, pa.constantLimits.factors_max},
        {scratch, pa.localLimits.scratch_min, pa.localLimits.scratch_max}
    );
}

void init_scratch(
    ProgramAllocations *pa,
    size_t gid, size_t wgid,
    SafeGlobalFloat4 input, SafeConstantFloat4 factors, SafeLocalFloat4 scratch) {

    // original: scratch[wgid] = input[gid]*factors[gid];
    write_safely(scratch, wgid, scratch.min, scratch.max, 
        read_safely(input.ptr, gid, input.min, input.max) *
        read_safely(factors.ptr, gid, factors.min, factors.max);
} 

__local float4* flip_to_awesomeness(
    ProgramAllocations *pa,
    size_t wgid, size_t wgsize, SafeLocalFloat4 scratch) {

    float4 index_vec = read_safely(scratch.ptr, wgid, scratch.min, scratch.max);
    float index_float = index_vec.x + index_vec.y + index_vec.z + index_vec.w;
    int index = (int)(index_float) % wgsize;
    
    // no check needed because we are not accessing memory yet, just getting address
    return &scratch.ptr[index];
}

void awesomize_original_kernel(
    ProgramAllocations *pa,
    SafeGlobalFloat4   input,
    SafeGlobalFloat4   output,
    SafeConstantFloat4 factors,
    SafeLocalFloat4    scratch) {

    size_t gid = get_global_id(0);
    size_t wgid = get_local_id(0);
    size_t wgsize = get_local_size(0);
   
    init_scratch(gid, wgid, wgsize, input, factors, scratch);

    __local size_t lotteryWinner = gid;
    barrier(CLK_LOCAL_MEM_FENCE);

    if (gid == 0) {
        write_safely(output.ptr, 0, output.min, output.max, lotteryWinner);
    } else {
        write_safely(output.ptr, gid, output.min, output.max, 
            read_safely(flip_to_awesomeness(wgid, wgsize, scratch), 0, 
                        pa->localLimits.scratch_min, pa->LocalLimits.scratch_max) * baseFactor);
    }
}

