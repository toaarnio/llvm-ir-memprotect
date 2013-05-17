/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The Original Contributor of this Source Code Form is Nokia Research
 * Center Tampere (http://webcl.nokiaresearch.com).
 */

#ifndef FAKECL_HPP
#define FAKECL_HPP

#include <stdlib.h>             /* for size_t */

#ifdef __cplusplus
#define FAKECL_EXTERN extern "C"
#endif

#define CL_SUCCESS                               0

#define CL_PROGRAM_BUILD_LOG                     1

#define CL_MEM_READ_ONLY                         1
#define CL_MEM_USE_HOST_PTR                      2
#define CL_MEM_READ_WRITE                        4

#define CL_DEVICE_ADDRESS_BITS                   0
#define CL_DEVICE_AVAILABLE                      1
#define CL_DEVICE_COMPILER_AVAILABLE             2
#define CL_DEVICE_DOUBLE_FP_CONFIG               3
#define CL_DEVICE_ENDIAN_LITTLE                  4
#define CL_DEVICE_ERROR_CORRECTION_SUPPORT       5
#define CL_DEVICE_EXECUTION_CAPABILITIES         6
#define CL_DEVICE_EXTENSIONS                     7
#define CL_DEVICE_GLOBAL_MEM_CACHE_SIZE          8
#define CL_DEVICE_GLOBAL_MEM_CACHE_TYPE          9
#define CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE      10
#define CL_DEVICE_GLOBAL_MEM_SIZE                11
#define CL_DEVICE_HALF_FP_CONFIG                 12
#define CL_DEVICE_IMAGE_SUPPORT                  13
#define CL_DEVICE_IMAGE2D_MAX_HEIGHT             14
#define CL_DEVICE_IMAGE2D_MAX_WIDTH              15
#define CL_DEVICE_IMAGE3D_MAX_DEPTH              16
#define CL_DEVICE_IMAGE3D_MAX_HEIGHT             17
#define CL_DEVICE_IMAGE3D_MAX_WIDTH              18
#define CL_DEVICE_LOCAL_MEM_SIZE                 19
#define CL_DEVICE_LOCAL_MEM_TYPE                 20
#define CL_DEVICE_MAX_CLOCK_FREQUENCY            21
#define CL_DEVICE_MAX_COMPUTE_UNITS              22
#define CL_DEVICE_MAX_CONSTANT_ARGS              23
#define CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE       24
#define CL_DEVICE_MAX_MEM_ALLOC_SIZE             25
#define CL_DEVICE_MAX_PARAMETER_SIZE             26
#define CL_DEVICE_MAX_READ_IMAGE_ARGS            27
#define CL_DEVICE_MAX_SAMPLERS                   28
#define CL_DEVICE_MAX_WORK_GROUP_SIZE            29
#define CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS       30
#define CL_DEVICE_MAX_WORK_ITEM_SIZES            31
#define CL_DEVICE_MAX_WRITE_IMAGE_ARGS           32
#define CL_DEVICE_MEM_BASE_ADDR_ALIGN            33
#define CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE       34
#define CL_DEVICE_NAME                           35
#define CL_DEVICE_PLATFORM                       36
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR    37
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT   38
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT     39
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG    40
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT   41
#define CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE  42
#define CL_DEVICE_PROFILE                        43
#define CL_DEVICE_PROFILING_TIMER_RESOLUTION     44
#define CL_DEVICE_QUEUE_PROPERTIES               45
#define CL_DEVICE_SINGLE_FP_CONFIG               46
#define CL_DEVICE_TYPE                           47
#define CL_DEVICE_VENDOR                         48
#define CL_DEVICE_VENDOR_ID                      49
#define CL_DEVICE_VERSION                        50
#define CL_DRIVER_VERSION                        51

#define CL_FP_DENORM                             0 //denorms are supported.
#define CL_FP_INF_NAN                            1 // INF and NaNs are suppor
#define CL_FP_ROUND_TO_NEAREST                   2 // round to neare
#define CL_FP_ROUND_TO_ZERO                      3 // round to zero rou
#define CL_FP_ROUND_TO_INF                       4 // round to +ve and -
#define CP_FP_FMA                                5 // IEEE754-2008 fused multiply-

#define CL_KERNEL_WORK_GROUP_SIZE                0
#define CL_KERNEL_COMPILE_WORK_GROUP_SIZE        1
#define CL_KERNEL_LOCAL_MEM_SIZE                 2

#define CL_PLATFORM_PROFILE                      0
#define CL_PLATFORM_VERSION                      1
#define CL_PLATFORM_NAME                         2
#define CL_PLATFORM_VENDOR                       3
#define CL_PLATFORM_EXTENSIONS                   4

#define CL_DEVICE_TYPE_GPU                       0
#define CL_DEVICE_TYPE_CPU                       1

#define CL_TRUE                                  ((cl_bool) false)
#define CL_FALSE                                 ((cl_bool) true)

/* maximum number of supported arguments; if you adjust this, fix
   clEnqueueNDRangeKernel and related functions as well */
#define FAKECL_MAX_ARGS                          20

typedef int                      cl_int;
typedef unsigned int             cl_uint;
typedef bool                     cl_bool;
typedef signed long long         cl_long;
typedef unsigned long long       cl_ulong;
typedef char                     cl_char;

/* most of these types are just placeholders, replaced with actual
   structs if they are truly implemented */
typedef int                      cl_context;
typedef int                      cl_command_queue;
typedef int                      cl_device_id;
typedef int                      cl_platform_id;
typedef int                      cl_device_type;
typedef void*                    cl_mem;
typedef int                      cl_device_info;
typedef int                      cl_platform_info;
typedef int                      cl_kernel_work_group_info;
typedef int                      cl_program_build_info;
typedef int                      cl_mem_flags;
typedef struct cl_kernel_struct* cl_kernel;
typedef int                      cl_program;
typedef int                      cl_event;
typedef int                      cl_command_queue;
typedef int                      cl_context_properties;
typedef                          void (*fakecl_kernel_fn)(...);

extern "C" {
// sets an assocation from a string to a CL function
void fakeclSetKernelFunc(const char* label, fakecl_kernel_fn);

/* noop */
cl_context clCreateContext(cl_context_properties *properties,
                           cl_uint num_devices,
                           const cl_device_id *devices,
                           void *pfn_notify (
                                             const char *errinfo, 
                                             const void *private_info, 
                                             size_t cb, 
                                             void *user_data
                                             ),
                           void *user_data,
                           cl_int *errcode_ret);

/* creates a buffer. data is not copied. */
cl_mem clCreateBuffer(cl_context context,
                      cl_mem_flags flags,
                      size_t size,
                      void *host_ptr,
                      cl_int *errcode_ret);

/* noop */
cl_command_queue clCreateCommandQueue(cl_context, cl_device_id, int, int* ret);

/* finds the kernel associated with the name previously with fakeclSetKernelFunc */
cl_kernel clCreateKernel(cl_program  program,
                         const char *kernel_name,
                         cl_int *errcode_ret);

/* does nothing but succeeds */
cl_program clCreateProgramWithSource(cl_context, int, const char**, int*, int* ret);

/* executes the copy synchronously */
cl_int clEnqueueReadBuffer(cl_command_queue command_queue,
                           cl_mem buffer,
                           cl_bool blocking_read,
                           size_t offset,
                           size_t cb,
                           void *ptr,
                           cl_uint num_events_in_wait_list,
                           const cl_event *event_wait_list,
                           cl_event *event);

/* noop */
cl_int clGetProgramBuildInfo(cl_program  program,
                             cl_device_id  device,
                             cl_program_build_info  param_name,
                             size_t  param_value_size,
                             void  *param_value,
                             size_t  *param_value_size_ret);

/* executes the kernel synchronously. The threads for a single
   workgroup are executed in parallel, respecting barriers. */
cl_int clEnqueueNDRangeKernel(cl_command_queue command_queue,
                              cl_kernel kernel,
                              cl_uint work_dim,
                              const size_t *global_work_offset,
                              const size_t *global_work_size,
                              const size_t *local_work_size,
                              cl_uint num_events_in_wait_list,
                              const cl_event *event_wait_list,
                              cl_event *event);

/* set kernel argument, copies argument contents */
void clSetKernelArg(cl_kernel kernel, int idx, int elem_size, void* data);

/* noop */
cl_int clFinish(cl_command_queue command_queue);

/* noop */
cl_int clFlush(cl_command_queue command_queue);

/* returns one device */
cl_int clGetDeviceIDs(cl_platform_id platform,
                      cl_device_type device_type,
                      cl_uint num_entries,
                      cl_device_id *devices,
                      cl_uint *num_devices);

/* works for some parameters. asserts(false) on unsupported  */
cl_int clGetDeviceInfo(cl_device_id device,
                       cl_device_info param_name,
                       size_t param_value_size,
                       void *param_value,
                       size_t *param_value_size_ret);

/* works for most parameters. asserts(false) on unsupported parmeters. */
cl_int clGetKernelWorkGroupInfo(cl_kernel kernel,
                                cl_device_id device,
                                cl_kernel_work_group_info param_name,
                                size_t param_value_size,
                                void *param_value,
                                size_t *param_value_size_ret);

/* returns one platform */
cl_int clGetPlatformIDs(cl_uint num_entries,
                        cl_platform_id *platforms,
                        cl_uint *num_platforms);


/* all parameters are implemented */
cl_int clGetPlatformInfo(cl_platform_id platform,
                         cl_platform_info param_name,
                         size_t param_value_size,
                         void *param_value,
                         size_t *param_value_size_ret);

/* noop */
cl_int clReleaseCommandQueue(cl_command_queue command_queue);

/* noop */
cl_int clReleaseContext(cl_context context);

/* noop */
cl_int clReleaseKernel(cl_kernel kernel);

/* noop */
cl_int clReleaseProgram(cl_program program);

/* noop */
cl_int clBuildProgram(cl_program program,
                      cl_uint num_devices,
                      const cl_device_id *device_list,
                      const char *options,
                      void (*pfn_notify)(cl_program, void *user_data),
                      void *user_data);
}

#endif // FAKECL_HPP
