/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The Original Contributor of this Source Code Form is Nokia Research
 * Center Tampere (http://webcl.nokiaresearch.com).
 */

#include "FakeCL.h"
#include <cstring>
#include <cassert>
#include <pthread.h>
#include <cstdio>
#include <sys/types.h>

#include <map>
#include <string>
#include <vector>

// used for returning values from parameter query functions
#define R(type, value) if (param_value_size_ret) *param_value_size_ret = sizeof(type); if (sizeof(type) <= param_value_size) * (type*) param_value = value; return 0;
#define RS(str) if (param_value_size_ret) *param_value_size_ret = sizeof(str); if (sizeof(str) <= param_value_size) std::memcpy(param_value, str, sizeof(str)); return 0;

const int work_group_size = 64;

namespace {
  std::map<std::string, fakecl_kernel_fn> fakecl_kernel_funcs;
}

pthread_mutex_t barrier_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t barrier_cond = PTHREAD_COND_INITIALIZER;
int barrier_max = work_group_size;
int barrier_enter = 0;          // number of threads waiting in barrier, in range 0..barrier_max
int barrier_completed_seq = 0;

typedef struct {
  void* ptr;
  size_t size;
  bool do_delete;
} cl_mem_info;

struct cl_arg {
  bool is_local;
  std::vector<char> data;
};

struct cl_kernel_struct {
  fakecl_kernel_fn fn;
  int arg_count;
  cl_arg args[FAKECL_MAX_ARGS];
};

namespace {
  std::map<cl_mem, cl_mem_info> cl_mem_data;
}

extern "C" {
void fakeclSetKernelFunc(const char* label, fakecl_kernel_fn fn)
{
  fakecl_kernel_funcs[label] = fn;
}

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
                           cl_int *errcode_ret)
{
  if (errcode_ret) {
    *errcode_ret = CL_SUCCESS;
  }
  return 1;
}

cl_context clCreateContextFromType(cl_context_properties   *properties,
                                   cl_device_type  device_type,
                                   void  (*pfn_notify) (const char *errinfo,
                                                        const void  *private_info,
                                                        size_t  cb,
                                                        void  *user_data),
                                   void  *user_data,
                                   cl_int  *errcode_ret)
{
  if (errcode_ret) {
    *errcode_ret = CL_SUCCESS;
  }
  return 1;
}

cl_int clGetContextInfo(cl_context context,
                        cl_context_info param_name,
                        size_t param_value_size,
                        void *param_value,
                        size_t* param_value_size_ret)
{
  switch (param_name) {
  case CL_CONTEXT_REFERENCE_COUNT: R(cl_uint, 1);
  case CL_CONTEXT_DEVICES:         R(cl_uint, 0);
  case CL_CONTEXT_PROPERTIES:      assert(false);
  }
  assert(false);
}

cl_command_queue clCreateCommandQueue(cl_context, cl_device_id, int, int* ret)
{
  if (ret) {
    *ret = CL_SUCCESS;
  }
  return 1;
}

cl_mem clCreateBuffer(cl_context context,
                      cl_mem_flags flags,
                      size_t size,
                      void *host_ptr,
                      cl_int *errcode_ret)
{
  cl_mem_info m;
  if (!host_ptr) {
    host_ptr = (void*) new char*[size];
    m.do_delete = true;
  } else {
    m.do_delete = false;
  }
  m.ptr = host_ptr;
  m.size = size;
  if (errcode_ret) {
    *errcode_ret = CL_SUCCESS;
  }
  cl_mem_data[host_ptr] = m;
  return host_ptr;
}

cl_int clReleaseMemObject(cl_mem memobj)
{
  assert(cl_mem_data.count(memobj));
  cl_mem_info& info = cl_mem_data[memobj];
  if (info.do_delete) {
    delete[] (char*) info.ptr;
  }
  cl_mem_data.erase(memobj);
}

cl_int clEnqueueWriteBuffer(cl_command_queue command_queue,
                            cl_mem buffer,
                            cl_bool blocking_write,
                            size_t offset,
                            size_t cb,
                            const void *ptr,
                            cl_uint num_events_in_wait_list,
                            const cl_event *event_wait_list,
                            cl_event *event)
{
  if (ptr != (char*) buffer + offset) {
    std::memcpy((char*) buffer + offset, ptr, cb);
  }
  return CL_SUCCESS;
}

cl_int clEnqueueReadBuffer(cl_command_queue command_queue,
                           cl_mem buffer,
                           cl_bool blocking_read,
                           size_t offset,
                           size_t cb,
                           void *ptr,
                           cl_uint num_events_in_wait_list,
                           const cl_event *event_wait_list,
                           cl_event *event)
{
  if (ptr != (char*) buffer + offset) {
    std::memcpy(ptr, (char*) buffer + offset, cb);
  }
  return CL_SUCCESS;
}

cl_kernel clCreateKernel (cl_program  program,
                          const char *kernel_name,
                          cl_int *errcode_ret)
{
  cl_kernel k = new cl_kernel_struct;
  k->fn = fakecl_kernel_funcs[kernel_name];
  assert(k->fn);
  k->arg_count = 0;
  if (errcode_ret) {
    *errcode_ret = CL_SUCCESS;
  }
  return k;
}

cl_program clCreateProgramWithSource(cl_context context,
                                     cl_uint count,
                                     const char **strings,
                                     const size_t *lengths,
                                     cl_int *errcode_ret)
{
  if (*errcode_ret) {
    *errcode_ret = CL_SUCCESS;
  }
  return 1;
}

cl_int clBuildProgram(cl_program program,
                      cl_uint num_devices,
                      const cl_device_id *device_list,
                      const char *options,
                      void (*pfn_notify)(cl_program, void *user_data),
                      void *user_data)
{
  return CL_SUCCESS;
}

cl_int clGetProgramBuildInfo(cl_program  program,
                             cl_device_id  device,
                             cl_program_build_info  param_name,
                             size_t  param_value_size,
                             void  *param_value,
                             size_t  *param_value_size_ret)
{
  switch (param_name) {
  case CL_PROGRAM_BUILD_LOG: RS("");
  }
}

namespace {
  struct EnqeueuKernelInfo {
    cl_kernel_struct* kernel;
    const size_t* local_work_size;
    int group_id;
    int local_id;
  };

  pthread_key_t thread_info_key;
  pthread_once_t thread_info_key_once;

  void thread_info_key_init()
  {
    pthread_key_create(&thread_info_key, NULL);
  }

  void* getClMemArg(cl_arg& arg)
  {
    if (arg.data.size() == sizeof(cl_mem)) {
      cl_mem* m = *(cl_mem**) &*arg.data.begin();
      if (cl_mem_data.count(m)) {
        return cl_mem_data.find(m)->second.ptr;
      } else {
        return 0;
      }
    } else {
      return 0;
    }
  }

  void* argPtr(cl_arg& arg)
  {
    void* p = getClMemArg(arg);
    if (!p) {
      if (arg.is_local) {
        p = &*arg.data.begin();
      } else if (arg.data.size()==sizeof(int)) {
        p = *(void**) &*arg.data.begin();
      } else {
        assert(false);
      }
    } else {
    }
    return p;
  }

  void* clthread(void* opaque)
  {
    EnqeueuKernelInfo* info = static_cast<EnqeueuKernelInfo*>(opaque);
    pthread_setspecific(thread_info_key, opaque);

    cl_arg* a = info->kernel->args;
    //#define A(n) a[n].elem_size, a[n].elem_count, a[n].data
#define A(n)                                                            \
    argPtr(a[n])

    cl_kernel_struct* k = info->kernel;

    switch (k->arg_count) {
    case  0: k->fn(); break;
    case  1: k->fn(A(0)); break;
    case  2: k->fn(A(0), A(1)); break;
    case  3: k->fn(A(0), A(1), A(2)); break;
    case  4: k->fn(A(0), A(1), A(2), A(3)); break;
    case  5: k->fn(A(0), A(1), A(2), A(3), A(4)); break;
    case  6: k->fn(A(0), A(1), A(2), A(3), A(4), A(5)); break;
    case  7: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6)); break;
    case  8: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7)); break;
    case  9: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8)); break;
    case 10: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9)); break;
    case 11: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10)); break;
    case 12: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10), A(11)); break;
    case 13: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10), A(11), A(12)); break;
    case 14: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10), A(11), A(12), A(13)); break;
    case 15: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10), A(11), A(12), A(13), A(14)); break;
    case 16: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10), A(11), A(12), A(13), A(14), A(15)); break;
    case 17: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10), A(11), A(12), A(13), A(14), A(15), A(16)); break;
    case 18: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10), A(11), A(12), A(13), A(14), A(15), A(16), A(17)); break;
    case 19: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10), A(11), A(12), A(13), A(14), A(15), A(16), A(17), A(18)); break;
    case 20: k->fn(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9), A(10), A(11), A(12), A(13), A(14), A(15), A(16), A(17), A(18), A(19)); break;
    default: assert(false);
    }
#undef A
    return NULL;
  }

}

extern "C"
size_t get_global_id()
{
  EnqeueuKernelInfo* info = static_cast<EnqeueuKernelInfo*>(pthread_getspecific(thread_info_key));
  return info->group_id * info->local_work_size[0] + info->local_id;
}

extern "C"
size_t get_group_id()
{
  EnqeueuKernelInfo* info = static_cast<EnqeueuKernelInfo*>(pthread_getspecific(thread_info_key));
  return info->group_id;
}

extern "C"
size_t get_local_id()
{
  EnqeueuKernelInfo* info = static_cast<EnqeueuKernelInfo*>(pthread_getspecific(thread_info_key));
  return info->local_id;
}

extern "C"
size_t get_local_size(cl_uint x)
{
  EnqeueuKernelInfo* info = static_cast<EnqeueuKernelInfo*>(pthread_getspecific(thread_info_key));
  return info->local_work_size[x];
}

namespace {
  size_t optimal_work_size(size_t work_group_size, size_t global_work_size)
  {
    int size = work_group_size;
    while (size > 1 && global_work_size % size != 0) {
      --size;
    }
    return size;
  }
}

cl_int clEnqueueNDRangeKernel(cl_command_queue command_queue,
                              cl_kernel kernel,
                              cl_uint work_dim,
                              const size_t *global_work_offset,
                              const size_t *global_work_size,
                              const size_t *local_work_size,
                              cl_uint num_events_in_wait_list,
                              const cl_event *event_wait_list,
                              cl_event *event)
{
  size_t temp_local_work_size;

  assert(event_wait_list == NULL);
  assert(event == NULL);
  assert(work_dim == 1);
  assert(global_work_size);
  assert(global_work_offset == NULL);
  if (!local_work_size) {
    local_work_size = &temp_local_work_size;
    temp_local_work_size = optimal_work_size(work_group_size, *global_work_size);
  }
  assert(*local_work_size <= work_group_size);
  assert(num_events_in_wait_list == 0);

  pthread_once(&thread_info_key_once, thread_info_key_init);

  pthread_mutex_lock(&barrier_mutex);
  barrier_max = *local_work_size;
  pthread_mutex_unlock(&barrier_mutex);

  pthread_t threads[work_group_size];
  EnqeueuKernelInfo infos[work_group_size];

  //printf("Local work size: %d\n", *local_work_size);
  //printf("Global work size: %d\n", *global_work_size);
  for (int group_id = 0;
       group_id < (*global_work_size + *local_work_size - 1) / *local_work_size * *local_work_size;
       group_id += *local_work_size) {
    //printf("Processing starting at group %d %d\n", group_id, *local_work_size);
    for (int tid = 0; tid < *local_work_size; ++tid) {
      infos[tid].kernel = kernel;
      infos[tid].local_work_size = local_work_size;
      infos[tid].group_id = group_id / *local_work_size;
      infos[tid].local_id = tid;
      pthread_create(&threads[tid], NULL, clthread, infos + tid);
    }
    for (int tid = 0; tid < *local_work_size; ++tid) {
      pthread_join(threads[tid], NULL);
    }
  }

  return CL_SUCCESS;
}

void clSetKernelArg(cl_kernel kernel, int idx, int elem_size, void* data)
{
  if (kernel->arg_count < idx + 1) {
    kernel->arg_count = idx + 1;
    assert(kernel->arg_count <= FAKECL_MAX_ARGS);
  }
  cl_arg& arg = kernel->args[idx];
  arg.is_local = data == 0;
  if (data) {
    arg.data = std::vector<char>((char*) data, (char*) data + elem_size);
  } else {
    arg.data.resize(elem_size);
  }
}

cl_int clFinish(cl_command_queue)
{
  return CL_SUCCESS;
}

cl_int clFlush(cl_command_queue)
{
  return CL_SUCCESS;
}

cl_int clGetDeviceIDs(cl_platform_id platform,
                      cl_device_type device_type,
                      cl_uint num_entries,
                      cl_device_id *devices,
                      cl_uint *num_devices)
{
  if (devices && num_entries > 0) {
    devices[0] = 0;
    if (num_devices) {
      *num_devices = 1;
    }
  }
  return CL_SUCCESS;
}


cl_int clGetDeviceInfo(cl_device_id device,
                       cl_device_info param_name,
                       size_t param_value_size,
                       void *param_value,
                       size_t *param_value_size_ret)
{
  switch (param_name) {
  case CL_DEVICE_ADDRESS_BITS                  : R(cl_uint, 64);
  case CL_DEVICE_AVAILABLE                     : R(cl_bool, true);
  case CL_DEVICE_COMPILER_AVAILABLE            : R(cl_bool, true); // a lie
  case CL_DEVICE_DOUBLE_FP_CONFIG              : assert(false);
  case CL_DEVICE_ENDIAN_LITTLE                 : assert(false);
  case CL_DEVICE_ERROR_CORRECTION_SUPPORT      : assert(false);
  case CL_DEVICE_EXECUTION_CAPABILITIES        : assert(false);
  case CL_DEVICE_EXTENSIONS                    : assert(false);
  case CL_DEVICE_GLOBAL_MEM_CACHE_SIZE         : assert(false);
  case CL_DEVICE_GLOBAL_MEM_CACHE_TYPE         : assert(false);
  case CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE     : assert(false);
  case CL_DEVICE_GLOBAL_MEM_SIZE               : R(cl_uint, 2<<20);
  case CL_DEVICE_HALF_FP_CONFIG                : assert(false);
  case CL_DEVICE_IMAGE_SUPPORT                 : assert(false);
  case CL_DEVICE_IMAGE2D_MAX_HEIGHT            : assert(false);
  case CL_DEVICE_IMAGE2D_MAX_WIDTH             : assert(false);
  case CL_DEVICE_IMAGE3D_MAX_DEPTH             : assert(false);
  case CL_DEVICE_IMAGE3D_MAX_HEIGHT            : assert(false);
  case CL_DEVICE_IMAGE3D_MAX_WIDTH             : assert(false);
  case CL_DEVICE_LOCAL_MEM_SIZE                : R(cl_uint, 2<<20);
  case CL_DEVICE_LOCAL_MEM_TYPE                : assert(false);
  case CL_DEVICE_MAX_CLOCK_FREQUENCY           : R(cl_uint, 1);
  case CL_DEVICE_MAX_COMPUTE_UNITS             : R(cl_uint, work_group_size);
  case CL_DEVICE_MAX_CONSTANT_ARGS             : assert(false);
  case CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE      : assert(false);
  case CL_DEVICE_MAX_MEM_ALLOC_SIZE            : assert(false);
  case CL_DEVICE_MAX_PARAMETER_SIZE            : assert(false);
  case CL_DEVICE_MAX_READ_IMAGE_ARGS           : assert(false);
  case CL_DEVICE_MAX_SAMPLERS                  : assert(false);
  case CL_DEVICE_MAX_WORK_GROUP_SIZE           : assert(false);
  case CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS      : assert(false);
  case CL_DEVICE_MAX_WORK_ITEM_SIZES           : assert(false);
  case CL_DEVICE_MAX_WRITE_IMAGE_ARGS          : assert(false);
  case CL_DEVICE_MEM_BASE_ADDR_ALIGN           : assert(false);
  case CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE      : assert(false);
  case CL_DEVICE_NAME                          : RS("FakeCL");
  case CL_DEVICE_PLATFORM                      : assert(false);
  case CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR   : assert(false);
  case CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT  : assert(false);
  case CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT    : assert(false);
  case CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG   : assert(false);
  case CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT  : assert(false);
  case CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE : assert(false);
  case CL_DEVICE_PROFILE                       : assert(false);
  case CL_DEVICE_PROFILING_TIMER_RESOLUTION    : assert(false);
  case CL_DEVICE_QUEUE_PROPERTIES              : assert(false);
  case CL_DEVICE_SINGLE_FP_CONFIG              : assert(false);
  case CL_DEVICE_TYPE                          : assert(false);
  case CL_DEVICE_VENDOR                        : RS("NRC");
  case CL_DEVICE_VENDOR_ID                     : assert(false);
  case CL_DEVICE_VERSION                       : RS("1.1");
  case CL_DRIVER_VERSION                       : RS("0.1");
  } 
  return CL_SUCCESS;
}

cl_int clGetKernelWorkGroupInfo(cl_kernel kernel,
                                cl_device_id device,
                                cl_kernel_work_group_info param_name,
                                size_t param_value_size,
                                void *param_value,
                                size_t *param_value_size_ret)
{
  switch (param_name) {
  case CL_KERNEL_WORK_GROUP_SIZE: R(size_t, work_group_size);
  case CL_KERNEL_COMPILE_WORK_GROUP_SIZE: assert(false);
  case CL_KERNEL_LOCAL_MEM_SIZE: R(cl_ulong, ~0ull);
  }
  return CL_SUCCESS;
}

cl_int clGetPlatformIDs(cl_uint num_entries,
                        cl_platform_id *platforms,
                        cl_uint *num_platforms)
{
  if (num_entries > 0 && platforms) {
    platforms[0] = 0;
    if (num_platforms) {
      *num_platforms = 1;
    }
  }
  return CL_SUCCESS;
}

cl_int clGetPlatformInfo(cl_platform_id platform,
                         cl_platform_info param_name,
                         size_t param_value_size,
                         void *param_value,
                         size_t *param_value_size_ret)
{
  switch (param_name) {
  case CL_PLATFORM_PROFILE: RS("FULL_PROFILE");
  case CL_PLATFORM_VERSION: RS("1.0")
  case CL_PLATFORM_NAME: RS("FakeCL");
  case CL_PLATFORM_VENDOR: RS("NRC");
  case CL_PLATFORM_EXTENSIONS : RS("");
  }
  return CL_SUCCESS;
}

cl_int clReleaseCommandQueue(cl_command_queue command_queue)
{
  return CL_SUCCESS;
}

cl_int clReleaseContext(cl_context context)
{
  return CL_SUCCESS;
}

cl_int clReleaseKernel(cl_kernel kernel)
{
  delete kernel;
  return CL_SUCCESS;
}

cl_int clReleaseProgram(cl_program program)
{
  return CL_SUCCESS;
}

extern "C" void barrier(int)
{
  pthread_mutex_lock(&barrier_mutex);
  int start_seq = barrier_completed_seq;
  ++barrier_enter;
  if (barrier_enter != barrier_max) {
    while (barrier_completed_seq == start_seq) {
      pthread_cond_wait(&barrier_cond, &barrier_mutex);
    }
  } else {
    // if we are the last one to enter, notify everyone
    //printf("---\n"); fflush(stdout);
    ++barrier_completed_seq;
    barrier_enter = 0;
    pthread_cond_broadcast(&barrier_cond);
  }
  pthread_mutex_unlock(&barrier_mutex);
}

} // extern "C"
