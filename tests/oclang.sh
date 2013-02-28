#!/bin/sh
#set -x

SCRIPT_PATH=$(pushd `dirname $0` > /dev/null && pwd && popd > /dev/null)
POCL_KERNEL_H=$SCRIPT_PATH/pocl_kernel.h

# Compiles .cl file to llvm IR
echo oclang.sh $@
clang -x cl -include $POCL_KERNEL_H -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -c $@
