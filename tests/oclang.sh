#!/usr/bin/env bash
#set -x

SCRIPT_PATH=$(pushd `dirname $0` > /dev/null && pwd && popd > /dev/null)

# default our cpu target... SPIR in future...
if [ -z  "$TARGET_FLAGS" ]; then 
TARGET_FLAGS="-include $SCRIPT_PATH/pocl_kernel.h -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64"
fi

# Compiles .cl file to llvm IR
echo oclang.sh $@
clang -x cl -fno-builtin $TARGET_FLAGS -emit-llvm -c $@
