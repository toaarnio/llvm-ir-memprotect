#!/usr/bin/env bash
#set -x

SCRIPT_PATH=$(pushd `dirname $0` > /dev/null && pwd && popd > /dev/null)

# default our cpu target... SPIR in future...
if [ -z  "$TARGET_FLAGS" ]; then 
TARGET_FLAGS="-include $SCRIPT_PATH/pocl_kernel.h -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64"
fi

if [ "x$USE_CLC" = "x1" ]; then
  if [ -z "$LIBCLC" ]; then 
    TOPDIR=$PWD
    while [ -n "$TOPDIR" -a ! -d $TOPDIR/libclc ]; do
      TOPDIR="`echo $TOPDIR | sed 's,/[^/]*$,,'`"
    done
    if [ -z "$TOPDIR" ]; then
	echo "Cannot find libclc. Set environment variable LIBCLC to point to its source directory."
	exit 1
    fi
    LIBCLC=$TOPDIR/libclc
  fi
  TARGET_FLAGS="-I$LIBCLC/generic/include -include $LIBCLC/generic/include/clc/clc.h -Dcl_clang_storage_class_specifiers"
fi

if [ "x$BUILDING_RUNKERNEL" = x1 ]; then 
    TARGET_FLAGS="-DBUILDING_RUNKERNEL=1 $TARGET_FLAGS"
fi

# Compiles .cl file to llvm IR
echo oclang.sh $@
clang -x cl -fno-builtin $TARGET_FLAGS -emit-llvm -c $@
