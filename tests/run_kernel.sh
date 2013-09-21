#!/usr/bin/env bash
#set -x

#
# Simple tester for kernels.
#

# setup required variables if not set
if [ ! -f "$OCLANG" ]; then
OCLANG="$(dirname $0)/oclang.sh"
echo "OCLANG was not set, setting default to $OCLANG" >&2
fi

function show_usage {
    echo "Usage: run_kernel.sh <kernel.clamped.ll> <kernel_name> <global_work_group_size> \"<arg1>:<arg2>:<arg3>:<arg4>:...\"";
    echo "<kernel.clamped.ll>      LLVM ll/bc file, which has kernel which is going to be ran.";
    echo "<kernel_name>            Function name which will be called for each work_item.";
    echo "<global_work_group_size> Number of work item ids that get_global_id(0) returns.";
    echo "<arg1> ... <argn>        Arguments to pass kernel each argument needs to be in (type,value) format. Like: (__global float*,{0.01,0.02,0.03})";
    echo " ";
    echo "e.g. ./run_kernel.sh run_temp/test_hello_world.cl.clamped.optimized.ll square 5 2 \"(__global float,{1.0f,2.0f,3.0f,4.0f,5.0f}):(int,10):(__global float,{0,0,0,0,0})\""
    echo "";
    exit 1;
}

if [ $# -lt 4 ]; then 
    show_usage; 
fi

kernel_path=$1
shift
kernel_function=$1
shift
work_group_size=$1
shift
arg_list=$@

TOPDIR=$PWD
while [ -n "$TOPDIR" -a ! -d $TOPDIR/pocl ]; do
    TOPDIR="`echo $TOPDIR | sed 's,/[^/]*$,,'`"
done

TEMP_DIR=$(mktemp -d -t krunnerXXXX);
KRUNNER_C=$TEMP_DIR/krunner.c
KRUNNER_BC=$TEMP_DIR/krunner.bc
KRUNNER_LINKED_BC=$TEMP_DIR/krunner_linked.bc
LIBRARY_BC=$TOPDIR/pocl/library.bc

# create get_global_id() implementation
echo "// automatically generated runner for testing kernel." > $KRUNNER_C;
echo "#pragma clang diagnostic ignored \"-Wimplicit-function-declaration\""  >> $KRUNNER_C;
echo "// looks like clang -x cl does not care currently if value in constant address space is modified" >> $KRUNNER_C;
echo "constant uint current_global_id = 0;" >> $KRUNNER_C;
echo "size_t get_global_id(uint dim) { return current_global_id; }" >> $KRUNNER_C;

# create kernel runner main()
IFS=":";
arg_index=0;
echo "int main() {" >> $KRUNNER_C;
echo "  uint workitem_count = $work_group_size;" >> $KRUNNER_C;
kernel_argument_list="";
for arg in $arg_list; do
    type=$(echo ${arg} | sed -E 's@\(([^,]+),.*@\1@');
    # parse initializer from the first comma until arg separator ":"
    initializer=$(echo $arg | sed -E 's@[^,]+,([^\:]+)\).*@\1@');
    not_table=$(echo $initializer | grep -q "{"; echo $?);
    echo "Parameter: $type $initializer" >&2
    if [ "$not_table" == "0" ]; then 
        echo "  $type arg${arg_index}[] = $initializer;"  >> $KRUNNER_C;
    else
        echo "  $type arg${arg_index} = $initializer;"  >> $KRUNNER_C;
    fi
    kernel_argument_list="$kernel_argument_list,arg$arg_index";
    arg_index=$(expr $arg_index + 1);
done
# cut first comma from call
kernel_argument_list=$(echo $kernel_argument_list|cut -c2-);
echo "  for (int i = 0; i < workitem_count; i++) {" >> $KRUNNER_C
echo "    current_global_id = i;" >> $KRUNNER_C
echo "    $kernel_function($kernel_argument_list);" >> $KRUNNER_C
echo "  }" >> $KRUNNER_C
echo "}" >> $KRUNNER_C

BUILDING_RUNKERNEL=1 $OCLANG -o $KRUNNER_BC $KRUNNER_C >&2 &&
llvm-link $LIBRARY_BC  $KRUNNER_BC $kernel_path -o $KRUNNER_LINKED_BC &&
if [ "x$BENCHMARK" = "x1" ]; then
    time lli $KRUNNER_LINKED_BC
else
    lli $KRUNNER_LINKED_BC
fi

ret_val=$?
 
if [ $ret_val -eq 0 ]; then
    rm -f $KRUNNER_C $KRUNNER_BC $KRUNNER_LINKED_BC
    rmdir $TEMP_DIR
else
    echo $@ >&2;
    echo "Failed, ret_val: $ret_val not deleting temp dir: $TEMP_DIR" >&2;
fi

exit $ret_val;
