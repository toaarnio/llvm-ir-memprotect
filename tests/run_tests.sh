#!/usr/bin/env bash

#set -x

[ -z "$GCOV" ] && GCOV=gcov
current_dir=$(pwd)
temp_dir=$current_dir/run_temp
mkdir -p $temp_dir

if [ -z "$CLAMP_PLUGIN" ]; then
    echo "CLAMP_PLUGIN variable must be set to point the loadable plugin module (absolute path)"
    exit 1;
fi

if [ -r $CLAMP_PLUGIN ]; then
    # make sure that plugin is accessible from run_temp path too
    cd $temp_dir;
    if [ ! -r $CLAMP_PLUGIN ]; then
        echo "CLAMP_PLUGIN $CLAMP_PLUGIN must be absolute path.";
        exit 1;
    fi
else
    echo "CLAMP_PLUGIN $CLAMP_PLUGIN was not found";
    exit 1;
fi


cd $current_dir;

export OCLANG=$PWD/oclang.sh
export RUN_KERNEL=$PWD/run_kernel.sh

failed_tests=""

# if test file not given in $1 then find the tests
if [ -z $1 ]; then
    tests=$(ls -1 test_*.c test_*.ll test_*.cl 2> /dev/null);
else
    tests=$1;
fi

function get_run_command {
    file_name=$1
    grep -E "(//|;)\s*RUN:" $file_name | sed -E "s@.+RUN:[* ]*(.+)@\1@"
}

function run_test {
    echo "########################### ------------- Running $1 ..."
    cd $temp_dir;
    TEST_SRC=$current_dir/$1;
    OUT_FILE=$temp_dir/$1;
    if [ -r ../../Debug+Coverage+Asserts/ClampPointers.gcno ]; then
	( cd ../../Debug+Coverage+Asserts/ && rm -f *.gcda )
    fi
    test_command=$(get_run_command $TEST_SRC);
    if eval $test_command; then
        echo ">>>>>>>>>> OK";
    else
        echo "TEST FAILED! intermediate files should be found in run_temp directory";
        echo $test_command
        echo ">>>>>>>>>> !!! !!! FAIL";
        failed_tests="$failed_tests $1";
    fi
    if [ -r ../../Debug+Coverage+Asserts/ClampPointers.gcno ]; then
	(   ORIGPWD="$PWD"
            cd ../../Debug+Coverage+Asserts/ &&
	    cp -a ../ClampPointers.cpp . &&
	    "$GCOV" ClampPointers.cpp >/dev/null &&
	    cp -v ClampPointers.cpp.gcov "$ORIGPWD/../$test_file".gcov )
    fi
}

# cleanup old files and run the tests
rm -f $temp_dir/*
for test_file in $tests;do
    run_test $test_file;
done;

if [ ! -z "$failed_tests" ]; then
    echo "###################### FAIL ###################################";
    echo "## Failed tests: $failed_tests";
else
    echo "###################### ALL GOOD ###############################";    
fi
