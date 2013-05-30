#!/usr/bin/env bash
#set +x

function show_usage {
    echo "Usage: clean_unused_code.sh <kernel.ll>";
    exit 1;
}

if [ $# -lt 1 ]; then 
    show_usage; 
fi

# hacky way to get list of kernel function names
KERNELS=$(cat $1 | grep metadata | grep "@" | sed -E 's/.*{.*@(.*)}.*/\1/')

#create list of functions
API=""
for i in $KERNELS; do API=$API,$i ;done
API=$(echo $API | cut -b2-)

# mark only kernel functions to be public and run global dce
opt -internalize -internalize-public-api-list=$API -globaldce $1 -S -o $1.cleaned.ll
