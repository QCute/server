#!/bin/bash

## script path
script=$(dirname $0)

make(){
    ## make all(default)
    cd ${script}/../
    erl -make
    erlc +debug_info -o ../beam ../src/debug/user_default.erl
    cd - > /dev/null
}

maker(){
    cd ${script}/../../src/make/
    erl -make
    cd -
}

clean(){
    rm ${script}/../../beam/*
}

## execute function
if [[ $# = 0 ]];then
    make
elif [[ "$1" = "clean" ]];then
    clean
elif [[ "$1" = "maker" ]];then
    maker
elif [[ "$1" = "beam" ]];then
    escript ${script}/../../src/debug/user_default.erl update_include
    erlc +debug_info -o ${script}/../../beam/ ${script}/../../src/debug/user_default.erl
elif [[ "$1" = "protocol" ]];then
    name=$2
    shift 2
    escript ${script}/../../src/make/protocol/protocol_script_${name}.erl $*
elif [[ "$1" = "pt" ]];then
    name=$2
    shift 2
    escript ${script}/../../src/make/protocol/protocol_script_${name}.erl $*
elif [[ "$1" == "unix" ]];then
    IFS=$'\n';
    for FILE in $(grep -rlP "\r" ${script}/../../src/);do
        dos2unix ${FILE}
    done;
    for FILE in $(grep -rlP "\r" ${script}/../../include/);do
        dos2unix ${FILE}
    done;
    for FILE in $(grep -rlP "\r" ${script}/../../script/);do
        dos2unix ${FILE}
    done;
    for FILE in $(grep -rlP "\r" ${script}/../../config/);do
        dos2unix ${FILE}
    done;
elif [[ "$1" == "tab" ]];then
    # replace tab with 4 space
    sed -i "s/\t/    /g" `grep -rlP "\t" ${script}/../../src/` 2> /dev/null
    sed -i "s/\t/    /g" `grep -rlP "\t" ${script}/../../include/` 2> /dev/null
    sed -i "s/\t/    /g" `grep -rlP "\t" ${script}/../../script/` 2> /dev/null
    sed -i "s/\t/    /g" `grep -rlP "\t" ${script}/../../config/` 2> /dev/null
else
    name=$1
    shift
    escript ${script}/../../src/make/script/${name}_script.erl $*
fi