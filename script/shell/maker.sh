#!/bin/bash

## script path
script=$(dirname $0)

make(){
    ## make all(default)
    cd ${script}/../
    erl -make
    cd -
}

maker(){
    cd ${script}/../../src/make/maker/
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
    for FILE in $(find ${script}../../);do
        dos2unix ${FILE}
    done;
else
    name=$1
    shift
    escript ${script}/../../src/make/script/${name}_script.erl $*
fi