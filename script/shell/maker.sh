#!/bin/bash

## script path
script=$(dirname $0)

make(){
    ## make all(default)
    cd ${script}
    erl -make
    cd -
}

maker(){
    ${script}/../../src/make/maker/
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
elif [[ "$1" = "include" ]];then
    escript ${script}/../../src/debug/beam.erl
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