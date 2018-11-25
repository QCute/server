#!/bin/bash

make(){
    ## make all(default)
    erl -make
}

clean(){
    rm ../../beam/*
}

## execute function
if [[ $# = 0 ]];then
    make
elif [[ "$1" = "clean" ]];then
    clean
elif [[ "$1" = "include" ]];then
    escript ../../src/debug/beam.erl
elif [[ "$1" == "unix" ]];then
    IFS=$'\n';
    for FILE in $(find .);do
        dos2unix ${FILE}
    done;
else
    escript ../../src/make/script/$1_script.erl $2
fi