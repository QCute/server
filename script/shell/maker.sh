#!/bin/bash

## script path
script=$(dirname $0)

make() {
    ## make all(default)
    cd ${script}/../
    erl -make
    erlc +debug_info -o ../beam ../src/tool/user_default.erl
    cd - > /dev/null
}

maker() {
    cd ${script}/../../src/make/
    erl -make
    cd -
}

clean() {
    rm ${script}/../../beam/*
}

script() {
    name=$1
    shift
    escript ${script}/../../src/make/script/${name}_script.erl $*
}

help() {
    echo "usage: compile all file by default
    clean                                     remove all beam
    maker                                     compile maker
    pt/protocol number                        make protocol file
    excel [xml|table] [filename|tablename]    convert xml/table to table/xml
    record name                               make record file
    sql name [select|join] [all]              make sql file
    data name                                 make base data config file
    log name                                  make log file
    word                                      make sensitive word file
    key [-amount|-type|-prefix]               make active key 
    config                                    make erlang application config interface
    "
}

## execute function
if [[ $# = 0 ]];then
    make
elif [[ "$1" = "clean" ]];then
    clean
elif [[ "$1" = "maker" ]];then
    maker
elif [[ "$1" = "beam" ]];then
    escript ${script}/../../src/debug/script.erl update_include
    erlc +debug_info -o ${script}/../../beam/ ${script}/../../src/tool/user_default.erl
elif [[ "$1" = "protocol" ]];then
    name=$2
    shift 2
    escript ${script}/../../src/make/protocol/protocol_script_${name}.erl $*
elif [[ "$1" = "pt" ]];then
    name=$2
    shift 2
    escript ${script}/../../src/make/protocol/protocol_script_${name}.erl $*
elif [[ "$1" == "unix" ]];then
    # trans dos(CRLF) to unix(LF) format
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
elif [[ "$1" == "excel" ]];then
    script $*
elif [[ "$1" == "record" ]];then
    script $*
elif [[ "$1" == "sql" ]];then
    script $*
elif [[ "$1" == "data" ]];then
    script $*
elif [[ "$1" == "log" ]];then
    script $*
elif [[ "$1" == "word" ]];then
    script $*
elif [[ "$1" == "key" ]];then
    script $*
elif [[ "$1" == "config" ]];then
    script $*
else
    help
fi