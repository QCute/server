#!/bin/bash

## script path
script=$(dirname $0)

help() {
    echo "usage: compile all file by default
    clean                                     remove all beam
    maker                                     compile maker
    pt/protocol number                        make protocol file
    excel [xml|table] [filename|table name]   convert xml/table to table/xml
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
    ## make all(default)
    cd ${script}/../
    erl -make
    erlc +debug_info -o ../beam ../src/tool/user_default.erl
    cd - > /dev/null
elif [[ "$1" = "clean" ]];then
    rm ${script}/../../beam/*
elif [[ "$1" = "maker" ]];then
    cd ${script}/../../src/make/
    erl -make
    cd -
elif [[ "$1" = "beam" ]];then
    #escript ${script}/../../src/debug/script.erl update_include
    head="-module(user_default).\n-compile(nowarn_export_all).\n-compile(export_all).\n"
    for name in $(ls ${script}/../../include); do 
        head="${head}-include(\"../../include/"${name}"\").\n" 
    done;
    # delete old includes in file directory
    sed -i '/^-module.*\.\|^-compile.*\.\|^-include.*\./d' ${script}/../../src/tool/user_default.erl
    if [[ ! -s ${script}/../../src/tool/user_default.erl ]]; then
        # file was empty, write it covered
       echo -e $head > ${script}/../../src/tool/user_default.erl
    else
        # insert to head
        sed -i '1i'"${head}" ${script}/../../src/tool/user_default.erl
    fi
    # recompile it
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
    # trans dos(CR/LF) to unix(LF) format
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
    shift 1
    escript ${script}/../../src/make/script/excel_script.erl $*
elif [[ "$1" == "record" ]];then
    shift 1
    escript ${script}/../../src/make/script/record_script.erl $*
elif [[ "$1" == "sql" ]];then
    shift 1
    escript ${script}/../../src/make/script/sql_script.erl $*
elif [[ "$1" == "data" ]];then
    shift 1
    escript ${script}/../../src/make/script/data_script.erl $*
elif [[ "$1" == "log" ]];then
    shift 1
    escript ${script}/../../src/make/script/log_script.erl $*
elif [[ "$1" == "word" ]];then
    shift 1
    escript ${script}/../../src/make/script/word_script.erl $*
elif [[ "$1" == "key" ]];then
    shift 1
    escript ${script}/../../src/make/script/key_script.erl $*
elif [[ "$1" == "config" ]];then
    shift 1
    escript ${script}/../../src/make/script/config_script.erl $*
else
    help
fi