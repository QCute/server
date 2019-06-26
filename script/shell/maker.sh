#!/bin/bash

## script path
script=$(dirname $0)

helps() {
    echo "usage: compile all file by default
    clean                                     remove all beam
    maker                                     compile maker
    now                                       append now to update sql script
    need date(Y-M-D)                          cut from date(start) to now(end), write to need sql script
    pt/protocol number                        make protocol file
    excel [xml|table] [filename|table name]   convert xml/table to table/xml
    record name                               make record file
    sql name [select|join] [all]              make sql file
    data name                                 make base data config file
    lua name                                  make lua data config file
    log name                                  make log file
    word                                      make sensitive word file
    key [-amount|-type|-prefix]               make active key 
    config                                    make erlang application config interface
    "
}

## execute function
if [[ $# = 0 ]];then
    ## make all(default)
    OTP_RELEASE=$(erl -noshell -eval "erlang:display(erlang:system_info(otp_release)),erlang:halt()." | sed "s/\"/'/g")
    OTP_VERSION=$(erl -noshell -eval "erlang:display(erlang:system_info(version)),erlang:halt()." | sed "s/\"/'/g")
    # otp 17 or earlier, referring to built-in type queue as a remote type; please take out the module name
    ERL_VERSION=$(erl +V 2>&1 | awk '{print $NF}' | awk -F "." '{print $1}')
    if [[ ${ERL_VERSION} -ge 6 ]];then
        # remote type option
        REMOTE_VERSION=",{d,otp}"
    fi
    OPTIONS="-env ERL_COMPILER_OPTIONS [{d,'RELEASE',${OTP_RELEASE}},{d,'VERSION',${OTP_VERSION}}${REMOTE_VERSION}]"
    cd ${script}/../debug/
    erl ${OPTIONS} -make
    erlc +debug_info -o ../../beam ../../src/tool/extension/user_default.erl
    cd - > /dev/null
elif [[ "$1" = "release" ]];then
    ## make all(default)
    cd ${script}/../release/
    erl -make
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
        head="${head}-include(\"../../../include/"${name}"\").\n"
    done;
    # delete last lf
    head=${head:0:${#head}-2}
    # delete old includes in file directory
    sed -i '/^-module.*\.\|^-compile.*\.\|^-include.*\./d' ${script}/../../src/tool/extension/user_default.erl
    if [[ ! -s ${script}/../../src/tool/extension/user_default.erl ]]; then
        # file was empty, write it covered
       echo -e ${head} > ${script}/../../src/tool/extension/user_default.erl
    else
        # insert to head
        sed -i '1i'"${head}" ${script}/../../src/tool/extension/user_default.erl
    fi
    # recompile it
    erlc +debug_info -o ${script}/../../beam/ ${script}/../../src/tool/extension/user_default.erl
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
elif [[ "$1" == "now" ]];then
   now=$(date "+%Y-%m-%d")
   now="-- ${now}"
   echo ${now} >> "${script}/../../script/sql/update.sql"
elif [[ "$1" = "need" ]];then
    shift 1
    # stop when start date not passed
    if [[ ! -n $1 ]];then
        echo "please support valid date format"
        exit
    fi
    # sql script file
    sql="${script}/../../script/sql/update.sql"
    need="${script}/../../script/sql/need.sql"
    # find start line number
    start=`grep -n "$1" ${sql} | grep -Po "^\d+(?=:)"`
    # find end line number
    end=`grep -n $(date "+%Y-%m-%d") ${sql} | grep -Po "^\d+(?=:)"`
    # stop when start line number not found
    if [[ ! -n ${start} ]];then
        echo "start date not found, please support valid date format"
        exit
    fi
    # if now line number not found, use end of file line number
    if [[ ! -n ${end} ]];then
        # confirm replace method
        read -p "now tag not found, use end file replace it ?(y/Y): " confirm
        if [[ ${confirm} == y || ${confirm} == Y ]];then
            end=`wc -l ${sql} | awk '{print $1}'`
        else
            exit
        fi
    fi
    # cut file from start line to end line, rewrite to need
    sed -n "${start},${end}p" ${sql} > ${need}
elif [[ "$1" = "pt" || "$1" = "protocol" ]];then
    name=$2
    shift 2
    escript ${script}/../../src/make/protocol/protocol_script_${name}.erl $*
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
elif [[ "$1" == "lua" ]];then
    shift 1
    escript ${script}/../../src/make/script/lua_script.erl $*
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
elif [[ "$1" == "map" ]];then
    shift 1
    escript ${script}/../../src/make/script/map_script.erl $*
else
    helps
fi
