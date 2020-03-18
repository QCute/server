#!/bin/bash

# script path
script=$(dirname "$0")

helps() {
    echo "usage: compile all file by default
    debug module                                      make with debug mode
    release module                                    make with release mode
    clean                                             remove all beam
    maker                                             compile maker
    beam                                              update beam abstract code
    now                                               append now to update sql script
    tag                                               append tag to update sql script
    need                                              cut last tag to end file, write to need sql script
    need date(Y-M-D)                                  cut from date(start) to now(end), write to need sql script
    pt name                                           make protocol file
    protocol                                          make all protocol file
    excel [table|xml] [table-name|file-name]          convert/restore table/xml to xml/table
    xml table-name                                    convert table to xml, same as excel xml table-name
    table file-name                                   restore xml to table, same as excel table file-name
    record name                                       make record file
    sql name                                          make sql file
    data name                                         make erl data configure file
    lua name                                          make lua data configure file
    json name                                         make json data configure file
    log name                                          make log file
    word                                              make sensitive word file
    key [-number|-type|-prefix]                       make active key
    config                                            make erlang application config interface
    router                                            make protocol route
    loop                                              make load/save/reset/clean/expire code
    "
}

## execute function
if [[ $# = 0 || "$1" == "debug" ]] && [[ "$2" == "" ]];then
    # make all(default)
    # OTP_RELEASE=$(erl -noinput -boot start_clean -eval "io:format(\"~w\", [list_to_atom(erlang:system_info(otp_release))]),erlang:halt().")
    # OTP_VERSION=$(erl -noinput -boot start_clean -eval "io:format(\"~w\", [list_to_atom(erlang:system_info(version))]),erlang:halt().")
    # otp 17 or earlier, referring to built-in type queue as a remote type; please take out the module name
    # ERL_VERSION=$(erl +V 2>&1 | awk '{print $NF}' | awk -F "." '{print $1}')
    # if [[ ${ERL_VERSION} -ge 6 ]];then
    #     # remote type option
    #     REMOTE_VERSION=",{d,otp}"
    # fi
    # OPTIONS="-env ERL_COMPILER_OPTIONS [{d,'RELEASE',${OTP_RELEASE}},{d,'VERSION',${OTP_VERSION}}${REMOTE_VERSION}]"
    cd "${script}/../debug/" || exit
    # erl "${OPTIONS}" -make
    erl -make
elif [[ "$1" = "debug" ]];then
    ## make one
    cd "${script}/../../" || exit
    file=$(find src/ -name "$2.erl" 2>/dev/null)
    if [[ "${file}" == "" ]];then
        echo "$2.erl: no such file or directory"
    else
        erlc -I include -o beam +debug_info -D DEBUG "${file}"
        echo ok
    fi
elif [[ "$1" = "release" && "$2" == "" ]];then
    ## make all(default)
    cd "${script}/../release/" || exit
    erl -make
    # strip all beam file
    erl -noinput -eval "beam_lib:strip_files(filelib:wildcard(\"../../beam/*.beam\")),erlang:halt()."
    # user_default must compile with debug info mode (beam abstract code contain)
    # use abs path "$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)/$(basename $0)" beam compile
    "../shell/$(basename "$0")" beam compile
elif [[ "$1" = "release" ]];then
    ## make one
    cd "${script}/../../" || exit
    file=$(find src/ -name "$2.erl" 2>/dev/null)
    if [[ "${file}" == "" ]];then
        echo "$2.erl: no such file or directory"
    else
        erlc -I include -o beam -Werror +"{hipe,o3}" +native "${file}"
        # strip beam file
        erl -noinput -eval "beam_lib:strip_files(filelib:wildcard(\"beam/${2}.beam\")),erlang:halt()."
        echo ok
    fi
elif [[ "$1" = "clean" ]];then
    rm "${script}"/../../beam/*.beam
elif [[ "$1" = "maker" ]];then
    cd "${script}/../make/" || exit
    erl -make
    erl -noinput -eval "beam_lib:strip_files(filelib:wildcard(\"../../beam/*maker.beam\")),erlang:halt()."
elif [[ "$1" = "beam" ]];then
    # reload all includes (default)
    if [[ "$2" == "" ]];then
        head="-module(user_default).\n-compile(nowarn_export_all).\n-compile(export_all).\n"
        head="${head}$(find include/*.hrl -exec basename {} \; | sed "s/^/-include(\"..\/..\/..\/include\//g;s/$/\")./g")"
        # for name in $(find "${script}/../../include" -name "*.hrl" -exec basename {} \;); do
        # find "${script}/../../include" -name "*.hrl" -exec basename {} \; | while read -r head
        # do
        #    head="${head}-include(\"../../../include/${name}\").\n"
        # done;
        # delete last lf
        # head=${head:0:${#head}-2}
        # delete old includes in file directory
        sed -i '/^-module.*\.\|^-compile.*\.\|^-include.*\./d' "${script}/../../src/tool/extension/user_default.erl"
        if [[ ! -s "${script}/../../src/tool/extension/user_default.erl" ]]; then
            # file was empty, write it covered
        echo -e "${head}" > "${script}/../../src/tool/extension/user_default.erl"
        else
            # insert to head
            sed -i '1i'"${head}" "${script}/../../src/tool/extension/user_default.erl"
        fi
    fi
    # remove old beam file
    rm "${script}/../../beam/user_default.beam"
    # recompile it with debug info mode (beam abstract code contain)
    erlc +debug_info -o "${script}/../../beam/" "${script}/../../src/tool/extension/user_default.erl"
elif [[ "$1" == "unix" ]];then
    # trans dos(CR/LF) to unix(LF) format

    # for file in $(grep -rlP "\r" "${script}/../../app/");do
    grep -rlP "\r" "${script}/../../app/" | while read -r file
    do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "${script}/../../config/");do
    grep -rlP "\r" "${script}/../../config/" | while read -r file
    do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "${script}/../../include/");do
    grep -rlP "\r" "${script}/../../include/" | while read -r file
    do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "${script}/../../script/");do
    grep -rlP "\r" "${script}/../../script/" | while read -r file
    do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "${script}/../../src/");do
    grep -rlP "\r" "${script}/../../src/" | while read -r file
    do
        dos2unix "${file}"
    done
elif [[ "$1" == "tab" ]];then
    # replace tab with 4 space
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "${script}/../../app/")" 2> /dev/null
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "${script}/../../config/")" 2> /dev/null
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "${script}/../../include/")" 2> /dev/null
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "${script}/../../script/")" 2> /dev/null
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "${script}/../../src/")" 2> /dev/null
elif [[ "$1" == "now" ]];then
    now=$(date "+%Y-%m-%d")
    now="-- ${now}"
    echo "${now}" >> "${script}/../../script/sql/update.sql"
elif [[ "$1" == "tag" ]];then
    {
        echo "-- ------------------------------------------------------------------";
        echo "-- :tag:";
        echo "-- ------------------------------------------------------------------";
    } >> "${script}/../../script/sql/update.sql"
elif [[ "$1" == "need" && "$2" == "" ]];then
    sql="${script}/../../script/sql/update.sql"
    need="${script}/../../script/sql/need.sql"
    # find last line number
    start=$(($(grep -no ":tag:" "${sql}" | tail -n 1 | awk -F ":" '{print $1}') - 1))
    # calculate line number
    end=$(wc -l "${sql}" | awk '{print $1}')
    # stop when start line number not found
    if [[ -z ${start} ]];then
        echo "tag not found, please check tag exists"
        exit
    fi
    # cut file from start line to end line, rewrite to need
    sed -n "${start},${end}p" "${sql}" > "${need}"
    # append new tag
    $0 tag
elif [[ "$1" = "need" ]];then
    shift 1
    # stop when start date not passed
    if [[ -z $1 ]];then
        echo "please support valid date format"
        exit
    fi
    # sql script file
    sql="${script}/../../script/sql/update.sql"
    need="${script}/../../script/sql/need.sql"
    # find start line number
    start=$(grep -n "$1" "${sql}" | grep -Po "^\d+(?=:)")
    # find end line number
    end=$(grep -n "$(date '+%Y-%m-%d')" "${sql}" | grep -Po "^\d+(?=:)")
    # stop when start line number not found
    if [[ -z ${start} ]];then
        echo "start date not found, please support valid date format"
        exit
    fi
    # if now line number not found, use end of file line number
    if [[ -z ${end} ]];then
        # confirm replace method
        read -p -r "now tag not found, use end file replace it ?(y/Y): " confirm
        if [[ ${confirm} == y || ${confirm} == Y ]];then
            end=$(wc -l "${sql}" | awk '{print $1}')
        else
            exit
        fi
    fi
    # cut file from start line to end line, rewrite to need
    sed -n "${start},${end}p" "${sql}" > "${need}"
    # append new tag
    $0 tag
elif [[ "$1" = "import" && "$2" == "" ]];then
    # set sql ifs
    OLD_IFS="$IFS"
    IFS=";"
    touch "${script}/../../script/sql/retain.sql"
    sql=$(cat "${script}/../../script/sql/need.sql")
    for sentence in ${sql[@]}
    do
        if [[ -n $(echo "${sentence}" | tr "\n" " " | grep -P "ALTER\s*TABLE\s*\`?\w+_log\`?") ]];then
            # append log table fields change sql to log data file
            echo -n "${sentence};" >> "${script}/../../script/sql/retain.sql"
        fi
    done
    echo "" >> "${script}/../../script/sql/retain.sql"
    IFS="${OLD_IFS}"
    # cd "${script}/../../" || exit
    # for config in $(find config/ -name "*.config");do
    find "${script}/../../config/" -name "*.config" | while read -r config
    do
        # exact but slow
        # USER=$(erl -noinput -boot start_clean -eval "erlang:display(proplists:get_value(user, proplists:get_value(mysql_connector, proplists:get_value(main, hd(element(2, file:consult(\"${config}\")))), []), [])),erlang:halt().")
        # PASSWORD=$(erl -noinput -boot start_clean -eval "erlang:display(proplists:get_value(password, proplists:get_value(mysql_connector, proplists:get_value(main, hd(element(2, file:consult(\"${config}\")))), []), [])),erlang:halt().")
        # DB=$(erl -noinput -boot start_clean -eval "erlang:display(proplists:get_value(database, proplists:get_value(mysql_connector, proplists:get_value(main, hd(element(2, file:consult(\"${config}\")))), []), [])),erlang:halt().")
        # sketchy but fast
        USER=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        PASSWORD=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        DATABASE=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        if [[ -n ${DATABASE} ]];then
            mysql --user="${USER}" --password="${PASSWORD}" --database="${DATABASE}" < "script/sql/need.sql"
        fi
    done
elif [[ "$1" = "import" ]];then
    # cd "${script}/../../" || exit
    if [[ -f "config/${2}.config" || -f "${2}" ]];then
        CONFIG=$(basename "${2}" ".config")
        # sketchy but fast
        USER=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${CONFIG}.config" | grep -Po "(?<=\")\w+(?=\")")
        PASSWORD=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${CONFIG}.config" | grep -Po "(?<=\")\w+(?=\")")
        DATABASE=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${CONFIG}.config" | grep -Po "(?<=\")\w+(?=\")")
        if [[ -n ${DATABASE} ]];then
            mysql --user="${USER}" --password="${PASSWORD}" --database="${DATABASE}" < "${script}/../../script/sql/need.sql"
        else
            echo "configure not contain database data"
        fi
    else
        echo "${2}.config: no such configure in config directory"
    fi
elif [[ "$1" = "retain" ]];then
    touch "${script}/../../script/sql/retain.sql"
    yesterday=$(((($(date +%s) + 8 * 3600) / 86400 * 86400 - 8 * 3600) - 86400))
    today=$(((($(date +%s) + 8 * 3600) / 86400 * 86400 - 8 * 3600) - 1))
    find "${script}/../../config/" -name "*.config" | while read -r config
    do
        USER=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        PASSWORD=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        DATABASE=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        if [[ -n ${DATABASE} ]];then
            # collect all log tables
            tables=$(mysql --user="${USER}" --password="${PASSWORD}" --skip-column-names --execute="SELECT \`TABLE_NAME\` FROM information_schema.\`TABLES\` WHERE \`TABLE_SCHEMA\` = '${DATABASE}' AND \`TABLE_NAME\` LIKE '%_log'" | grep -P "\w+" | paste -sd " ")
            # dump yesterday all log table data
            mysqldump --user="${USER}" --password="${PASSWORD}" --databases ${DATABASE} --tables ${tables} --where="\`time\` BETWEEN ${yesterday} AND ${today}" --no-create-info --compact >> "${script}/../../script/sql/retain.sql"
        fi
    done
elif [[ "$1" = "pt" ]];then
    name=$2
    shift 2
    escript "${script}/../make/protocol/protocol_script_${name}.erl" "$@"
    escript "${script}/../make/script/router_script.erl"
elif [[ "$1" = "protocol" ]];then
    shift 1
    find "${script}/../../script/make/protocol/" -name "*.erl" -exec escript {} "$@" \;
    escript "${script}/../make/script/router_script.erl"
elif [[ "$1" == "excel" ]];then
    shift 1
    escript "${script}/../make/script/excel_script.erl" "$@"
elif [[ "$1" == "table" || "$1" == "xml" ]];then
    escript "${script}/../make/script/excel_script.erl" "$@"
elif [[ "$1" == "record" ]];then
    shift 1
    escript "${script}/../make/script/record_script.erl" "$@"
elif [[ "$1" == "sql" ]];then
    shift 1
    escript "${script}/../make/script/sql_script.erl" "$@"
elif [[ "$1" == "data" ]];then
    shift 1
    escript "${script}/../make/script/data_script.erl" "$@"
elif [[ "$1" == "lua" ]];then
    shift 1
    escript "${script}/../make/script/lua_script.erl" "$@"
elif [[ "$1" == "json" ]];then
    shift 1
    escript "${script}/../make/script/json_script.erl" "$@"
elif [[ "$1" == "log" ]];then
    shift 1
    escript "${script}/../make/script/log_script.erl" "$@"
elif [[ "$1" == "word" ]];then
    shift 1
    escript "${script}/../make/script/word_script.erl" "$@"
elif [[ "$1" == "key" ]];then
    shift 1
    escript "${script}/../make/script/key_script.erl" "$@"
elif [[ "$1" == "config" ]];then
    shift 1
    escript "${script}/../make/script/config_script.erl" "$@"
elif [[ "$1" == "map" ]];then
    shift 1
    escript "${script}/../make/script/map_script.erl" "$@"
elif [[ "$1" == "router" ]];then
    shift 1
    escript "${script}/../make/script/router_script.erl" "$@"
elif [[ "$1" == "loop" ]];then
    shift 1
    escript "${script}/../make/script/loop_script.erl" "$@"
elif [[ "$1" == "attribute" || "$1" == "attr" ]];then
    shift 1
    escript "${script}/../make/script/attribute_script.erl" "$@"
else
    helps
fi
