#!/usr/bin/env bash

# script path
script=$(dirname "$0")

helps() {
    echo "usage: $(basename "$0")
    debug [module]                                make (module) with debug mode
    release [module]                              make (module) with release mode
    clean                                         remove all beam
    maker                                         compile maker
    beam                                          update beam abstract code
    now                                           append now to update sql script
    tag                                           append tag to update sql script
    need                                          cut last tag to end file, write to need sql script
    need date(Y-M-D)                              cut from date(start) to now(end), write to need sql script
    import [name]                                 import need sql to database, import to all database when the name not set
    pt name                                       make protocol file
    protocol                                      make all protocol file
    excel [table|xml] [table-name|file-name]      convert/restore table/xml to xml/table
    xml table-name                                convert table to xml, same as excel xml table-name
    table file-name                               restore xml to table, same as excel table file-name
    record name                                   make record file
    sql name                                      make sql file
    data name                                     make erl data configure file
    lua name                                      make lua data configure file
    js name                                       make js data configure file
    log name                                      make log file
    word                                          make sensitive word file
    key [-number|-type|-prefix]                   make active key
    config                                        make erlang application config interface
    router                                        make protocol route
    loop                                          make load/save/reset/clean/expire code
    attribute                                     make attribute code
    asset                                         make asset code
    event                                         make event code
    helps                                         lookup help manual
    "
}

## execute function
if [[ $# == 0 ]];then
    helps
elif [[ "$1" == "debug" ]] && [[ "$2" == "" ]];then
    # make all(default)
    # OTP_RELEASE=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~w\", [list_to_atom(erlang:system_info(otp_release))]),erlang:halt().")
    # OTP_VERSION=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~w\", [list_to_atom(erlang:system_info(version))]),erlang:halt().")
    # otp 17 or earlier, referring to built-in type queue as a remote type; please take out the module name
    # ERL_VERSION=$(erl +V 2>&1 | awk '{print $NF}' | awk -F "." '{print $1}')
    # if [[ ${ERL_VERSION} -ge 6 ]];then
    #     # remote type option
    #     REMOTE_VERSION=",{d,otp}"
    # fi
    # OPTIONS="-env ERL_COMPILER_OPTIONS [{d,'RELEASE',${OTP_RELEASE}},{d,'VERSION',${OTP_VERSION}}${REMOTE_VERSION}]"
    cd "${script}/../../" || exit
    # erl "${OPTIONS}" -make
    # erl -pa ../../beam/ -make
    emake='{["src/*", "src/*/*", "src/*/*/*", "src/*/*/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, {d, '\'DEBUG\'', true}]}'
    erl -pa beam/ +B -boot no_dot_erlang -noshell -eval "make:all([{emake, [${emake}]}]), erlang:halt()."
    $0 beam compile
elif [[ "$1" = "debug" ]];then
    ## make one
    cd "${script}/../../" || exit
    file=$(find src/ -name "$2.erl" 2>/dev/null)
    if [[ "${file}" == "" ]];then
        echo "$2.erl: no such file or directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    else
        erlc -I include -o beam +debug_info -D DEBUG "${file}"
        echo ok
    fi
elif [[ "$1" = "release" && "$2" == "" ]];then
    ## make all(default)
    cd "${script}/../../" || exit
    # erl -pa ../../beam/ -make
    ERL_VERSION=$(erl +V 2>&1 | awk '{print $NF}' | awk -F "." '{print $1}')
    if [[ ${ERL_VERSION} -ge 12 ]];then
        # otp 24 or later remove hipe
        emake='{["src/*", "src/*/*", "src/*/*/*", "src/*/*/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, warnings_as_errors]}'
    else
        # with hipe
        emake='{["src/*", "src/*/*", "src/*/*/*", "src/*/*/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, warnings_as_errors, native, {hipe, o3}]}'
    fi
    erl -pa beam/ +B -boot no_dot_erlang -noshell -eval "make:all([{emake, [${emake}]}]), erlang:halt()."
    # user_default must compile with debug info mode (beam abstract code contain)
    # use abs path "$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)/$(basename $0)" beam compile
    # "../shell/$(basename "$0")" beam compile
    $0 beam
elif [[ "$1" = "release" ]];then
    ## make one
    cd "${script}/../../" || exit
    file=$(find src/ -name "$2.erl" 2>/dev/null)
    if [[ "${file}" == "" ]];then
        echo "$2.erl: no such file or directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    else
        ERL_VERSION=$(erl +V 2>&1 | awk '{print $NF}' | awk -F "." '{print $1}')
        if [[ ${ERL_VERSION} -ge 12 ]];then
            # otp 24 or later remove hipe
            erlc -I include -o beam -Werror +debug_info "${file}"
        else
            # with hipe
            erlc -I include -o beam -Werror +debug_info +"{hipe,o3}" +native "${file}"
        fi
        echo ok
    fi
elif [[ "$1" = "clean" ]];then
    rm -f "${script}"/../../beam/*.beam
elif [[ "$1" = "plt" ]];then
    # locate erl lib ebin path
    path=$(dirname "$(type erl | awk '{print $3}')")/../lib/erlang/lib/
    # load add std lib
    while read -r lib;do
        plt="${plt} ${lib}"
    done <<<"$(find "${path}" -maxdepth 2 -name "ebin")"
    # build plt
    dialyzer --build_plt -r ${plt}
elif [[ "$1" == "dialyzer" ]];then
    shift
    dialyzer --no_check_plt "$@" -I "${script}"/../../include/ --src -r "${script}"/../../src/ "${script}"/../make/script/ "${script}"/../make/protocol/ "${script}"/../make/maker/
elif [[ "$1" = "maker" ]];then
    cd "${script}/../../" || exit
    # erl -make
    ERL_VERSION=$(erl +V 2>&1 | awk '{print $NF}' | awk -F "." '{print $1}')
    if [[ ${ERL_VERSION} -ge 12 ]];then
        # otp 24 or later remove hipe
        emake='{["script/make/maker/*", "src/tool/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, warnings_as_errors]}'
    else
        # with hipe
        emake='{["script/make/maker/*", "src/tool/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, warnings_as_errors, native, {hipe, o3}]}'
    fi
    erl -pa beam/ +B -boot no_dot_erlang -noshell -eval "make:all([{emake, [${emake}]}]), erlang:halt()."
elif [[ "$1" = "beam" ]];then
    # reload all includes (default)
    if [[ "$2" == "" ]];then
        head="-module(user_default).\n-compile(nowarn_export_all).\n-compile(export_all).\n"
        head="${head}$(find include/*.hrl -exec basename {} \; | sed "s/^/-include(\"..\/..\/..\/include\//g;s/$/\")./g")"
        # for name in $(find "${script}/../../include" -name "*.hrl" -exec basename {} \;); do
        # find "${script}/../../include" -name "*.hrl" -exec basename {} \; | while read -r head;do
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
    rm -f "${script}/../../beam/user_default.beam"
    # recompile it with debug info mode (beam abstract code contain)
    erlc +debug_info -o "${script}/../../beam/" "${script}/../../src/tool/extension/user_default.erl"
elif [[ "$1" == "version" ]];then
    if [[ -z "$2" ]];then
        code=$(erl -pa "${script}/../../beam/" +B -boot no_dot_erlang -noshell -eval "io:format(\"~w\", try [binary_to_integer(version:code())+1] catch _:_ -> [1] end),erlang:halt().")
    else
        code="$2"
    fi
    if [[ -z "$3" ]];then
        date=$(date +"%Y-%m-%d")
    else
        date="$3"
    fi
    if [[ -z "$4" ]];then
        time=$(date +"%H:%M:%S")
    else
        time="$4"
    fi
    code="{function,1,code,0,[{clause,1,[],[],[{bin,1,[{bin_element,1,{string,1,\"${code}\"},default,default}]}]}]},{eof,1}"
    date="{function,1,date,0,[{clause,1,[],[],[{bin,1,[{bin_element,1,{string,1,\"${date}\"},default,default}]}]}]},{eof,1}"
    time="{function,1,time,0,[{clause,1,[],[],[{bin,1,[{bin_element,1,{string,1,\"${time}\"},default,default}]}]}]},{eof,1}"
    form="[{attribute,1,file,{\"version from maker\",1}},{attribute,1,module,version},{attribute,1,export,[{code,0},{date,0},{time,0}]},${code},${date},${time}]"
    code="file:write_file(\"${script}/../../beam/version.beam\", element(3, compile:forms(${form}))),erlang:halt()."
    echo "Old version digest:" 1> >(sed $'s/.*/\e[31m&\e[m/'>&1)
    erl -pa "${script}/../../beam/" +B -boot no_dot_erlang -noshell -eval "io:format(\"    Old Code: ~s~n    Old Date: ~s~n    Old Time: ~s ~n\", try [version:code(), version:date(), version:time()] catch _:_ -> [<<>>, <<>>, <<>>] end),erlang:halt()."
    erl +B -boot no_dot_erlang -noshell -eval "${code}"
    echo "New version digest:" 1> >(sed $'s/.*/\e[32m&\e[m/'>&1)
    erl -pa "${script}/../../beam/" +B -boot no_dot_erlang -noshell -eval "io:format(\"    New Code: ~s~n    New Date: ~s~n    New Time: ~s ~n\", try [version:code(), version:date(), version:time()] catch _:_ -> [<<>>, <<>>, <<>>] end),erlang:halt()."
elif [[ "$1" == "unix" ]];then
    # trans dos(CR/LF) to unix(LF) format

    # for file in $(grep -rlP "\r" "${script}/../../app/");do
    grep -rlP "\r" "${script}/../../config/app/" | while read -r file;do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "${script}/../../config/");do
    grep -rlP "\r" "${script}/../../config/" | while read -r file;do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "${script}/../../include/");do
    grep -rlP "\r" "${script}/../../include/" | while read -r file;do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "${script}/../../script/");do
    grep -rlP "\r" "${script}/../../script/" | while read -r file;do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "${script}/../../src/");do
    grep -rlP "\r" "${script}/../../src/" | while read -r file;do
        dos2unix "${file}"
    done
elif [[ "$1" == "tab" ]];then
    # replace tab with 4 space
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "${script}/../../config/app/")" 2> /dev/null
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
    if [[ -z "${start}" ]];then
        echo "tag not found, please check tag exists" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
    # cut file from start line to end line, rewrite to need
    sed -n "${start},${end}p" "${sql}" > "${need}"
    # append new tag
    $0 tag
    # generate open sql
    $0 open_sql
    # generate merge sql
    $0 merge_sql
elif [[ "$1" = "need" ]];then
    shift 1
    # stop when start date not passed
    if [[ -z $1 ]];then
        echo "please support valid date format" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
    # sql script file
    sql="${script}/../../script/sql/update.sql"
    need="${script}/../../script/sql/need.sql"
    # find start line number
    start=$(grep -n "$1" "${sql}" | grep -Po "^\d+(?=:)")
    # find end line number
    end=$(grep -n "$(date '+%Y-%m-%d')" "${sql}" | grep -Po "^\d+(?=:)")
    # stop when start line number not found
    if [[ -z "${start}" ]];then
        echo "start date not found, please support valid date format" >&2
        exit 1
    fi
    # if now line number not found, use end of file line number
    if [[ -z "${end}" ]];then
        # confirm replace method
        read -r -p "now tag not found, use end file replace it ?(y/Y): " confirm
        if [[ "${confirm}" == "y" || "${confirm}" == "Y" ]];then
            end=$(wc -l "${sql}" | awk '{print $1}')
        else
            exit 1
        fi
    fi
    # cut file from start line to end line, rewrite to need
    sed -n "${start},${end}p" "${sql}" > "${need}"
    # append new tag
    $0 tag
    # generate open sql
    $0 open_sql
    # generate merge sql
    $0 merge_sql
elif [[ "$1" = "import" && "$2" == "" ]];then
    # cd "${script}/../../" || exit
    find "${script}/../../config/" -name "*.config" | while read -r file;do
        # config
        config=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", element(2, file:consult(\"${file}\"))),erlang:halt().")
        # main config
        main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(main, ${config}, [])]),erlang:halt().")
        # connector
        connector=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(mysql_connector, ${main}, [])]),erlang:halt().")
        # database connector config
        host=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(host, ${connector}, [])]),erlang:halt().")
        port=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(port, ${connector}, [])]),erlang:halt().")
        user=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(user, ${connector}, [])]),erlang:halt().")
        password=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(password, ${connector}, [])]),erlang:halt().")
        database=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(database, ${connector}, [])]),erlang:halt().")
        # import sql
        if [[ -n "${database}" ]];then
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" < "${script}/../../script/sql/need.sql"
        else
            echo "${file}: database not found in configure, skip" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
    done
elif [[ "$1" = "import" ]];then
    # cd "${script}/../../" || exit
    file="${script}/../../config/$(basename "$2" ".config").config"
    if [[ -f "${file}" ]];then
        # config
        config=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", element(2, file:consult(\"${file}\"))),erlang:halt().")
        # main config
        main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(main, ${config}, [])]),erlang:halt().")
        # connector
        connector=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(mysql_connector, ${main}, [])]),erlang:halt().")
        # database connector config
        host=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(host, ${connector}, [])]),erlang:halt().")
        port=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(port, ${connector}, [])]),erlang:halt().")
        user=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(user, ${connector}, [])]),erlang:halt().")
        password=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(password, ${connector}, [])]),erlang:halt().")
        database=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(database, ${connector}, [])]),erlang:halt().")
        # import sql
        if [[ -n "${database}" ]];then
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" < "${script}/../../script/sql/need.sql"
        else
            echo "${file}: database not found in configure, skip" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
    else
        echo "$2: no such configure file or name in config directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
elif [[ "$1" == "open_sql" ]];then
    # find local node config src file
    file=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [File || File <- filelib:wildcard(\"${script}/../../config/src/*.config\"), proplists:get_value(node_type, proplists:get_value(main, hd(element(2, file:consult(File))))) == local]),erlang:halt().")
    if [[ -f "${file}" ]];then
        # config
        config=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", element(2, file:consult(\"${file}\"))),erlang:halt().")
        # main config
        main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(main, ${config}, [])]),erlang:halt().")
        # connector
        connector=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(mysql_connector, ${main}, [])]),erlang:halt().")
        # database connector config
        host=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(host, ${connector}, [])]),erlang:halt().")
        port=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(port, ${connector}, [])]),erlang:halt().")
        user=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(user, ${connector}, [])]),erlang:halt().")
        password=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(password, ${connector}, [])]),erlang:halt().")
        database=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(database, ${connector}, [])]),erlang:halt().")
        # dump
        mysqldump --host="${host}" --port="${port}" --user="${user}" --password="${password}" --no-data --compact --add-drop-table --skip-set-charset "${database}" | sed 's/\bAUTO_INCREMENT=[0-9]*\s*//g' > "${script}/../../script/sql/open.sql"
        # remove virtual field
        grep -n "GENERATED ALWAYS" "${script}"/../../script/sql/open.sql | awk -F ":" '{print $1}' | while read -r line;do
            # add -- remove virtual field
            : # sed -i "${line}s/^/-- /" "${script}/../../script/sql/open.sql"
        done
    else
        echo "cannot found any local type configure file in config src directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
elif [[ "$1" == "open_server" ]];then
    # check node name exists
    name=$(basename "$2" ".config")
    if [[ -z "$2" ]];then
        echo "server name empty" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    elif [[ -z "${STY}" ]];then
        mkdir -p "/tmp/screen/"
        echo -n > "/tmp/screen/open_server.log"
        screen -L -Logfile "/tmp/screen/open_server.log" -dmS "open_server"
        screen -x -p 0 -S "open_server" -X stuff "$0 $*; for ((i=10;i>=0;i--));do echo -ne \"exit after \\\${i} seconds ...\"; sleep 1; echo -ne \"\\\r\"; done; exit; \n"
        screen -r "open_server"
    elif [[ ! -f "${script}/../../config/${name}.config" ]];then
        # find local node config src file
        file=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [File || File <- filelib:wildcard(\"${script}/../../config/src/*.config\"), proplists:get_value(node_type, proplists:get_value(main, hd(element(2, file:consult(File))))) == local]),erlang:halt().")
        if [[ -f "${file}" ]];then
            # config
            config=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", element(2, file:consult(\"${file}\"))),erlang:halt().")
            # main config
            main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(main, ${config}, [])]),erlang:halt().")
            # check src server id and last server id is matched ?
            last_server_id=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(server_id, ${main}) + 1]),erlang:halt().")
            # connector
            connector=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(mysql_connector, ${main}, [])]),erlang:halt().")
            # database connector config
            host=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(host, ${connector}, [])]),erlang:halt().")
            port=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(port, ${connector}, [])]),erlang:halt().")
            user=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(user, ${connector}, [])]),erlang:halt().")
            password=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(password, ${connector}, [])]),erlang:halt().")
            database=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(database, ${connector}, [])]),erlang:halt().")
            # new database
            echo "create database: ${name}"
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --execute="CREATE DATABASE IF NOT EXISTS \`${name}\` DEFAULT CHARACTER SET utf8mb4 DEFAULT COLLATE utf8mb4_unicode_ci;"
            # import sql
            echo "import open sql ..."
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${name}" < "${script}/../../script/sql/open.sql"
            ## new config file ##
            # replace database
            connector=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [lists:keyreplace(database, 1, ${connector}, {database, \"${name}\"})]),erlang:halt().")
            # replace connector
            main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [lists:keyreplace(mysql_connector, 1, ${main}, {mysql_connector, ${connector}})]),erlang:halt().")
            # replace server id
            main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [lists:keyreplace(server_id, 1, ${main}, {server_id, ${last_server_id} + 1})]),erlang:halt().")
            # replace open time
            open_time=$(date -d "$(date -d "now" +%Y-%m-%d)" +%s)
            main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [lists:keyreplace(open_time, 1, ${main}, {open_time, ${open_time}})]),erlang:halt().")
            # replace main
            config=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [lists:keyreplace(main, 1, ${config}, {main, ${main}})]),erlang:halt().")
            # write config src file
            echo "update config src file ..."
            erl +B -boot no_dot_erlang -noshell -eval "file:write_file(\"${file}\", io_lib:format(\"~p.\", [${config}])),erlang:halt()."
            # format config src file
            escript "${script}/../make/script/config_script.erl" "${file}"
            # write config file
            echo "create config file ..."
            erl +B -boot no_dot_erlang -noshell -eval "file:write_file(\"${script}/../../config/${name}.config\", io_lib:format(\"~p.\", [${config}])),erlang:halt()."
            # format config file
            escript "${script}/../make/script/config_script.erl" "${script}/../../config/${name}.config"
            # completed
            echo "open server ${name} completed" | sed $'s/.*/\e[32m&\e[m/'
        else
            echo "cannot found any local type configure file in config src directory" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
    else
        echo "configure $2 already in config directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
elif [[ "$1" == "merge_sql" ]];then
    # find local node config src file
    file=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [File || File <- filelib:wildcard(\"${script}/../../config/src/*.config\"), proplists:get_value(node_type, proplists:get_value(main, hd(element(2, file:consult(File))))) == local]),erlang:halt().")
    if [[ -f "${file}" ]];then
        # config
        config=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", element(2, file:consult(\"${file}\"))),erlang:halt().")
        # main config
        main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(main, ${config}, [])]),erlang:halt().")
        # connector
        connector=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(mysql_connector, ${main}, [])]),erlang:halt().")
        # database connector config
        host=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(host, ${connector}, [])]),erlang:halt().")
        port=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(port, ${connector}, [])]),erlang:halt().")
        user=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(user, ${connector}, [])]),erlang:halt().")
        password=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(password, ${connector}, [])]),erlang:halt().")
        database=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(database, ${connector}, [])]),erlang:halt().")
        ## generate the update server id sql ##
        # reset server id
        start=$(grep -n "@make_update_server_id_sql_start" script/sql/merge.sql | awk -F ":" '{print $1+1}' | head -n 1)
        end=$(grep -n "@make_update_server_id_sql_end" script/sql/merge.sql | awk -F ":" '{print $1-1}' | head -n 1)
        sql=$(sed -n "${start},${end}p" < "${script}/../../script/sql/merge.sql" | sed 's/--//g')
        start=$(grep -n "@update_server_id_sql_start" "${script}/../../script/sql/merge.sql" | awk -F ":" '{print $1+1}' | bc)
        end=$(grep -n "@update_server_id_sql_end" "${script}/../../script/sql/merge.sql" | awk -F ":" '{print $1-1}' | bc)
        # remove old merge sql
        [[ $(expr "${start}" "<" "${end}") == "1" ]] && sed -i "${start},${end}d" "${script}/../../script/sql/merge.sql"
        # replace database
        sql=${sql/"{{database}}"/"${database}"}
        # query
        mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" --raw --silent --execute="${sql}" | while read -r line;do
            # write
            sed -i "${start}i${line}" "${script}/../../script/sql/merge.sql"
        done
        ## generate the merge sql ##
        # merge same table data
        start=$(grep -n "@make_merge_sql_start" script/sql/merge.sql | awk -F ":" '{print $1+1}' | head -n 1)
        end=$(grep -n "@make_merge_sql_end" script/sql/merge.sql | awk -F ":" '{print $1-1}' | head -n 1)
        sql=$(sed -n "${start},${end}p" < "${script}/../../script/sql/merge.sql" | sed 's/--//g')
        start=$(grep -n "@merge_sql_start" "${script}/../../script/sql/merge.sql" | awk -F ":" '{print $1+1}' | bc)
        end=$(grep -n "@merge_sql_end" "${script}/../../script/sql/merge.sql" | awk -F ":" '{print $1-1}' | bc)
        # remove old merge sql
        [[ $(expr "${start}" "<" "${end}") == "1" ]] && sed -i "${start},${end}d" "${script}/../../script/sql/merge.sql"
        # replace database
        sql=${sql/"{{database}}"/"${database}"}
        # query
        mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" --raw --silent --execute="${sql}" | while read -r line;do
            # write
            sed -i "${start}i${line}" "${script}/../../script/sql/merge.sql"
        done
    else
        echo "cannot found any local type configure src file in config directory" | sed $'s/.*/\e[31m&\e[m/'
    fi
elif [[ "$1" == "merge_server" ]];then
    # src
    src=$(basename "$2" ".config")
    src_file="${script}/../../config/${src}.config"
    # dst
    dst=$(basename "$3" ".config")
    dst_file="${script}/../../config/${dst}.config"
    if [[ -z "${src}" ]];then
        echo "src server empty" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    elif [[ -z "${dst}" ]];then
        echo "dst server empty" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    elif [[ "${src}" == "${dst}" ]];then
        echo "src server equals dst server" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    elif [[ -z "${STY}" ]];then
        mkdir -p "/tmp/screen/"
        echo -n > "/tmp/screen/merge_server.log"
        screen -L -Logfile "/tmp/screen/merge_server.log" -dmS "merge_server"
        screen -x -p 0 -S "merge_server" -X stuff "$0 $*; for ((i=10;i>=0;i--));do echo -ne \"exit after \\\${i} seconds ...\"; sleep 1; echo -ne \"\\\r\"; done; exit; \n"
        screen -r "merge_server"
    # check config file exists
    elif [[ -f "${src_file}" && -f "${dst_file}" ]];then
        # src config
        src_config=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", element(2, file:consult(\"${src_file}\"))),erlang:halt().")
        # src main config
        src_main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(main, ${src_config}, [])]),erlang:halt().")
        # src node type
        src_node_type=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(node_type, ${src_main}, [])]),erlang:halt().")
        # src connector
        src_connector=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(mysql_connector, ${src_main}, [])]),erlang:halt().")
        # src database connector config
        src_host=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(host, ${src_connector}, [])]),erlang:halt().")
        src_port=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(port, ${src_connector}, [])]),erlang:halt().")
        src_user=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(user, ${src_connector}, [])]),erlang:halt().")
        src_password=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(password, ${src_connector}, [])]),erlang:halt().")
        src_database=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(database, ${src_connector}, [])]),erlang:halt().")
        # src server id
        src_server_id=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(server_id, ${src_main}, [])]),erlang:halt().")
        src_server_id_list=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(server_id_list, ${src_main}, [])]),erlang:halt().")
        ##  src↑ <--> dst↓  ##
        # dst config
        dst_config=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", element(2, file:consult(\"${dst_file}\"))),erlang:halt().")
        # dst main config
        dst_main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(main, ${dst_config}, [])]),erlang:halt().")
        # dst node type
        dst_node_type=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(node_type, ${dst_main}, [])]),erlang:halt().")
        # dst connector
        dst_connector=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(mysql_connector, ${dst_main}, [])]),erlang:halt().")
        # dst database connector config
        dst_host=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(host, ${dst_connector}, [])]),erlang:halt().")
        dst_port=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(port, ${dst_connector}, [])]),erlang:halt().")
        dst_user=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(user, ${dst_connector}, [])]),erlang:halt().")
        dst_password=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(password, ${dst_connector}, [])]),erlang:halt().")
        dst_database=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~s\", [proplists:get_value(database, ${dst_connector}, [])]),erlang:halt().")
        # dst server id
        dst_server_id=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(server_id, ${dst_main}, [])]),erlang:halt().")
        dst_server_id_list=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [proplists:get_value(server_id_list, ${dst_main}, [])]),erlang:halt().")
        ##  merge start  ##
        if [[ "${src_node_type}" != "${dst_node_type}" ]];then
            echo "src server node type: '${src_node_type}' not equals dst server node type: '${dst_node_type}'" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
        ## check table ##
        result=$(mysql --host="${dst_host}" --port="${dst_port}" --user="${dst_user}" --password="${dst_password}" --execute="SELECT \`LEFT\`.\`TABLE_NAME\` AS \`${src}\` FROM information_schema.\`TABLES\` AS \`LEFT\` LEFT JOIN ( SELECT TABLE_NAME FROM information_schema.\`TABLES\` WHERE TABLE_SCHEMA = '${dst}' ORDER BY \`TABLE_NAME\` ) AS \`RIGHT\` ON \`LEFT\`.\`TABLE_NAME\` = \`RIGHT\`.\`TABLE_NAME\` WHERE TABLE_SCHEMA = '${src}' AND \`RIGHT\`.\`TABLE_NAME\` IS NULL ORDER BY \`LEFT\`.\`TABLE_NAME\`" | tail -n +2)
        if [[ -n "${result}" ]];then
            echo -e "found table not exists database ${dst}, but exists ${src}\n${result}" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
        ## check table ##
        result=$(mysql --host="${dst_host}" --port="${dst_port}" --user="${dst_user}" --password="${dst_password}" --execute="SELECT \`LEFT\`.\`TABLE_NAME\` AS \`${dst}\` FROM information_schema.\`TABLES\` AS \`LEFT\` LEFT JOIN ( SELECT TABLE_NAME FROM information_schema.\`TABLES\` WHERE TABLE_SCHEMA = '${src}' ORDER BY \`TABLE_NAME\` ) AS \`RIGHT\` ON \`LEFT\`.\`TABLE_NAME\` = \`RIGHT\`.\`TABLE_NAME\` WHERE TABLE_SCHEMA = '${dst}' AND \`RIGHT\`.\`TABLE_NAME\` IS NULL ORDER BY \`LEFT\`.\`TABLE_NAME\`" | tail -n +2)
        if [[ -n "${result}" ]];then
            echo -e "found table not exists database ${src}, but exists ${dst}\n${result}" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
        ## check table struct ##
        # temp file
        echo "generate merge server sql ..."
        cp "${script}/../../script/sql/merge.sql" "${script}/../../script/sql/merge_server.sql"
        # remove commemt
        sed -i '/^--/d' "${script}/../../script/sql/merge_server.sql"
        # replace src
        sed -i "s/{{src}}/\`${src_database}\`/g" "${script}/../../script/sql/merge_server.sql"
        # replace dst
        sed -i "s/{{dst}}/\`${dst_database}\`/g" "${script}/../../script/sql/merge_server.sql"
        # replace src server id
        sed -i "s/{{src_server_id}}/${src_server_id}/g" "${script}/../../script/sql/merge_server.sql"
        # replace dst server id
        sed -i "s/{{dst_server_id}}/${dst_server_id}/g" "${script}/../../script/sql/merge_server.sql"
        # remove \n
        sed -i ":label;N;s/\n/ /g;b label" "${script}/../../script/sql/merge_server.sql"
        # replace \n to ;\n
        sed -i "s/;/;\n/g" "${script}/../../script/sql/merge_server.sql"
        # start merge table
        echo "start merge table ..."
        IFS=';'
        while read -r line;do
            [[ "${line}" =~ "UPDATE" ]] && echo "${line}" | awk '{print "UPDATE: "$2}'
            [[ "${line}" =~ "INSERT" ]] && echo "${line}" | awk '{print "INSERT: "$3}'
            # execute merge sql script
            mysql --host="${dst_host}" --port="${dst_port}" --user="${dst_user}" --password="${dst_password}" --execute="${line}" || exit 1
        done <<<"$(cat "${script}/../../script/sql/merge_server.sql")"
        # drop database
        echo "merge table completed, drop src database ..."
        mysql --host="${src_host}" --port="${src_port}" --user="${src_user}" --password="${src_password}" --execute="DROP DATABASE IF EXISTS \`${src_database}\`;"
        # remove config file
        rm "${src_file}"
        # remove temp file
        rm -f "${script}/../../script/sql/merge_server.sql"
        ##  after merge  ##
        echo "update dst server(${dst}) config server id list ..."
        # merge src merge server id list src server id and dst merge server id list
        now=$(date -d "$(date -d "now" +%Y-%m-%d)" +%s)
        server_id_list=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [[{${src_server_id}, ${now}}] ++ ${src_server_id_list} ++ ${dst_server_id_list}]),erlang:halt().")
        # replace server id list
        dst_main=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [lists:keyreplace(server_id_list, 1, ${dst_main}, {server_id_list, ${server_id_list}})]),erlang:halt().")
        # replace main
        dst_config=$(erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~p\", [lists:keyreplace(main, 1, ${dst_config}, {main, ${dst_main}})]),erlang:halt().")
        # write dst file
        echo "update dst server(${dst}) config file ..."
        erl +B -boot no_dot_erlang -noshell -eval "file:write_file(\"${dst_file}\", io_lib:format(\"~p.\", [${dst_config}])),erlang:halt()."
        # format dst file
        escript "${script}/../make/script/config_script.erl" "${dst_file}"
        # completed
        echo "merge src server ${src} to dst server ${dst}" | sed $'s/.*/\e[32m&\e[m/'
    elif [[ ! -f "${src_file}" ]];then
        echo "cannot found $2 in config directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    elif [[ ! -f "${dst_file}" ]];then
        echo "cannot found $3 in config directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
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
elif [[ "$1" == "js" ]];then
    shift 1
    escript "${script}/../make/script/js_script.erl" "$@"
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
elif [[ "$1" == "router" ]];then
    shift 1
    escript "${script}/../make/script/router_script.erl" "$@"
elif [[ "$1" == "loop" ]];then
    shift 1
    escript "${script}/../make/script/loop_script.erl" "$@"
elif [[ "$1" == "map" ]];then
    shift 1
    escript "${script}/../make/script/map_script.erl" "$@"
elif [[ "$1" == "attribute" ]];then
    shift 1
    escript "${script}/../make/script/attribute_script.erl" "$@"
elif [[ "$1" == "asset" ]];then
    shift 1
    escript "${script}/../make/script/asset_script.erl" "$@"
elif [[ "$1" == "event" ]];then
    shift 1
    escript "${script}/../make/script/event_script.erl" "$@"
else
    [[ "$1" != "helps" ]] && echo "unknown option: $1" | sed $'s/.*/\e[31m&\e[m/' >&2
    helps
    exit 1
fi
