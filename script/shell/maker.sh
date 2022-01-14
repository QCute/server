#!/usr/bin/env bash

# script path
script=$(dirname "$0")
# enter project root directory
cd "${script}/../../" || exit 1

helps() {
    echo "usage: $(basename "$0")
    debug [module]                                make (module) with debug mode
    release [module]                              make (module) with release mode
    clean                                         remove all beam
    maker                                         compile maker
    beam                                          update beam abstract code
    now                                           append now to update sql script
    tag                                           append tag to update sql script
    migrate                                       cut last tag to end file, write to migrate sql script
    migrate date(Y-M-D)                           cut from date(start) to now(end), write to migrate sql script
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
    # OTP_RELEASE=$(erl +pc unicode +B -boot no_dot_erlang -noshell -eval "io:format(\"~w\", [list_to_atom(erlang:system_info(otp_release))]),erlang:halt().")
    # OTP_VERSION=$(erl +pc unicode +B -boot no_dot_erlang -noshell -eval "io:format(\"~w\", [list_to_atom(erlang:system_info(version))]),erlang:halt().")
    # otp 17 or earlier, referring to built-in type queue as a remote type; please take out the module name
    # ERL_VERSION=$(erl +V 2>&1 | awk '{print $NF}' | awk -F "." '{print $1}')
    # if [[ ${ERL_VERSION} -ge 6 ]];then
    #     # remote type option
    #     REMOTE_VERSION=",{d,otp}"
    # fi
    # OPTIONS="-env ERL_COMPILER_OPTIONS [{d,'RELEASE',${OTP_RELEASE}},{d,'VERSION',${OTP_VERSION}}${REMOTE_VERSION}]"
    # erl "${OPTIONS}" -make
    # erl -pa ../../beam/ -make
    emake='{["src/*", "src/*/*", "src/*/*/*", "src/*/*/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, {d, '\'DEBUG\'', true}]}'
    erl -pa beam/ +pc unicode +B -boot no_dot_erlang -noshell -eval "make:all([{emake, [${emake}]}]), erlang:halt()."
    $0 beam compile
elif [[ "$1" = "debug" ]];then
    ## make one
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
    # erl -pa ../../beam/ -make
    ERL_VERSION=$(erl +V 2>&1 | awk '{print $NF}' | awk -F "." '{print $1}')
    if [[ ${ERL_VERSION} -ge 12 ]];then
        # otp 24 or later remove hipe
        emake='{["src/*", "src/*/*", "src/*/*/*", "src/*/*/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, warnings_as_errors]}'
    else
        # with hipe
        emake='{["src/*", "src/*/*", "src/*/*/*", "src/*/*/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, warnings_as_errors, native, {hipe, o3}]}'
    fi
    erl -pa beam/ +pc unicode +B -boot no_dot_erlang -noshell -eval "make:all([{emake, [${emake}]}]), erlang:halt()."
    # user_default must compile with debug info mode (beam abstract code contain)
    $0 beam
elif [[ "$1" = "release" ]];then
    ## make one
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
    rm -f beam/*.beam
elif [[ "$1" = "plt" ]];then
    # locate erl lib ebin path
    path=$(dirname "$(type erl | awk '{print $3}')")/../lib/erlang/lib/
    # load add std lib
    while read -r lib;do
        plt="${plt} ${lib}"
    done <<<"$(find "${path}" -maxdepth 2 -name "ebin")"
    # build plt
    # shellcheck disable=SC2086
    dialyzer --build_plt -r ${plt}
elif [[ "$1" == "dialyzer" ]];then
    shift
    dialyzer --statistics --no_check_plt "$@" -I include/ --src -r src/ script/make/script/ script/make/protocol/ script/make/maker/
elif [[ "$1" = "maker" ]];then
    # erl -make
    ERL_VERSION=$(erl +V 2>&1 | awk '{print $NF}' | awk -F "." '{print $1}')
    if [[ ${ERL_VERSION} -ge 12 ]];then
        # otp 24 or later remove hipe
        emake='{["script/make/maker/*", "src/tool/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, warnings_as_errors]}'
    else
        # with hipe
        emake='{["script/make/maker/*", "src/tool/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, warnings_as_errors, native, {hipe, o3}]}'
    fi
    erl -pa beam/ +pc unicode +B -boot no_dot_erlang -noshell -eval "make:all([{emake, [${emake}]}]), erlang:halt()."
elif [[ "$1" = "beam" ]];then
    # reload all includes (default)
    if [[ "$2" == "" ]];then
        head="-module(user_default).\n-compile(nowarn_export_all).\n-compile(export_all).\n"
        head="${head}$(find include/*.hrl -exec basename {} \; | sed "s/^/-include(\"..\/..\/..\/include\//g;s/$/\")./g")"
        # for name in $(find "include" -name "*.hrl" -exec basename {} \;); do
        # find "include" -name "*.hrl" -exec basename {} \; | while read -r head;do
        #    head="${head}-include(\"../../../include/${name}\").\n"
        # done;
        # delete last lf
        # head=${head:0:${#head}-2}
        # delete old includes in file directory
        sed -i '/^-module.*\.\|^-compile.*\.\|^-include.*\./d' "src/tool/extension/user_default.erl"
        if [[ ! -s "src/tool/extension/user_default.erl" ]]; then
            # file was empty, write it covered
        echo -e "${head}" > "src/tool/extension/user_default.erl"
        else
            # insert to head
            sed -i '1i'"${head}" "src/tool/extension/user_default.erl"
        fi
    fi
    # remove old beam file
    rm -f "beam/user_default.beam"
    # recompile it with debug info mode (beam abstract code contain)
    erlc +debug_info -o "beam/" "src/tool/extension/user_default.erl"
elif [[ "$1" == "version" ]];then
    if [[ -z "$2" ]];then
        code=$(erl -pa "beam/" +pc unicode +B -boot no_dot_erlang -noshell -eval "io:format(\"~w\", try [binary_to_integer(version:code())+1] catch _:_ -> [1] end),erlang:halt().")
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
    code="file:write_file(\"beam/version.beam\", element(3, compile:forms(${form}))),erlang:halt()."
    echo "Old version digest:" 1> >(sed $'s/.*/\e[31m&\e[m/'>&1)
    erl -pa "beam/" +pc unicode +B -boot no_dot_erlang -noshell -eval "io:format(\"    Old Code: ~s~n    Old Date: ~s~n    Old Time: ~s ~n\", try [version:code(), version:date(), version:time()] catch _:_ -> [<<>>, <<>>, <<>>] end),erlang:halt()."
    erl +pc unicode +B -boot no_dot_erlang -noshell -eval "${code}"
    echo "New version digest:" 1> >(sed $'s/.*/\e[32m&\e[m/'>&1)
    erl -pa "beam/" +pc unicode +B -boot no_dot_erlang -noshell -eval "io:format(\"    New Code: ~s~n    New Date: ~s~n    New Time: ~s ~n\", try [version:code(), version:date(), version:time()] catch _:_ -> [<<>>, <<>>, <<>>] end),erlang:halt()."
elif [[ "$1" == "unix" ]];then
    # trans dos(CR/LF) to unix(LF) format

    # for file in $(grep -rlP "\r" "app/");do
    grep -rlP "\r" "config/app/" | while read -r file;do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "config/");do
    grep -rlP "\r" "config/" | while read -r file;do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "include/");do
    grep -rlP "\r" "include/" | while read -r file;do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "script/");do
    grep -rlP "\r" "script/" | while read -r file;do
        dos2unix "${file}"
    done
    # for file in $(grep -rlP "\r" "src/");do
    grep -rlP "\r" "src/" | while read -r file;do
        dos2unix "${file}"
    done
elif [[ "$1" == "tab" ]];then
    # replace tab with 4 space
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "config/app/")" 2> /dev/null
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "config/")" 2> /dev/null
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "include/")" 2> /dev/null
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "script/")" 2> /dev/null
    sed -i "s/\t/    /g" "$(grep -rlP "\t" "src/")" 2> /dev/null
elif [[ "$1" == "now" ]];then
    now=$(date "+%Y-%m-%d")
    now="-- ${now}"
    echo "${now}" >> "script/sql/update.sql"
elif [[ "$1" == "tag" ]];then
    {
        echo "-- ------------------------------------------------------------------";
        echo "-- :tag:";
        echo "-- ------------------------------------------------------------------";
    } >> "script/sql/update.sql"
elif [[ "$1" == "migrate" && "$2" == "" ]];then
    sql="script/sql/update.sql"
    migrate="script/sql/migrate.sql"
    # find last line number
    start=$(($(grep -no ":tag:" "${sql}" | tail -n 1 | awk -F ":" '{print $1}') - 1))
    # calculate line number
    end=$(wc -l "${sql}" | awk '{print $1}')
    # stop when start line number not found
    if [[ -z "${start}" ]];then
        echo "tag not found, please check tag exists" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
    # cut file from start line to end line, rewrite to migrate
    sed -n "${start},${end}p" "${sql}" > "${migrate}"
    # append new tag
    $0 tag
    # generate open sql
    $0 open_sql
    # generate merge sql
    $0 merge_sql
elif [[ "$1" = "migrate" ]];then
    shift 1
    # stop when start date not passed
    if [[ -z $1 ]];then
        echo "please support valid date format" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
    # sql script file
    sql="script/sql/update.sql"
    migrate="script/sql/migrate.sql"
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
    # cut file from start line to end line, rewrite to migrate
    sed -n "${start},${end}p" "${sql}" > "${migrate}"
    # append new tag
    $0 tag
    # generate open sql
    $0 open_sql
    # generate merge sql
    $0 merge_sql
elif [[ "$1" == "open_sql" ]];then
    # find local node config src file
    file=$($0 cfg-src find local)
    if [[ -f "${file}" ]];then
        # database connector config
        host=$($0 cfg-src get "${file}" "main, mysql_connector, host")
        port=$($0 cfg-src get "${file}" "main, mysql_connector, port")
        user=$($0 cfg-src get "${file}" "main, mysql_connector, user")
        password=$($0 cfg-src get "${file}" "main, mysql_connector, password")
        database=$($0 cfg-src get "${file}" "main, mysql_connector, database")
        # dump
        mysqldump --host="${host}" --port="${port}" --user="${user}" --password="${password}" --no-data --compact --add-drop-table --skip-set-charset "${database}" | sed 's/\bAUTO_INCREMENT=[0-9]*\s*//g' > "script/sql/open.sql"
        # remove virtual field
        grep -n "GENERATED ALWAYS" script/sql/open.sql | awk -F ":" '{print $1}' | while read -r line;do
            # add -- remove virtual field
            : # sed -i "${line}s/^/-- /" "script/sql/open.sql"
        done
    else
        echo "cannot found any local type configure file in config src directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
elif [[ "$1" == "open_server" ]];then
    # check node name exists
    name=$(basename "$2" ".config")
    if [[ -z "${STY}" ]];then
        mkdir -p "/tmp/screen/"
        echo -n > "/tmp/screen/open_server.log"
        screen -L -Logfile "/tmp/screen/open_server.log" -dmS "open_server"
        screen -x -p 0 -S "open_server" -X stuff "$0 $*; for ((i=10;i>=0;i--));do echo -ne \"exit after \\\${i} seconds ...\"; sleep 1; echo -ne \"\\\r\"; done; exit; \n"
        screen -r "open_server"
    elif [[ ! -f "config/${name}.config" ]];then
        # find local node config src file
        file=$($0 cfg-src find local)
        if [[ -f "${file}" ]];then
            # generate config
            cp "${file}" "config/${name}.config"
            # last server id
            last_server_id=$($0 cfg-src get "${file}" "main, server_id")
            # database connector config
            host=$($0 cfg get "${name}" "main, mysql_connector, host")
            port=$($0 cfg get "${name}" "main, mysql_connector, port")
            user=$($0 cfg get "${name}" "main, mysql_connector, user")
            password=$($0 cfg get "${name}" "main, mysql_connector, password")
            database=$($0 cfg get "${name}" "main, mysql_connector, database")
            # new database
            echo "create database: ${name}"
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --execute="CREATE DATABASE IF NOT EXISTS \`${name}\` DEFAULT CHARACTER SET utf8mb4 DEFAULT COLLATE utf8mb4_unicode_ci;"
            # import sql
            echo "import open sql ..."
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${name}" < "script/sql/open.sql"
            ## new config file ##
            # database
            $0 cfg set "${name}" "main, mysql_connector, database" "\"${name}\""
            # server id
            $0 cfg set "${name}" "main, server_id" "${last_server_id} + 1"
            # open time
            $0 cfg set "${name}" "main, open_time" "$(date -d "$(date -d "now" +%Y-%m-%d)" +%s)"
            # write config src file
            echo "update config src file ..."
            $0 cfg-src set "${file}" "main, server_id" "${last_server_id} + 1"
            # completed
            echo "open server ${name} completed" | sed $'s/.*/\e[32m&\e[m/'
        else
            echo "cannot found any local type configure file in config src directory" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
    elif [[ -z "$2" ]];then
        echo "server name empty" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    else
        echo "configure $2 already in config directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
elif [[ "$1" == "merge_sql" ]];then
    # find local node config src file
    file=$($0 cfg-src find local)
    if [[ -f "${file}" ]];then
        # database connector config
        host=$($0 cfg-src get "${file}" "main, mysql_connector, host")
        port=$($0 cfg-src get "${file}" "main, mysql_connector, port")
        user=$($0 cfg-src get "${file}" "main, mysql_connector, user")
        password=$($0 cfg-src get "${file}" "main, mysql_connector, password")
        database=$($0 cfg-src get "${file}" "main, mysql_connector, database")
        # template marker
        grep -Pn "@make\w+start" script/sql/merge.sql | while read -r line;do
            # relocation
            start_marker=$(echo "${line}" | awk '{print $NF}')
            start=$(grep -Pn "${start_marker}" script/sql/merge.sql | awk -F ":" '{print $1}')
            [[ -z "${start}" ]] && echo "could not locate make sql start marker from file" && exit 1
            end_marker=$(echo "${start_marker}" | awk '{print $NF}' | sed "s/start/end/g")
            end=$(grep -Pn "${end_marker}" script/sql/merge.sql | awk -F ":" '{print $1}')
            [[ -z "${end}" ]] && echo "could not locate make sql end marker from file" && exit 1
            # exit when start better then end
            [[ $start -gt $end ]] && echo "could not locate make sql from file, start make sql marker ${start_marker}:${start} greater then or equals to end make sql marker ${end_marker}:${end}" | sed $'s/.*/\e[31m&\e[m/' >&2 && exit 1
            # take make sql
            # remove first line tail -n +2
            # remove last line head -n -2
            sql=$(sed -n "$((start+1)),$((end-1))p" < script/sql/merge.sql | sed 's/--//g' | paste -sd " ")
            # relocation
            start_marker="${start_marker//make_/}"
            start=$(grep -Pn "${start_marker}" script/sql/merge.sql | awk -F ":" '{print $1}')
            [[ -z "${start}" ]] && echo "could not locate sql start marker from file" && exit 1
            end_marker="${start_marker//start/end}"
            end=$(grep -Pn "${end_marker}" script/sql/merge.sql | awk -F ":" '{print $1}')
            [[ -z "${end}" ]] && echo "could not locate sql end marker from file" && exit 1
            # exit when start better then end
            [[ $start -ge $end ]] && echo "could not locate sql from file, start sql marker ${start_marker}:${start} greater then end sql marker ${end_marker}:${end}" | sed $'s/.*/\e[31m&\e[m/' >&2 && exit 1
            # remove old data exists
            [[ $start -lt $end-1 ]] && sed -i "$((start+1)),$((end-1))d" script/sql/merge.sql
            # query
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" --raw --silent --execute="${sql}" | while read -r line;do
                # write line
                sed -i "$((start+1))i${line}" script/sql/merge.sql
            done
        done
    else
        echo "cannot found any local type configure src file in config directory" | sed $'s/.*/\e[31m&\e[m/'
    fi
elif [[ "$1" == "merge_server" ]];then
    # src
    src=$(basename "$2" ".config")
    src_file="config/${src}.config"
    # dst
    dst=$(basename "$3" ".config")
    dst_file="config/${dst}.config"
    if [[ "${src}" == "${dst}" ]];then
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
        # src database connector config
        src_host=$($0 cfg get "${src_file}" "main, mysql_connector, host")
        src_port=$($0 cfg get "${src_file}" "main, mysql_connector, port")
        src_user=$($0 cfg get "${src_file}" "main, mysql_connector, user")
        src_password=$($0 cfg get "${src_file}" "main, mysql_connector, password")
        src_database=$($0 cfg get "${src_file}" "main, mysql_connector, database")
        # node type
        src_node_type=$($0 cfg get "${src_file}" "main, mysql_connector, node_type")
        # src server id
        src_server_id=$($0 cfg get "${src_file}" "main, server_id")
        src_server_id_list=$($0 cfg get "${src_file}" "main, server_id_list")
        # dst database connector config
        dst_host=$($0 cfg get "${dst_file}" "main, mysql_connector, host")
        dst_port=$($0 cfg get "${dst_file}" "main, mysql_connector, port")
        dst_user=$($0 cfg get "${dst_file}" "main, mysql_connector, user")
        dst_password=$($0 cfg get "${dst_file}" "main, mysql_connector, password")
        dst_database=$($0 cfg get "${dst_file}" "main, mysql_connector, database")
        # node type
        dst_node_type=$($0 cfg get "${dst_file}" "main, mysql_connector, node_type")
        # dst server id
        dst_server_id=$($0 cfg get "${dst_file}" "main, server_id")
        dst_server_id_list=$($0 cfg get "${dst_file}" "main, server_id_list")
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
        cp "script/sql/merge.sql" "script/sql/merge_server.sql"
        # remove commemt
        sed -i '/^--/d' "script/sql/merge_server.sql"
        # replace src
        sed -i "s/{{src}}/\`${src_database}\`/g" "script/sql/merge_server.sql"
        # replace dst
        sed -i "s/{{dst}}/\`${dst_database}\`/g" "script/sql/merge_server.sql"
        # replace src server id
        sed -i "s/{{src_server_id}}/${src_server_id}/g" "script/sql/merge_server.sql"
        # replace dst server id
        sed -i "s/{{dst_server_id}}/${dst_server_id}/g" "script/sql/merge_server.sql"
        # remove \n
        sed -i ":label;N;s/\n/ /g;b label" "script/sql/merge_server.sql"
        # replace \n to ;\n
        sed -i "s/;/;\n/g" "script/sql/merge_server.sql"
        # start merge table
        echo "start merge table ..."
        IFS=';'
        while read -r line;do
            [[ "${line}" =~ "UPDATE" ]] && echo "${line}" | awk '{print "UPDATE: "$2}'
            [[ "${line}" =~ "INSERT" ]] && echo "${line}" | awk '{print "INSERT: "$3}'
            # execute merge sql script
            mysql --host="${dst_host}" --port="${dst_port}" --user="${dst_user}" --password="${dst_password}" --execute="${line}" || exit 1
        done <<<"$(cat "script/sql/merge_server.sql")"
        # drop database
        echo "merge table completed, drop src database ..."
        mysql --host="${src_host}" --port="${src_port}" --user="${src_user}" --password="${src_password}" --execute="DROP DATABASE IF EXISTS \`${src_database}\`;"
        # remove config file
        rm "${src_file}"
        # remove temp file
        rm -f "script/sql/merge_server.sql"
        ##  after merge  ##
        echo "update dst server(${dst}) config ..."
        # merge src merge server id list src server id and dst merge server id list
        now=$(date -d "$(date -d "now" +%Y-%m-%d)" +%s)
        $0 cfg set "${dst_file}" "main, server_id_list" "[{${src_server_id}, ${now}}] ++ ${src_server_id_list} ++ ${dst_server_id_list}"
        # completed
        echo "merge src server ${src} to dst server ${dst}" | sed $'s/.*/\e[32m&\e[m/'
    elif [[ -z "${src}" ]];then
        echo "src server empty" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    elif [[ -z "${dst}" ]];then
        echo "dst server empty" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    elif [[ ! -f "${src_file}" ]];then
        echo "cannot found $2 in config directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    elif [[ ! -f "${dst_file}" ]];then
        echo "cannot found $3 in config directory" | sed $'s/.*/\e[31m&\e[m/' >&2
        exit 1
    fi
elif [[ "$1" = "cfg" || "$1" == "cfg-src" ]];then
    # config dir
    dir=$(echo "$1" | awk -F "-" '{print $2}')
    if [[ "$2" == "get" ]];then
        # config
        config=$(basename "$3" ".config")
        config_file="config/${dir}/${config}.config"
        if [[ -f "${config_file}" && -n "$4" ]];then
            # cfg/cfg-src get local "main, server_id"
            erl +pc unicode +B -boot no_dot_erlang -noshell -eval "io:setopts([{encoding, unicode}]), V = lists:foldl(fun(K, A) -> proplists:get_value(K, A) end, hd(element(2, file:consult(\"${config_file}\"))), [$4]), case io_lib:printable_list(V) of true when V =/= [] -> io:format(\"~ts\", [V]); _ -> io:format(\"~0tp\", [V]) end, erlang:halt()."
        elif [[ -z "$3" ]];then
            echo "config empty" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        elif [[ ! -f "${config_file}" ]];then
            echo "cannot found $3 in config ${dir} directory" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        else
            echo "key not set" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
    elif [[ "$2" == "set" ]];then
        # config
        config=$(basename "$3" ".config")
        config_file="config/${dir}/${config}.config"
        if [[ -f "${config_file}" && -n "$4" && -n "$5" ]];then
            # cfg/cfg-src set local "main, server_id" 10010
            erl +pc unicode +B -boot no_dot_erlang -noshell -eval "file:write_file(\"${config_file}\", io_lib:format(\"~p.\", lists:foldl(fun(K, [A, P | T]) -> [lists:keystore(K, 1, P, {K, A}) | T] end, [$5 | lists:foldl(fun(K, [A | T]) -> [proplists:get_value(K, A), A | T] end, element(2, file:consult(\"${config_file}\")), lists:droplast([$4]))], lists:reverse([$4])))), erlang:halt()"
            # format config file
            erl +pc unicode +B -boot no_dot_erlang -noshell -eval "file:write_file(\"${config_file}\", [\"[\\n\", string:join(lists:reverse(hd(lists:foldl(fun F({K, V}, [S, D]) -> case is_atom(V) orelse is_number(V) orelse io_lib:printable_list(V) of true -> [[io_lib:format(\"~s{~p, ~s~tp}\", [lists:duplicate(D * 4, 16#20), K, lists:duplicate(52 - (D * 4 + 1 + length(lists:concat([K]))), 16#20), V]) | S], D]; false -> [[io_lib:format(\"~s{~p, [\\n~ts\\n~s]}\", [lists:duplicate(D * 4, 16#20), K, string:join(lists:reverse(hd(lists:foldl(F, [[], D + 1], V))), \",\\n\"), lists:duplicate(D * 4, 16#20)]) | S], D] end end, [[], 1], hd(element(2, file:consult(\"${config_file}\")))))), \",\\n\"), \"\\n].\"]), erlang:halt()"
        elif [[ -z "$3" ]];then
            echo "config empty" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        elif [[ ! -f "${config_file}" ]];then
            echo "cannot found $3 in config ${dir} directory" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        elif [[ -z "$4" ]];then
            echo "key not set" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        elif [[ -z "$5" ]];then
            echo "value not set" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
    elif [[ "$2" == "find" ]];then
        if [[ -n "$3" ]];then
            erl +pc unicode +B -boot no_dot_erlang -noshell -eval "io:format([File || File <- filelib:wildcard(\"config/${dir}/*.config\"), proplists:get_value(node_type, proplists:get_value(main, hd(element(2, file:consult(File))))) == $3]),erlang:halt()."
        else
            echo "node type not set" | sed $'s/.*/\e[31m&\e[m/' >&2
            exit 1
        fi
    else
        echo "unknown option $2" | sed $'s/.*/\e[31m&\e[m/' >&2
        helps
        exit 1
    fi
elif [[ "$1" = "pt" ]];then
    name=$2
    shift 2
    escript "script/make/protocol/protocol_script_${name}.erl" "$@"
    escript "script/make/script/router_script.erl"
elif [[ "$1" = "protocol" ]];then
    shift 1
    find "script/make/protocol/" -name "*.erl" -exec escript {} "$@" \;
    escript "script/make/script/router_script.erl"
elif [[ "$1" == "excel" ]];then
    shift 1
    escript "script/make/script/excel_script.erl" "$@"
elif [[ "$1" == "table" || "$1" == "xml" ]];then
    escript "script/make/script/excel_script.erl" "$@"
elif [[ "$1" == "record" ]];then
    shift 1
    escript "script/make/script/record_script.erl" "$@"
elif [[ "$1" == "sql" ]];then
    shift 1
    escript "script/make/script/sql_script.erl" "$@"
elif [[ "$1" == "data" ]];then
    shift 1
    escript "script/make/script/data_script.erl" "$@"
elif [[ "$1" == "lua" ]];then
    shift 1
    escript "script/make/script/lua_script.erl" "$@"
elif [[ "$1" == "js" ]];then
    shift 1
    escript "script/make/script/js_script.erl" "$@"
elif [[ "$1" == "log" ]];then
    shift 1
    escript "script/make/script/log_script.erl" "$@"
elif [[ "$1" == "word" ]];then
    shift 1
    escript "script/make/script/word_script.erl" "$@"
elif [[ "$1" == "key" ]];then
    shift 1
    escript "script/make/script/key_script.erl" "$@"
elif [[ "$1" == "config" ]];then
    shift 1
    escript "script/make/script/config_script.erl" "$@"
elif [[ "$1" == "router" ]];then
    shift 1
    escript "script/make/script/router_script.erl" "$@"
elif [[ "$1" == "loop" ]];then
    shift 1
    escript "script/make/script/loop_script.erl" "$@"
elif [[ "$1" == "map" ]];then
    shift 1
    escript "script/make/script/map_script.erl" "$@"
elif [[ "$1" == "attribute" ]];then
    shift 1
    escript "script/make/script/attribute_script.erl" "$@"
elif [[ "$1" == "asset" ]];then
    shift 1
    escript "script/make/script/asset_script.erl" "$@"
elif [[ "$1" == "event" ]];then
    shift 1
    escript "script/make/script/event_script.erl" "$@"
else
    [[ "$1" != "helps" ]] && echo "unknown option: $1" | sed $'s/.*/\e[31m&\e[m/' >&2
    helps
    exit 1
fi
