#!/usr/bin/bash

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
    helps                                         lookup help manual
    "
}

## execute function
if [[ $# == 0 ]];then
    helps
elif [[ "$1" == "debug" ]] && [[ "$2" == "" ]];then
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
    cd "${script}/../../" || exit
    # erl "${OPTIONS}" -make
    # erl -pa ../../beam/ -make
    emake='{["src/*", "src/*/*", "src/*/*/*", "src/*/*/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, debug_info, {d, '\'DEBUG\'', true}]}'
    erl -pa beam/ -noinput -eval "make:all([{emake, [${emake}]}]), erlang:halt()."
    $0 beam compile
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
    cd "${script}/../../" || exit
    # erl -pa ../../beam/ -make
    emake='{["src/*", "src/*/*", "src/*/*/*", "src/*/*/*/*", "src/lib/*/src/*"], [{i, "include/"}, {outdir, "beam/"}, warnings_as_errors, native, {hipe, o3}]}'
    erl -pa beam/ -noinput -eval "make:all([{emake, [${emake}]}]), erlang:halt()."
    # user_default must compile with debug info mode (beam abstract code contain)
    # use abs path "$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)/$(basename $0)" beam compile
    # "../shell/$(basename "$0")" beam compile
    $0 beam
elif [[ "$1" = "release" ]];then
    ## make one
    cd "${script}/../../" || exit
    file=$(find src/ -name "$2.erl" 2>/dev/null)
    if [[ "${file}" == "" ]];then
        echo "$2.erl: no such file or directory"
    else
        erlc -I include -o beam -Werror +"{hipe,o3}" +native "${file}"
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
    dialyzer --no_check_plt -I "${script}"/../../include/ --src -r "${script}"/../../src/
elif [[ "$1" = "maker" ]];then
    cd "${script}/../../" || exit
    # erl -make
    emake='{["script/make/maker/*", "src/tool/*/*", "src/lib/*/src/*"], [{i, "include"}, {outdir, "beam/"}, debug_info, warnings_as_errors, native, {hipe, o3}]}'
    erl -pa beam/ -noinput -eval "make:all([{emake, [${emake}]}]), erlang:halt()."
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
        echo "tag not found, please check tag exists"
        exit
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
    if [[ -z "${start}" ]];then
        echo "start date not found, please support valid date format"
        exit
    fi
    # if now line number not found, use end of file line number
    if [[ -z "${end}" ]];then
        # confirm replace method
        read -p "now tag not found, use end file replace it ?(y/Y): " confirm
        if [[ "${confirm}" == "y" || "${confirm}" == "Y" ]];then
            end=$(wc -l "${sql}" | awk '{print $1}')
        else
            exit
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
    # confirm replace method
    read -p "now tag not found, use end file replace it ?(y/Y/n/N): " confirm
    [[ "${confirm}" != "y" && "${confirm}" != "Y" ]] && exit
    # for config in $(find config/ -name "*.config");do
    find "${script}/../../config/" -name "*.config" | while read -r config;do
        # exact but slow
        # user=$(erl -noinput -boot start_clean -eval "erlang:display(proplists:get_value(user, proplists:get_value(mysql_connector, proplists:get_value(main, hd(element(2, file:consult(\"${config}\")))), []), [])),erlang:halt().")
        # password=$(erl -noinput -boot start_clean -eval "erlang:display(proplists:get_value(password, proplists:get_value(mysql_connector, proplists:get_value(main, hd(element(2, file:consult(\"${config}\")))), []), [])),erlang:halt().")
        # database=$(erl -noinput -boot start_clean -eval "erlang:display(proplists:get_value(database, proplists:get_value(mysql_connector, proplists:get_value(main, hd(element(2, file:consult(\"${config}\")))), []), [])),erlang:halt().")
        # sketchy but fast
        host=$(grep -Po "\{\s*host\s*,\s*\{?\s*(local)?,?\s*\".*?\"\s*\}?\s*\}" "${config}" | grep -Po "(?<=\").*?(?=\")" | awk '{ if ( system("test -S " $1) ) { print $1 } else { print "127.0.0.1" } }')
        port=$(grep -Po "\{\s*port\s*,\s*\d+\s*\}" "${config}" | grep -Po "\d+" | awk '{ if ( $1 == 0 ) { print "3306" } else { print $1 } }')
        user=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        password=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        database=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        if [[ -n "${database}" ]];then
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" < "${script}/../../script/sql/need.sql"
        fi
    done
elif [[ "$1" = "import" ]];then
    # cd "${script}/../../" || exit
    config=$(basename "$2" ".config")
    if [[ -f "${script}/../../config/$2.config" ]];then
        # sketchy but fast
        host=$(grep -Po "\{\s*host\s*,\s*\{?\s*(local)?,?\s*\".*?\"\s*\}?\s*\}" "${script}/../../config/${config}.config" | grep -Po "(?<=\").*?(?=\")" | awk '{ if ( system("test -S " $1) ) { print $1 } else { print "127.0.0.1" } }')
        port=$(grep -Po "\{\s*port\s*,\s*\d+\s*\}" "${script}/../../config/${config}.config" | grep -Po "\d+" | awk '{ if ( $1 == 0 ) { print "3306" } else { print $1 } }')
        user=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${script}/../../config/${config}.config" | grep -Po "(?<=\")\w+(?=\")")
        password=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${script}/../../config/${config}.config" | grep -Po "(?<=\")\w+(?=\")")
        database=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${script}/../../config/${config}.config" | grep -Po "(?<=\")\w+(?=\")")
        if [[ -n "${database}" ]];then
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" < "${script}/../../script/sql/need.sql"
        else
            echo "configure not contain database data"
        fi
    else
        echo "$2.config: no such configure in config directory"
    fi
elif [[ "$1" == "open_sql" ]];then
    # find local node config src file
    config=$(grep -Pr "\{node_type,\s*local\}" "${script}"/../../config/src/*.config.src | awk -F ":" '{print $1}' | head -n 1)
    if [[ -f "${config}" ]];then
        host=$(grep -Po "\{\s*host\s*,\s*\{?\s*(local)?,?\s*\".*?\"\s*\}?\s*\}" "${config}" | grep -Po "(?<=\").*?(?=\")" | awk '{ if ( system("test -S " $1) ) { print $1 } else { print "127.0.0.1" } }')
        port=$(grep -Po "\{\s*port\s*,\s*\d+\s*\}" "${config}" | grep -Po "\d+" | awk '{ if ( $1 == 0 ) { print "3306" } else { print $1 } }')
        user=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        password=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        database=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        # dump
        mysqldump --host="${host}" --port="${port}" --user="${user}" --password="${password}" --no-data --compact --add-drop-table "${database}" | sed 's/\bAUTO_INCREMENT=[0-9]*\s*//g' > "${script}/../../script/sql/open.sql"
        grep -n "GENERATED ALWAYS" "${script}"/../../script/sql/open.sql | awk -F ":" '{print $1}' | while read -r line;do
            # add -- remove virtual field
            : # sed -i "${line}s/^/-- /" "${script}/../../script/sql/open.sql"
        done
    else
        echo "cannot found any local type configure src file in config directory"
    fi
elif [[ "$1" == "open_server" ]];then
    # check node name exists
    name=$(basename "$2" ".config")
    if [[ ! -f "${script}/../../config/${name}.config" ]];then
        # find local node config src file
        config=$(grep -Pr "\{node_type,\s*local\}" "${script}"/../../config/src/*.config.src | awk -F ":" '{print $1}' | head -n 1)
        if [[ -f "${config}" ]];then
            # next server id
            new_server_id=$(grep -Po "\{\s*server_id\s*,\s*\d+\s*\}" "${config}" | grep -Po "\d+" | awk '{print $1+1}')
            # check src server id and last server id is matched ?
            files=$(grep -Pr "\{\s*node_type\s*,\s*local\s*\}" "${script}"/../../config/*.config "${script}"/../../config/src/*.config.src | awk -F ":" '{print $1}' | paste -sd " ")
            last_server_id=$(grep -Pro "\{\s*server_id\s*,\s*\d+\s*\}" ${files} | grep -Po "\d+" | sort -n | tail -n 1 | awk '{print $1+1}')
            [[ "${new_server_id}" != "${last_server_id}" ]] && echo "src server id: ${new_server_id} is not equal to last server id: ${last_server_id}" && exit
            # config
            host=$(grep -Po "\{\s*host\s*,\s*\{?\s*(local)?,?\s*\".*?\"\s*\}?\s*\}" "${config}" | grep -Po "(?<=\").*?(?=\")" | awk '{ if ( system("test -S " $1) ) { print $1 } else { print "127.0.0.1" } }')
            port=$(grep -Po "\{\s*port\s*,\s*\d+\s*\}" "${config}" | grep -Po "\d+" | awk '{ if ( $1 == 0 ) { print "3306" } else { print $1 } }')
            user=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
            password=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
            # new database
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --execute="CREATE DATABASE IF NOT EXISTS \`${name}\` DEFAULT CHARACTER SET utf8mb4 DEFAULT COLLATE utf8mb4_unicode_ci;"
            # import sql
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${name}" < "${script}/../../script/sql/open.sql"
            # copy config file
            cp "${config}" "${script}/../../config/${name}.config"
            # replace src file server id
            sed -i "$(grep -Pn "\{\s*server_id\s*,\s*\d+\s*\}" "${config}" | awk -F ":" '{print $1}')s/[[:digit:]]\+/${new_server_id}/" "${config}"
            # replace database
            sed -Ei "$(grep -Pn "\{\s*database\s*,\s*\"\w+\"\s*\}" "${script}/../../config/${name}.config" | awk -F ":" '{print $1}')s/\"\w+\"/\"${name}\"/" "${script}/../../config/${name}.config"
            # replace server id
            sed -i "$(grep -Pn "\{\s*server_id\s*,\s*\d+\s*\}" "${script}/../../config/${name}.config" | awk -F ":" '{print $1}')s/[[:digit:]]\+/${new_server_id}/" "${script}/../../config/${name}.config"
            # replace open time
            sed -i "$(grep -Pn "\{\s*open_time\s*,\s*\d+\s*\}" "${script}/../../config/${name}.config" | awk -F ":" '{print $1}')s/[[:digit:]]\+/$(date -d "$(date -d "now" +%Y-%m-%d)" +%s)/" "${script}/../../config/${name}.config"
        else
            echo "cannot found any local type configure src file in config directory"
        fi
    else
        echo "configure $2 already in config directory"
    fi
elif [[ "$1" == "merge_sql" ]];then
    # find local node config src file
    config=$(grep -Pr "\{node_type,\s*local\}" "${script}"/../../config/src/*.config.src | awk -F ":" '{print $1}' | head -n 1)
    if [[ -f "${config}" ]];then
        ## generate the update sql ##
        # reset auto increment offset
        start=$(grep -n "@make_update_sql_start" script/sql/merge.sql | awk -F ":" '{print $1+1}' | head -n 1)
        end=$(grep -n "@make_update_sql_end" script/sql/merge.sql | awk -F ":" '{print $1-1}' | head -n 1)
        sql=$(sed -n "${start},${end}p" < "${script}/../../script/sql/merge.sql" | sed 's/--//g')
        start=$(grep -n "@update_sql_start" "${script}/../../script/sql/merge.sql" | awk -F ":" '{print $1+1}' | bc)
        end=$(grep -n "@update_sql_end" "${script}/../../script/sql/merge.sql" | awk -F ":" '{print $1-1}' | bc)
        # remove old merge sql
        [[ $(expr "${start}" "<" "${end}") == "1" ]] && sed -i "${start},${end}d" "${script}/../../script/sql/merge.sql"
        # config
        host=$(grep -Po "\{\s*host\s*,\s*\{?\s*(local)?,?\s*\".*?\"\s*\}?\s*\}" "${config}" | grep -Po "(?<=\").*?(?=\")" | awk '{ if ( system("test -S " $1) ) { print $1 } else { print "127.0.0.1" } }')
        port=$(grep -Po "\{\s*port\s*,\s*\d+\s*\}" "${config}" | grep -Po "\d+" | awk '{ if ( $1 == 0 ) { print "3306" } else { print $1 } }')
        user=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        password=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        database=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        # replace database
        sql=${sql/"{{database}}"/"${database}"}
        # query
        mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" --raw --silent --execute="${sql}" | while read -r line;do
            [[ "${line}" =~ "Unknown Reference Field: " ]] && echo "${line}" && exit
            # write
            sed -i "${start}i${line}" "${script}/../../script/sql/merge.sql"
        done
        ## generate the update server id sql ##
        # reset server id
        start=$(grep -n "@make_update_server_id_sql_start" script/sql/merge.sql | awk -F ":" '{print $1+1}' | head -n 1)
        end=$(grep -n "@make_update_server_id_sql_end" script/sql/merge.sql | awk -F ":" '{print $1-1}' | head -n 1)
        sql=$(sed -n "${start},${end}p" < "${script}/../../script/sql/merge.sql" | sed 's/--//g')
        start=$(grep -n "@update_server_id_sql_start" "${script}/../../script/sql/merge.sql" | awk -F ":" '{print $1+1}' | bc)
        end=$(grep -n "@update_server_id_sql_end" "${script}/../../script/sql/merge.sql" | awk -F ":" '{print $1-1}' | bc)
        # remove old merge sql
        [[ $(expr "${start}" "<" "${end}") == "1" ]] && sed -i "${start},${end}d" "${script}/../../script/sql/merge.sql"
        # config
        host=$(grep -Po "\{\s*host\s*,\s*\{?\s*(local)?,?\s*\".*?\"\s*\}?\s*\}" "${config}" | grep -Po "(?<=\").*?(?=\")" | awk '{ if ( system("test -S " $1) ) { print $1 } else { print "127.0.0.1" } }')
        port=$(grep -Po "\{\s*port\s*,\s*\d+\s*\}" "${config}" | grep -Po "\d+" | awk '{ if ( $1 == 0 ) { print "3306" } else { print $1 } }')
        user=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        password=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        database=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
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
        # config
        host=$(grep -Po "\{\s*host\s*,\s*\{?\s*(local)?,?\s*\".*?\"\s*\}?\s*\}" "${config}" | grep -Po "(?<=\").*?(?=\")" | awk '{ if ( system("test -S " $1) ) { print $1 } else { print "127.0.0.1" } }')
        port=$(grep -Po "\{\s*port\s*,\s*\d+\s*\}" "${config}" | grep -Po "\d+" | awk '{ if ( $1 == 0 ) { print "3306" } else { print $1 } }')
        user=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        password=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        database=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
        # replace database
        sql=${sql/"{{database}}"/"${database}"}
        # query
        mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" --raw --silent --execute="${sql}" | while read -r line;do
            # write
            sed -i "${start}i${line}" "${script}/../../script/sql/merge.sql"
        done
    else
        echo "cannot found any local type configure src file in config directory"
    fi
elif [[ "$1" == "merge_server" ]];then
    # check node name exists
    src=$(basename "$2" ".config")
    dst=$(basename "$3" ".config")
    if [[ -f "${script}/../../config/${src}.config" && -f "${script}/../../config/${dst}.config" ]];then
        # find local node config src file
        config=$(grep -Pr "\{node_type,\s*local\}" "${script}"/../../config/src/*.config.src | awk -F ":" '{print $1}' | head -n 1)
        if [[ -f "${config}" ]];then
            # config
            host=$(grep -Po "\{\s*host\s*,\s*\{?\s*(local)?,?\s*\".*?\"\s*\}?\s*\}" "${config}" | grep -Po "(?<=\").*?(?=\")" | awk '{ if ( system("test -S " $1) ) { print $1 } else { print "127.0.0.1" } }')
            port=$(grep -Po "\{\s*port\s*,\s*\d+\s*\}" "${config}" | grep -Po "\d+" | awk '{ if ( $1 == 0 ) { print "3306" } else { print $1 } }')
            user=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
            password=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${config}" | grep -Po "(?<=\")\w+(?=\")")
            # get src server id
            src_server_id=$(grep -Po "\{\s*server_id\s*,\s*\d+\s*\}" "${script}/../../config/${src}.config" | grep -Po "\d+")
            # get dst server id
            dst_server_id=$(grep -Po "\{\s*server_id\s*,\s*\d+\s*\}" "${script}/../../config/${dst}.config" | grep -Po "\d+")
            # temp file
            cp "${script}/../../script/sql/merge.sql" "${script}/../../script/sql/merge_server.sql"
            # remove commemt
            sed -i '/^--/d' "${script}/../../script/sql/merge_server.sql"
            # replace src
            sed -i "s/{{src}}/\`${src}\`/g" "${script}/../../script/sql/merge_server.sql"
            # replace dst
            sed -i "s/{{dst}}/\`${dst}\`/g" "${script}/../../script/sql/merge_server.sql"
            # replace src server id
            sed -i "s/{{src_server_id}}/${src_server_id}/g" "${script}/../../script/sql/merge_server.sql"
            # replace dst server id
            sed -i "s/{{dst_server_id}}/${dst_server_id}/g" "${script}/../../script/sql/merge_server.sql"
            # remove \n
            sed -i ":label;N;s/\n/ /g;b label" "${script}/../../script/sql/merge_server.sql"
            # replace \n to ;\n
            sed -i "s/;/;\n/g" "${script}/../../script/sql/merge_server.sql"
            IFS=';'
            while read -r line;do
                [[ "${line}" =~ "UPDATE" ]] && echo "${line}" | awk '{print "UPDATE: "$2}'
                [[ "${line}" =~ "INSERT" ]] && echo "${line}" | awk '{print "INSERT: "$3}'
                # execute merge sql script
                mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --execute="${line}" || exit 0
            done <<<"$(cat "${script}/../../script/sql/merge_server.sql")"
            # drop database
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --execute="DROP DATABASE IF EXISTS \`${src}\`;"
            # remove temp file
            rm -f "${script}/../../script/sql/merge_server.sql"
            # insert merge server id to server id list
            src_server_id_list=$(grep -Po "\{\s*server_id_list\s*,\s*\[.*?\]\s*\}" "${script}/../../config/${src}.config" | grep -Po "\[.*?\]")
            dst_server_id_list=$(grep -Po "\{\s*server_id_list\s*,\s*\[.*?\]\s*\}" "${script}/../../config/${dst}.config" | grep -Po "\[.*?\]")
            # merge src merge server id list src server id and dst merge server id list
            server_id_list=$(erl -noinput -boot start_clean -eval "erlang:display([{${src_server_id}, $(date -d "$(date -d "now" +%Y-%m-%d)" +%s)}] ++ ${src_server_id_list} ++ ${dst_server_id_list}),erlang:halt()." | tr -d "\r\n")
            # replace dst config file merge server id list
            sed -i "$(grep -Pn "\bserver_id_list\b" "${script}/../../config/${dst}.config" | awk -F ":" '{print $1}')s/\\[.*\\]/${server_id_list}/" "${script}/../../config/${dst}.config"
            # remove dst config file
            rm -f "${script}/../../config/${src}.config"
        else
            echo "cannot found any local type configure src file in config directory"
        fi
    elif [[ ! -f "${script}/../../config/${src}.config" ]];then
        echo "cannot found $2 in config directory"
    elif [[ ! -f "${script}/../../config/${dst}.config" ]];then
        echo "cannot found $3 in config directory"
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
else
    [[ "$1" != "helps" ]] && echo "unknown option: $1"
    helps
fi
