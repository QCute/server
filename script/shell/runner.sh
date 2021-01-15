#!/usr/bin/bash

# script path
script=$(dirname "$0")
# enter project root directory
cd "${script}/../../" || exit

# get first device(not virtual)
# DEVICE=$(diff /sys/class/net/ /sys/devices/virtual/net/ | grep -P "(?i)only\s*in\s*/sys/class/net/" | head -n 1 | awk '{print $NF}')
for device in $(diff /sys/class/net/ /sys/devices/virtual/net/ | grep -P "(?i)only\s*in\s*/sys/class/net/" | awk '{print $NF}');do
    [[ -z $(ip address show "${device}" 2>&1 1>/dev/null) ]] && DEVICE="${device}" && break
done
# if physical adapter not found get first non-lo up device
# ifconfig deprecated
# [[ -z ${DEVICE} ]] && DEVICE=$(ifconfig | grep -Po "^[^(lo)]\w+(?=:)" | head -n 1)
[[ -z ${DEVICE} ]] && DEVICE=$(ip address show up scope link | head -n 1 | awk -F ":" '{print $2}' | sed 's/[[:space:]]//g')
# select ipv4 address
IP=$(ip -4 address show "${DEVICE}" | head -n 2 | tail -n 1 | awk '{print $2}' | awk -F "/" '{print $1}')
# select ipv6 address
#IP=$(ip -6 address show "${DEVICE}" | head -n 2 | tail -n 1 | awk '{print $2}' | awk -F "/" '{print $1}')

# date time format
DATE_TIME=$(date "+%Y_%m_%d__%H_%M_%S")

# erl param
ATOM=1048576
PROCESSES=1048576
# Set the distribution buffer busy limit (dist_buf_busy_limit) in kilobytes. Valid range is 1-2097151. Default is 1024.
ZDBBL=1024
HPDS=2

# random node name
# this will loop always in erlang os:cmd
# echo $(strings /dev/urandom | tr -dc A-Za-z0-9 | head -c 16)
function random() {
    echo "$(head -c 256 /dev/urandom | tr -dc A-Za-z0-9 | head -c 16)@${IP}"
}

# collect all nodes
function nodes {
    grep -Plr "\{\s*node_type\s*,\s*$1\s*\}" config/*.config | xargs basename 2>/dev/null | awk -F "." '{print $1}' | sed "s/^/'/g;s/$/@${IP}'/g" | paste -sd ","
}

# collect all modules
function modules {
    echo "$@" | sed "s/^\|$/'/g;s/[[:space:]]/\',\'/g"
}

function helps() {
    echo "usage: $(basename "$0")
    name [bg|sh|stop]                             run/run detached/remote shell/stop node
    [name|-] [-load|-force] modules ...           load modules on node/nodes
    [name|-] -eval script                         execute script on node/nodes
    [name|-] -sql [script]                        execute sql script on node/nodes
    - start                                       start nodes
    - stop                                        stop nodes

wildcard flag '-' can use node type restrict, such as:
    $(basename "$0") -local -load ...
    $(basename "$0") -center -eval ...
    $(basename "$0") -world -sql ..."
}

# start
if [[ $# == 0 ]];then
    # list all run nodes
    helps
elif [[ "$1" == "-" || "$1" == "-local" || "$1" == "-center" || "$1" == "-world" ]];then
    # type 
    if [[ "$1" == "-" ]];then
        type="local|center|world"
        # cookie
        COOKIE=$(grep -Pro "(?<=\{)\s*cookie\s*,\s*\w+\s*(?=\})" config/src/*.config.src | sed 's/[[:space:]]\|cookie\|,//g' | awk -F ":" '{print $2}' | sort -u | paste -sd " ")
        # exit when config cookie not found
        [[ -z "${COOKIE}" ]] && echo "could not found any cookie from config file" && exit
        # exit when config cookie not match
        [[ "$(echo "${COOKIE}" | awk '{print NF}')" != "1" ]] && echo "cookie not match: ${COOKIE}" && exit
    else
        type="${1##-}"
        # cookie
        COOKIE=$(grep -Po "(?<=\{)\s*cookie\s*,\s*\w+\s*(?=\})" "config/src/${1##-}.config.src" | sed 's/[[:space:]]\|cookie\|,//g')
        # exit when config cookie not found
        [[ -z "${COOKIE}" ]] && echo "could not found cookie from config file: config/src/${1##-}.config.src" && exit
    fi
    # function
    if [[ "$2" == "-load" || "$2" == "-force" ]];then
        # load module on all node (nodes provide by config file)
        mode="${2##-}"
        shift 2
        erl -noinput +K true +sub true +pc unicode -hidden -pa beam -pa config -pa config/app +hpds "${HPDS}" +P "${PROCESSES}" +t "${ATOM}" +zdbbl "${ZDBBL}" -setcookie "${COOKIE}" -name "$(random)" -eval "beam:load([$(nodes "${type}")], [$(modules "$@")], '${mode}'), erlang:halt()." 1> >(sed $'s/\\bmodule\\b/\e[32m&\e[m/g;s/\\bskip\\b/\e[34m&\e[m/g;s/\\berror\\b/\e[31m&\e[m/g'>&1) 2> >(sed $'s/.*/\e[31m&\e[m/'>&2)
    elif [[ "$2" == "-eval" ]];then
        # eval script on all node (nodes provide by config file)
        erl -noinput +K true +sub true +pc unicode -hidden -pa beam -pa config -pa config/app +hpds "${HPDS}" +P "${PROCESSES}" +t "${ATOM}" +zdbbl "${ZDBBL}" -setcookie "${COOKIE}" -name "$(random)" -eval "parser:evaluate([$(nodes "${type}")], \"$3\"), erlang:halt()."
    elif [[ "$2" == "-sql" ]];then
        # run on all nodes
        # for one in $(find config/ -name "*.config" | grep -Po "\w+(?=\.config)");do
        # find config/ -name "*.config" | while read -r config
        grep -Plr "\{\s*node_type\s*,\s*${type}\s*\}" config/*.config | awk -F ":" '{print $1}' | while read -r config;do
            # run as detached mode by default
            "$0" "${config}" -sql "$3"
        done;
    else
        echo "unknown option: $2"
        helps
    fi
elif [[ -f "config/$(basename "$1" ".config" 2>/dev/null).config" ]];then
    # config file
    NAME=$(basename "$1" ".config" 2>/dev/null)
    NODE="${NAME}@${IP}"
    CONFIG="config/${NAME}"
    CONFIG_FILE="${CONFIG}.config"
    DUMP="logs/${NAME}_erl_crash.dump"
    # log
    KERNEL_LOG="logs/${NAME}_${DATE_TIME}.log"
    SASL_LOG="logs/${NAME}_${DATE_TIME}.sasl"
    # cookie
    COOKIE=$(grep -Po "(?<=\{)\s*cookie\s*,\s*\w+\s*(?=\})" "${CONFIG_FILE}" | sed 's/[[:space:]]//g' | awk -F "," '{print $2}')
    # exit when config cookie not found
    [[ -z "${COOKIE}" ]] && echo "could not found cookie from config file: ${CONFIG_FILE}" && exit
    # function
    if [[ -z "$2" ]];then
        # interactive mode, print sasl log to tty
        erl +K true +sub true +pc unicode -hidden -pa beam -pa config -pa config/app +hpds "${HPDS}" +P "${PROCESSES}" +t "${ATOM}" +zdbbl "${ZDBBL}" -setcookie "${COOKIE}" -name "${NODE}" -config "${CONFIG}" -env ERL_CRASH_DUMP "${DUMP}" -boot start_sasl -s main start
    elif [[ "$2" == "bg" ]];then
        # detached mode, print sasl log to file
        erl -detached -noinput +K true +sub true +pc unicode -hidden -pa beam -pa config -pa config/app +hpds "${HPDS}" +P "${PROCESSES}" +t "${ATOM}" +zdbbl "${ZDBBL}" -setcookie "${COOKIE}" -name "${NODE}" -config "${CONFIG}" -env ERL_CRASH_DUMP "${DUMP}" -boot start_sasl -kernel error_logger \{file,\""${KERNEL_LOG}"\"\} -sasl sasl_error_logger \{file,\""${SASL_LOG}"\"\} -s main start
    elif [[ "$2" == "sh" ]];then
        # remote shell node
        erl +K true +sub true +pc unicode -hidden -pa beam -pa config -pa config/app +hpds "${HPDS}" +P "${PROCESSES}" +t "${ATOM}" +zdbbl "${ZDBBL}" -setcookie "${COOKIE}" -name "$(random)" -config "${CONFIG}" -remsh "${NODE}"
    elif [[ "$2" == "-load" || "$2" == "-force" ]];then
        # load module on one node
        mode="${2##-}"
        shift 2
        erl -noinput +K true +sub true +pc unicode -hidden -pa beam -pa config -pa config/app +hpds "${HPDS}" +P "${PROCESSES}" +t "${ATOM}" +zdbbl "${ZDBBL}" -setcookie "${COOKIE}" -name "$(random)" -eval "beam:load(['${NODE}'], [$(modules "$@")], '${mode}'), erlang:halt()." 1> >(sed $'s/\\bmodule\\b/\e[32m&\e[m/g;s/\\bskip\\b/\e[34m&\e[m/g;s/\\berror\\b/\e[31m&\e[m/g'>&1) 2> >(sed $'s/.*/\e[31m&\e[m/'>&2)
    elif [[ "$2" == "-eval" ]];then
        # eval script on one node
        erl -noinput +K true +sub true +pc unicode -hidden -pa beam -pa config -pa config/app +hpds "${HPDS}" +P "${PROCESSES}" +t "${ATOM}" +zdbbl "${ZDBBL}" -setcookie "${COOKIE}" -name "$(random)" -eval "parser:evaluate(['${NODE}'], \"$3\"), erlang:halt()."
    elif [[ "$2" == "-sql" ]];then
        # find connect info from config
        host=$(grep -Po "\{\s*host\s*,\s*\{?\s*(local)?,?\s*\".*?\"\s*\}?\s*\}" "${CONFIG_FILE}" | grep -Po "(?<=\").*?(?=\")" | awk '{ if ( system("test -S " $1) ) { print $1 } else { print "127.0.0.1" } }')
        port=$(grep -Po "\{\s*port\s*,\s*\d+\s*\}" "${CONFIG_FILE}" | grep -Po "\d+" | awk '{ if ( $1 == 0 ) { print "3306" } else { print $1 } }')
        user=$(grep -Po "\{\s*user\s*,\s*\"\w+\"\s*\}" "${CONFIG_FILE}" | grep -Po "(?<=\")\w+(?=\")")
        password=$(grep -Po "\{\s*password\s*,\s*\"\w+\"\s*\}" "${CONFIG_FILE}" | grep -Po "(?<=\")\w+(?=\")")
        database=$(grep -Po "\{\s*database\s*,\s*\"\w+\"\s*\}" "${CONFIG_FILE}" | grep -Po "(?<=\")\w+(?=\")")
        # execute sql
        if [[ -n "${database}" && -z "$3" ]];then
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}"
        elif [[ -n "${database}" && -n "$3" ]];then
            echo "database \`${database}\` execute result:"
            mysql --host="${host}" --port="${port}" --user="${user}" --password="${password}" --database="${database}" --execute="$3"
        else
            echo "cannot found database name in config file: ${CONFIG_FILE}"
        fi
    else
        echo "unknown option: $2"
        helps
    fi
else
    echo "unknown option: $1"
    helps
fi
