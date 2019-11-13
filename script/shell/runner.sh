#!/bin/bash

script=$(dirname $0)

cd ${script}/../../

# get first device(not virtual)
# delete virtual from all, remain physical adapter and get first one
DEVICE=$(echo $(ls /sys/class/net/) | sed "s/$(echo $(ls /sys/devices/virtual/net/) | sed 's/[[:space:]]/\\|/g')//g" | head -n 1 | awk '{print $1}')
# select ipv4 address
IP=$(ip address show ${DEVICE} | head -n 3 | tail -n 1 | awk '{print $2}' | awk -F "/" '{print $1}')
# select ipv6 address
#IP=$(ip address show ${device} | head -n 5 | tail -n 1 | awk '{print $2}' | awk -F "/" '{print $1}')

# date time format
DATE_TIME=$(date "+%Y_%m_%d__%H_%M_%S")

# erl param
SMP=disable
ATOM=10485760
PROCESSES=1024000
POLL=true
# Set the distribution buffer busy limit (dist_buf_busy_limit) in kilobytes. Valid range is 1-2097151. Default is 1024.
ZDBBL=1024
# chose config
if [[ "$1" == "" ]] ;then
    NAME=main
    NODE="${NAME}@${IP}"
    CONFIG="config/${NAME}"
    CONFIG_FILE="${CONFIG}.config"
    DUMP="-env ERL_CRASH_DUMP ${NAME}_erl_crash.dump"
elif [[ -f $1 ]];then
    NAME=$(echo "$1"  | awk -F "/" '{print $NF} ' | awk -F "." '{print $1}')
    NODE="${NAME}@${IP}"
    CONFIG="config/${NAME}"
    CONFIG_FILE="${CONFIG}.config"
    DUMP="-env ERL_CRASH_DUMP ${NAME}_erl_crash.dump"
else
    NAME=$1
    NODE="${NAME}@${IP}"
    CONFIG="config/${NAME}"
    CONFIG_FILE="${CONFIG}.config"
    DUMP="-env ERL_CRASH_DUMP ${NAME}_erl_crash.dump"
fi
# first cookie define
COOKIE=$(echo $(grep -Po "(?<=cookie,)\s*.*(?=\})" ${CONFIG_FILE} 2>/dev/null))
# :: set default cookie when config cookie not define 
if [[ "${COOKIE}" == "" ]];then COOKIE=erlang; fi
# log
KERNEL_LOG="logs/${NAME}_${DATE_TIME}.log"
SASL_LOG="logs/${NAME}_${DATE_TIME}.sasl"

# random node name
# this will loop always in erlang os:cmd
# echo $(strings /dev/urandom | tr -dc A-Za-z0-9 | head -c 16)
function random() {
    echo "$(head -c 256 /dev/urandom | tr -dc A-Za-z0-9 | head -c 16)@${IP}"
}

# list all nodes
function nodes {
    # echo $(ls config/ | grep -Po "\w+(?=\.config)" | tr "\n" " " | sed -e "s/ /@${IP} /g")
    echo "'$(echo $(ls config/ | grep -Po "\w+(?=\.config)" | tr "\n" " " | sed -e "s/ /@${IP} /g") | sed "s/[[:space:]]/\',\'/g")'"
}

# start
if [[ ! -n $1 ]];then
    # list all run nodes
    epmd -names
elif [[ "$1" == "+" && "$2" == "" ]];then
    # run all nodes
    for one in $(find config/ -name *.config | grep -Po "\w+(?=\.config)");do
        # run as detached mode by default
        $0 ${one} bg &
    done;
elif [[ -f ${CONFIG_FILE} && "$2" == "" ]];then
    # interactive mode, print sasl log to tty
    erl -hidden +pc unicode -pa beam -pa config -pa app -smp true +P ${PROCESSES} +t ${ATOM} +K ${POLL} +zdbbl ${ZDBBL} -setcookie ${COOKIE} -name ${NODE} -config ${CONFIG} ${DUMP} -boot start_sasl -s main start
elif [[ -f ${CONFIG_FILE} && "$2" == "bg" ]];then
    # detached mode, print sasl log to file
    erl -noinput -detached -hidden +pc unicode -pa beam -pa config -pa app -smp true +P ${PROCESSES} +t ${ATOM} +K ${POLL} +zdbbl ${ZDBBL} -setcookie ${COOKIE} -name ${NODE} -config ${CONFIG} ${DUMP} -boot start_sasl -kernel error_logger \{file,\"${KERNEL_LOG}\"\} -sasl sasl_error_logger \{file,\"${SASL_LOG}\"\} -s main start
elif [[ -f ${CONFIG_FILE} && "$2" == "sh" ]];then
    # remote shell node
    erl -hidden +pc unicode -pa beam -pa config -pa app -setcookie ${COOKIE} -name $(random) -config ${CONFIG} -remsh ${NODE}
elif [[ -f ${CONFIG_FILE} && "$2" == "stop" ]];then
    # stop one node
    erl -noinput -hidden +pc unicode -pa beam -setcookie ${COOKIE} -name $(random) -eval "main:stop_safe(['${NODE}']),erlang:halt()."
elif [[ "$1" == "-" && "$2" == "" ]];then
    # stop all node
    erl -noinput -hidden +pc unicode -pa beam -setcookie ${COOKIE} -name $(random) -eval "main:stop_safe([$(nodes)]),erlang:halt()."
elif [[ -f ${CONFIG_FILE} && "$2" == "eval" && $# -gt 2 ]];then
    # eval script on one node
    erl -noinput -hidden +pc unicode -pa beam -setcookie ${COOKIE} -name $(random) -eval "parser:evaluate(['${NODE}'], \"${3}\"),erlang:halt()."
elif [[ "$1" == "=" && "$2" == "eval" && $# -gt 2 ]];then
    # eval script on all node (nodes provide by local node config)
    erl -noinput -hidden +pc unicode -pa beam -setcookie ${COOKIE} -name $(random) -eval "parser:evaluate([$(nodes)], \"${3}\"),erlang:halt()."
elif [[ "$2" == "eval" && $# == 2 ]];then
    echo no eval script
    exit 1
elif [[ -f ${CONFIG_FILE} ]] && [[ "$2" == "load" || "$2" == "force" ]] && [[ $# -gt 2 ]];then
    # load module on one node
    mode=$2
    shift 2
    modules="'$(echo $@ | sed "s/[[:space:]]/\',\'/g")'"
    erl -noinput -hidden +pc unicode -pa beam -setcookie ${COOKIE} -name $(random) -eval "beam:load(['${NODE}'], [${modules}], '${mode}'),erlang:halt()." 1> >(sed $'s/true/\e[32m&\e[m/g;s/false\\|unloaded/\e[31m&\e[m/g'>&1) 2> >(sed $'s/.*/\e[31m&\e[m/'>&2)
elif [[ "$1" == "=" ]] && [[ "$2" == "load" || "$2" == "force" ]] && [[ $# -gt 2 ]];then
    # load module on all node (nodes provide by local node config)
    mode=$2
    shift 2
    modules="'$(echo $@ | sed "s/[[:space:]]/\',\'/g")'"
    erl -noinput -hidden +pc unicode -pa beam -setcookie ${COOKIE} -name $(random) -eval "beam:load([$(nodes)], [${modules}], '${mode}'),erlang:halt()." 1> >(sed $'s/true/\e[32m&\e[m/g;s/false\\|unloaded/\e[31m&\e[m/g'>&1) 2> >(sed $'s/.*/\e[31m&\e[m/'>&2)
elif [[ "$2" == "load" || "$2" == "force" ]] && [[ $# == 2 ]];then
    echo no load module
    exit 1
elif [[ -f ${CONFIG_FILE} && "$2" == "sql" && $# == 2 ]];then
    USER=$(echo $(grep -Po "(?<=user,)\s*.*(?=\})" ${CONFIG_FILE} 2>/dev/null | sed 's/\"//g'))
    PASSWORD=$(echo $(grep -Po "(?<=password,)\s*.*(?=\})" ${CONFIG_FILE} 2>/dev/null | sed 's/\"//g'))
    DATABASE=$(echo $(grep -Po "(?<=database,)\s*.*(?=\})" ${CONFIG_FILE} 2>/dev/null | sed 's/\"//g'))
    if [[ ${DATABASE} ]];then
        if [[ $(type mycli 2>/dev/null) ]];then
            # use mycli
            mycli --user=${USER} --password=${PASSWORD} --database=${DATABASE}
        else
            # use mysql
            mysql --user=${USER} --password=${PASSWORD} --database=${DATABASE}
        fi
    else
        echo "${CONFIG_FILE}: cannot find database name in this config file"
    fi
elif [[ -f ${CONFIG_FILE} && "$2" == "sql" && $# -gt 2 ]];then
    USER=$(echo $(grep -Po "(?<=user,)\s*.*(?=\})" ${CONFIG_FILE} 2>/dev/null | sed 's/\"//g'))
    PASSWORD=$(echo $(grep -Po "(?<=password,)\s*.*(?=\})" ${CONFIG_FILE} 2>/dev/null | sed 's/\"//g'))
    DATABASE=$(echo $(grep -Po "(?<=database,)\s*.*(?=\})" ${CONFIG_FILE} 2>/dev/null | sed 's/\"//g'))
    if [[ ${DATABASE} ]];then
        if [[ $(type mycli 2>/dev/null) ]];then
            # use mycli
            shift 2
            echo "${DATABASE} execute result:"
            mycli --user=${USER} --password=${PASSWORD} --database=${DATABASE} --execute="$*"
            echo
        else
            # use mysql
            shift 2
            echo "${DATABASE} execute result:"
            mysql --user=${USER} --password=${PASSWORD} --database=${DATABASE} --execute="$*"
            echo
        fi
    else
        echo "${CONFIG_FILE}: cannot find database name in this config file"
        echo
    fi
elif [[ "$1" == "=" && "$2" == "sql" && $# -gt 2 ]];then
    # run all nodes
    for one in $(find config/ -name *.config | grep -Po "\w+(?=\.config)");do
        # run as detached mode by default
        $0 ${one} sql "$3"
    done;
elif [[ ! -f ${CONFIG_FILE} ]];then
    echo config file: ${CONFIG_FILE} not found
    exit 1
elif [[ -n $2 ]];then
    echo unknown option: $2
fi

# return to working directory
cd - > /dev/null
