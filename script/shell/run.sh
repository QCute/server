#!/bin/bash

script=$(dirname $0)

cd ${script}/../../

# get first device(not virtual)
# delete virtual from all, remain physical adapter and get first one
device=$(echo `ls /sys/class/net/` | sed "s/$(echo `ls /sys/devices/virtual/net/` | sed 's/[[:space:]]/\\|/g')//g" | head -n 1 | awk '{print $1}')
# select ipv4 address
IP=$(ip address show ${device} | head -n 3 | tail -n 1 | awk '{print $2}' | awk -F "/" '{print $1}')
# select ipv6 address
#IP=$(ip address show ${device} | head -n 5 | tail -n 1 | awk '{print $2}' | awk -F "/" '{print $1}')

# date time format
date_time=`date "+%Y-%m-%d_%H-%M-%S"`

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
    NODE=main@${IP}
    CONFIG=config/main
    DUMP="-env ERL_CRASH_DUMP main_erl_crash.dump"
else
    NAME=$1
    NODE=$1@${IP}
    CONFIG=config/$1
    DUMP="-env ERL_CRASH_DUMP $1_erl_crash.dump"
fi
# first cookie define
COOKIE=`grep -oP "(?<=cookie,)\s*.*(?=\})" ${CONFIG}".config" 2>/dev/null`
# :: set default cookie when config cookie not define 
if [[ "${COOKIE}" == "" ]];then
    COOKIE=erlang
fi
# log
KERNEL_LOG=logs/${NAME}_${date_time}.log
SASL_LOG=logs/${NAME}_${date_time}.sasl

# start
if [[ ! -n $1 ]];then
    read -p "run all ?(Y/n): " confirm
    if [[ "${confirm}" == "Y" || "${confirm}" == "y" ]];then
        $0 +
    fi
elif [[ "$1" == "+" && "$2" == "" ]];then
    # run all when node not given
    for one in $(find config/ -name *.config | grep -Po "\w+(?=\.config)");do
        # run as detached mode by default
        $0 ${one} bg
    done;
elif [[ "$1" == "-" && "$2" == "" ]];then
    # run all when node not given
    for one in $(find config/ -name *.config | grep -Po "\w+(?=\.config)");do
        # stop async 
        $0 ${one} stop &
    done;
elif [[ "$2" == "" ]];then
    # interactive mode
    erl -hidden +pc unicode -pa beam -pa config -smp true +P ${PROCESSES} +t ${ATOM} +K ${POLL} +zdbbl ${ZDBBL} -setcookie ${COOKIE} -name ${NODE} -config ${CONFIG} ${DUMP} -boot start_sasl -kernel error_logger \{file,\"${KERNEL_LOG}\"\} -sasl sasl_error_logger \{file,\"${SASL_LOG}\"\} -s main start
elif [[ "$2" == "bg" ]];then
    # detached mode
    erl -noinput -detached -hidden +pc unicode -pa beam -pa config -smp true +P ${PROCESSES} +t ${ATOM} +K ${POLL} +zdbbl ${ZDBBL} -setcookie ${COOKIE} -name ${NODE} -config ${CONFIG} ${DUMP} -boot start_sasl -kernel error_logger \{file,\"${KERNEL_LOG}\"\} -sasl sasl_error_logger \{file,\"${SASL_LOG}\"\} -s main start
elif [[ "$2" == "rsh" ]];then
    # remote shell node
    random=$(strings /dev/urandom | tr -dc A-Za-z0-9 | head -c 16)
    erl -hidden +pc unicode -pa beam -pa config -setcookie ${COOKIE} -name ${random}@${IP} -config ${CONFIG} -remsh ${NODE}
elif [[ "$2" == "load" ]];then
    # load module on one node 
    shift 2
    random=$(strings /dev/urandom | tr -dc A-Za-z0-9 | head -c 16)
    # full text
    # 1> >(sed $'s,.*,\e[32m&\e[m,'>&1) 2> >(sed $'s,.*,\e[31m&\e[m,'>&2) 
    erl -noinput -hidden +pc unicode -pa beam -pa config -setcookie ${COOKIE} -name ${random}@${IP} -env BEAM_LOADER_NODE ${NAME} -s beam load $* -s init stop 1> >(sed $'s/true/\e[32m&\e[m/;s/false\\|nofile/\e[31m&\e[m/'>&1) 2> >(sed $'s/.*/\e[31m&\e[m/'>&2) 
elif [[ "$2" == "stop" ]];then
    random=$(strings /dev/urandom | tr -dc A-Za-z0-9 | head -c 16)
    erl -noinput -hidden +pc unicode -pa beam -pa config -setcookie ${COOKIE} -name ${random}@${IP} -s main stop_safe ${NODE} -s init stop
fi

# return to shell work directory
cd - > /dev/null
