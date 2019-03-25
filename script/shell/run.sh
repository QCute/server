#!/bin/bash

script=$(dirname $0)

cd ${script}/../../

# first ip address
IP=$(ip addr show `ifconfig | grep -P "^\w+(?=:)" -o | grep -P "[^(lo)]" | head -n 1` | head -n 3 | tail -n 1 | awk '{print $2}' | awk -F "/" '{print $1}')
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
    name=main
    NODE=main@${IP}
    CONFIG=config/main
    export ERL_CRASH_DUMP=main_erl_crash.dump
else
    name=$1
    NODE=$1@${IP}
    CONFIG=config/$1
    export ERL_CRASH_DUMP=$1_erl_crash.dump
fi
# first cookie define
COOKIE=`grep -oP "(?<=cookie,)\s*.*(?=\})" ${CONFIG}".config"`
# :: set default cookie when config cookie not define 
if [[ "${COOKIE}" == "" ]];then
    COOKIE=erlang
fi
# log
KERNEL_LOG=logs/${name}_${date_time}.log
SASL_LOG=logs/${name}_${date_time}.sasl

# start
if [[ ! -n $1 ]]; then
    # run all when node not given
    for one in $(find config/ -name *.config | grep -Po "\w+(?=\.config)");do
        # run as decathed mode by default
        $0 ${one} bg
    done
elif [[ "$2" == "" ]] ;then
    # interactive mode
    erl -hidden -pa beam -pa config -smp true +P ${PROCESSES} +t ${ATOM} +K ${POLL} +zdbbl ${ZDBBL} -setcookie ${COOKIE} -name ${NODE} -config ${CONFIG} -boot start_sasl -kernel error_logger \{file,\"${KERNEL_LOG}\"\} -sasl sasl_error_logger \{file,\"${SASL_LOG}\"\} -s main start
elif [[ "$2" == "bg" ]] ;then
    # detache mode
    erl -noinput -detached -hidden -pa beam -pa config -smp true +P ${PROCESSES} +t ${ATOM} +K ${POLL} +zdbbl ${ZDBBL} -setcookie ${COOKIE} -name ${NODE} -config ${CONFIG} -boot start_sasl -kernel error_logger \{file,\"${KERNEL_LOG}\"\} -sasl sasl_error_logger \{file,\"${SASL_LOG}\"\} -s main start
elif [[ "$2" == "remsh" ]] ;then
    # remsh node
    erl -hidden -pa beam -pa config -setcookie ${COOKIE} -name debug@${IP} -config ${CONFIG} -remsh ${NODE}
fi

# return to shell work directory
cd - > /dev/null
