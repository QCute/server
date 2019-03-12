#!/bin/bash

script=$(dirname $0)

cd ${script}/../../

# first ip address
IP=$(ip addr show `ifconfig | grep -P "^\w+(?=:)" -o | grep -P "[^(lo)]" | head -n 1` | head -n 3 | tail -n 1 | awk '{print $2}' | awk -F "/" '{print $1}')
# date time format
DATE_TIME=`date "+%Y-%m-%d_%H-%M-%S"`

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
else
    NAME=$1
    NODE=$1@${IP}
    CONFIG=config/$1
fi
# first cookie define
COOKIE=`grep -oP "(?<=cookie,)\s*.*(?=\})" ${CONFIG}".config"`
# :: set default cookie when config cookie not define 
if [[ "${COOKIE}" == "" ]];then
    COOKIE=erlang
fi
# log
KERNEL_LOG=logs/${NAME}_${DATE_TIME}.log
SASL_LOG=logs/${NAME}_${DATE_TIME}.sasl

# start
if [[ "$2" == "" ]] ;then
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
