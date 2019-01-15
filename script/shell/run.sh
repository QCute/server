#!/bin/bash

script=$(dirname $0)

cd ${script}/../../

# first ip address
IP=$(ip addr show `ifconfig | grep -P "^\w+(?=:)" -o | grep -P "[^(lo)]"` | head -n 3 | tail -n 1 | awk '{print $2}' | awk -F "/" '{print $1}')
# date time format
DATE_TIME=`date "+%Y-%m-%d_%H-%M-%S"`

# erl param
SMP=disable
ATOM=10485760
PROCESSES=1024000
POLL=true
COOKIE=erlang

if [[ "$1" == "" ]] ;then
    NODE=main@${IP}
else
    NODE=%1@${IP}
fi
if [[ "$1" == "" ]] ;then 
    CONFIG=config/main
else
    CONFIG=config/$1
fi

# log
KERNEL_LOG=logs/${NODE}_${DATE_TIME}.log
SASL_LOG=logs/${NODE}_${DATE_TIME}.sasl


# start 
erl -pa beam -pa config -smp true +P ${PROCESSES} +t ${ATOM} +K ${POLL} -setcookie ${COOKIE} -boot start_sasl -name ${NODE} -config ${CONFIG} -kernel error_logger \{file,\"${KERNEL_LOG}\"\} -sasl sasl_error_logger \{file,\"${SASL_LOG}\"\} -s main start

# return to shell work directory
cd - > /dev/null
