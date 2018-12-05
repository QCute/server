#!/bin/bash

script=$(dirname $0)

cd ${script}/../../config/

main(){
    erl +K true +P 1000000 -smp true -pa ../beam -name erlang@127.0.0.1 -setcookie erlang -boot start_sasl -config main -s main start
}

debug(){
    erl +K true +P 1000000 -smp true -pa ../beam -name erlang@127.0.0.1 -setcookie erlang -boot start_sasl -config main -s debug_application start
}

log(){
    screen -L -S main -t "main_`date +%Y-%m-%d_%H-%M-%S`" erl +K true +P 1000000 -smp true -pa ../beam -name erlang@127.0.0.1 -setcookie erlang -boot start_sasl -config main -s main start
}

bgt(){
    screen -dmL -S main -t "main_`date +%Y-%m-%d_%H-%M-%S`" erl +K true +P 1000000 -smp true -pa ../beam -name erlang@127.0.0.1 -setcookie erlang -boot start_sasl -config main -s main start
}


## execute function
if [[ $# = 0 ]];then
    main
else
    $1
fi

