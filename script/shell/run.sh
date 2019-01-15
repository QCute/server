#!/bin/bash

script=$(dirname $0)

cd ${script}/../../

main(){
    erl +K true +P 1000000 -smp true -pa beam -pa config -name erlang@127.0.0.1 -setcookie erlang -boot start_sasl -config config/main -s main start
    back
}

debug(){
    erl +K true +P 1000000 -smp true -pa beam -pa config -name erlang@127.0.0.1 -setcookie erlang -boot start_sasl -config config/main -s debug_application start
	back
}

log(){
    screen -L -S main -t "main_`date +%Y-%m-%d_%H-%M-%S`" erl +K true +P 1000000 -smp true -pa beam -pa config -name erlang@127.0.0.1 -setcookie erlang -boot start_sasl -config config/main -s main start
    back
}

bgt(){
    screen -dmL -S main -t "main_`date +%Y-%m-%d_%H-%M-%S`" erl +K true +P 1000000 -smp true -pa beam -pa config -name erlang@127.0.0.1 -setcookie erlang -boot start_sasl -config config/main -s main start
    back
}

back(){
	cd - > /dev/null
}

## execute function
if [[ $# = 0 ]];then
    main
else
    $1
fi

