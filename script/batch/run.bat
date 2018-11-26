@echo off
chcp 65001>nul

set pwd=%cd%
set script=%~dp0
:: enter work directory
cd %script%\..\..\config\

if "%1"=="" goto main
goto %1

:main
erl +P 1024000 -smp enable -pa ../beam -name erlang@192.168.1.76 -setcookie erlang -boot start_sasl -config main -s main start
goto end


:debug
erl +P 1024 -smp enable -pa ../beam -name erlang@192.168.1.1 -setcookie erlang -boot start_sasl -config main -s debug_application start
goto end


:: end target
:end


:: return to batch directory
cd %pwd%\..\script\batch\
