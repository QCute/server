@echo off
chcp 65001>nul

set pwd=%cd%
set script=%~dp0
:: enter work directory
cd %script%\..\..\

:: first ip address
:: tokens part 2 with delimiter :
for /f "tokens=2 delims=:" %%x in ('ipconfig^|findstr /IC:"IPv4 Address"') do (if not defined ip set ip=%%x)
:: ip(trim space)
set ip=%ip: =%
:: date(replace / to -)
set date=%date:~0,10%
set date=%date:/=-%
:: time(replace / to - and space to 0)
set time=%time:~0,8%
set time=%time::=-%
set time=%time: =0%
:: date time format
set date_time=%date%_%time%

:: erl param
set SMP=true
set ATOM=10485760
set PROCESSES=1024000
:: windows nt not support kernel poll
set POLL=false
:: Set the distribution buffer busy limit (dist_buf_busy_limit) in kilobytes. Valid range is 1-2097151. Default is 1024.
set ZDBBL=1024
:: chose config
if "%1" == "" (
    set name=main
    set NODE=main@%ip%
    set config_file=config\\main.config
    set CONFIG=config/main
) else (
    set name=%1
    set NODE=%1@%ip%
    set config_file=config\\%1.config
    set CONFIG=config/%1
)
:: first cookie define
for /f "tokens=2 delims=,}" %%x in ('findstr /r "\<cookie\s*,.*}\>" %config_file%') do (if not defined COOKIE set COOKIE=%%x)
:: set default cookie when config cookie not define
if "%cookie%" == "" (
    set COOKIE=erlang
)
:: log
set KERNEL_LOG=logs/%name%_%date_time%.log
set SASL_LOG=logs/%name%_%date_time%.sasl

:: start
:: windows not support detache/remsh
erl -hidden -pa beam -pa config -smp true +P %PROCESSES% +t %ATOM% +zdbbl %ZDBBL% -setcookie %COOKIE% -name %NODE% -config %CONFIG% -boot start_sasl -kernel error_logger {file,\"%KERNEL_LOG%\"} -sasl sasl_error_logger {file,\"%SASL_LOG%\"} -s main start

:: return to batch directory
cd %pwd%
