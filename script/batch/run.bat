@echo off
chcp 65001>nul

SetLocal
set pwd=%cd%
set script=%~dp0
:: enter work directory
cd %script%\..\..\

:: filter physical adapter
for /f %%i in ('wmic nic get GUID^,PNPDeviceID ^| findstr /R PCI') do (
    :: get address string from adapter config
    for /f "delims=\\" %%j in ('wmic nicconfig get IPAddress^,SettingID ^| findstr %%i') do (
        :: match and set ipv4 address (ipv6 match spec \"\w+::\w+:\w+:\w+:\w+\")
        for /f %%k in ('PowerShell "chcp 437 > $null; \"%%j\" -match \"\d+\.\d+\.\d+\.\d+\" > $null; if ($Matches -ne $null){echo $Matches[0]}else{ echo \"\"}"') do (
            if "%%k" neq "" if not defined IP set IP=%%k
        )
    )
)
:: extract date string
:: for /f %%x in ('Powershell "\"%date%\" -match \"\d+/\d+/\d+\" > $null; if ($Matches -ne $null){echo $Matches[0]}"') do ( if not defined dates set dates=%%x )
for %%x in (%date%) do ( echo %%x | findstr /C:"/" 2>&1 1>nul && set NOW_DATE=%%x )
:: date(replace / to _)
set NOW_DATE=%NOW_DATE:/=_%
:: date(trim space)
set NOW_DATE=%NOW_DATE: =%
:: extract time string
for %%x in (%time%) do ( echo %%x | findstr /C:":" 2>&1 1>nul && set NOW_TIME=%%x )
:: cut time (hour:minute:second:micron) to (hour:minute:second)
set NOW_TIME=%NOW_TIME:~0,8%
:: time(replace : to _)
set NOW_TIME=%NOW_TIME::=_%
:: time(trim space)
set NOW_TIME=%NOW_TIME: =%
:: date time format
set DATE_TIME=%NOW_DATE%__%NOW_TIME%

:: erl param
set SMP=true
set ATOM=10485760
set PROCESSES=1024000
:: windows nt not support kernel poll
:: set POLL=false
:: Set the distribution buffer busy limit (dist_buf_busy_limit) in kilobytes. Valid range is 1-2097151. Default is 1024.
set ZDBBL=1024
:: chose config
if "%1" == "" (
    set NAME=main
    set NODE=main@%IP%
    set CONFIG_FILE=config\\main.config
    set CONFIG=config/main
    set DUMP=-env ERL_CRASH_DUMP main_erl_crash.dump
) else (
    set NAME=%1
    set NODE=%1@%IP%
    set CONFIG_FILE=config\\%1.config
    set CONFIG=config/%1
    set DUMP=-env ERL_CRASH_DUMP %1_erl_crash.dump
)
:: first cookie define
for /f "tokens=2 delims=,}" %%x in ('findstr /r "\<cookie\s*,.*}\>" %CONFIG_FILE% 2^>nul') do (if not defined COOKIE set COOKIE=%%x)
:: set default cookie when config cookie not define
if not defined COOKIE set COOKIE=erlang

:: log
set KERNEL_LOG=logs/%NAME%_%DATE_TIME%.log
set SASL_LOG=logs/%NAME%_%DATE_TIME%.sasl

:: prepare start
if not exist %CONFIG_FILE% ( echo config file not found && exit /b )
:: start in interactive mode
:: windows not support detached/remote-shell mode
:: erlang port map daemon(epmd -names) not work ??? i don't know !!!
erl -hidden +pc unicode -pa beam -pa config -smp true +P %PROCESSES% +t %ATOM% +zdbbl %ZDBBL% -setcookie %COOKIE% -name %NODE% -config %CONFIG% %DUMP% -boot start_sasl -kernel error_logger {file,\"%KERNEL_LOG%\"} -sasl sasl_error_logger {file,\"%SASL_LOG%\"} -s main start

:: return to working directory
cd %pwd%
EndLocal
