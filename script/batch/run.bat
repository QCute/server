@echo off
chcp 65001>nul

SetLocal
:: script path
set script=%~dp0
:: enter project root directory
cd "%script%\..\..\"

:: get first physical adapter
:: start powershell must set active code page to 437, the output text of utf8 code page will carry BOM
for /f %%i in ('PowerShell -NoLogo -NonInteractive -NoProfile -WindowStyle Hidden -Command "chcp 437 > $null; Get-WmiObject -Class Win32_NetworkAdapter | Select-Object -Property GUID,PNPDeviceID | Where-Object { $_ -match 'PCI' } | ForEach-Object { $GUID=$_.GUID; Get-WmiObject -Class Win32_NetworkAdapterConfiguration | Select-Object -Property SettingID,IPAddress | Where-Object { $_ -match $GUID } | ForEach-Object { $_.IPAddress -match '\d+\.\d+\.\d+\.\d+' } }"') do (
    :: not contain False matches
    if "%%i" neq "False" ( if "%%i" neq "" if not defined IP set IP=%%i )
)
:: if first physical adapter ip not exists, get first ip
if not defined IP (
    :: start powershell must set active code page to 437, the output text of utf8 code page will carry BOM
    for /f %%i in ('PowerShell -NoLogo -NonInteractive -NoProfile -WindowStyle Hidden -Command "chcp 437 > $null; Get-WmiObject -Class Win32_NetworkAdapterConfiguration | Select-Object -Property SettingID,IPAddress | ForEach-Object { $_.IPAddress -match '\d+\.\d+\.\d+\.\d+' }"') do (
        :: not contain False matches
        if "%%i" neq "False" ( if "%%i" neq "" if not defined IP set IP=%%i )
    )
)
:: IP(trim space)
set IP=%IP: =%
:: check
if not defined IP (
    echo "could not found any ip address"
    exit /b
)
:: filter physical adapter
:: for /f %%i in ('wmic nic get GUID^,PNPDeviceID ^| findstr /R PCI') do (
::     :: get address string from adapter config
::     for /f "delims=\\" %%j in ('wmic nicconfig get IPAddress^,SettingID ^| findstr %%i') do (
::         :: match and set ipv4 address (ipv6 match spec \"\w+::\w+:\w+:\w+:\w+\")
::         for /f %%k in ('PowerShell "chcp 437 > $null; \"%%j\" -match \"\d+\.\d+\.\d+\.\d+\" > $null; if ($Matches -ne $null){echo $Matches[0]}else{ echo \"\"}"') do (
::             if "%%k" neq "" if not defined IP set IP=%%k
::         )
::     )
:: )
:: extract date string
:: for /f %%x in ('PowerShell "\"%date%\" -match \"\d+/\d+/\d+\" > $null; if ($Matches -ne $null){echo $Matches[0]}"') do ( if not defined dates set dates=%%x )
for %%x in (%date%) do ( echo %%x | findstr /C:"/" 2>&1 1>nul && set NOW_DATE=%%x )
:: date(replace / to _)
set NOW_DATE="%NOW_DATE:/=_%"
:: date(trim space)
set NOW_DATE="%NOW_DATE: =%"
:: extract time string
for %%x in (%time%) do ( echo %%x | findstr /C:":" 2>&1 1>nul && set NOW_TIME=%%x )
:: cut time (hour:minute:second:micron) to (hour:minute:second)
set NOW_TIME="%NOW_TIME:~0,8%"
:: time(replace : to _)
set NOW_TIME="%NOW_TIME::=_%"
:: time(trim space)
set NOW_TIME="%NOW_TIME: =%"
:: date time format
set DATE_TIME="%NOW_DATE%__%NOW_TIME%"

:: erl param
set ATOM=1048576
set PROCESSES=1048576
set PORT=65535
:: windows nt not support kernel poll
:: Set the distribution buffer busy limit (dist_buf_busy_limit) in kilobytes. Valid range is 1-2097151. Default is 1024.
set ZDBBL=1024
set HPDS=2
set ETS=65536
:: chose config
if "%1" == "" (
    goto helper
) else (
    goto config_file
)

:helper
echo usage: %~nx0
echo     name                                          run config/name.config by interactive mode
goto end

:config_file
set NAME=%1
set NAME=%NAME:config\=%
set NAME=%NAME:.config=%
set NODE=%NAME%@%IP%
set CONFIG_FILE=config\\%NAME%.config
set CONFIG=config/%NAME%
set DUMP=logs/%NAME%_erl_crash.dump
goto ok

:: chose config finished
:ok
:: first cookie define
for /f "tokens=2 delims=,}" %%x in ('findstr /r "\<cookie\s*,.*}\>" %CONFIG_FILE% 2^>nul') do ( if not defined COOKIE set COOKIE=%%x )
:: set default cookie when config cookie not define
if not defined COOKIE set COOKIE=erlang
:: cookie(trim space)
set COOKIE=%COOKIE: =%

:: log
set KERNEL_LOG=logs/%NAME%_%DATE_TIME%.log
set SASL_LOG=logs/%NAME%_%DATE_TIME%.sasl

:: prepare start
if not exist %CONFIG_FILE% ( echo config file: %1 not found && exit /b 1 )
:: start in interactive mode
:: windows not support detached/remote-shell mode
:: erlang port map daemon(epmd -names) not work ??? i don't know !!!
:: erl +sub true +pc unicode -hidden -pa beam -pa config -pa config/app +hpds %HPDS% +P %PROCESSES% +t %ATOM% +zdbbl %ZDBBL% -setcookie %COOKIE% -name %NODE% -config %CONFIG% -env ERL_CRASH_DUMP %DUMP% -boot start_sasl -kernel error_logger {file,\"%KERNEL_LOG%\"} -sasl sasl_error_logger {file,\"%SASL_LOG%\"} -s main start
:: interactive mode, print sasl log to tty
:: set io options unicode encoding
erl +sub true +pc unicode -pa beam -pa config -pa config/app +hpds %HPDS% +e %ETS% +P %PROCESSES% +Q %PORT% +t %ATOM% +zdbbl %ZDBBL% -setcookie %COOKIE% -name %NODE% -config %CONFIG% -env ERL_CRASH_DUMP %DUMP% -boot start_sasl -eval "io:setopts([{encoding, unicode}]), io:setopts(standard_error,[{encoding, unicode}])." -s main start

:: end target
:end

EndLocal
