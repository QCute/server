@echo off

SetLocal
set pwd=%cd%
set script=%~dp0

:: jump
if "%1" == "" goto make_debug
if "%1" == "debug" (if "%2" == "" goto make_debug)
if "%1" == "debug" goto make_debug_single
if "%1" == "release" (if "%2" == "" goto make_release)
if "%1" == "release" goto make_release_single
if "%1" == "clean" goto clean
if "%1" == "maker" goto maker
if "%1" == "beam" goto beam
if "%1" == "pt" goto protocol
if "%1" == "protocol" goto protocol
if "%1" == "excel" (if "%2" == "table" goto table)
if "%1" == "excel" (if "%2" == "xml" goto xml)
if "%1" == "record" goto script
if "%1" == "sql" goto script
if "%1" == "data" goto script
if "%1" == "lua" goto script
if "%1" == "json" goto script
if "%1" == "log" goto script
if "%1" == "word" goto script
if "%1" == "key" goto script
if "%1" == "config" goto script
if "%1" == "router" goto script
if "%1" == "lsc" goto script
if "%1" == "map" goto script
goto helper

:make_debug
:: make all (default)
for /f "delims=." %%x in ('erl -noinput -eval "erlang:display(erlang:system_info(otp_release)),erlang:halt()."') do (
    if not defined OTP_RELEASE set OTP_RELEASE=%%x
)
for /f "delims=\\" %%x in ('erl -noinput -eval "erlang:display(erlang:system_info(version)),erlang:halt()."') do (
    if not defined OTP_VERSION set OTP_VERSION=%%x
)
set OTP_RELEASE=%OTP_RELEASE:"='%
set OTP_VERSION=%OTP_VERSION:"='%
:: otp 17 or earlier, referring to built-in type queue as a remote type; please take out the module name
for /f "tokens=6" %%x in ('cmd /C "erl +V 2>&1"') do (
    for /f "delims=. tokens=1" %%y in ("%%x") do (
        :: # remote type option
        if %%x geq 6 set REMOTE_VERSION=,{d,otp}
    )
)
set OPTIONS=-env ERL_COMPILER_OPTIONS [{d,'RELEASE',%OTP_RELEASE%},{d,'VERSION',%OTP_VERSION%}%REMOTE_VERSION%]

cd %script%\..\debug\
erl %OPTIONS% -make
cd %pwd%
goto end

:make_debug_single
for /f %%x in ('where /r src %2.erl 2^>nul') do (
    erlc -I include -o beam +debug_info -D DEBUG %%x
)
goto end

:make_release
cd %script%\..\release\
erl -make
cd %pwd%
%0 beam compile
goto end

:: windows hipe(high performance erlang) native code not support
:make_release_single
for /f %%x in ('where /r src %2.erl 2^>nul') do (
    erlc -I include -o beam -Werror %%x
)
goto end

:clean
:: clean all beam
cd %script%\..\..\beam\
del *.beam
cd %pwd%
goto end

:maker
cd %script%\..\make\
erl -make
cd %pwd%
goto end

:beam
if "%2" == "" (
    :: remove old head(module/compile/include) data
    PowerShell "$in = (Get-Content %script%\..\\..\src\tool\extension\user_default.erl -encoding UTF8 | Select-String -Pattern '^-module.*\.|^-compile.*\.|^-include.*\.' -NotMatch); Set-Content '%script%\..\\..\src\tool\extension\user_default.erl' $in -encoding UTF8;"
    :: build write new data
    PowerShell "$lf=$('' | Out-String);$head='-module(user_default).'+$lf+'-compile(nowarn_export_all).'+$lf+'-compile(export_all).'+$lf; foreach ($name in (Get-ChildItem -Name 'include')) { $head+=('-include(\"../../../include/'+$name+'\").'+$lf) }; $tail=(Get-Content %script%\..\\..\src\tool\extension\user_default.erl -encoding UTF8); Set-Content '%script%\..\\..\src\tool\extension\user_default.erl' $head.SubString(0, $head.Length - 2), $tail -encoding UTF8;"
    :: remove utf8 bom and convert cr/lf(dos) to lf(unix)
    PowerShell "$in = ([System.IO.File]::ReadAllText('%script%\..\\..\src\tool\extension\user_default.erl', [System.Text.UTF8Encoding]($False)) -replace \"`r\"); [System.IO.File]::WriteAllText('%script%\..\\..\src\tool\extension\user_default.erl', $in, [System.Text.UTF8Encoding]($False));"
)
:: remove old beam file
del /q %script%\..\..\beam\user_default.beam
:: recompile it with debug info mode (beam abstract code contain)
erlc +debug_info -o %script%/../../beam/ %script%/../../src/tool/extension/user_default.erl
goto end

:protocol
escript %script%\..\make\protocol\protocol_script_%2.erl %3 %4 %5 %6 %7 %8 %9
goto end

:table
SetLocal EnableDelayedExpansion
:: windows console pass utf8 characters convert to utf8 byte list
for /f %%I in ('PowerShell "[Text.Encoding]::UTF8.GetBytes(\"%3\")"') do (set encode=!encode! %%I)
escript %script%\..\make\script\excel_script.erl %2 list %encode%
goto end

:xml
escript %script%\..\make\script\excel_script.erl %2 %3
goto end

:script
escript %script%\..\make\script\%1_script.erl %2 %3 %4 %5 %6 %7 %8 %9
goto end

:helper
echo usage: compile all file by default
echo     debug [module]                            make with debug mode
echo     release [module]                          make with release mode
echo     clean                                     remove all beam
echo     maker                                     compile maker
echo     beam                                      update beam abstract code
echo     pt/protocol number                        make protocol file
echo     excel [xml^|table] [filename^|table name]   convert xml/table to table/xml
echo     record name                               make record file
echo     sql name [select^|join] [all]              make sql file
echo     data name                                 make erl data configure file
echo     lua name                                  make lua data configure file
echo     json name                                 make json data configure file
echo     log name                                  make log file
echo     word                                      make sensitive word file
echo     key [-amount^|-type^|-prefix]               make active key 
echo     config                                    make erlang application config interface
echo     router                                    make protocol route
echo     lsc                                       make load/save/clean code
:: end target
:end
EndLocal