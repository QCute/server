@echo off
chcp 65001>nul

SetLocal
:: script path
set script=%~dp0
:: enter project root directory
cd "%script%\..\..\"

:: jump
if "%1" == "" goto helps
if "%1" == "debug" (if "%2" == "" goto make_debug)
if "%1" == "debug" goto make_debug_single
if "%1" == "release" (if "%2" == "" goto make_release)
if "%1" == "release" goto make_release_single
if "%1" == "maker" goto maker
if "%1" == "lib" goto lib
if "%1" == "beam" goto beam
if "%1" == "clean" goto clean
if "%1" == "plt" goto plt
if "%1" == "dialyzer" goto dialyzer
if "%1" == "pt" goto pt
if "%1" == "protocol" goto protocol
if "%1" == "book" goto book
if "%1" == "sheet" goto sheet
if "%1" == "collection" goto collection
if "%1" == "table" goto table
if "%1" == "record" goto script
if "%1" == "sql" goto script
if "%1" == "erl" goto script
if "%1" == "lua" goto script
if "%1" == "js" goto script
if "%1" == "log" goto script
if "%1" == "word" goto script
if "%1" == "key" goto script
if "%1" == "config" goto script
if "%1" == "router" goto script
if "%1" == "map" goto script
if "%1" == "attribute" goto script
if "%1" == "asset" goto script
if "%1" == "event" goto script
if NOT "%1" == "helps" echo unknown option: %1
goto helps

:make_debug
:: make all (default)
:: for /f "delims=." %%x in ('erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~w\", [list_to_atom(erlang:system_info(otp_release))]),erlang:halt()."') do (
::     if not defined OTP_RELEASE set OTP_RELEASE=%%x
:: )
:: for /f "delims=\\" %%x in ('erl +B -boot no_dot_erlang -noshell -eval "io:format(\"~w\", [list_to_atom(erlang:system_info(version))]),erlang:halt()."') do (
::     if not defined OTP_VERSION set OTP_VERSION=%%x
:: )
:: otp 17 or earlier, referring to built-in type queue as a remote type; please take out the module name
:: for /f "tokens=6" %%x in ('cmd /C "erl +V 2>&1"') do (
::     for /f "delims=. tokens=1" %%y in ("%%x") do (
           :: # remote type option
::         if %%x geq 6 set REMOTE_VERSION=,{d,otp}
::     )
:: )
:: set OPTIONS=-env ERL_COMPILER_OPTIONS [{d,'RELEASE',%OTP_RELEASE%},{d,'VERSION',%OTP_VERSION%}%REMOTE_VERSION%]
:: erl %OPTIONS% -make

:: usr abs path %~f0 beam compile
:: compile lib
call "script/batch/%~nx0" lib
:: compile maker
call "script/batch/%~nx0" maker
:: execute reload beam 
call "script/batch/%~nx0" beam
:: execute reload event
call "script/batch/%~nx0" event
:: erl -pa beam/ -make
set make="make:all([{emake, [{[\"src/*/*\", \"src/*/*/*\", \"script/make/*/data/*\", \"script/make/protocol/erl/*\", \"script/make/protocol/erl/*/*\"], [{i, \"include/\"}, {outdir, \"beam/\"}, debug_info, {d, 'DEBUG', true} | [{i, D} || D <- filelib:wildcard(\"lib/*/include/\")]]}]}]), erlang:halt()."
erl -pa beam/ +B -boot no_dot_erlang -noshell -eval %make%

goto end

:make_debug_single
:: erlc -I include -o beam +debug_info -D DEBUG %%x
for /f %%x in ('where /r . %2.erl 2^>nul') do (
    if not defined FILE set FILE=%%x
)
:: show message
SetLocal EnableDelayedExpansion
if "%FILE%"=="" (
    echo %2.erl: no such file or directory
    exit /b 1
) else (
    :: time(replace \ to /)
    set FILE="%FILE:\=/%"
    set make="make:all([{emake, [{[\"!FILE!\"], [{i, \"include/\"}, {outdir, \"beam/\"}, debug_info, {d, 'DEBUG', true} | [{i, D} || D <- filelib:wildcard(\"lib/*/include/\")]]}]}]), erlang:halt()."
    erl -pa beam/ +B -boot no_dot_erlang -noshell -eval !make!
    echo ok
)
EndLocal
goto end

:make_release
:: usr abs path %~f0 beam compile
:: compile lib
call "script/batch/%~nx0" lib
:: compile maker
call "script/batch/%~nx0" maker
:: execute reload beam 
call "script/batch/%~nx0" beam
:: execute reload event
call "script/batch/%~nx0" event
:: erl -pa beam/ -make
set make="make:all([{emake, [{[\"src/*/*\", \"src/*/*/*\", \"script/make/*/data/*\", \"script/make/protocol/erl/*\", \"script/make/protocol/erl/*/*\"], [{i, \"include/\"}, {outdir, \"beam/\"}, debug_info, warnings_as_errors | [{i, D} || D <- filelib:wildcard(\"lib/*/include/\")]]}]}]), erlang:halt()."
erl -pa beam/ +B -boot no_dot_erlang -noshell -eval %make%
goto end

:make_release_single
:: windows hipe(high performance erlang) native code not support
:: erlc -I include -o beam -Werror %%x
for /f %%x in ('where /r . %2.erl 2^>nul') do (
    if not defined FILE set FILE=%%x
)
:: show message
SetLocal EnableDelayedExpansion
if "%FILE%"=="" (
    echo %2.erl: no such file or directory
    exit /b 1
) else (
    set FILE="%FILE:\=/%"
    set make="make:all([{emake, [{[\"!FILE!\"], [{i, \"include/\"}, {outdir, \"beam/\"}, debug_info, warnings_as_errors | [{i, D} || D <- filelib:wildcard(\"lib/*/include/\")]]}]}]), erlang:halt()."
    erl -pa beam/ +B -boot no_dot_erlang -noshell -eval !make!
    echo ok
)
EndLocal
goto end

:maker
:: compile lib
call "script/batch/%~nx0" lib
:: compile maker
set make="make:all([{emake, [{[\"script/make/*/*maker*\", \"src/tool/*/*\"], [{i, \"include/\"}, {outdir, \"beam/\"}, debug_info, warnings_as_errors | [{i, D} || D <- filelib:wildcard(\"lib/*/include/\")]]}]}]), erlang:halt()."
erl -pa beam/ +B -boot no_dot_erlang -noshell -eval %make%
goto end

:lib
SetLocal EnableDelayedExpansion
for /d %%x in (lib/*) do (
    set make="make:all([{emake, [{[lists:concat([filename:dirname(X), \"/*\"]) || X<-filelib:wildcard(\"lib/%%x/src/**/*.erl\")], [{i, \"%%x/include/\"}, {outdir, \"beam/\"}, debug_info, warnings_as_errors]}]}]), erlang:halt()."
    erl -pa beam/ +B -boot no_dot_erlang -noshell -eval !make!
)
EndLocal
goto end

:beam
if "%2"=="" (
    :: remove old head(module/compile/include) data
    PowerShell "$in = (Get-Content src\tool\extension\user_default.erl -encoding UTF8 | Select-String -Pattern '^-module.*\.|^-compile.*\.|^-include.*\.' -NotMatch); Set-Content 'src\tool\extension\user_default.erl' $in -encoding UTF8;"
    :: build write new data
    PowerShell "$lf=$('' | Out-String);$head='-module(user_default).'+$lf+'-compile(nowarn_export_all).'+$lf+'-compile(export_all).'+$lf; foreach ($name in (Get-ChildItem -Name 'include')) { $head+=('-include(\"../../../include/'+$name+'\").'+$lf) }; $tail=(Get-Content src\tool\extension\user_default.erl -encoding UTF8); Set-Content 'src\tool\extension\user_default.erl' $head.SubString(0, $head.Length - 2), $tail -encoding UTF8;"
    :: remove utf8 bom and convert cr/lf(dos) to lf(unix)
    PowerShell "$in = ([System.IO.File]::ReadAllText('src\tool\extension\user_default.erl', [System.Text.UTF8Encoding]($False)) -replace \"`r\"); [System.IO.File]::WriteAllText('src\tool\extension\user_default.erl', $in, [System.Text.UTF8Encoding]($False));"
)
:: remove old beam file
del /Q "beam\user_default.beam"
:: recompile it with debug info mode (beam abstract code contain)
erlc +debug_info -o "beam/" "src/tool/extension/user_default.erl"
goto end

:clean
:: clean all beam
del /Q "beam\*.beam"
goto end

:plt
SetLocal EnableDelayedExpansion
for /f "delims=^" %%i in ('where erl') do (
    set "erl=%%~dpi..\lib\"
    for /f "delims=^" %%j in ('dir /b "!erl!"') do (
        if exist !erl!%%j\ebin (
            set plt=!plt! "!erl!%%j\ebin"
        )
    )
)
dialyzer --build_plt -r %plt%
EndLocal
goto end

:dialyzer
SetLocal EnableDelayedExpansion
for /f "delims=^" %%i in ('dir /b lib') do (
    if exist lib\%%i\include\ (
        set include=!include! -I lib\%%i\include\
    )
)
for /f "delims=^" %%i in ('dir /b lib') do (
    if exist lib\%%i\src\ (
        set lib=!lib! lib\%%i\src\
    )
)
dialyzer --no_check_plt %2 %3 %4 %5 %6 %7 %8 %9 -I "include" %include% --src -r %lib% "lib\*\src\" "src" "script\make\"
EndLocal
goto end

:pt
escript "script\make\protocol\protocol_script_%2.erl" %3 %4 %5 %6 %7 %8 %9
escript "script\make\router\router_script.erl"
goto end

:protocol
for /f %%x in ('dir /b "script\\make\protocol\*script*.erl" 2^>nul') do (
    escript "script\make\protocol\%%x" %2 %3 %4 %5 %6 %7 %8 %9
)
escript "script\make\router\router_script.erl"
goto end

:book
:sheet
:collection
:table
:: SetLocal EnableDelayedExpansion
:: earlier then otp 17.0 unicode characters problem
:: windows console pass utf8 characters convert to utf8 byte list
:: for /f %%I in ('PowerShell "[Text.Encoding]::UTF8.GetBytes(\"%file%\")"') do (set encode=!encode! %%I)
:: echo escript script\make\excel\excel_script.erl %1 %2 %3 %4 %5 %6 %7 %8 %9 -encode %encode%
escript "script\make\excel\excel_script.erl" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto end

:script
escript "script\make\%1\%1_script.erl" %2 %3 %4 %5 %6 %7 %8 %9
goto end

:helps
echo usage: %~nx0
echo     debug [module]                                make (module) with debug mode
echo     release [module]                              make (module) with release mode
echo     maker                                         compile maker
echo     lib                                           compile lib
echo     beam                                          update beam abstract code
echo     clean                                         remove all beam
echo     plt                                           make .dialyzer_plt file
echo     dialyzer                                      run dialyzer
echo     pt name                                       make protocol file
echo     protocol                                      make all protocol file
echo     book file-name                                convert tables to excel book
echo     sheet table-name                              convert table to excel sheet, same as excel table-name
echo     collection file-name                          restore book file to tables
echo     table file-name                               restore sheet file to table, same as excel table file-name
echo     record name                                   make record file
echo     sql name                                      make sql file
echo     erl name                                      make erl data configure file
echo     lua name                                      make lua data configure file
echo     js name                                       make js data configure file
echo     log name                                      make log file
echo     word                                          make sensitive word file
echo     key [-number^|-type^|-prefix]                   make active key
echo     config                                        make erlang application config interface
echo     router                                        make protocol route
echo     attribute                                     make attribute code
echo     asset                                         make asset code
echo     event                                         make event code
echo     helps                                         lookup help manual
exit /b 1
:: end target
:end
EndLocal
