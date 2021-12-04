@echo off
chcp 65001>nul

SetLocal
:: script path
set script=%~dp0

:: jump
if "%1" == "" goto helps
if "%1" == "debug" (if "%2" == "" goto make_debug)
if "%1" == "debug" goto make_debug_single
if "%1" == "release" (if "%2" == "" goto make_release)
if "%1" == "release" goto make_release_single
if "%1" == "clean" goto clean
if "%1" == "plt" goto plt
if "%1" == "dialyzer" goto dialyzer
if "%1" == "maker" goto maker
if "%1" == "beam" goto beam
if "%1" == "pt" goto pt
if "%1" == "protocol" goto protocol
if "%1" == "excel" (if "%2" == "table" goto table)
if "%1" == "table" goto table
if "%1" == "excel" (if "%2" == "xml" goto xml)
if "%1" == "xml" goto xml
if "%1" == "record" goto script
if "%1" == "sql" goto script
if "%1" == "data" goto script
if "%1" == "lua" goto script
if "%1" == "js" goto script
if "%1" == "log" goto script
if "%1" == "word" goto script
if "%1" == "key" goto script
if "%1" == "config" goto script
if "%1" == "router" goto script
if "%1" == "loop" goto script
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

cd "%script%\..\..\"
:: erl %OPTIONS% -make
:: erl -pa ../../beam/ -make
set emake={[\"src/*\", \"src/*/*\", \"src/*/*/*\", \"src/*/*/*/*\", \"src/lib/*/src/*\"], [{i, \"include/\"}, {outdir, \"beam/\"}, debug_info, {d, 'DEBUG', true}]}
erl -pa beam/ +B -boot no_dot_erlang -noshell -eval "make:all([{emake, [%emake%]}]), erlang:halt()."
:: usr abs path %~f0 beam compile
"../batch/%~nx0" beam compile
goto end

:make_debug_single
:: project root directory
cd "%script%\..\..\"
:: erlc -I include -o beam +debug_info -D DEBUG %%x
for /f %%x in ('where /r src %2.erl 2^>nul') do (
    if not defined FILE set FILE=%%x
)
:: show message
if "%FILE%"=="" (
    echo %2.erl: no such file or directory
    exit /b 1
) else (
    erlc -I include -o beam +debug_info -D DEBUG %FILE%
    echo ok
)

goto end

:make_release
:: make script directory
cd "%script%\..\..\"
:: erl -pa ../../beam/ -make
set emake={[\"src/*\", \"src/*/*\", \"src/*/*/*\", \"src/*/*/*/*\", \"src/lib/*/src/*\"], [{i, \"include/\"}, {outdir, \"beam/\"}, warnings_as_errors]}
erl -pa beam/ +B -boot no_dot_erlang -noshell -eval "make:all([{emake, [%emake%]}]), erlang:halt()."
:: execute reload beam
:: usr abs path %~f0 beam compile
"../batch/%~nx0" beam
goto end

:make_release_single
:: project root directory
cd "%script%\..\..\"
:: windows hipe(high performance erlang) native code not support
:: erlc -I include -o beam -Werror %%x
for /f %%x in ('where /r src %2.erl 2^>nul') do (
    if not defined FILE set FILE=%%x
)

:: show message
if "%FILE%"=="" (
    echo %2.erl: no such file or directory
    exit /b 1
) else (
    erlc -I include -o beam -Werror %FILE%
    echo ok
)

goto end

:clean
:: clean all beam
del /Q "%script%\..\..\beam\*.beam"
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
goto end

:dialyzer
dialyzer --no_check_plt %2 %3 %4 %5 %6 %7 %8 %9 -I "%script%\..\..\include" --src -r "%script%\..\..\src" "%script%\..\make\script" "%script%\..\make\protocol" "%script%\..\make\maker"

:maker
cd "%script%\..\..\"
:: erl -make
set emake={[\"script/make/maker/*\", \"src/tool/*/*\", \"src/lib/*/src/*\"], [{i, \"include/\"}, {outdir, \"beam/\"}, debug_info, warnings_as_errors]}
erl -pa beam/ +B -boot no_dot_erlang -noshell -eval "make:all([{emake, [%emake%]}]), erlang:halt()."
goto end

:beam
if "%2"=="" (
    :: remove old head(module/compile/include) data
    PowerShell "$in = (Get-Content %script%\..\\..\src\tool\extension\user_default.erl -encoding UTF8 | Select-String -Pattern '^-module.*\.|^-compile.*\.|^-include.*\.' -NotMatch); Set-Content '%script%\..\\..\src\tool\extension\user_default.erl' $in -encoding UTF8;"
    :: build write new data
    PowerShell "$lf=$('' | Out-String);$head='-module(user_default).'+$lf+'-compile(nowarn_export_all).'+$lf+'-compile(export_all).'+$lf; foreach ($name in (Get-ChildItem -Name 'include')) { $head+=('-include(\"../../../include/'+$name+'\").'+$lf) }; $tail=(Get-Content %script%\..\\..\src\tool\extension\user_default.erl -encoding UTF8); Set-Content '%script%\..\\..\src\tool\extension\user_default.erl' $head.SubString(0, $head.Length - 2), $tail -encoding UTF8;"
    :: remove utf8 bom and convert cr/lf(dos) to lf(unix)
    PowerShell "$in = ([System.IO.File]::ReadAllText('%script%\..\\..\src\tool\extension\user_default.erl', [System.Text.UTF8Encoding]($False)) -replace \"`r\"); [System.IO.File]::WriteAllText('%script%\..\\..\src\tool\extension\user_default.erl', $in, [System.Text.UTF8Encoding]($False));"
)
:: remove old beam file
del /Q "%script%\..\..\beam\user_default.beam"
:: recompile it with debug info mode (beam abstract code contain)
erlc +debug_info -o "%script%/../../beam/" "%script%/../../src/tool/extension/user_default.erl"
goto end

:pt
escript "%script%\..\make\protocol\protocol_script_%2.erl" %3 %4 %5 %6 %7 %8 %9
escript "%script%\..\make\script\router_script.erl"
goto end

:protocol
for /f %%x in ('dir /b "%script%\..\..\script\make\protocol\*.erl" 2^>nul') do (
    escript "%script%\..\..\script\make\protocol\%%x" %2 %3 %4 %5 %6 %7 %8 %9
)
goto end

:xml
:table
:: if "%1"=="table" (set file=%2)
:: if "%1"=="excel" (set file=%3)
:: SetLocal EnableDelayedExpansion
:: earlier then otp 17.0 unicode characters problem
:: windows console pass utf8 characters convert to utf8 byte list
:: for /f %%I in ('PowerShell "[Text.Encoding]::UTF8.GetBytes(\"%file%\")"') do (set encode=!encode! %%I)
:: echo escript %script%\..\make\script\excel_script.erl %1 %2 %3 %4 %5 %6 %7 %8 %9 -encode %encode%
escript "%script%\..\make\script\excel_script.erl" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto end

:script
escript "%script%\..\make\script\%1_script.erl" %2 %3 %4 %5 %6 %7 %8 %9
goto end

:helps
echo usage: %~nx0
echo     debug [module]                                make (module) with debug mode
echo     release [module]                              make (module) with release mode
echo     clean                                         remove all beam
echo     maker                                         compile maker
echo     beam                                          update beam abstract code
echo     pt name                                       make protocol file
echo     protocol                                      make all protocol file
echo     excel [table^|xml] [table-name^|file-name]      convert/restore table/xml to xml/table
echo     xml table-name                                convert table to xml, same as excel xml table-name
echo     table file-name                               restore xml to table, same as excel table file-name
echo     record name                                   make record file
echo     sql name                                      make sql file
echo     data name                                     make erl data configure file
echo     lua name                                      make lua data configure file
echo     js name                                       make js data configure file
echo     log name                                      make log file
echo     word                                          make sensitive word file
echo     key [-number^|-type^|-prefix]                   make active key
echo     config                                        make erlang application config interface
echo     router                                        make protocol route
echo     loop                                          make load/save/reset/clean/expire code
echo     attribute                                     make attribute code
echo     asset                                         make asset code
echo     event                                         make event code
echo     helps                                         lookup help manual
exit /b 1
:: end target
:end
EndLocal
