@echo off

set pwd=%cd%
set script=%~dp0

:: jump
if "%1"=="" goto make
if "%1"=="clean" goto clean
if "%1"=="maker" goto maker
if "%1"=="beam" goto beam
if "%1"=="protocol" goto protocol
if "%1"=="pt" goto protocol
goto other

:make (default)
:: make all

cd %script%\..\
erl -make
erlc +debug_info -o ../beam ../src/debug/user_default.erl
cd %pwd%
goto end

:clean
:: clean all beam
cd %script%\..\..\beam\
del *.beam
cd %pwd%
goto end

:maker
cd %script%\..\..\src\make\maker\
erl -make
cd %pwd%
goto end

:beam
escript %script%\..\..\src\debug\user_default.erl update_include
erlc +debug_info -o %script%/../../beam/ %script%/../../src/debug/user_default.erl 
goto end

:protocol
escript %script%\..\..\src\make\protocol\protocol_script_%2.erl %3 %4 %5 %6 %7 %8 %9
goto end

:other
escript %script%\..\..\src\make\script\%1_script.erl %2 %3 %4 %5 %6 %7 %8 %9
goto end


:: end target
:end