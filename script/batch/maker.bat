@echo off

set pwd=%cd%
set script=%~dp0

:: jump
if "%1"=="" goto make
if "%1"=="clean" goto clean
if "%1"=="include" goto include
goto other

:make (default)
:: make all
cd %script%
erl -make
cd %pwd%
goto end

:clean
:: clean all beam
cd %script%\..\..\beam\
del *.beam
cd %pwd%
goto end

:include
escript %script%\..\..\src\debug\beam.erl
goto end

:other
escript %script%\..\..\src\make\script\%1_script.erl %2
goto end


:: end target
:end