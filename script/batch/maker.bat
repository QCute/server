@echo off

:: jump
if "%1"=="" goto make
if "%1"=="clean" goto clean
if "%1"=="include" goto include
goto other

:make (default)
:: make all
erl -make
goto end

:clean
:: clean all beam
cd ../../beam/
del *.beam
cd ../script/batch
goto end

:include
escript ../../src/debug/beam.erl
goto end

:other
escript ../../src/make/script/%1_script.erl %2
goto end


:: end target
:end