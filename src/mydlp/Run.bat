@echo off
set ERLANG_OPTS=+A 4 +P 1048576 +fnu -smp auto -env ERL_MAX_ETS_TABLES 16384

if "%ERLBIN%"=="" (
    set ERLBIN=erl.exe
    set ERLANG_OPTS=%ERLANG_OPTS% -noinput -noshell -detached
)

if not exist "%ERLANG_HOME%\bin\%ERLBIN%" (
    echo.
    echo ******************************
    echo ERLANG_HOME not set correctly.
    echo ******************************
    echo.
    exit /B
)

"%ERLANG_HOME%\bin\%ERLBIN%" ^
%ERLANG_OPTS% ^
-sname system ^
-workdir %MYDLPBEAMDIR% ^
-env MYDLP_CONF %MYDLP_CONF% ^
-env MYDLP_APPDIR %MYDLP_APPDIR% ^
-boot mydlp

rem @erlsrv remove "MyDLP Engine"
rem @erlsrv add "MyDLP Engine" -internalservicename mydlpengine -comment "MyDLP Engine" -workdir %MYDLPBEAMDIR% -sname system -args "+fnu -boot mydlp"
rem @erlsrv start "MyDLP Engine"



