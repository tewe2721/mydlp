@echo off
set CURXMX=%JAVAXMX%

set JAVAOPTS=-Xms2m

:TRYJAVA
%JRE_BIN_DIR%\java.exe %JAVAOPTS% -Xmx%CURXMX%m -version > nul 2> nul
IF %ERRORLEVEL% EQU 0 GOTO RUNJAVA 
GOTO REDUCEXMX

:REDUCEXMX
set /a CURXMX=%CURXMX%-50
IF %CURXMX% EQU 64 GOTO FAILRUN
IF %CURXMX% LSS 64 GOTO MINXMX
GOTO TRYJAVA

:MINXMX
set /a CURXMX=64
GOTO TRYJAVA

:RUNJAVA
echo "XMX: %JAVAXMX%m is proposed, but using %CURXMX%"
%JRE_BIN_DIR%\java.exe %JAVAOPTS% -Xmx%CURXMX%m -Dmydlp.appdir=%MYDLP_APPDIR% -cp %BACKEND_DIR%\tika-xps.jar;%BACKEND_DIR%\mydlp-backend.jar com.mydlp.backend.Main
GOTO END

:FAILRUN
echo "Can not run JVM."
GOTO END

:END
echo "Batch finished."
