@echo off
%JRE_BIN_DIR%\java.exe -Xms32m -Xmx%JAVAXMX%m -Dmydlp.appdir=%MYDLP_APPDIR% -cp %BACKEND_DIR%\tika-xps.jar;%BACKEND_DIR%\mydlp-backend.jar com.mydlp.backend.Main
