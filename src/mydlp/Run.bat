echo start erlang
set

@erlsrv remove "MyDLP Engine"
@erlsrv add "MyDLP Engine" -internalservicename mydlpengine -comment "MyDLP Engine" -workdir %MYDLPBEAMDIR% -env MYDLP_CONF=%MYDLP_CONF% -env MYDLP_APPDIR=%MYDLP_APPDIR% -sname system -args "-boot mydlp"
@erlsrv start "MyDLP Engine"

