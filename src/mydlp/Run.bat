echo start erlang
set

@erlsrv remove "MyDLP"
@erlsrv add "MyDLP" -internalservicename mydlp-engine -comment "MyDLP Engine" -workdir %MYDLPBEAMDIR% -env MYDLP_CONF=%MYDLP_CONF% -sname system -args "-boot mydlp"
@erlsrv start "MyDLP"

