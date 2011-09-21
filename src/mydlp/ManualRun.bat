set MYDLP_CONF=C:/workspace/mydlp-endpoint-win/EndPoint/Engine/mydlp/src/mydlp/mydlp-ep.conf
set path=%path%;C:\workspace\mydlp-deployment-env\erl5.7.4\bin;C:\workspace\mydlp-deployment-env\erts-5.7.4\bin
echo start erlang
set
erl -sname system -boot mydlp
