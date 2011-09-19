@path %path%;C:\workspace\mydlp-deployment-env\erl5.7.4\bin;C:\workspace\mydlp-deployment-env\erl5.7.4\erts-5.7.4\bin

@erlsrv add "MyDLP" -w "C:\workspace\mydlp-endpoint-win\EndPoint\Engine\mydlp\src\mydlp" -sname system -args "-boot mydlp"
