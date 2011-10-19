@path %path%;C:\workspace\mydlp-deployment-env\erl5.8.5\bin;C:\workspace\mydlp-deployment-env\erl5.8.5\erts-5.8.5\bin

@erlsrv add "MyDLP Engine" -w "C:\workspace\mydlp-endpoint-win\EndPoint\Engine\mydlp\src\mydlp" -sname system -args "+fnu -boot mydlp"
