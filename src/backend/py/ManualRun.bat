echo start python
set PYTHONPATH=C:\workspace\mydlp-endpoint-win\EndPoint\Engine\mydlp\src\thrift\gen-py
set PATH=%PATH%;C:\workspace\mydlp-development-env\Python26
set MYDLP_APPDIR=C:/workspace/mydlp-development-env
python MyDLPBackendServer.py mydlp-backend-py.pid
