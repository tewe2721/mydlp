@set path=%path%;..\..\..\..\..\..\mydlp-development-env\apache-maven-3.0.4\bin;..\..\..\..\..\..\mydlp-development-env\jdk1.7.0_02\bin
@set JAVA_HOME=..\..\..\..\..\..\mydlp-development-env\jdk1.7.0_02

mvn clean

cmd /c "cd ..\lib\tika-xps && mvn install && cd ..\..\backend\target && del /q tika-xps.jar && copy ..\lib\tika-xps\target\tika-xps-1.0.0.jar tika-xps.jar"

cmd /c "mvn compile assembly:single && cd target && del /q mydlp-backend.jar && copy mydlp-backend-1.0-jar-with-dependencies.jar mydlp-backend.jar"


