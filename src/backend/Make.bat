@set path=%path%;..\..\..\..\..\..\mydlp-development-env\apache-maven-3.0.4\bin;..\..\..\..\..\..\mydlp-development-env\jdk1.7.0_02\bin;..\..\..\..\..\..\..\mydlp-development-env\apache-maven-3.0.4\bin;..\..\..\..\..\..\..\mydlp-development-env\jdk1.7.0_02\bin
@set BASE_JAVA_HOME=..\..\..\..\..\..\mydlp-development-env\jdk1.7.0_02

@set JAVA_HOME=%BASE_JAVA_HOME%

cmd /c "mvn initialize"

cmd /c "mvn clean"

@set JAVA_HOME=..\%BASE_JAVA_HOME%

cmd /c "cd ..\lib\tika-xps && mvn clean install && cd ..\..\backend && mkdir target && cd target && copy ..\..\lib\tika-xps\target\tika-xps-1.0.0.jar tika-xps.jar"

@set JAVA_HOME=%BASE_JAVA_HOME%

cmd /c "mvn compile assembly:single && cd target && copy mydlp-backend-1.0-jar-with-dependencies.jar mydlp-backend.jar"


