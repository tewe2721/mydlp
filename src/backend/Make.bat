@set path=%path%;..\..\..\..\..\..\mydlp-development-env\apache-maven-3.0.4\bin;..\..\..\..\..\..\mydlp-development-env\jdk1.7.0_02\bin
@set JAVA_HOME=..\..\..\..\..\..\mydlp-development-env\jdk1.7.0_02

@mvn clean compile assembly:single

