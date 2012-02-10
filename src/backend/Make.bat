@set path=%path%;C:\workspace\mydlp-development-env\apache-maven-3.0.4\bin;C:\workspace\mydlp-development-env\jdk1.7.0_02\bin
@set JAVA_HOME=C:\workspace\mydlp-development-env\jdk1.7.0_02

@mvn clean compile assembly:single

