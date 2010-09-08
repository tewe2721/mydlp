###
###    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@medra.com.tr>
###
###--------------------------------------------------------------------------
###    This file is part of MyDLP.
###
###    MyDLP is free software: you can redistribute it and/or modify
###    it under the terms of the GNU General Public License as published by
###    the Free Software Foundation, either version 3 of the License, or
###    (at your option) any later version.
###
###    MyDLP is distributed in the hope that it will be useful,
###    but WITHOUT ANY WARRANTY; without even the implied warranty of
###    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###    GNU General Public License for more details.
###
###    You should have received a copy of the GNU General Public License
###    along with MyDLP.  If not, see <http://www.gnu.org/licenses/>.
###--------------------------------------------------------------------------

mydlpjbdir = $(datadir)/mydlp

mydlpjb_DATA = \
        target/mydlp-backend-java-0.1.0-jar-with-dependencies.jar

src/main/java/Mydlp_bz.java:
	[ -e src/main/java/Mydlp_bz.java ] || \
		$(LN_S) ../../../../../thrift/gen-java/Mydlp_bz.java src/main/java/

target/mydlp-backend-java-0.1.0-jar-with-dependencies.jar: pom.xml \
			src/main/java/Mydlp_bz.java src/main/java/BayessianZemberekBackend.java \
			src/main/resources/logback.xml
	@$(MVN) assembly:assembly

# http://barelyenough.org/blog/2005/03/java-daemon/
run: target/mydlp-backend-java-0.1.0-jar-with-dependencies.jar
	@echo "Running application..."
	@java -Ddaemon.pidfile=/tmp/.mydlp-backend-java.pid -Dbayesdb.confidential=/tmp/.mydlp-bayes-confidential.dat -Dbayesdb.public=/tmp/.mydlp-bayes-public.dat -jar target/mydlp-backend-java-0.1.0-jar-with-dependencies.jar 

clean-local:
	@rm -f src/main/java/Mydlp_bz.java
	@$(MVN) clean
