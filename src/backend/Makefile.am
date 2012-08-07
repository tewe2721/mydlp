###
###    Copyright (C) 2012 Huseyin Kerem Cevahir <kerem@mydlp.com>
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

backenddir = $(datadir)/mydlp/backend

backend_DATA = \
	target/mydlp-backend.jar

target/mydlp-backend.jar: pom.xml
	@$(MVN) clean compile assembly:single
	cp -af target/mydlp-backend-1.0-jar-with-dependencies.jar target/mydlp-backend.jar

run:
	java -cp ../lib/tika-xps/target/tika-xps-1.0.0.jar -jar target/mydlp-backend-1.0-jar-with-dependencies.jar

clean-local:
	@$(MVN) clean

