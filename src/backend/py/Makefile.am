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

python_PYTHON = \
	sitecustomize.py \
	MyDLPBackendServer.py \
	BCFileIntegrity.py \
	daemon.py

lib_LTLIBRARIES = libbfi.la
libbfi_la_SOURCES = bfi.c
libbfi_la_LDFLAGS = -no-undefined -version-info 0:0:0

LIBS = -lelf -lmagic 

run: libbfi.la
	@echo "Running application..."
	PYTHONPATH="/usr/lib/python2.6/site-packages:../../thrift/gen-py" python MyDLPBackendServer.py /tmp/.mydlp-backend-py.pid

clean-local:
	rm -f *.pyc *.pyo
