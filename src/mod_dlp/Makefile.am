## This is the shared library to be built
noinst_LTLIBRARIES = libmoddlp.la

## Define the source file for the module
libmoddlp_la_SOURCES = mod_dlp.cpp
libmoddlp_la_LDFLAGS = -no-undefined -version-info 0:0:0


## Define that an include directory is required.
INCLUDES = \
	-I/usr/include/apache2 \
	-I/usr/include/apr-1.0 \
	-I/usr/include/openssl \
	-I/usr/include/xmltok \
	-I/usr/include/thrift/ \
	-I../thrift/gen-cpp/

LIBS = -lpthread

install-data-hook:
	/usr/bin/apxs2 -i -A -n dlp libmoddlp.la 

