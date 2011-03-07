APXS=apxs2

APXS_CC=`$(APXS) -q CC`   
APXS_TARGET=`$(APXS) -q TARGET`   
APXS_CFLAGS=`$(APXS) -q CFLAGS`   
APXS_SBINDIR=`$(APXS) -q SBINDIR`   
APXS_CFLAGS_SHLIB=`$(APXS) -q CFLAGS_SHLIB`   
APXS_INCLUDEDIR=`$(APXS) -q INCLUDEDIR`   
APXS_LD_SHLIB=`$(APXS) -q LD_SHLIB`
APXS_LIBEXECDIR=`$(APXS) -q LIBEXECDIR`
APXS_LDFLAGS_SHLIB=`$(APXS) -q LDFLAGS_SHLIB`
APXS_SYSCONFDIR=`$(APXS) -q SYSCONFDIR`
APXS_LIBS_SHLIB=`$(APXS) -q LIBS_SHLIB`

apachemoddir = $(libdir)/apache2/modules

DLPINCLUDES = \
	-I/usr/include/thrift/ \
	-I../thrift/gen-cpp/

SOLIBS = \
	-lthrift \
	-lthriftnb \
	-lthriftz \
	-levent \
	-lz

GENCPP_BASE = ../thrift/gen-cpp
mydlp_ui_o = \
	mydlp_ui_constants.o \
	Mydlp_ui.o \
	Mydlp_ui_server.skeleton.o \
	mydlp_ui_types.o

all: mod_dlp.so

mydlp_ui_constants.o: $(GENCPP_BASE)/mydlp_ui_constants.cpp
	g++ -c -fPIC $(DLPINCLUDES) -Wall -o $@ $<

Mydlp_ui.o: $(GENCPP_BASE)/Mydlp_ui.cpp
	g++ -c -fPIC $(DLPINCLUDES) -Wall -o $@ $<

Mydlp_ui_server.skeleton.o: $(GENCPP_BASE)/Mydlp_ui_server.skeleton.cpp
	g++ -c -fPIC $(DLPINCLUDES) -Wall -o $@ $<

mydlp_ui_types.o: $(GENCPP_BASE)/mydlp_ui_types.cpp
	g++ -c -fPIC $(DLPINCLUDES) -Wall -o $@ $<

mod_dlp.o: mod_dlp.cpp
	g++ -c -fPIC $(DLPINCLUDES) -I$(APXS_INCLUDEDIR) -I. $(APXS_CFLAGS) $(APXS_CFLAGS_SHLIB) -Wall -o $@ $<

mod_dlp.so: mod_dlp.o $(mydlp_ui_o)
	g++ -fPIC -shared -o $@ $< $(mydlp_ui_o) $(APXS_LIBS_SHLIB) $(SOLIBS)

#install-data-hook:
#	install -D -m 644 mod_dlp.so $(apachemoddir)/mod_dlp.so
#	/usr/bin/apxs2 -i -A -n dlp mod_dlp.so

clean:
	rm -f *.so *.o *~
