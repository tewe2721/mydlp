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

mydlp_erl = \
	gen-erl/mydlp_thrift.erl \
	gen-erl/mydlp_types.erl

mydlp_hrl = \
	gen-erl/mydlp_thrift.hrl \
	gen-erl/mydlp_constants.hrl \
	gen-erl/mydlp_types.hrl

mydlp_ui_erl = \
	gen-erl/mydlp_ui_thrift.erl \
	gen-erl/mydlp_ui_types.erl

mydlp_ui_hrl = \
	gen-erl/mydlp_ui_thrift.hrl \
	gen-erl/mydlp_ui_constants.hrl \
	gen-erl/mydlp_ui_types.hrl

dist_erl_DATA = \
	$(mydlp_erl) \
	$(mydlp_ui_erl) \
	$(moddlp_erl)
	
dist_hrl_DATA = \
	$(mydlp_hrl) \
	$(mydlp_ui_hrl) \
	$(moddlp_hrl)

beam_DATA = \
	$(dist_erl_DATA:.erl=.beam)

erldir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/src
hrldir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/include
beamdir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/ebin
privdir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/priv
htmldir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/doc
bootdir = $(libdir)/erlang/bin
#CLEANFILES = gen-erl/* gen-py/*
ERLCFLAGS += -W -o gen-erl

$(beam_DATA): $(dist_erl_DATA) 
$(boot_DATA): $(beam_DATA)

$(mydlp_erl) $(mydlp_hrl) $(pkgpython_PYTHON): mydlp.thrift
	@$(THRIFT) --gen erl --gen java $<
	cp -af gen-java/com/mydlp/backend/thrift/Mydlp.java ../backend/src/main/java/com/mydlp/backend/thrift/Mydlp.java

$(mydlp_ui_erl) $(mydlp_ui_hrl) $(mydlpphplib_DATA) $(mydlp_ui_cpp) $(mydlp_ui_h): mydlp_ui.thrift
	@$(THRIFT) --gen erl --gen java $<
	cp -af gen-java/com/mydlp/ui/thrift/Mydlp_ui.java ../ui/mydlp-ui-thrift/src/main/java/com/mydlp/ui/thrift/Mydlp_ui.java

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<

clean-local:
	rm -rf gen-erl gen-java
