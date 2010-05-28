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

dist_erl_DATA = \
	gen-erl/mydlp_thrift.erl \
	gen-erl/mydlp_types.erl

dist_hrl_DATA = \
	gen-erl/mydlp_thrift.hrl \
	gen-erl/mydlp_constants.hrl \
	gen-erl/mydlp_types.hrl

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

gen-erl/mydlp_thrift.erl: mydlp.thrift
	$(THRIFT) --gen erl --gen py $<

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<

clean-local:
	rm -rf gen-erl gen-py
