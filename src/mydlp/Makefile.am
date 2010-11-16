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

mydlpconfdir = $(sysconfdir)/mydlp

mydlpconf_DATA = \
	mydlp.app \
	mydlp-production.config \
	denied_page.html

resourcesdir = $(datadir)/mydlp/resources

resources_DATA = mydlp_nlp_tr_kokler.txt

dist_erl_DATA = \
	dynamic_compile.erl \
	gen_server_cluster.erl \
	mime_util.erl \
	mydlp_acl.erl \
	mydlp_api.erl \
	mydlp_app.erl \
	mydlp_acceptor.erl \
	mydlp_bayeserl_store.erl \
	mydlp_distributor.erl \
	mydlp_dynamic.erl \
	mydlp_fsm.erl \
	mydlp_http_fsm.erl \
	mydlp_icap_fsm.erl \
	mydlp_loglevel.erl \
	mydlp_logger_h.erl \
	mydlp_matchers.erl \
	mydlp_mnesia.erl \
	mydlp_mysql.erl \
	mydlp_nlp_tr.erl \
	mydlp_pg_sup.erl \
	mydlp_regex.erl \
	mydlp_smtp_fsm.erl \
	mydlp_smtpc.erl \
	mydlp_sup.erl \
	mydlp_tc.erl \
	mydlp_ts.erl \
	mydlp_trainer.erl \
	mydlp_worker_sup.erl \
	smtpc.erl \
	smtpc_fsm.erl \
	smtpd_cmd.erl

dist_hrl_DATA = \
	mydlp.hrl \
	mydlp_http.hrl \
	mydlp_smtp.hrl \
	mydlp_schema.hrl

beam_DATA = \
	$(dist_erl_DATA:.erl=.beam)

boot_DATA = \
	mydlp.boot \
	mydlp.script

dist_noinst_DATA = \
	overview.edoc.in

html_DATA = \
	$(dist_erl_DATA:.erl=.html) \
	erlang.png \
	edoc-info \
	stylesheet.css \
	index.html \
	overview-summary.html \
	modules-frame.html \
	packages-frame.html

erldir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/src
hrldir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/include
beamdir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/ebin
privdir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/priv
htmldir = $(ERLANG_INSTALL_LIB_DIR_mydlp)/doc
bootdir = $(libdir)/erlang/bin
ERLCFLAGS += -W -I ../thrift/gen-erl

$(beam_DATA): $(dist_hrl_DATA)
$(boot_DATA): $(beam_DATA)

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<

.rel.boot:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. $<

.rel.script:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. $<

$(html_DATA): $(dist_erl_DATA) overview.edoc 
	@echo "Generating documentation..."
	@$(ERL) -noshell -run edoc_run application mydlp '"."' '[{dir,"."}]'

mydlp_nlp_tr_kokler.txt:
	@$(WGET) http://zemberek.googlecode.com/svn/trunk/zemberek2/kaynaklar/tr/bilgi/duzyazi-kilavuz.txt -O mydlp_nlp_tr_kokler.txt

check:
	@echo "Running unit tests..."
	@$(ERL) -eval "eunit:test({application,mydlp})" -noshell -s init stop

uniq-console:
	@echo "Spawning console..."
	@$(ERL) -pa ../thrift/gen-erl -boot start_sasl -sname console-$$$$ 

run: $(beam_DATA) $(boot_DATA)
	@echo "Running application..."
	@$(ERL) -pa ../thrift/gen-erl -sname console -boot mydlp 

run-production: $(beam_DATA) $(boot_DATA)
	@echo "Running application..."
	@$(ERL) -pa ../thrift/gen-erl -sname console -config mydlp-production -mnesia dir '"/var/lib/mydlp/mnesia"' -boot mydlp 

clean-local:
	rm -rf $(beam_DATA) $(boot_DATA) $(html_DATA) mydlp.rel overview.edoc Mnesia.*

install-data-hook: 
	cd $(DESTDIR)$(beamdir) && \
		$(LN_S) $(mydlpconfdir)/mydlp.app mydlp.app

