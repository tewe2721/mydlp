dist_erl_DATA = \
	gen_server_cluster.erl \
	mydlp.erl \
	mydlp_app.erl \
	mydlp_acceptor.erl \
	mydlp_echo_worker.erl \
	mydlp_echo_fsm.erl \
	mydlp_fsm.erl \
	mydlp_sup.erl \
	mydlp_worker_sup.erl

dist_hrl_DATA = \
	mydlp.hrl

beam_DATA = \
	$(dist_erl_DATA:.erl=.beam) \
	mydlp.app

#dist_priv_DATA = \
#	mydlp.conf

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
CLEANFILES = $(beam_DATA) $(boot_DATA) $(html_DATA) mydlp.rel overview.edoc
ERLCFLAGS += -W

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

check:
	@echo "Running unit tests..."
	@$(ERL) -eval "eunit:test({application,mydlp})" -noshell -s init stop

console:
	@echo "Running application..."
	@$(ERL) -boot start_sasl -sname console-$$$$ 
