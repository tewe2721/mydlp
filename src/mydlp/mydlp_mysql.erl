%%%
%%%    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@mydlp.com>
%%%
%%%--------------------------------------------------------------------------
%%%    This file is part of MyDLP.
%%%
%%%    MyDLP is free software: you can redistribute it and/or modify
%%%    it under the terms of the GNU General Public License as published by
%%%    the Free Software Foundation, either version 3 of the License, or
%%%    (at your option) any later version.
%%%
%%%    MyDLP is distributed in the hope that it will be useful,
%%%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%    GNU General Public License for more details.
%%%
%%%    You should have received a copy of the GNU General Public License
%%%    along with MyDLP.  If not, see <http://www.gnu.org/licenses/>.
%%%--------------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@mydlp.com>
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-ifdef(__MYDLP_NETWORK).

-module(mydlp_mysql).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

%% API
-export([start_link/0,
	compile_customer/0,
	compile_customer/1,
	push_log/9,
	requeued/1,
	is_multisite/0,
	get_denied_page/0,
	insert_log_file/5,
	insert_log_file/2,
	insert_log_requeue/1,
	delete_log_requeue/1,
	repopulate_mnesia/0,
	save_fingerprints/2,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {
	host,
	port,
	user,
	password,
	database,
	database_l,
	pool_size,
	master_pid,
	pool_pids,
	pool_pids_l
}).

%%%%%%%%%%%%% MyDLP Thrift RPC API

push_log(Time, Channel, RuleId, Action, Ip, User, To, ITypeId, Misc) ->
	gen_server:call(?MODULE, {push_log, {Time, Channel, RuleId, Action, Ip, User, To, ITypeId, Misc}}, 60000).

insert_log_requeue(LogId) -> 
	gen_server:cast(?MODULE, {insert_log_requeue, LogId}).

delete_log_requeue(LogId) -> 
	gen_server:cast(?MODULE, {delete_log_requeue, LogId}).

insert_log_file(LogId, Filename) -> 
	gen_server:cast(?MODULE, {insert_log_file, LogId, Filename}).

insert_log_file(LogId, Filename, MimeType, Size, Path) -> 
	gen_server:cast(?MODULE, {insert_log_file, LogId, Filename, MimeType, Size, Path}).

save_fingerprints(DocumentId, FingerprintList) -> 
	gen_server:call(?MODULE, {save_fingerprints, DocumentId, FingerprintList}).

requeued(LogId) -> 
	gen_server:cast(?MODULE, {requeued, LogId}).

repopulate_mnesia() ->
	gen_server:cast(?MODULE, repopulate_mnesia).

compile_customer() -> compile_customer(mydlp_mnesia:get_dfid()).

compile_customer(FilterId) when is_integer(FilterId) ->
	gen_server:call(?MODULE, {compile_customer, FilterId} , 60000).

%is_multisite() -> gen_server:call(?MODULE, is_multisite).
is_multisite() -> false.

get_denied_page() -> gen_server:call(?MODULE, get_denied_page).

%%%%%%%%%%%%%% gen_server handles

handle_call({compile_customer, FilterId}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			mydlp_mnesia:remove_site(FilterId),
			populate_site(FilterId),
                        Worker ! {async_reply, ok, From}
		end, 60000),
        {noreply, State};

%handle_call(is_multisite, _From, State) ->
%	{ok, ATQ} = psq(app_type),
%	Reply = case ATQ of
%		[] -> false;
%		[[0]] -> false;
%		[[1]] -> true end,
%        {reply, Reply, State};

handle_call(get_denied_page, _From, State) ->
	% Probably will create problems in multisite use.
	{ok, DPQ} = psq(denied_page),
	Reply = case DPQ of
		[[DeniedPage]] when is_binary(DeniedPage) -> DeniedPage;
		_Else -> not_found end,
        {reply, Reply, State};

handle_call({push_log, {Time, Channel, RuleId, Action, Ip, User, To, ITypeId, Misc}}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			{_FilterId, RuleId1, Ip1, User1, To1, ActionS, ChannelS, Visible} = 
				pre_push_log(RuleId, Ip, User, To, Action, Channel),
			{atomic, ILId} = ltransaction(fun() ->
					psqt(insert_incident, 
						[Time, ChannelS, RuleId1, Ip1, User1, To1, ITypeId, ActionS, Misc, Visible]),
					last_insert_id_t() end, 30000),
			Reply = ILId,	
                        Worker ! {async_reply, Reply, From}
		end, 30000),
	{noreply, State};

handle_call({save_fingerprints, DocumentId, FingerprintList}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			transaction(fun() ->
				lists:foreach(fun(F) -> psqt(insert_fingerprint, [F, DocumentId]) end, FingerprintList) 
			end, 60000),
                        Worker ! {async_reply, ok, From}
		end, 60000),
        {noreply, State};

handle_call(stop, _From,  State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({insert_log_file, LogId, Filename}, State) ->
	?ASYNC(fun() ->
		lpsq(insert_incident_file, [LogId, Filename, null], 30000)
	end, 30000),
	{noreply, State};

handle_cast({insert_log_requeue, LogId}, State) ->
	?ASYNC(fun() ->
		lpsq(insert_incident_requeue, [LogId], 30000)
	end, 30000),
	{noreply, State};

handle_cast({delete_log_requeue, LogId}, State) ->
	?ASYNC(fun() ->
		lpsq(delete_incident_requeue, [LogId], 30000)
	end, 30000),
	{noreply, State};

handle_cast({insert_log_file, LogId, Filename, MimeType, Size, Path}, State) ->
	% Probably will create problems in multisite use.
	?ASYNC(fun() ->
		{atomic, DataId} = ltransaction(fun() ->
			mysql:fetch(pl, <<"LOCK TABLE log_archive_data WRITE">>),
			Query =  psqt(incident_data_by_path, [Path]),
			DId = case Query of
				{ok, [] } ->	psqt(insert_incident_data, [MimeType, Size, Path]),
						last_insert_id_t();
				{ok, [[Id]|_]} -> Id end,
			mysql:fetch(pl, <<"UNLOCK TABLES">>), DId
			end, 60000),
		lpsq(insert_incident_file, [LogId, Filename, DataId], 30000)
	end, 100000),
	{noreply, State};

handle_cast(repopulate_mnesia, State) ->
	% Probably will create problems in multisite use.
	?ASYNC0(fun() ->
		mydlp_mnesia:wait_for_tables(),
		case mydlp_mysql:is_multisite() of
			false -> mydlp_mysql:compile_customer(mydlp_mnesia:get_dfid());
			true -> ok % should be implemented for multi site usage
		end
	end),
	{noreply, State};

handle_cast({requeued, LogId}, State) ->
	?ASYNC0(fun() ->
		lpsq(update_requeue_status, [LogId], 30000)
	end),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info({'DOWN', _, _, MPid , _}, #state{master_pid=MPid} = State) ->
	{stop, normalStop, State};

handle_info({'DOWN', _, _, Pid , _}, #state{host=Host,
		user=User, password=Password, database=DB, database_l=LDB,
		pool_pids=PoolPids, pool_pids_l=PoolPidsL} = State) ->
	PPTuple  = case lists:any(fun(P) -> P == Pid end, PoolPids) of
		true -> PoolPids1 = lists:delete(Pid, PoolPids),
			case mysql:connect(pp, Host, undefined, User, Password, DB, true) of
				{ok, NewPid} -> {[NewPid|PoolPids1], PoolPidsL};
				_ -> error end;
		false -> PoolPidsL1 = lists:delete(Pid, PoolPidsL),
			case mysql:connect(pl, Host, undefined, User, Password, LDB, true) of
				{ok, NewPid} -> {PoolPids, [NewPid|PoolPidsL1]};
				_ -> error end
	end,

	case PPTuple of
		error -> {stop, normalStop, State};
		{PP, PPL} -> {noreply, State#state{pool_pids=PP, pool_pids_l=PPL}}
	end;

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	Host = ?CFG(mysql_host),
	Port = ?CFG(mysql_port),
	User = ?CFG(mysql_user),
	Password = ?CFG(mysql_password),
	DB = ?CFG(mysql_database),
	LDB = ?CFG(mysql_log_database),
	PoolSize = ?CFG(mysql_pool_size),
	
	{ok, MPid} = mysql:start_link(pp, Host, Port, User, Password, DB, fun(_,_,_,_) -> ok end),
	erlang:monitor(process, MPid), 
	
	PoolReturns = [ mysql:connect(pp, Host, undefined, User, Password, DB, true) || _I <- lists:seq(1, 2)],
	PPids = [ P || {ok, P} <- PoolReturns ],
	[ erlang:monitor(process, P) || P <- PPids ],

	%{ok, MPid2} = mysql:start_link(pl, Host, Port, User, Password, LDB, fun(_,_,_,_) -> ok end),
	%erlang:monitor(process, MPid2), 
	
	PoolReturns2 = [ mysql:connect(pl, Host, undefined, User, Password, LDB, true) || _I <- lists:seq(1, PoolSize)],
	PPids2 = [ P || {ok, P} <- PoolReturns2 ],
	[ erlang:monitor(process, P) || P <- PPids2 ],

	[ mysql:prepare(Key, Query) || {Key, Query} <- [
		{last_insert_id, <<"SELECT last_insert_id()">>},
		{configs, <<"SELECT configKey,value FROM Config">>},
		{rules, <<"SELECT id,DTYPE,action FROM Rule WHERE enabled=1 order by priority desc">>},
		{network_by_rule_id, <<"SELECT n.ipBase,n.ipMask FROM Network AS n, RuleItem AS ri WHERE ri.rule_id=? AND n.id=ri.item_id">>},
		{user_s_by_rule_id, <<"SELECT u.username FROM RuleUserStatic AS u, RuleItem AS ri WHERE ri.rule_id=? AND u.id=ri.item_id">>},
		{user_ad_u_by_rule_id, <<"SELECT u.id FROM ADDomainUser u, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=u.id">>},
		{user_ad_o_by_rule_id, <<"SELECT u.id FROM ADDomainUser u, ADDomainItem i, ADDomainOU o, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=o.id AND o.id=i.parent_id AND i.id=u.id">>},
		{domain_item_parent_by_id, <<"SELECT i.parent_id FROM ADDomainItem AS i WHERE i.id=?">>},
		{domain_root_by_id, <<"SELECT r.id, r.domain_id FROM ADDomainRoot AS r WHERE r.id=?">>},
		{domain_names_by_id, <<"SELECT d.domainName, d.netbiosName FROM ADDomain AS d WHERE d.id=?">>},
		{domain_aliases_by_id, <<"SELECT a.domainAlias FROM ADDomainAlias AS a, ADDomain_ADDomainAlias AS da WHERE da.ADDomain_id=? AND a.id=da.aliases_id">>},
		{domain_user_sam_by_id, <<"SELECT u.sAMAccountName FROM ADDomainUser AS u WHERE u.id=?">>},
		{domain_user_aliases_by_id, <<"SELECT a.userAlias FROM ADDomainUserAlias AS a, ADDomainUser_ADDomainUserAlias AS ua WHERE ua.ADDomainUser_id=? AND a.id=ua.aliases_id">>},
		{itype_by_rule_id, <<"SELECT t.id, CASE WHEN d.distanceEnabled=1 THEN 1 ELSE 0 END, d.distance FROM InformationType AS t, RuleItem AS ri, InformationDescription AS d WHERE ri.rule_id=? AND t.id=ri.item_id AND t.InformationDescription_id=d.id">>},
		{data_formats_by_itype_id, <<"SELECT df.dataFormats_id FROM InformationType_DataFormat AS df WHERE df.InformationType_id=?">>},
		{ifeature_by_itype_id, <<"SELECT f.threshold,f.matcher_id FROM InformationFeature AS f, InformationDescription_InformationFeature df, InformationType t WHERE t.id=? AND t.informationDescription_id=df.InformationDescription_id AND df.features_id=f.id">>},
		{match_by_id, <<"SELECT m.id,m.functionName FROM Matcher AS m WHERE m.id=?">>},
		{regex_by_matcher_id, <<"SELECT re.regex FROM MatcherArgument AS ma, RegularExpression AS re WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=re.id">>},
		{kg_regexes_by_matcher_id, <<"SELECT re.regex FROM MatcherArgument AS ma, NonCascadingArgument AS nca, RegularExpressionGroup_RegularExpressionGroupEntry AS gre, RegularExpressionGroupEntry AS re WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=nca.id AND nca.argument_id=gre.RegularExpressionGroup_id AND gre.entries_id=re.id">>},
		{kg_rdbms_regexes_by_matcher_id, <<"SELECT rev.string FROM MatcherArgument AS ma, NonCascadingArgument AS nca, RegularExpressionGroup AS reg, RDBMSEnumeratedValue AS rev WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=nca.id AND nca.argument_id=reg.id AND reg.rdbmsInformationTarget_id=rev.informationTarget_id">>},
		{dd_by_matcher_id, <<"SELECT dd.id FROM MatcherArgument AS ma, NonCascadingArgument AS nca, DocumentDatabase AS dd WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=nca.id AND nca.argument_id=dd.id">>},
		{filehash_by_dd_id, <<"SELECT ddfe.id, ddfe.md5Hash FROM DocumentDatabase_DocumentDatabaseFileEntry AS dd, DocumentDatabaseFileEntry AS ddfe WHERE dd.DocumentDatabase_id=? AND dd.fileEntries_id=ddfe.id">>},
		{filefingerprint_by_dd_id, <<"SELECT ddfe.id, df.fingerprint FROM DocumentDatabase_DocumentDatabaseFileEntry AS dd, DocumentDatabaseFileEntry AS ddfe, DocumentFingerprint AS df WHERE dd.DocumentDatabase_id=? AND dd.fileEntries_id=ddfe.id AND df.document_id=ddfe.id">>},
		%{user_by_rule_id, <<"SELECT eu.id, eu.username FROM sh_ad_entry_user AS eu, sh_ad_cross AS c, sh_ad_rule_cross AS rc WHERE rc.parent_rule_id=? AND rc.group_id=c.group_id AND c.entry_id=eu.entry_id">>},
		{mimes_by_data_format_id, <<"SELECT m.mimeType FROM MIMEType AS m, DataFormat_MIMEType dm WHERE dm.DataFormat_id=? and dm.mimeTypes_id=m.id">>},
		{usb_devices, <<"SELECT deviceId, action FROM USBDevice">>},
		{insert_fingerprint, <<"INSERT INTO DocumentFingerprint (id, fingerprint, document_id) VALUES (NULL, ?, ?)">>},
		%{customer_by_id, <<"SELECT id,static_ip FROM sh_customer WHERE id=?">>},
		{insert_incident, <<"INSERT INTO IncidentLog (id, date, channel, ruleId, sourceIp, sourceUser, destination, informationTypeId, action, matcherMessage, visible) VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>},
		{insert_incident_file, <<"INSERT INTO IncidentLogFile (id, incidentLog_id, filename, content_id) VALUES (NULL, ?, ?, ?)">>},
%		{insert_archive, <<"INSERT INTO log_archive (id, customer_id, rule_id, protocol, src_ip, src_user, destination, log_archive_file_id) VALUES (NULL, ?, ?, ?, ?, ?, ?, ?)">>},
%		{new_archive_file_entry, <<"INSERT INTO log_archive_file (id) VALUES (NULL)">>},
%		{update_archive_file, <<"UPDATE log_archive_file SET filename=?, log_archive_data_id=? WHERE id = ?">>},
		{incident_data_by_path, <<"SELECT id FROM IncidentLogFileContent WHERE localPath = ?">>},
		{insert_incident_data, <<"INSERT INTO IncidentLogFileContent (id, mimeType, size, localPath) VALUES (NULL, ?, ?, ?)">>},
		{insert_incident_requeue, <<"INSERT INTO IncidentLogRequeueStatus (id, incidentLog_id, isRequeued) VALUES (NULL, ?, false)">>},
		{delete_incident_requeue, <<"DELETE FROM IncidentLogRequeueStatus WHERE incidentLog_id=?">>},
		{update_requeue_status, <<"UPDATE IncidentLogRequeueStatus SET isRequeued=TRUE, date=now() WHERE incidentLog_id=?">>},
		{denied_page, <<"SELECT c.value FROM Config AS c WHERE c.configKey=\"denied_page_html\"">>}

	]],

	{ok, #state{host=Host, port=Port, 
			user=User, password=Password, 
			database=DB, database_l=LDB, pool_size=PoolSize, 
			master_pid=MPid, pool_pids=PPids, pool_pids_l=PPids}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%% internal api

psq(PreparedKey) -> psq(PreparedKey, []).

psq(PreparedKey, Params) -> psq(PreparedKey, Params, 5000).

psq(PreparedKey, Params, Timeout) ->
	case mysql:execute(pp, PreparedKey, Params, Timeout) of
		{data,{mysql_result,_,Result,_,_}} -> {ok, Result};
		{updated,{mysql_result, _,_,RowCount,_}} -> {updated, RowCount};
		Else -> throw({error, Else})
	end.

%lpsq(PreparedKey) -> lpsq(PreparedKey, []).

% lpsq(PreparedKey, Params) -> lpsq(PreparedKey, Params, 5000).

lpsq(PreparedKey, Params, Timeout) ->
	case mysql:execute(pl, PreparedKey, Params, Timeout) of
		{data,{mysql_result,_,Result,_,_}} -> {ok, Result};
		{updated,{mysql_result, _,_,RowCount,_}} -> {updated, RowCount};
		Else -> throw({error, Else})
	end.

%transaction(Fun) -> transaction(Fun, 5000).

transaction(Fun, Timeout) -> mysql:transaction(pp, Fun, Timeout).

%ltransaction(Fun) -> ltransaction(Fun, 5000).

ltransaction(Fun, Timeout) -> mysql:transaction(pl, Fun, Timeout).

last_insert_id_t() ->
	{ok, [[LIId]]} = psqt(last_insert_id), LIId.

psqt(PreparedKey) -> psqt(PreparedKey, []).

psqt(PreparedKey, Params) ->
	case mysql:execute(PreparedKey, Params) of
		{data,{mysql_result,_,Result,_,_}} -> {ok, Result};
		{updated,{mysql_result, _,_,RowCount,_}} -> {updated, RowCount};
		Else -> throw({error, Else})
	end.

%%%%%%%%%%%% internal

mydlp_mnesia_write(I) when is_list(I) ->
	L = get(mydlp_mnesia_write),
	put(mydlp_mnesia_write, lists:append(I,L)),
	ok;

mydlp_mnesia_write(I) when is_tuple(I) ->
	L = get(mydlp_mnesia_write),
	put(mydlp_mnesia_write, [I|L]),
	ok.

populate_site(FilterId) ->
	put(mydlp_mnesia_write, []),
	%TODO: refine this
	%{ok, FQ} = psq(filters_by_cid, [FilterId]),
	populate_filters([[FilterId, <<"pass">> ]], FilterId),

	% This will create problems in multi-site
	{ok, CQ} = psq(configs),
	populate_configs(CQ, FilterId),

	%TODO: should add for multi-site
	%{ok, SQ} = psq(customer_by_id, [FilterId]),
	%populate_site_desc(SQ),

	{ok, UDQ} = psq(usb_devices),
	populate_usb_devices(UDQ, FilterId),
	mydlp_mnesia:write(get(mydlp_mnesia_write)),
	erase(mydlp_mnesia_write),
	mydlp_mnesia:compile_regex(),
	ok.

populate_configs([[Key, Value]|Rows], FilterId) ->
	Id = mydlp_mnesia:get_unique_id(config),
	C = #config{id=Id, filter_id=FilterId, key=Key, value=Value},
	mydlp_mnesia_write(C),
	populate_configs(Rows, FilterId);
populate_configs([], _FilterId) -> ok.

%populate_filters(Rows) -> populate_filters(Rows, mydlp_mnesia:get_dfid()).

populate_filters([[Id, DActionS]|Rows], Id) ->
	DAction = rule_action_to_atom(DActionS),
	{ok, RQ} = psq(rules),
	populate_rules(RQ, Id),
	F = #filter{id=Id, default_action=DAction},
	mydlp_mnesia_write(F),
	populate_filters(Rows, Id);
populate_filters([], _FilterId) -> ok.

populate_rules([[Id, DTYPE, ActionS] |Rows], FilterId) ->
	Action = rule_action_to_atom(ActionS),
	Channel = rule_dtype_to_channel(DTYPE),
	populate_rule(Id, Channel, Action, FilterId),
	populate_rules(Rows, FilterId);
populate_rules([], _FilterId) -> ok.

populate_rule(OrigId, Channel, Action, FilterId) ->
	RuleId = mydlp_mnesia:get_unique_id(rule),

	{ok, IQ} = psq(network_by_rule_id, [OrigId]),
	populate_iprs(IQ, RuleId),

	populate_rule_users(OrigId, RuleId),

	{ok, ITQ} = psq(itype_by_rule_id, [OrigId]),
	populate_itypes(ITQ, RuleId),

	%{ok, MQ} = psq(match_by_rule_id, [Id]),
	%populate_matches(MQ, Parent),
	%{ok, MGQ} = psq(mgroup_by_rule_id, [Id]),
	%populate_matchGroups(MGQ, Parent),
	%{ok, TDQ} = psq(tdomains_by_rid, [Id]),
	%TDs = lists:flatten(TDQ),
	R = #rule{id=RuleId, orig_id=OrigId, channel=Channel, action=Action, filter_id=FilterId},
	mydlp_mnesia_write(R).

populate_iprs([[Base, Subnet]| Rows], RuleId) ->
	B1 = int_to_ip(Base),
	S1 = int_to_ip(Subnet),
	Id = mydlp_mnesia:get_unique_id(ipr),
	I = #ipr{id=Id, rule_id=RuleId, ipbase=B1, ipmask=S1},
	mydlp_mnesia_write(I),
	populate_iprs(Rows, RuleId);
populate_iprs([], _RuleId) -> ok.

populate_rule_users(RuleOrigId, RuleId) -> 
	{ok, USQ} = psq(user_s_by_rule_id, [RuleOrigId]),
	populate_users_s(USQ, RuleId),

	{ok, UAUQ} = psq(user_ad_u_by_rule_id, [RuleOrigId]),
	populate_users_ad_u(UAUQ, RuleId),

	{ok, UAOQ} = psq(user_ad_o_by_rule_id, [RuleOrigId]),
	populate_users_ad_u(UAOQ, RuleId),

	ok.

populate_users_s([[Username]| Rows], RuleId) ->
	new_user(Username, RuleId),
	populate_users_s(Rows, RuleId);
populate_users_s([], _RuleId) -> ok.

populate_users_ad_u([[OrigId]| Rows], RuleId) ->
	Usernames = get_usernames(OrigId),
	lists:foreach(fun(Username) -> new_user(Username, RuleId) end, Usernames),
	populate_users_ad_u(Rows, RuleId);
populate_users_ad_u([], _RuleId) -> ok.

get_usernames(OrigId) ->
	Users = get_users(OrigId),
	Domains = get_domains(OrigId),
	L = lists:map(fun(U) ->
		lists:map(fun(D) ->
			concat_username(U,D)
		end, Domains)
	end, Users),
	lists:flatten(L).

concat_username(<<>>, _D) -> []; % lists:flatten will drop this.
concat_username(U, <<>>) -> <<U/binary>>;
concat_username(U, D) -> <<U/binary, "@" , D/binary>>.

get_domains(OrigId) ->
	DomainId = get_domain_id(OrigId),
	{ok, [[DomainName,NetbiosName]]} = psq(domain_names_by_id, [DomainId]), %% TODO: cache may be used to improve performance.
	{ok, DAQ} = psq(domain_aliases_by_id, [DomainId]),
	DomainAliases = lists:append(DAQ),
	[DomainName,NetbiosName] ++ DomainAliases.

get_users(OrigId) ->
	{ok, [[SAMName]]} = psq(domain_user_sam_by_id, [OrigId]),
	{ok, AAQ} = psq(domain_user_aliases_by_id, [OrigId]),
	UserAliases = lists:append(AAQ),
	[SAMName] ++ UserAliases.

get_domain_id(ItemId) ->
	{ok, [[ParentId]]} = psq(domain_item_parent_by_id, [ItemId]),
	case psq(domain_root_by_id, [ParentId]) of
		{ok, []} -> get_domain_id(ParentId);
		{ok, [[ParentId, DomainId]]} -> DomainId end.

new_user(Username, RuleId) ->
	Id = mydlp_mnesia:get_unique_id(m_user),
	UsernameH = mydlp_api:hash_un(Username),
	User = #m_user{id=Id, rule_id=RuleId, un_hash=UsernameH},
	mydlp_mnesia_write(User),
	ok.

int_to_ip(nil) -> nil;
int_to_ip(N4) ->
	I4 = N4 rem 256,
	N3 = N4 div 256,
	I3 = N3 rem 256,
	N2 = N3 div 256,
	I2 = N2 rem 256,
	N1 = N2 div 256,
	I1 = N1,
	I1 = N1 rem 256,
	{I1, I2, I3, I4}.

ip_to_int(nil) -> nil;
ip_to_int({I1,I2,I3,I4}) ->
	(I1*256*256*256)+(I2*256*256)+(I3*256)+I4.

pr_data_formats(ITypeOrigId) ->
	{ok, DFQ} = psq(data_formats_by_itype_id, [ITypeOrigId]),
	DataFormats = lists:usort(lists:flatten(DFQ)),
	populate_data_formats(DataFormats),
	case DataFormats of
		[DFId] ->
			{ok, MQ} = psq(mimes_by_data_format_id, [DFId]),
			case MQ of
				[[<<"mydlp-internal/all">>]] -> all;
				_Else2 -> DataFormats end;
		_Else -> DataFormats end.

populate_itypes([[OrigId,DistanceEnabled,Distance]| Rows], RuleId) ->
	DataFormats = pr_data_formats(OrigId),
	ITypeId = mydlp_mnesia:get_unique_id(itype),
	DistanceValue = case {DistanceEnabled,Distance} of
		{0, _} -> undefined;
		{1, D} -> D end,
	T = #itype{id=ITypeId, orig_id=OrigId, rule_id=RuleId, data_formats=DataFormats, distance=DistanceValue},
	{ok, IFQ} = psq(ifeature_by_itype_id, [OrigId]),
	populate_ifeatures(IFQ, ITypeId),
	mydlp_mnesia_write(T),
	populate_itypes(Rows, RuleId);
populate_itypes([], _RuleId) -> ok.

populate_ifeatures([[Threshold, MatcherId]| Rows], ITypeId) ->
	IFeatureId = mydlp_mnesia:get_unique_id(ifeature),
	F = #ifeature{id=IFeatureId, itype_id=ITypeId, threshold=Threshold},
	{ok, MQ} = psq(match_by_id, [MatcherId]),
	populate_matches(MQ, IFeatureId),
	mydlp_mnesia_write(F),
	populate_ifeatures(Rows, ITypeId);
populate_ifeatures([], _RuleId) -> ok.

populate_matches(Rows, IFeatureId) -> populate_matches(Rows, IFeatureId, []).

populate_matches([[Id, Func]| Rows], IFeatureId, Matches) ->
	Match = populate_match(Id, Func, IFeatureId),
	populate_matches(Rows, IFeatureId, [Match|Matches]);
populate_matches([], _IFeatureId, Matches) -> 
	% TODO: whitefiles
	%Matches1 = expand_matches(lists:reverse(Matches)),
	%Matches2 = whitefile(Matches1),
	write_matches(Matches),
	ok.

%whitefile(Matches) -> whitefile(Matches, []).

%whitefile([#match{func=whitefile} = Match|Matches], Returns) -> 
%	L1 = lists:append([lists:reverse(Matches), Returns, [Match]]),
%	lists:reverse(L1);
%whitefile([Match|Matches], Returns) -> whitefile(Matches, [Match|Returns]);
%whitefile([], Returns) -> lists:reverse(Returns).

new_match(Id, Parent, Func) -> new_match(Id, Parent, Func, []).

new_match(OrigId, IFeatureId, Func, FuncParams) ->
	#match{orig_id=OrigId, ifeature_id=IFeatureId, func=Func, func_params=FuncParams}.

write_regex(RegexGroupId, RegexS) ->
	RegexId = mydlp_mnesia:get_unique_id(regex),
	R = #regex{id=RegexId, group_id=RegexGroupId, plain=RegexS},
	mydlp_mnesia_write(R).

write_matches(Matches) ->
	Matches1 = [ M#match{id=mydlp_mnesia:get_unique_id(match)} || M <- Matches ],
	mydlp_mnesia_write(Matches1).

populate_match(Id, <<"encrypted_archive">>, IFeatureId) ->
	Func = e_archive_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"encrypted_file">>, IFeatureId) ->
	Func = e_file_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"trid">>, IFeatureId) ->
	Func = trid_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"ssn">>, IFeatureId) ->
	Func = ssn_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"iban">>, IFeatureId) ->
	Func = iban_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"aba">>, IFeatureId) ->
	Func = aba_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"cc">>, IFeatureId) ->
	Func = cc_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"canada_sin">>, IFeatureId) ->
	Func = canada_sin_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"france_insee">>, IFeatureId) ->
	Func = france_insee_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"uk_nino">>, IFeatureId) ->
	Func = uk_nino_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"said">>, IFeatureId) ->
	Func = said_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"pan">>, IFeatureId) ->
	Func = pan_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"cpf">>, IFeatureId) ->
	Func = cpf_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"china_icn">>, IFeatureId) ->
	Func = china_icn_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"cc_edate">>, IFeatureId) ->
	Func = cc_edate_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"gdate">>, IFeatureId) ->
	Func = gdate_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"birthdate">>, IFeatureId) ->
	Func = birthdate_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"scode">>, IFeatureId) ->
	Func = scode_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"scode_ada">>, IFeatureId) ->
	Func = scode_ada_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"all">>, IFeatureId) ->
	Func = all,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"keyword">>, IFeatureId) ->
	Func = regex_match,
	{ok, REQ} = psq(regex_by_matcher_id, [Id]),
	[[RegexS]] = REQ,
	RegexGroupId = mydlp_mnesia:get_unique_id(regex_group_id),
	Regex1 = list_to_binary(mydlp_api:escape_regex(binary_to_list(RegexS))),
	write_regex(RegexGroupId, Regex1),
	FuncParams=[RegexGroupId],
	new_match(Id, IFeatureId, Func, FuncParams);

populate_match(Id, <<"keyword_group">>, IFeatureId) ->
	Func = regex_match,
	RegexGroupId = mydlp_mnesia:get_unique_id(regex_group_id),
	{ok, REQ} = psq(kg_regexes_by_matcher_id, [Id]),
	lists:foreach(fun([RegexS]) ->
		Regex1 = list_to_binary(mydlp_api:escape_regex(binary_to_list(RegexS))),
		write_regex(RegexGroupId, Regex1)
	end, REQ),
	{ok, REREQ} = psq(kg_rdbms_regexes_by_matcher_id, [Id]),
	lists:foreach(fun([RegexS]) ->
		Regex2 = list_to_binary(mydlp_api:escape_regex(binary_to_list(RegexS))),
		write_regex(RegexGroupId, Regex2)
	end, REREQ),
	FuncParams=[RegexGroupId],
	new_match(Id, IFeatureId, Func, FuncParams);

populate_match(Id, <<"regex">>, IFeatureId) ->
	Func = regex_match,
	{ok, REQ} = psq(regex_by_matcher_id, [Id]),
	[[RegexS]] = REQ,
	RegexGroupId = mydlp_mnesia:get_unique_id(regex_group_id),
	write_regex(RegexGroupId, RegexS),
	FuncParams=[RegexGroupId],
	new_match(Id, IFeatureId, Func, FuncParams);

populate_match(Id, <<"document_hash">>, IFeatureId) ->
	Func = md5_match,
	{ok, DDQ} = psq(dd_by_matcher_id, [Id]),
	[[DDId]] = DDQ,
	{ok, FHQ} = psq(filehash_by_dd_id, [DDId]),
	populate_filehashes(FHQ, DDId),
	FuncParams=[DDId],
	new_match(Id, IFeatureId, Func, FuncParams);

populate_match(Id, <<"document_pdm">>, IFeatureId) ->
	Func = pdm_match,
	{ok, DDQ} = psq(dd_by_matcher_id, [Id]),
	[[DDId]] = DDQ,
	{ok, FFQ} = psq(filefingerprint_by_dd_id, [DDId]),
	populate_filefingerprints(FFQ, DDId),
	FuncParams=[DDId],
	new_match(Id, IFeatureId, Func, FuncParams);

populate_match(Id, Matcher, _) -> throw({error, {unsupported_match, Id, Matcher} }).

populate_data_formats([DFId|DFs]) ->
	{ok, MQ} = psq(mimes_by_data_format_id, [DFId]),
	populate_mimes(MQ, DFId),
	populate_data_formats(DFs);
populate_data_formats([]) -> ok.

populate_mimes([[Mime]|Rows], DataFormatId) ->
	Id=mydlp_mnesia:get_unique_id(mime_type),
	M = #mime_type{id=Id, mime=Mime, data_format_id=DataFormatId},
	mydlp_mnesia_write(M),
	populate_mimes(Rows, DataFormatId);
populate_mimes([], _DataFormatId) -> ok.

populate_filehashes([[FileId,HexHash]|Rows], DDId) ->
	Id=mydlp_mnesia:get_unique_id(file_hash),
	F = #file_hash{id=Id, file_id=FileId, group_id=DDId, hash=mydlp_api:hex2bytelist(HexHash)},
	mydlp_mnesia_write(F),
	populate_filehashes(Rows, DDId);
populate_filehashes([], _DDId) -> ok.

populate_filefingerprints([[FileId,Fingerprint]|Rows], DDId) ->
	Id=mydlp_mnesia:get_unique_id(file_fingerprint),
	F = #file_fingerprint{id=Id, file_id=FileId, group_id=DDId, fingerprint=Fingerprint},
	mydlp_mnesia_write(F),
	populate_filefingerprints(Rows, DDId);
populate_filefingerprints([], _DDId) -> ok.

%populate_site_desc([[Id, StaticIpI]|Rows]) ->
%	IpAddr = int_to_ip(StaticIpI),
%	S = #site_desc{filter_id=Id, ipaddr=IpAddr},
%	mydlp_mnesia_write(S),
%	populate_site_desc(Rows);
%populate_site_desc([]) -> ok.

populate_usb_devices([[DeviceId, ActionB]|Rows], FilterId) ->
	Action =  rule_action_to_atom(ActionB),
	U = #usb_device{id=mydlp_mnesia:get_unique_id(usb_device), 
			filter_id=FilterId, device_id=DeviceId, action=Action},
	mydlp_mnesia_write(U),
	populate_usb_devices(Rows, FilterId);
populate_usb_devices([], _FilterId) -> ok.

%get_rule_cid(RuleId) ->
%	case psq(cid_of_rule_by_id, [RuleId]) of
%		{ok, [[FilterId]]} -> FilterId;
%		_Else -> 0 end.

rule_action_to_atom(<<"PASS">>) -> pass;
rule_action_to_atom(<<"LOG">>) -> log;
rule_action_to_atom(<<"BLOCK">>) -> block;
rule_action_to_atom(<<"QUARANTINE">>) -> quarantine;
rule_action_to_atom(<<"ARCHIVE">>) -> archive;
rule_action_to_atom(<<"pass">>) -> pass;
rule_action_to_atom(<<"log">>) -> log;
rule_action_to_atom(<<"block">>) -> block;
rule_action_to_atom(<<"quarantine">>) -> quarantine;
rule_action_to_atom(<<"archive">>) -> archive;
rule_action_to_atom(<<"">>) -> pass;
rule_action_to_atom(Else) -> throw({error, unsupported_action_type, Else}).

rule_dtype_to_channel(<<"WebRule">>) -> web;
rule_dtype_to_channel(<<"MailRule">>) -> mail;
rule_dtype_to_channel(<<"EndpointRule">>) -> endpoint;
rule_dtype_to_channel(<<"PrinterRule">>) -> printer;
rule_dtype_to_channel(<<"DiscoveryRule">>) -> discovery;
rule_dtype_to_channel(Else) -> throw({error, unsupported_rule_type, Else}).

pre_push_log(RuleId, Ip, User, Destination, Action, Channel) -> 
%	{FilterId, RuleId1} = case RuleId of
%		{dr, CId} -> {CId, 0};
%		-1 = RuleId -> {mydlp_mnesia:get_dfid(), RuleId};	% this shows default action had been enforeced 
							% this should be refined for multisite use
%		RId when is_integer(RId) -> {get_rule_cid(RId), RId} end,
	User1 = case User of
		nil -> null;
		U when is_list(U) -> string:to_lower(U);
		U when is_binary(U) -> string:to_lower(binary_to_list(U))
	end,
	Ip1 = case ip_to_int(Ip) of
		nil -> null;
		Else2 -> Else2
	end,
	Destination1 = case Destination of
		nil -> null;
		Else3 -> Else3
	end,
	ActionS = case Action of
		pass -> <<"P">>;
		block -> <<"B">>;
		log -> <<"L">>;
		quarantine -> <<"Q">>;
		archive -> <<"A">> 
	end,
	ChannelS = case Channel of
		web -> <<"W">>;
		mail -> <<"M">>;
		endpoint -> <<"E">>;
		printer -> <<"P">>;
		discovery -> <<"D">> 
	end,
	Visible = case RuleId of
		-1 -> 0;
		_Else -> 1 end,
	{0, RuleId, Ip1, User1, Destination1, ActionS, ChannelS, Visible}.

-endif.

