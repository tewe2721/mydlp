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
	compile_filters/0,
	compile_customer/1,
	push_log/8,
%	is_multisite/0,
	get_denied_page/0,
	insert_log_file/5,
	insert_log_file/2,
	repopulate_mnesia/0,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

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

push_log(Channel, RuleId, Action, Ip, User, To, ITypeId, Misc) ->
	gen_server:call(?MODULE, {push_log, {Channel, RuleId, Action, Ip, User, To, ITypeId, Misc}}, 60000).

insert_log_file(LogId, Filename) -> 
	gen_server:cast(?MODULE, {insert_log_file, LogId, Filename}).

insert_log_file(LogId, Filename, MimeType, Size, Path) -> 
	gen_server:cast(?MODULE, {insert_log_file, LogId, Filename, MimeType, Size, Path}).

repopulate_mnesia() ->
	gen_server:cast(?MODULE, repopulate_mnesia).

compile_filters() -> 
	gen_server:call(?MODULE, compile_filters, 60000).

compile_customer(FilterId) when is_integer(FilterId) ->
	gen_server:call(?MODULE, {compile_customer, FilterId} , 60000).

%is_multisite() -> gen_server:call(?MODULE, is_multisite).

get_denied_page() -> gen_server:call(?MODULE, get_denied_page).

%%%%%%%%%%%%%% gen_server handles

handle_call({compile_customer, FilterId}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			mydlp_mnesia:remove_site(FilterId),
			Reply = populate_site(FilterId),
                        Worker ! {async_reply, Reply, From}
		end, 60000),
        {noreply, State};

handle_call(compile_filters, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			mydlp_mnesia:truncate_nondata(),
			Reply = populate(),
                        Worker ! {async_reply, Reply, From}
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

handle_call({push_log, {Channel, RuleId, Action, Ip, User, To, ITypeId, Misc}}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			{_FilterId, RuleId1, Ip1, User1, ActionS, ChannelS} = 
				pre_push_log(RuleId, Ip, User, Action, Channel),
			{atomic, ILId} = ltransaction(fun() ->
					psqt(insert_incident, 
						[ChannelS, RuleId1, Ip1, User1, To, ITypeId, ActionS, Misc]),
					last_insert_id_t() end, 30000),
			Reply = ILId,	
                        Worker ! {async_reply, Reply, From}
		end, 30000),
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
		{rules, <<"SELECT id,DTYPE,action FROM Rule WHERE enabled=1 order by priority desc">>},
		%{cid_of_rule_by_id, <<"SELECT f.customer_id FROM sh_rule AS r, sh_filter AS f WHERE r.filter_id=f.id AND r.id=?">>},
		{network_by_rule_id, <<"SELECT n.ipBase,n.ipMask FROM Network AS n, RuleItem AS ri WHERE ri.rule_id=? AND n.id=ri.item_id">>},
		{itype_by_rule_id, <<"SELECT t.id,d.threshold FROM InformationType AS t, InformationDescription AS d, RuleItem AS ri WHERE ri.rule_id=? AND t.id=ri.item_id AND d.id=t.informationDescription_id">>},
		{data_formats_by_itype_id, <<"SELECT df.dataFormats_id FROM InformationType_DataFormat AS df WHERE df.InformationType_id=?">>},
		{ifeature_by_itype_id, <<"SELECT f.weight,f.matcher_id FROM InformationFeature AS f, InformationDescription_InformationFeature df, InformationType t WHERE t.id=? AND t.informationDescription_id=df.InformationDescription_id AND df.features_id=f.id">>},
		{match_by_id, <<"SELECT m.id,m.functionName FROM Matcher AS m WHERE m.id=?">>},
		{regex_by_matcher_id, <<"SELECT re.regex FROM MatcherParam AS mp, RegularExpression AS re WHERE mp.matcher_id=? AND mp.id=re.id">>},
		%{user_by_rule_id, <<"SELECT eu.id, eu.username FROM sh_ad_entry_user AS eu, sh_ad_cross AS c, sh_ad_entry AS e, sh_ad_group AS g, sh_ad_rule_cross AS rc WHERE rc.parent_rule_id=? AND rc.group_id=g.id AND rc.group_id=c.group_id AND c.entry_id=e.id AND c.entry_id=eu.entry_id">>},
		%{user_by_rule_id, <<"SELECT eu.id, eu.username FROM sh_ad_entry_user AS eu, sh_ad_cross AS c, sh_ad_rule_cross AS rc WHERE rc.parent_rule_id=? AND rc.group_id=c.group_id AND c.entry_id=eu.entry_id">>},
		{mimes_by_data_format_id, <<"SELECT m.mimeType FROM MIMEType AS m, DataFormat_MIMEType dm WHERE dm.DataFormat_id=? and dm.mimeTypes_id=m.id">>},
		%{usb_device_by_cid, <<"SELECT device_id, action FROM ep_usb_device WHERE customer_id=?">>},
		%{customer_by_id, <<"SELECT id,static_ip FROM sh_customer WHERE id=?">>},
		{insert_incident, <<"INSERT INTO IncidentLog (id, channel, ruleId, sourceIp, sourceUser, destination, informationTypeId, action, matcherMessage) VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?)">>},
		{insert_incident_file, <<"INSERT INTO IncidentLogFile (id, incidentLog_id, filename, content_id) VALUES (NULL, ?, ?, ?)">>},
%		{insert_archive, <<"INSERT INTO log_archive (id, customer_id, rule_id, protocol, src_ip, src_user, destination, log_archive_file_id) VALUES (NULL, ?, ?, ?, ?, ?, ?, ?)">>},
%		{new_archive_file_entry, <<"INSERT INTO log_archive_file (id) VALUES (NULL)">>},
%		{update_archive_file, <<"UPDATE log_archive_file SET filename=?, log_archive_data_id=? WHERE id = ?">>},
		{incident_data_by_path, <<"SELECT id FROM IncidentLogFileContent WHERE localPath = ?">>},
		{insert_incident_data, <<"INSERT INTO IncidentLogFileContent (id, mimeType, size, localPath) VALUES (NULL, ?, ?, ?)">>},
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

%transaction(Fun, Timeout) -> mysql:transaction(pl, Fun, Timeout).

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

populate() -> 
	populate_site(mydlp_mnesia:dcid()),
	ok.

populate_site(FilterId) ->
	%TODO: refine this
	%{ok, FQ} = psq(filters_by_cid, [FilterId]),
	populate_filters([[FilterId, <<"pass">> ]], FilterId),
	
	%TODO: should add for multi-site
	%{ok, SQ} = psq(customer_by_id, [FilterId]),
	%populate_site_desc(SQ),

	% TODO: refine and implement this
	%{ok, UDQ} = psq(usb_device_by_cid, [FilterId]),
	%populate_usb_devices(UDQ, FilterId),
	ok.

%populate_filters(Rows) -> populate_filters(Rows, mydlp_mnesia:get_dfid()).

populate_filters([[Id, DActionS]|Rows], Id) ->
	DAction = rule_action_to_atom(DActionS),
	{ok, RQ} = psq(rules),
	populate_rules(RQ, Id),
	F = #filter{id=Id, default_action=DAction},
	mydlp_mnesia:write(F),
	populate_filters(Rows, Id);
populate_filters([], _FilterId) -> ok.

populate_rules([[Id, DTYPE, ActionS] |Rows], FilterId) ->
	Action = rule_action_to_atom(ActionS),
	Channel = rule_dtype_to_channel(DTYPE),
	populate_rule(Id, Channel, Action, FilterId),
	populate_rules(Rows, FilterId);
populate_rules([], _FilterId) -> ok.

populate_rule(OrigId, Channel, Action, FilterId) ->
	{ok, IQ} = psq(network_by_rule_id, [OrigId]),
	RuleId = mydlp_mnesia:get_unique_id(rule),
	populate_iprs(IQ, RuleId),
	%{ok, UQ} = psq(user_by_rule_id, [Id]),
	%populate_users(UQ, Parent),

	{ok, ITQ} = psq(itype_by_rule_id, [OrigId]),
	populate_itypes(ITQ, RuleId),

	%{ok, MQ} = psq(match_by_rule_id, [Id]),
	%populate_matches(MQ, Parent),
	%{ok, MGQ} = psq(mgroup_by_rule_id, [Id]),
	%populate_matchGroups(MGQ, Parent),
	%{ok, TDQ} = psq(tdomains_by_rid, [Id]),
	%TDs = lists:flatten(TDQ),
	R = #rule{id=RuleId, orig_id=OrigId, channel=Channel, action=Action, filter_id=FilterId},
	mydlp_mnesia:write(R).

populate_iprs([[Base, Subnet]| Rows], RuleId) ->
	B1 = int_to_ip(Base),
	S1 = int_to_ip(Subnet),
	Id = mydlp_mnesia:get_unique_id(ipr),
	I = #ipr{id=Id, rule_id=RuleId, ipbase=B1, ipmask=S1},
	mydlp_mnesia:write(I),
	populate_iprs(Rows, RuleId);
populate_iprs([], _RuleId) -> ok.

%populate_users([[Id, Username]| Rows], Parent) ->
%	U = #m_user{id=Id, parent=Parent, username=Username},
%	mydlp_mnesia:write(U),
%	populate_users(Rows, Parent);
%populate_users([], _Parent) -> ok.

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

populate_itypes([[OrigId, Threshold]| Rows], RuleId) ->
	DataFormats = pr_data_formats(OrigId),
	ITypeId = mydlp_mnesia:get_unique_id(itype),
	T = #itype{id=ITypeId, orig_id=OrigId, rule_id=RuleId, data_formats=DataFormats, threshold=Threshold},
	{ok, IFQ} = psq(ifeature_by_itype_id, [OrigId]),
	populate_ifeatures(IFQ, ITypeId),
	mydlp_mnesia:write(T),
	populate_itypes(Rows, RuleId);
populate_itypes([], _RuleId) -> ok.

populate_ifeatures([[Weight, MatcherId]| Rows], ITypeId) ->
	IFeatureId = mydlp_mnesia:get_unique_id(ifeature),
	F = #ifeature{id=IFeatureId, itype_id=ITypeId, weight=Weight},
	{ok, MQ} = psq(match_by_id, [MatcherId]),
	populate_matches(MQ, IFeatureId),
	mydlp_mnesia:write(F),
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

write_matches(Matches) ->
	Matches1 = [ M#match{id=mydlp_mnesia:get_unique_id(match)} || M <- Matches ],
	mydlp_mnesia:write(Matches1).

populate_match(Id, <<"e_archive">>, IFeatureId) ->
	Func = e_archive_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"e_file">>, IFeatureId) ->
	Func = e_file_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"i_archive">>, IFeatureId) ->
	Func = i_archive_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"i_binary">>, IFeatureId) ->
	Func = i_binary_match,
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

populate_match(Id, <<"scode">>, IFeatureId) ->
	Func = scode_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"scode_ada">>, IFeatureId) ->
	Func = scode_ada_match,
	new_match(Id, IFeatureId, Func);

populate_match(Id, <<"keyword">>, IFeatureId) ->
	Func = regex_match,
	{ok, REQ} = psq(regex_by_matcher_id, [Id]),
	[[RegexS]] = REQ,
	RegexId = mydlp_mnesia:get_unique_id(regex),
	RegexGroupId = mydlp_mnesia:get_unique_id(regex_group_id),
	R = #regex{id=RegexId, group_id=RegexGroupId, plain=RegexS},
	mydlp_mnesia:write(R),
	FuncParams=[RegexGroupId],
	new_match(Id, IFeatureId, Func, FuncParams);

% TODO: refine this, currently unused
populate_match(Id, <<"regex">>, IFeatureId) ->
	Func = regex_match,
	GroupsS = get_func_params(Id),
	FuncParams = [ mydlp_api:binary_to_integer(G) || G <- GroupsS ],
	new_match(Id, IFeatureId, Func, FuncParams);

% TODO: refine this, currently unused
populate_match(Id, <<"file">>, IFeatureId) ->
	Func = {group,file},
	GroupsS = get_func_params(Id),
	GroupsI = [ mydlp_api:binary_to_integer(G) || G <- GroupsS ],
	{ok, [[SentenceHashI, SHCount, SHPercI, BayesI, BThresI, WhiteFileI]]} = psq(file_params_by_match_id, [Id]),
	SentenceHash = case SentenceHashI of 0 -> false; _ -> true end,
	SHPerc = SHPercI / 100,
	Bayes = case BayesI of 0 -> false; _ -> true end,
	BThres = BThresI / 100,
	WhiteFile = case WhiteFileI of 0 -> false; _ -> true end,

	FuncParams = [{shash,SentenceHash}, {shash_count,SHCount}, {shash_percentage, SHPerc},
			{bayes, Bayes}, {bayes_threshold, BThres},
			{whitefile, WhiteFile}, {group_ids, GroupsI}],
	new_match(Id, IFeatureId, Func, FuncParams);

populate_match(Id, Matcher, _) -> throw({error, {unsupported_match, Id, Matcher} }).

get_func_params(MatchId) ->
	{ok, PQ} = psq(params_by_match_id, [MatchId]),
	lists:append(PQ).


populate_data_formats([DFId|DFs]) ->
	{ok, MQ} = psq(mimes_by_data_format_id, [DFId]),
	populate_mimes(MQ, DFId),
	populate_data_formats(DFs);
populate_data_formats([]) -> ok.

populate_mimes([[Mime]|Rows], DataFormatId) ->
	Id=mydlp_mnesia:get_unique_id(mime_type),
	M = #mime_type{id=Id, mime=Mime, data_format_id=DataFormatId},
	mydlp_mnesia:write(M),
	populate_mimes(Rows, DataFormatId);
populate_mimes([], _DataFormatId) -> ok.

%populate_site_desc([[Id, StaticIpI]|Rows]) ->
%	IpAddr = int_to_ip(StaticIpI),
%	S = #site_desc{filter_id=Id, ipaddr=IpAddr},
%	mydlp_mnesia:write(S),
%	populate_site_desc(Rows);
%populate_site_desc([]) -> ok.

%populate_usb_devices([[DeviceId, ActionB]|Rows], FilterId) ->
%	Action = case ActionB of
%		<<"pass">> -> pass;
%		<<"block">> -> block end,
%	U = #usb_device{id=mydlp_mnesia:get_unique_id(usb_device), 
%			filter_id=FilterId, device_id=DeviceId, action=Action},
%	mydlp_mnesia:write(U),
%	populate_usb_devices(Rows, FilterId);
%populate_usb_devices([], _FilterId) -> ok.

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
rule_dtype_to_channel(Else) -> throw({error, unsupported_rule_type, Else}).

pre_push_log(RuleId, Ip, User, Action, Channel) -> 
%	{FilterId, RuleId1} = case RuleId of
%		{dr, CId} -> {CId, 0};
%		-1 = RuleId -> {mydlp_mnesia:get_dfid(), RuleId};	% this shows default action had been enforeced 
							% this should be refined for multisite use
%		RId when is_integer(RId) -> {get_rule_cid(RId), RId} end,
	User1 = case User of
		nil -> null;
		Else -> Else
	end,
	Ip1 = case ip_to_int(Ip) of
		nil -> null;
		Else2 -> Else2
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
		printer -> <<"P">> 
	end,
	{0, RuleId, Ip1, User1, ActionS, ChannelS}.

-endif.

