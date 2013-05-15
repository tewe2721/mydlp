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
	push_log/10,
	push_opr_log/2,
	push_discovery_report/4,
	update_report_status/2,
	update_report_as_finished/1,
	mark_as_finish_all_reports/0,
	requeued/1,
	is_multisite/0,
	get_denied_page/0,
	insert_log_blueprint/5,
	insert_log_data/6,
	insert_log_detail/2,
	insert_log_requeue/1,
	delete_log_requeue/1,
	repopulate_mnesia/0,
	save_fingerprints/2,
	del_fingerprints_with_file_id/1,
	populate_discovery_targets/1,
	get_progress/0,
	is_all_ep_discovery_finished/3,
	is_all_discovery_finished/1,
	insert_file_entry/3,
	insert_dd_file_entry/2,
	get_remote_storage_by_id/1,
	update_document_fingerprinting_status/2,
	does_hash_exist_in_dd/2,
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
	database_r,
	pool_size,
	master_pid,
	pool_pids,
	pool_pids_l,
	pool_pids_r,
	compile_progress
}).

-define(RFS_FINISHED, "rfs_finished").
-define(WEB_FINISHED, "web_finished").

%%%%%%%%%%%%% MyDLP Thrift RPC API

push_log(Time, Channel, RuleId, Action, Ip, User, To, ITypeId, Misc, GroupId) ->
	gen_server:call(?MODULE, {push_log, {Time, Channel, RuleId, Action, Ip, User, To, ITypeId, Misc, GroupId}}, 60000).

push_opr_log(Context, Term) ->
	gen_server:call(?MODULE, {push_opr_log, Context, Term}, 60000).

push_discovery_report(StartDate, GroupId, RuleId, Status) ->
	gen_server:call(?MODULE, {push_discovery_report, StartDate, GroupId, RuleId, Status}, 60000).

update_report_status(GroupId, NewStatus) ->
	gen_server:cast(?MODULE, {update_report_status, GroupId, NewStatus}).

update_report_as_finished(GroupId) ->
	gen_server:cast(?MODULE, {update_report_as_finished, GroupId}).

mark_as_finish_all_reports() ->
	gen_server:cast(?MODULE, mark_as_finish_all_reports).

is_all_ep_discovery_finished(GroupId, Endpoints, Status) ->
	gen_server:call(?MODULE, {is_all_ep_discovery_finished, GroupId, Endpoints, Status}, 60000).

is_all_discovery_finished(GroupId) ->
	gen_server:call(?MODULE, {is_all_discovery_finished, GroupId}, 60000).

insert_log_requeue(LogId) -> 
	gen_server:cast(?MODULE, {insert_log_requeue, LogId}).

delete_log_requeue(LogId) -> 
	gen_server:cast(?MODULE, {delete_log_requeue, LogId}).

insert_log_blueprint(LogId, Filename, MimeType, Size, Hash) -> 
	gen_server:cast(?MODULE, {insert_log_blueprint, LogId, Filename, MimeType, Size, Hash}).

insert_log_data(LogId, Filename, MimeType, Size, Hash, Path) -> 
	gen_server:cast(?MODULE, {insert_log_data, LogId, Filename, MimeType, Size, Hash, Path}).

insert_log_detail(LogId, MatchingDetails) ->
	gen_server:cast(?MODULE, {insert_log_detail, LogId, MatchingDetails}).

save_fingerprints(DocumentId, FingerprintList) -> 
	gen_server:call(?MODULE, {save_fingerprints, DocumentId, FingerprintList}, 60000).

del_fingerprints_with_file_id(FileId) ->
	gen_server:cast(?MODULE, {del_fingerprints_with_file_id, FileId}).

populate_discovery_targets(RuleId) ->
	gen_server:call(?MODULE, {populate_discovery_targets, RuleId}, 150000).

requeued(LogId) -> 
	gen_server:cast(?MODULE, {requeued, LogId}).

repopulate_mnesia() ->
	gen_server:cast(?MODULE, repopulate_mnesia).

compile_customer() -> compile_customer(mydlp_mnesia:get_dfid()).

compile_customer(FilterId) when is_integer(FilterId) ->
	gen_server:cast(?MODULE, {compile_customer, FilterId}).

does_hash_exist_in_dd(MD5Hex, DDId) when is_list(MD5Hex) -> 
	does_hash_exist_in_dd(list_to_binary(string:to_lower(MD5Hex)), DDId);
does_hash_exist_in_dd(MD5Hex, DDId) when is_binary(MD5Hex) ->
	gen_server:call(?MODULE, {does_hash_exist_in_dd, MD5Hex, DDId}, 15000).

%is_multisite() -> gen_server:call(?MODULE, is_multisite).
is_multisite() -> false.

get_denied_page() -> gen_server:call(?MODULE, get_denied_page).

set_progress(Progress) -> gen_server:cast(?MODULE, {set_progress, Progress}).

get_progress() -> gen_server:call(?MODULE, get_progress).

insert_file_entry(Filename, Md5Hash, Date) -> 
	DId = gen_server:call(?MODULE, insert_document),
	gen_server:call(?MODULE, {insert_file_entry, DId, Filename, Md5Hash, Date}).

insert_dd_file_entry(FileEntryId, DDId) -> gen_server:cast(?MODULE, {insert_dd_file_entry,FileEntryId, DDId}).

get_remote_storage_by_id(RSId) -> gen_server:call(?MODULE, {get_remote_storage_by_id, RSId}).

update_document_fingerprinting_status(DDIds, Status) -> gen_server:cast(?MODULE, {update_document_fingerprinting_status, DDIds, Status}).

%%%%%%%%%%%%%% gen_server handles

%handle_call(is_multisite, _From, State) ->
%	{ok, ATQ} = psq(app_type),
%	Reply = case ATQ of
%		[] -> false;
%		[[0]] -> false;
%		[[1]] -> true end,
%        {reply, Reply, State};

handle_call(get_progress, _From, #state{compile_progress=Progress} = State) ->
        {reply, Progress, State};

handle_call(get_denied_page, _From, State) ->
	% Probably will create problems in multisite use.
	{ok, DPQ} = psq(denied_page),
	Reply = case DPQ of
		[[DeniedPage]] when is_binary(DeniedPage) -> DeniedPage;
		_Else -> not_found end,
        {reply, Reply, State};

handle_call({push_log, {Time, Channel, RuleId, Action, Ip, User, To, ITypeId, Misc, GroupId}}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			{_FilterId, RuleId1, Ip1, User1, To1, ActionS, ChannelS, Misc1, GroupId1, Visible} = 
				pre_push_log(RuleId, Ip, User, To, Action, Channel, Misc, GroupId),
			{atomic, ILId} = ltransaction(fun() ->
					psqt(insert_incident, 
						[Time, ChannelS, RuleId1, Ip1, User1, To1, ITypeId, ActionS, Misc1, GroupId1, Visible]),
					last_insert_id_t() end, 30000),
			Reply = ILId,	
                        Worker ! {async_reply, Reply, From}
		end, 30000),
	{noreply, State};

handle_call({push_opr_log, Context, {opr_log, #opr_log{time=Time, channel=Channel, rule_id=RuleId, message_key=MessageKey, group_id=GroupId}}}, From , State) ->
	Worker = self(),
	?ASYNC(fun() ->
			{Time1, _ChannelS, RuleId1, MessageKey1, GroupId1, Visible, Severity} = pre_push_opr_log(Time, Channel, RuleId, MessageKey, GroupId),
			{atomic, ILId} = ltransaction(fun() ->
					psqt(insert_opr_log_disc, 
						[Time1, Context, RuleId1, GroupId1, MessageKey1, Visible, Severity]),
					last_insert_id_t() end, 30000),
			Reply = ILId,	
                        Worker ! {async_reply, Reply, From}
		end, 30000),
	{noreply, State};

handle_call({push_opr_log, Context, {ep_opr_log, #opr_log{time=Time, channel=Channel, rule_id=RuleId, message_key=MessageKey, group_id=GroupId, ip_address=IpAddress}}}, From , State) ->
	Worker = self(),
	?ASYNC(fun() ->
			{Time1, _ChannelS, RuleId1, MessageKey1, GroupId1, Visible, Severity} = pre_push_opr_log(Time, Channel, RuleId, MessageKey, GroupId),
			{I1, I2, I3, I4} = IpAddress,
			IpAddress1 = integer_to_list(I1)++"."++integer_to_list(I2)++"."++integer_to_list(I3)++"."++integer_to_list(I4),
			{ok, [[Alias]]} = lpsq(get_alias_with_ip, [IpAddress1], 5000),
			{ok, [[Source]]} = psq(get_id_with_endpoint_alias, [Alias]),
			{atomic, ILId} = ltransaction(fun() ->
					psqt(insert_opr_log_ep_disc, 
						[Time1, Context, RuleId1, GroupId1, MessageKey1, Visible, Severity, Source]),
					last_insert_id_t() end, 30000),
			Reply = ILId,	
                        Worker ! {async_reply, Reply, From}
		end, 30000),
	{noreply, State};

handle_call({push_opr_log, Context, {key, MessageKey}}, From, State) ->
	Worker = self(),
	Time=erlang:universaltime(),
	?ASYNC(fun() ->
			{atomic, ILId} = ltransaction(fun() ->
					psqt(insert_opr_log, 
						[Time, Context, MessageKey, 1, 0]), % 1 for visible column, 0 for severity
					last_insert_id_t() end, 30000),
			Reply = ILId,	
                        Worker ! {async_reply, Reply, From}
		end, 30000),
	{noreply, State};

handle_call({push_discovery_report, StartDate, GroupId, RuleId, Status}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			{atomic, ILId} = rtransaction(fun() ->
					psqt(insert_discovery_report, 
						[StartDate, GroupId, RuleId, Status]), 
					last_insert_id_t() end, 30000),
			Reply = ILId,	
                        Worker ! {async_reply, Reply, From}
		end, 30000),
	{noreply, State};

handle_call({is_all_ep_discovery_finished, GroupId, Endpoints, Status}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			Reply = lists:all(fun(E) -> is_ep_discovery_finished(GroupId, E, Status) end, Endpoints),
                        Worker ! {async_reply, Reply, From}
		end, 30000),
	{noreply, State};

handle_call({is_all_discovery_finished, GroupId}, From, State) ->
	Worker = self(),
	case lpsq(get_opr_with_group_id_and_status, [GroupId, ?RFS_FINISHED], 5000) of
		{ok, [[_]|_]} -> 
			case lpsq(get_opr_with_group_id_and_status, [GroupId, ?WEB_FINISHED], 5000) of
				{ok, [[_]|_]} -> Worker ! {async_reply, true, From};
				_ -> Worker ! {async_reply, false, From} end;
		_ -> Worker ! {async_reply, false, From}
	end,
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

handle_call({populate_discovery_targets, RuleId}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
		{ok, Aliasses} =  psq(get_endpoint_alias),
		case Aliasses of
			[] -> ok;
			_ ->
				IpsAndNames = get_identities(Aliasses),
				lists:map(fun(I) -> mydlp_mnesia:update_ep_schedules(I, RuleId) end, IpsAndNames) end,
		Worker ! {async_reply, ok, From}
	end, 149000),
	{noreply, State};

handle_call({get_remote_storage_by_id, RSId}, _From, State) ->
	case psq(remote_sshfs_dir, [RSId]) of
		{ok, [R|_]} -> {reply, {sshfs, R}, State};
		_ ->
	case psq(remote_ftpfs_dir, [RSId]) of
		{ok, [R1|_]} -> {reply, {ftpfs, R1}, State};
		_ ->
	case psq(remote_nfs_dir, [RSId]) of
		{ok, [R2|_]} -> {reply, {nfs, R2}, State};
		_ ->
	case psq(remote_windows_dir, [RSId]) of
		{ok, [R4|_]} -> {reply, {windows, R4}, State};
		_ ->{reply, none, State} end end end end;

handle_call(insert_document, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			{atomic, ILId} = transaction(fun() ->
					psqt(insert_document), 
					last_insert_id_t() end, 30000),
			Reply = ILId,	
                        Worker ! {async_reply, Reply, From}
		end, 30000),
	{noreply, State};

handle_call({insert_file_entry, Id, Filename0, Md5Hash, Date}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			{Filename} = pre_insert_log(Filename0),
			transaction(fun() ->
				psqt(insert_file_entry, 
					[Id, Filename, Md5Hash, Date])
				end, 30000),
                        Worker ! {async_reply, Id, From}
		end, 30000),
	{noreply, State};

handle_call({does_hash_exist_in_dd, MD5Hex, DDId}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
			Reply = case psq(file_entry_by_hash_and_dd, [MD5Hex, DDId]) of
				[] -> false;
				[_|_] -> true end,
                        Worker ! {async_reply, Reply, From}
		end, 14500),
	{noreply, State};

handle_call(stop, _From,  State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({del_fingerprints_with_file_id, FileId}, State) ->
	?ASYNC(fun() ->
			transaction(fun() -> psqt(del_fingerprints, [FileId]) 
			end, 60000)
		end, 60000),
        {noreply, State};

handle_cast({update_report_status, GroupId, NewStatus}, State) ->
	?ASYNC0(fun() ->
		rpsq(update_report_status, [NewStatus, GroupId], 60000)
	end),
	{noreply, State};

handle_cast({update_report_as_finished, GroupId}, State) ->
	Time = erlang:universaltime(),
	?ASYNC0(fun() ->
		rpsq(update_report_as_finished, ["stopped", Time, GroupId], 60000)
	end),
	{noreply, State};

handle_cast(mark_as_finish_all_reports, State) ->
	Time = erlang:universaltime(),
	?ASYNC0(fun() ->
		rpsq(mark_as_finish_all_reports, [Time], 60000)
	end),
	{noreply, State};

handle_cast({update_document_fingerprinting_status, DDIds, Status}, State) ->
	Value = case Status of
			true -> 1;
			false -> 0 end,
	?ASYNC0(fun() ->
		lists:foreach(fun(I) -> psq(update_document_fingerprinting_status, [Value, I], 60000) end, DDIds)
	end),
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

handle_cast({insert_log_data, LogId, Filename0, MimeType, Size, Hash, Path}, State) ->
	% Probably will create problems in multisite use.
	?ASYNC(fun() ->
		{Filename} = pre_insert_log(Filename0),
		{atomic, DataId} = ltransaction(fun() ->
			Query = case Hash of
				undefined -> {ok, [] };
				_Else -> psqt(incident_data_by_hash, [Hash]) end,
			case Query of
				{ok, [] } ->	psqt(insert_incident_data, [MimeType, Size, Hash, Path]),
						last_insert_id_t();
				{ok, [[Id]|_]} -> Id end
			end, 60000),
		lpsq(insert_incident_file_data, [LogId, Filename, DataId], 30000)
	end, 100000),
	{noreply, State};

handle_cast({insert_log_detail, LogId, MatchingDetails}, State) ->
	?ASYNC(fun() ->
			ltransaction(fun() ->
				lists:foreach(fun(#matching_detail{pattern=Pattern, matcher_func=MatcherFunc}) -> 
						PatternB = case Pattern of
							P when is_binary(P) -> P;
							P when is_list(P) ->  unicode:characters_to_binary(P) end,
						psqt(insert_log_detail, [LogId, PatternB, MatcherFunc]) 
					end, MatchingDetails) 
			end, 60000)
		end, 60000),
        {noreply, State};

handle_cast({insert_log_blueprint, LogId, Filename0, MimeType, Size, Hash}, State) ->
	% Probably will create problems in multisite use.
	?ASYNC(fun() ->
		{Filename} = pre_insert_log(Filename0),
		{atomic, BlueprintId} = ltransaction(fun() ->
			Query = case Hash of
				undefined -> {ok, [] };
				_Else -> psqt(incident_blueprint_by_hash, [Hash]) end,
			case Query of
				{ok, [] } ->	psqt(insert_incident_blueprint, [MimeType, Size, Hash]),
						last_insert_id_t();
				{ok, [[Id]|_]} -> Id end
			end, 60000),
		lpsq(insert_incident_file_bp, [LogId, Filename, BlueprintId], 30000)
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

handle_cast({compile_customer, FilterId}, State) ->
	?ASYNC(fun() ->
		try	set_progress(compile),
			mydlp_mnesia:remove_site(FilterId),
			populate_site(FilterId),
			ok
		after	set_progress(done)
		end
	end, 900000),
	{noreply, State};

handle_cast({set_progress, Progress}, State) ->
	{noreply, State#state{compile_progress=Progress}};

handle_cast({insert_dd_file_entry, FileEntryId, DDId}, State) ->
	?ASYNC(fun() ->
			transaction(fun() -> psqt(insert_dd_file_entry, [FileEntryId, DDId]) end, 60000) 
		end, 60000),	
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info({'DOWN', _, _, MPid , _}, #state{master_pid=MPid} = State) ->
	?ERROR_LOG("Master connection to MySQL is dead, restarting MySQL module.", []),
	case init([]) of
		{ok, NewState} -> {noreply, NewState};
		Err ->	?ERROR_LOG("Error occurred when trying to restart MySQL module. Error: "?S, [Err]),
			{stop, normalStop, State} end;

handle_info({'DOWN', _, _, Pid , _} = Msg, #state{host=Host,
		user=User, password=Password, database=DB, database_l=LDB, database_r=RDB,
		pool_pids=PoolPids, pool_pids_l=PoolPidsL, pool_pids_r=PoolPidsR} = State) ->
	PPTuple  = try case lists:member(Pid, PoolPids) of
			true -> PoolPids1 = lists:delete(Pid, PoolPids),
				case mysql:connect(pp, Host, undefined, User, Password, DB, utf8, true) of
					{ok, NewPid} -> 
						erlang:monitor(process, NewPid),
						{[NewPid|PoolPids1], PoolPidsL};
					Err -> {error, Err} end;
			false -> case lists:member(Pid, PoolPidsL) of
			true -> PoolPidsL1 = lists:delete(Pid, PoolPidsL),
				case mysql:connect(pl, Host, undefined, User, Password, LDB, utf8, true) of
					{ok, NewPid} -> 
						erlang:monitor(process, NewPid),
						{PoolPids, [NewPid|PoolPidsL1]};
					Err2 -> {error, Err2} end;
			false -> case lists:member(Pid, PoolPidsR) of
			true -> PoolPidsR1 = lists:delete(Pid, PoolPidsR),
				case mysql:connect(pr, Host, undefined, User, Password, RDB, utf8, true) of
					{ok, NewPid} -> 
						erlang:monitor(process, NewPid),
						{PoolPids, [NewPid|PoolPidsR1]};
					Err2 -> {error, Err2} end;
			false -> orphan end end end
		catch Class:Error -> {error, Class, Error} end,

	case PPTuple of
		orphan -> ?ERROR_LOG("Dead pid is orphan. Ignoring.~nDeadPid: "?S", State: "?S, [Pid, State]),
			{noreply, State};
		{error, C, E} -> ?ERROR_LOG("An error occurred when trying to create a new connection instead of dead one.~nClass: "?S" Error: "?S"~nState: "?S, [C, E,State]);
		{error, E} -> ?ERROR_LOG("An error occurred when trying to create a new connection instead of dead one.~nError: "?S"~nState: "?S, [E,State]) ,
			?ERROR_LOG("Waiting 500ms and retrying to create connection. Msg: "?S, [Msg]),
			timer:sleep(500),
			handle_info(Msg, State);
			%{stop, normalStop, State};
		error -> ?ERROR_LOG("An error occurred when trying to create a new connection instead of dead one.~nState: "?S, [State]) ,
			{stop, normalStop, State};
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
	RDB = ?CFG(mysql_report_database),
	PoolSize = ?CFG(mysql_pool_size),
	
	{ok, MPid} = mysql:start_link(pp, Host, Port, User, Password, DB, fun(_,_,_,_) -> ok end, utf8),
	erlang:monitor(process, MPid), 
	
	PoolReturns = [ mysql:connect(pp, Host, undefined, User, Password, DB, utf8, true) || _I <- lists:seq(1, 2)],
	PPids = [ P || {ok, P} <- PoolReturns ],
	[ erlang:monitor(process, P) || P <- PPids ],

	%{ok, MPid2} = mysql:start_link(pl, Host, Port, User, Password, LDB, fun(_,_,_,_) -> ok end),
	%erlang:monitor(process, MPid2), 
	
	PoolReturns2 = [ mysql:connect(pl, Host, undefined, User, Password, LDB, utf8, true) || _I <- lists:seq(1, PoolSize)],
	PPids2 = [ P || {ok, P} <- PoolReturns2 ],
	[ erlang:monitor(process, P) || P <- PPids2 ],

	PoolReturns3 = [ mysql:connect(pr, Host, undefined, User, Password, RDB, utf8, true) || _I <- lists:seq(1, PoolSize)],
	PPids3 = [ P || {ok, P} <- PoolReturns3 ],
	[ erlang:monitor(process, P) || P <- PPids3 ],

	[ mysql:prepare(Key, Query) || {Key, Query} <- [
		{last_insert_id, <<"SELECT last_insert_id()">>},
		{configs, <<"SELECT configKey,value FROM Config">>},
		{rules, <<"SELECT id,DTYPE,userMessage,action,customAction_id FROM Rule WHERE enabled=1 order by priority desc">>},
		{custom_action_by_id, <<"SELECT c.name, c.typeKey FROM CustomAction AS c WHERE c.id=?">>},
		{custom_action_seclore_by_id, <<"SELECT cs.hotFolderId, cs.activityComment FROM CustomActionDescription AS cd, CustomActionDescriptionSeclore AS cs WHERE cd.coupledCustomAction_id=? AND cd.id=cs.id">>},
		{network_by_rule_id, <<"SELECT n.ipBase,n.ipMask FROM Network AS n, RuleItem AS ri WHERE ri.rule_id=? AND n.id=ri.item_id">>},
		{domain_by_rule_id, <<"SELECT d.destinationString FROM Domain AS d, RuleItem AS ri WHERE ri.rule_id=? AND d.id=ri.item_id AND (ri.ruleColumn IS NULL OR ri.ruleColumn=\"DESTINATION\")">>},
		{directory_by_rule_id, <<"SELECT d.destinationString FROM FileSystemDirectory AS d, RuleItem AS ri WHERE ri.rule_id=? AND d.id=ri.item_id">>},
		{source_domain_by_rule_id, <<"SELECT d.destinationString FROM Domain AS d, RuleItem AS ri WHERE ri.rule_id=? AND d.id=ri.item_id AND ri.ruleColumn=\"SOURCE\"">>},
		{remote_sshfs, <<"SELECT r.address, r.port, r.path, r.username, r.password FROM RemoteStorageSSHFS r, RuleItem AS ri WHERE ri.rule_id=? AND r.id=ri.item_id">>},
		{remote_ftpfs, <<"SELECT r.address, r.path, r.username, r.password FROM RemoteStorageFTPFS r, RuleItem AS ri WHERE ri.rule_id=? AND r.id=ri.item_id">>},
		{remote_windows, <<"SELECT r.uncPath, r.username, r.password FROM RemoteStorageWindowsShare r, RuleItem AS ri WHERE ri.rule_id=? AND r.id=ri.item_id">>},
		{remote_nfs, <<"SELECT r.address, r.path FROM RemoteStorageNFS r, RuleItem AS ri WHERE ri.rule_id=? AND r.id=ri.item_id">>},
		{remote_sshfs_dir, <<"SELECT r.address, r.password, r.path, r.port, r.username FROM RemoteStorageSSHFS r WHERE r.id=?">>},
		{remote_ftpfs_dir, <<"SELECT r.address, r.password, r.path, r.username FROM RemoteStorageFTPFS r WHERE r.id=?">>},
		{remote_windows_dir, <<"SELECT r.uncPath, r.password, r.username FROM RemoteStorageWindowsShare r WHERE r.id=?">>},
		{remote_nfs_dir, <<"SELECT r.address, r.path FROM RemoteStorageNFS r WHERE r.id=?">>},
		{web_servers, <<"SELECT r.proto, r.address, r.port, r.digDepth, r.startPath FROM WebServer r, RuleItem AS ri WHERE ri.rule_id=? AND r.id=ri.item_id">>},
		{app_name_by_rule_id, <<"SELECT a.destinationString FROM ApplicationName AS a, RuleItem AS ri WHERE ri.rule_id=? AND a.id=ri.item_id">>},
		{email_notification_by_rule_id, <<"SELECT a.email FROM AuthUser AS a, NotificationItem AS ni, EmailNotificationItem AS eni, Rule r WHERE ni.rule_id=? AND ni.id=eni.id AND ni.authUser_id=a.id AND r.id=? AND r.notificationEnabled=1">>},
		{daily_schedule_by_rule_id, <<"SELECT s.hour FROM RuleSchedule AS rs, Schedule AS s, DailySchedule AS ds WHERE rs.rule_id=? AND rs.schedule_id=s.id AND s.id=ds.id">>},
		{weekly_schedule_by_rule_id, <<"SELECT s.hour, ws.mon, ws.tue, ws.wed, ws.thu, ws.fri, ws.sat, ws.sun FROM RuleSchedule AS rs, Schedule AS s, WeeklySchedule AS ws WHERE rs.rule_id=? AND rs.schedule_id=s.id AND s.id=ws.id">>},
		{schedule_intervals_by_rule_id, <<"SELECT si.mon_id, si.tue_id, si.wed_id, si.thu_id, si.fri_id, si.sat_id, si.sun_id FROM RuleSchedule AS rs, ScheduleIntervals si WHERE rs.rule_id=? AND rs.scheduleIntervals_id=si.id">>},
		{schedule_day_intervals_by_id, <<"SELECT * FROM ScheduleDayInterval WHERE id=?">>},
		{user_s_by_rule_id, <<"SELECT u.username FROM RuleUserStatic AS u, RuleItem AS ri WHERE ri.rule_id=? AND u.id=ri.item_id AND (ri.ruleColumn IS NULL OR ri.ruleColumn=\"SOURCE\")">>},
		{user_ad_u_by_rule_id, <<"SELECT u.id FROM ADDomainUser u, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=u.id AND (ri.ruleColumn IS NULL OR ri.ruleColumn=\"SOURCE\")">>},
		{user_ad_o_by_rule_id, <<"SELECT u.id FROM ADDomainUser u, ADDomainItem i, ADDomainOU o, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=o.id AND o.id=i.parent_id AND i.id=u.id AND (ri.ruleColumn IS NULL OR ri.ruleColumn=\"SOURCE\")">>},
		{ou_id_by_rule_id, <<"SELECT o.id FROM ADDomainOU o, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=o.id AND (ri.ruleColumn IS NULL OR ri.ruleColumn=\"SOURCE\")">>},
		{user_ad_g_by_rule_id, <<"SELECT u.id FROM ADDomainUser u, ADDomainItem i, ADDomainOU o, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=o.id AND o.id=i.parent_id AND i.id=u.id AND (ri.ruleColumn IS NULL OR ri.ruleColumn=\"SOURCE\")">>},

		{ou_ad_by_ou_id, <<"SELECT u.id FROM ADDomainOU u, ADDomainItem i, ADDomainOU o WHERE o.id=? AND o.id=i.parent_id AND i.id=u.id">>},
		{user_ad_by_ou_id, <<"SELECT u.id FROM ADDomainUser u, ADDomainItem i, ADDomainOU o WHERE o.id=? AND o.id=i.parent_id AND i.id=u.id">>},

		{dest_user_s_by_rule_id, <<"SELECT u.username FROM RuleUserStatic AS u, RuleItem AS ri WHERE ri.rule_id=? AND u.id=ri.item_id AND ri.ruleColumn=\"DESTINATION\"">>},
		{dest_user_ad_u_by_rule_id, <<"SELECT u.id FROM ADDomainUser u, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=u.id AND ri.ruleColumn=\"DESTINATION\"">>},
		{dest_user_ad_o_by_rule_id, <<"SELECT u.id FROM ADDomainUser u, ADDomainItem i, ADDomainOU o, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=o.id AND o.id=i.parent_id AND i.id=u.id AND ri.ruleColumn=\"DESTINATION\"">>},
		{dest_ou_id_by_rule_id, <<"SELECT o.id FROM ADDomainOU o, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=o.id AND ri.ruleColumn=\"DESTINATION\"">>},
		{dest_user_ad_g_by_rule_id, <<"SELECT u.id FROM ADDomainUser u, ADDomainItem i, ADDomainOU o, RuleUserAD AS ru, RuleItem AS ri WHERE ri.rule_id=? AND ru.id=ri.item_id AND ru.domainItem_id=o.id AND o.id=i.parent_id AND i.id=u.id AND ri.ruleColumn=\"DESTINATION\"">>},
		{domain_item_parent_by_id, <<"SELECT i.parent_id FROM ADDomainItem AS i WHERE i.id=?">>},
		{domain_root_by_id, <<"SELECT r.id, r.domain_id FROM ADDomainRoot AS r WHERE r.id=?">>},
		{domain_names_by_id, <<"SELECT d.domainName, d.netbiosName FROM ADDomain AS d WHERE d.id=?">>},
		{domain_aliases_by_id, <<"SELECT a.domainAlias FROM ADDomainAlias AS a, ADDomain_ADDomainAlias AS da WHERE da.ADDomain_id=? AND a.id=da.aliases_id">>},
		{domain_user_sam_by_id, <<"SELECT u.sAMAccountName FROM ADDomainUser AS u WHERE u.id=?">>},
		{domain_user_aliases_by_id, <<"SELECT a.userAlias FROM ADDomainUserAlias AS a, ADDomainUser_ADDomainUserAlias AS ua WHERE ua.ADDomainUser_id=? AND a.id=ua.aliases_id">>},
		{itype_by_rule_id, <<"SELECT t.id, CASE WHEN d.distanceEnabled=1 THEN 1 ELSE 0 END, d.distance FROM InformationType AS t, RuleItem AS ri, InformationDescription AS d WHERE ri.rule_id=? AND t.id=ri.item_id AND t.InformationDescription_id=d.id">>},
		{itype_grouped_by_rule_id, <<"SELECT t.id, CASE WHEN d.distanceEnabled=1 THEN 1 ELSE 0 END, d.distance FROM InformationType AS t, InventoryItem AS ii, RuleItemGroup AS rig, InformationDescription AS d WHERE rig.rule_id=? AND ii.group_id=rig.group_id AND t.id=ii.item_id AND t.InformationDescription_id=d.id">>},
		{data_formats_by_itype_id, <<"SELECT df.dataFormats_id FROM InformationType_DataFormat AS df WHERE df.InformationType_id=?">>},
		{ifeature_by_itype_id, <<"SELECT f.threshold,f.matcher_id FROM InformationFeature AS f, InformationDescription_InformationFeature df, InformationType t WHERE t.id=? AND t.informationDescription_id=df.InformationDescription_id AND df.features_id=f.id">>},
		{match_by_id, <<"SELECT m.id,m.functionName FROM Matcher AS m WHERE m.id=?">>},
		{regex_by_matcher_id, <<"SELECT re.regex FROM MatcherArgument AS ma, RegularExpression AS re WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=re.id">>},
		{strarg_by_matcher_id, <<"SELECT sa.argument FROM MatcherArgument AS ma, StringArgument AS sa WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=sa.id">>},
		{kg_bundled_by_matcher_id, <<"SELECT bkg.filename FROM MatcherArgument AS ma, NonCascadingArgument AS nca, BundledKeywordGroup AS bkg WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=nca.id AND nca.argument_id=bkg.id">>},
		{kg_regexes_by_matcher_id, <<"SELECT re.regex FROM MatcherArgument AS ma, NonCascadingArgument AS nca, RegularExpressionGroup_RegularExpressionGroupEntry AS gre, RegularExpressionGroupEntry AS re WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=nca.id AND nca.argument_id=gre.RegularExpressionGroup_id AND gre.entries_id=re.id">>},
		{kg_rdbms_regexes_by_matcher_id, <<"SELECT rev.string FROM MatcherArgument AS ma, NonCascadingArgument AS nca, RegularExpressionGroup AS reg, RDBMSEnumeratedValue AS rev WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=nca.id AND nca.argument_id=reg.id AND reg.rdbmsInformationTarget_id=rev.informationTarget_id">>},
		{dd_by_matcher_id, <<"SELECT dd.id FROM MatcherArgument AS ma, NonCascadingArgument AS nca, DocumentDatabase AS dd WHERE ma.coupledMatcher_id=? AND ma.coupledArgument_id=nca.id AND nca.argument_id=dd.id">>},
		{filehash_by_dd_id, <<"SELECT ddfe.md5Hash FROM DocumentDatabase_DocumentDatabaseFileEntry AS dd, DocumentDatabaseFileEntry AS ddfe WHERE dd.DocumentDatabase_id=? AND dd.fileEntries_id=ddfe.id">>},
		{file_entry_by_hash_and_dd, <<"SELECT ddfe.id FROM DocumentDatabase_DocumentDatabaseFileEntry AS dd, DocumentDatabaseFileEntry AS ddfe WHERE ddfe.md5Hash=? AND dd.DocumentDatabase_id=? AND dd.fileEntries_id=ddfe.id">>},
		{filefingerprint_by_dd_id, <<"SELECT df.fingerprint FROM DocumentDatabase_DocumentDatabaseFileEntry AS dd, DocumentDatabaseFileEntry AS ddfe, DocumentFingerprint AS df WHERE dd.DocumentDatabase_id=? AND dd.fileEntries_id=ddfe.id AND df.document_id=ddfe.id">>},
		{rdbmsfingerprint_by_dd_id, <<"SELECT df.fingerprint FROM DocumentDatabase_DocumentDatabaseRDBMSEntry AS dd, DocumentDatabaseRDBMSEntry AS ddre, DocumentFingerprint AS df WHERE dd.DocumentDatabase_id=? AND dd.rdbmsEntries_id=ddre.id AND df.document_id=ddre.id">>},
		%{user_by_rule_id, <<"SELECT eu.id, eu.username FROM sh_ad_entry_user AS eu, sh_ad_cross AS c, sh_ad_rule_cross AS rc WHERE rc.parent_rule_id=? AND rc.group_id=c.group_id AND c.entry_id=eu.entry_id">>},
		{mimes_by_data_format_id, <<"SELECT m.mimeType FROM MIMEType AS m, DataFormat_MIMEType dm WHERE dm.DataFormat_id=? and dm.mimeTypes_id=m.id">>},
		{usb_devices, <<"SELECT deviceId, action FROM USBDevice">>},
		{insert_fingerprint, <<"INSERT INTO DocumentFingerprint (id, fingerprint, document_id) VALUES (NULL, ?, ?)">>},
		{del_fingerprints, <<"DELETE FROM DocumentFingerprint where document_id=?">>},
		%{customer_by_id, <<"SELECT id,static_ip FROM sh_customer WHERE id=?">>},
		{insert_incident, <<"INSERT INTO IncidentLog (id, date, channel, ruleId, sourceIp, sourceUser, destination, informationTypeId, action, matcherMessage, groupId, visible) VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>},
		{insert_incident_file_data, <<"INSERT INTO IncidentLogFile (id, incidentLog_id, filename, content_id) VALUES (NULL, ?, ?, ?)">>},
		{insert_incident_file_bp, <<"INSERT INTO IncidentLogFile (id, incidentLog_id, filename, blueprint_id) VALUES (NULL, ?, ?, ?)">>},
		{insert_log_detail, <<"INSERT INTO MatchingDetail (id, incidentLog_id, matchingData, matcherFunc) VALUES (NULL, ?, ?, ?)">>},
%		{insert_archive, <<"INSERT INTO log_archive (id, customer_id, rule_id, protocol, src_ip, src_user, destination, log_archive_file_id) VALUES (NULL, ?, ?, ?, ?, ?, ?, ?)">>},
%		{new_archive_file_entry, <<"INSERT INTO log_archive_file (id) VALUES (NULL)">>},
%		{update_archive_file, <<"UPDATE log_archive_file SET filename=?, log_archive_data_id=? WHERE id = ?">>},
		{incident_data_by_hash, <<"SELECT id FROM IncidentLogFileContent WHERE md5Hash = ?">>},
		{incident_blueprint_by_hash, <<"SELECT id FROM IncidentLogFileBlueprint WHERE md5Hash = ?">>},
		{insert_incident_data, <<"INSERT INTO IncidentLogFileContent (id, mimeType, size, md5hash, localPath) VALUES (NULL, ?, ?, ?, ?)">>},
		{insert_incident_blueprint, <<"INSERT INTO IncidentLogFileBlueprint (id, mimeType, size, md5hash) VALUES (NULL, ?, ?, ?)">>},
		{insert_incident_requeue, <<"INSERT INTO IncidentLogRequeueStatus (id, incidentLog_id, isRequeued) VALUES (NULL, ?, false)">>},
		{delete_incident_requeue, <<"DELETE FROM IncidentLogRequeueStatus WHERE incidentLog_id=?">>},
		{update_requeue_status, <<"UPDATE IncidentLogRequeueStatus SET isRequeued=TRUE, date=now() WHERE incidentLog_id=?">>},
		{denied_page, <<"SELECT c.value FROM Config AS c WHERE c.configKey=\"denied_page_html\"">>},
		{insert_opr_log_disc, <<"INSERT INTO OperationLog (id, date, context, ruleId, groupId, message, messageKey, visible, severity, source) VALUES (NULL, ?, ?, ?, ?, NULL, ?, ?, ?, NULL)">>},
		{insert_opr_log_ep_disc, <<"INSERT INTO OperationLog (id, date, context, ruleId, groupId, message, messageKey, visible, severity, source) VALUES (NULL, ?, ?, ?, ?, NULL, ?, ?, ?, ?)">>},
		{insert_opr_log, <<"INSERT INTO OperationLog (id, date, context, ruleId, groupId, message, messageKey, visible, severity, source) VALUES (NULL, ?, ?, NULL, NULL, NULL, ?, ?, ?, NULL)">>},
		{insert_discovery_report, <<"INSERT INTO DiscoveryReport (id, startDate, finishDate, groupId, ruleId, status) VALUES (NULL, ?, NULL, ?, ?, ?)">>},
		{update_report_status, <<"UPDATE DiscoveryReport SET status=? WHERE groupId = ?">>},
		{update_report_as_finished, <<"UPDATE DiscoveryReport SET status=?, finishDate=? WHERE groupId = ?">>},
		{mark_as_finish_all_reports, <<"UPDATE DiscoveryReport SET status=\"stopped\", finishDate=? WHERE status != \"finished\" AND status != \"stopped\"">>},
		{update_document_fingerprinting_status, <<"UPDATE DocumentDatabase SET currentlyFingerprinting=? WHERE id=?">>},
		{get_endpoint_alias, <<"SELECT endpointAlias, endpointId FROM Endpoint">>},
		{get_id_with_endpoint_alias, <<"SELECT endpointId FROM Endpoint where endpointAlias=?">>},
		{get_ip_and_username, <<"SELECT ipAddress, username FROM EndpointStatus where endpointAlias=?">>},
		{get_alias_with_ip, <<"SELECT endpointAlias FROM EndpointStatus where ipAddress=?">>},
		{get_opr_with_group_id_and_ep, <<"SELECT o.id FROM OperationLog AS o WHERE groupId=? AND source=? AND messageKey=?">>},
		{get_opr_with_group_id_and_status, <<"SELECT o.id FROM OperationLog AS o WHERE groupId=? AND messageKey=?">>},
		{get_remote_document_databases, <<"SELECT * FROM DocumentDatabase_DocumentDatabaseRemoteStorage">>},
		{get_remote_sshfs, <<"SELECT r.id, r.address, r.password, r.path, r.port, r.username FROM RemoteStorageSSHFS AS r, DocumentDatabaseRemoteStorage AS d WHERE d.id=? and d.remoteStorage_id=r.id">>},
		{get_remote_ftpfs, <<"SELECT r.id, r.address, r.password, r.path, r.username FROM RemoteStorageFTPFS AS r, DocumentDatabaseRemoteStorage AS d WHERE d.id=? and d.remoteStorage_id=r.id">>},
		{get_remote_nfs, <<"SELECT r.id, r.address, r.path FROM RemoteStorageNFS AS r, DocumentDatabaseRemoteStorage as d WHERE d.id=? and d.remoteStorage_id=r.id">>},
		{get_remote_windows, <<"SELECT r.id, r.uncPath, r.password, r.username FROM RemoteStorageWindowsShare AS r, DocumentDatabaseRemoteStorage AS d WHERE d.id=? and d.remoteStorage_id=r.id">>},
		{get_exclude_files, <<"SELECT e.excludeFileName FROM DocumentDatabaseRemoteStorage AS d, DocumentDatabaseExcludeFile AS e WHERE d.id=? AND d.id=e.documentDatabaseRemoteStorage_id">>},
		{insert_file_entry, <<"INSERT INTO DocumentDatabaseFileEntry (id, filename, md5Hash, createdDate) VALUES (?, ?, ?, ?)">>},
		{insert_document, <<"INSERT INTO Document (id) VALUES (NULL)">>},
		{insert_dd_file_entry, <<"INSERT INTO DocumentDatabase_DocumentDatabaseFileEntry (fileEntries_id, DocumentDatabase_id) VALUES(?, ?)">>}

	]],

	{ok, #state{host=Host, port=Port, 
			user=User, password=Password, 
			database=DB, database_l=LDB, database_r=RDB, pool_size=PoolSize, 
			master_pid=MPid, pool_pids=PPids, pool_pids_l=PPids2, pool_pids_r=PPids3,
			compile_progress=done}}.

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

rpsq(PreparedKey, Params, Timeout) ->
	case mysql:execute(pr, PreparedKey, Params, Timeout) of
		{data,{mysql_result,_,Result,_,_}} -> {ok, Result};
		{updated,{mysql_result, _,_,RowCount,_}} -> {updated, RowCount};
		Else -> throw({error, Else})
	end.

%transaction(Fun) -> transaction(Fun, 5000).

transaction(Fun, Timeout) -> mysql:transaction(pp, Fun, Timeout).

%ltransaction(Fun) -> ltransaction(Fun, 5000).

ltransaction(Fun, Timeout) -> mysql:transaction(pl, Fun, Timeout).

rtransaction(Fun, Timeout) -> mysql:transaction(pr, Fun, Timeout).

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
	

get_identities(Rows) -> get_identities(Rows, []).

get_identities([[Alias, EndpointId]|Rows], Acc) ->
	case lpsq(get_ip_and_username, [Alias], 30000) of
		{ok, [[Ip,Username]]} -> get_identities(Rows, [{EndpointId, Ip, Username}|Acc]);
		_ -> ?ERROR_LOG("Unknown Endpoint Alias: ["?S"]", [Alias]), get_identities(Rows, Acc) end;
get_identities([], Acc) -> Acc.

populate_site(FilterId) ->
	set_progress(compile),
	%TODO: remove notification queue and sent waiting notifications
	init_mydlp_mnesia_write(),
	%TODO: refine this
	%{ok, FQ} = psq(filters_by_cid, [FilterId]),
	populate_filters([[FilterId, <<"pass">> ]], FilterId),

	% This will create problems in multi-site
	{ok, CQ} = psq(configs),
	populate_configs(CQ, FilterId),

	populate_remote_document_database(),

	%TODO: should add for multi-site
	%{ok, SQ} = psq(customer_by_id, [FilterId]),
	%populate_site_desc(SQ),

	{ok, UDQ} = psq(usb_devices),
	populate_usb_devices(UDQ, FilterId),
	mydlp_mnesia:write(get_mydlp_mnesia_write()),
	erase_mydlp_mnesia_write(),

	set_progress(post_compile),

	%%% post actions
	mydlp_mnesia:post_start(mnesia),
	mydlp_tc:load(),

	init_mydlp_mnesia_write(),
	populate_mc_modules(),
	mydlp_mnesia:write(get_mydlp_mnesia_write()),
	erase_mydlp_mnesia_write(),

	mydlp_mnesia:post_start(mc),

	set_progress(done),
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

populate_rules([[Id, DTYPE, UserMessage, ActionS, CustomActionId]|Rows], FilterId) ->
	Action = case rule_action_to_atom(ActionS) of
		custom -> {custom, populate_custom_action_detail(CustomActionId)};
		Else -> Else end,
	Channel = rule_dtype_to_channel(DTYPE),
	validate_action_for_channel(Channel, Action),
	populate_rule(Id, Channel, UserMessage, Action, FilterId),
	populate_rules(Rows, FilterId);
populate_rules([], _FilterId) -> ok.

populate_rule(OrigId, Channel, UserMessage, Action, FilterId) ->
	RuleId = mydlp_mnesia:get_unique_id(rule),

	populate_user_message(Channel, Action, OrigId, UserMessage),

	{ok, IQ} = psq(network_by_rule_id, [OrigId]),
	populate_iprs(IQ, RuleId),

	populate_rule_users(OrigId, RuleId),

	populate_rule_dest_users(OrigId, RuleId),

	{ok, ITQ} = psq(itype_by_rule_id, [OrigId]),
	populate_itypes(ITQ, RuleId),

	{ok, ITQ2} = psq(itype_grouped_by_rule_id, [OrigId]),
	populate_itypes(ITQ2, RuleId),

	{ok, DQ} = psq(domain_by_rule_id, [OrigId]),
	{ok, DIRQ} = psq(directory_by_rule_id, [OrigId]),
	{ok, AppName} = psq(app_name_by_rule_id, [OrigId]),
	populate_destinations(DQ++DIRQ++AppName, RuleId, Channel),
	
	{ok, ENT} = psq(email_notification_by_rule_id, [OrigId, OrigId]),
	populate_notifications(ENT, RuleId, email),

	{ok, SDN} = psq(source_domain_by_rule_id, [OrigId]),
	populate_source_domains(SDN, RuleId),

	%% TODO create below according to rule type
	populate_remote_storages(OrigId, RuleId),

	populate_discovery_schedule(OrigId, RuleId),

	populate_ep_schedules_entries(RuleId, OrigId),

	{ok, RWS} = psq(web_servers, [OrigId]),
	populate_web_servers(RWS, RuleId),

	R = #rule{id=RuleId, orig_id=OrigId, channel=Channel, action=Action, filter_id=FilterId},
	mydlp_mnesia_write(R).

populate_user_message(web, block, RuleOrigId, Message) -> populate_user_message1(RuleOrigId, Message);
populate_user_message(web, quarantine, RuleOrigId, Message) -> populate_user_message1(RuleOrigId, Message);
populate_user_message(mail, block, RuleOrigId, Message) -> populate_user_message1(RuleOrigId, Message);
populate_user_message(mail, quarantine, RuleOrigId, Message) -> populate_user_message1(RuleOrigId, Message);
populate_user_message(_Channel, _Action, _RuleOrigId, _Message) -> ok.

populate_user_message1(_RuleOrigId, undefined) -> ok;
populate_user_message1(_RuleOrigId, <<>>) -> ok;
populate_user_message1(RuleOrigId, <<" ", Rest/binary>>) -> populate_user_message1(RuleOrigId, Rest);
populate_user_message1(RuleOrigId, <<"\n", Rest/binary>>) -> populate_user_message1(RuleOrigId, Rest);
populate_user_message1(RuleOrigId, <<"\r", Rest/binary>>) -> populate_user_message1(RuleOrigId, Rest);
populate_user_message1(RuleOrigId, <<"\t", Rest/binary>>) -> populate_user_message1(RuleOrigId, Rest);
populate_user_message1(RuleOrigId, UserMessage) ->
	U = #user_message{rule_orig_id=RuleOrigId, message=UserMessage},
	mydlp_mnesia_write(U).

populate_custom_action_detail(CustomActionId) ->
        {ok, [[Name, TypeKey]]} = psq(custom_action_by_id, [CustomActionId]),
	Type = custom_action_type_to_atom(TypeKey),
	{PrimAction, CustomActionParam} = case Type of
		seclore -> {pass, populate_custom_action_seclore_param(CustomActionId)}
	end,
	{Type, PrimAction, Name, CustomActionParam}.

populate_custom_action_seclore_param(CustomActionId) ->
        {ok, [[HotFolderId, ActivityComment]]} = psq(custom_action_seclore_by_id, [CustomActionId]),
	{HotFolderId, ActivityComment}.

populate_iprs([[Base, Subnet]| Rows], RuleId) ->
	B1 = int_to_ip(Base),
	S1 = int_to_ip(Subnet),
	Id = mydlp_mnesia:get_unique_id(ipr),
	I = #ipr{id=Id, rule_id=RuleId, ipbase=B1, ipmask=S1},
	mydlp_mnesia_write(I),
	populate_iprs(Rows, RuleId);
populate_iprs([], _RuleId) -> ok.

populate_destinations([[Destination]|Rows], RuleId, Channel=screenshot) ->
	Id = mydlp_mnesia:get_unique_id(dest),
	I = #dest{id=Id, rule_id=RuleId, destination=Destination},
	mydlp_mnesia_write(I),
	populate_destinations(Rows, RuleId, Channel);
populate_destinations([[Destination]|Rows], RuleId, Channel) ->
	Id = mydlp_mnesia:get_unique_id(dest),
	D = case Destination of
		<<"all">> -> all; 
		<<"hasBCC">> -> has_bcc;  %% Workaround to select BCC objects
		_ -> Destination
	end,
	I = #dest{id=Id, rule_id=RuleId, destination=D},
	mydlp_mnesia_write(I),
	populate_destinations(Rows, RuleId,Channel);
populate_destinations([], _RuleId, _Channel) -> ok.

populate_notifications([[Notification]|Rows], RuleId, Type) ->
	Id = mydlp_mnesia:get_unique_id(notification),
	I = #notification{id=Id, rule_id=RuleId, type=Type, target=Notification},
	mydlp_mnesia_write(I),
	populate_notifications(Rows, RuleId, Type);
populate_notifications([], _RuleId, _Type) -> ok.

populate_source_domains([[SourceDomain]|Rows], RuleId) ->
	Id = mydlp_mnesia:get_unique_id(source_domain),
	S = case SourceDomain of
		<<"all">> -> all; 
		_ -> SourceDomain
	end,
	I = #source_domain{id=Id, rule_id=RuleId, domain_name=S},
	mydlp_mnesia_write(I),
	populate_source_domains(Rows, RuleId);
populate_source_domains([], _RuleId) ->ok.

populate_remote_storages(RuleOrigId, RuleId) ->
	{ok, RSSHFS} = psq(remote_sshfs, [RuleOrigId]),
	populate_remote_sshfs(RSSHFS, RuleId),

	{ok, RFTPFS} = psq(remote_ftpfs, [RuleOrigId]),
	populate_remote_ftpfs(RFTPFS, RuleId),

	{ok, RWindows} = psq(remote_windows, [RuleOrigId]),
	populate_remote_windows(RWindows, RuleId),

	{ok, RNFS} = psq(remote_nfs, [RuleOrigId]),
	populate_remote_nfs(RNFS, RuleId),

	ok.

populate_remote_sshfs([[Address, Port, Path, Username, Password]|Rows], RuleId) ->
	Id = mydlp_mnesia:get_unique_id(remote_storage),
	I = #remote_storage{id=Id, rule_id=RuleId, type=sshfs, details={Address, Port, Path, Username, Password}},
	mydlp_mnesia_write(I),
	populate_remote_sshfs(Rows, RuleId);
populate_remote_sshfs([], _RuleId) -> ok.

populate_remote_ftpfs([[Address, Path, Username, Password]|Rows], RuleId) ->
	Id = mydlp_mnesia:get_unique_id(remote_storage),
	I = #remote_storage{id=Id, rule_id=RuleId, type=ftpfs, details={Address, Path, Username, Password}},
	mydlp_mnesia_write(I),
	populate_remote_ftpfs(Rows, RuleId);
populate_remote_ftpfs([], _RuleId) -> ok.

populate_remote_windows([[UNCPath, Username, Password]|Rows], RuleId) ->
	Id = mydlp_mnesia:get_unique_id(remote_storage),
	I = #remote_storage{id=Id, rule_id=RuleId, type=windows, details={UNCPath, Username, Password}},
	mydlp_mnesia_write(I),
	populate_remote_windows(Rows, RuleId);
populate_remote_windows([], _RuleId) -> ok.

populate_remote_nfs([[Address, Path]|Rows], RuleId) ->
	Id = mydlp_mnesia:get_unique_id(remote_storage),
	I = #remote_storage{id=Id, rule_id=RuleId, type=nfs, details={Address, Path}},
	mydlp_mnesia_write(I),
	populate_remote_nfs(Rows, RuleId);
populate_remote_nfs([], _RuleId) -> ok.

convert_day_intervals_to_list([_|Rest]) ->
	L = lists:map(fun(I) -> binary_to_list(I) end, Rest),
	lists:flatten(L).

populate_day_intervals_as_list([DayIntervalId|Rest], Acc) ->
	{ok, [DayIntervalList]} = psq(schedule_day_intervals_by_id, [DayIntervalId]),
	Acc1 = convert_day_intervals_to_list(DayIntervalList),
	populate_day_intervals_as_list(Rest, [Acc1|Acc]);
populate_day_intervals_as_list([], Acc) -> lists:reverse(Acc).

populate_discovery_schedule(RuleOrigId, RuleId) ->
	{ok, Result} = psq(schedule_intervals_by_rule_id, [RuleOrigId]),

	case Result of 
		[] -> ok;
		[ScheduleDayIntervals] -> 	
			DayIntervals = populate_day_intervals_as_list(ScheduleDayIntervals, []),	
	
			{ok, DailySchedule} = psq(daily_schedule_by_rule_id, [RuleOrigId]),
			populate_discovery_schedule1(DailySchedule, DayIntervals, RuleId, RuleOrigId, daily),
		
			{ok, WeeklySchedule} = psq(weekly_schedule_by_rule_id, [RuleOrigId]),
			populate_discovery_schedule1(WeeklySchedule, DayIntervals, RuleId, RuleOrigId, weekly)
	end.

populate_discovery_schedule1([[Hour]|Rows], ScheduleIntervals, RuleId, RuleOrigId, daily) ->
	Id = mydlp_mnesia:get_unique_id(discovery_schedule),
	I = #discovery_schedule{id=Id, rule_id=RuleId, rule_orig_id=RuleOrigId, schedule_hour=Hour, details=daily, available_intervals=ScheduleIntervals},
	mydlp_mnesia_write(I),
	populate_discovery_schedule1(Rows, ScheduleIntervals, RuleId, RuleOrigId, daily);
populate_discovery_schedule1([[Hour, M, Tu, W, Th, F, Sa, Su]|Rows], ScheduleIntervals, RuleId, RuleOrigId, weekly) ->
	Detail = lists:flatten([binary_to_list(M), binary_to_list(Tu), binary_to_list(W), binary_to_list(Th), binary_to_list(F), binary_to_list(Sa), binary_to_list(Su)]),
	Id = mydlp_mnesia:get_unique_id(discovery_schedule),
	I = #discovery_schedule{id=Id, rule_id=RuleId, rule_orig_id=RuleOrigId,  schedule_hour=Hour, details={weekly, Detail}, available_intervals=ScheduleIntervals},
	mydlp_mnesia_write(I),
	populate_discovery_schedule1(Rows, ScheduleIntervals, RuleId, RuleOrigId, weekly);
populate_discovery_schedule1([], _, _, _, _) -> ok.

populate_ep_schedules_entries(RuleId, OrigId) ->
	Id = mydlp_mnesia:get_unique_id(discovery_targets),
	I = #discovery_targets{id=Id, rule_id=RuleId, orig_id=OrigId, targets=[]},
	mydlp_mnesia_write(I),
	ok.

populate_web_servers([[Proto, Address, Port, DigDepth, StartPath]|Rest], RuleId) ->
	Id = mydlp_mnesia:get_unique_id(web_server),
	I = #web_server{id=Id, rule_id=RuleId, proto=binary_to_list(Proto), address=binary_to_list(Address), port=Port, dig_depth=DigDepth, start_path=binary_to_list(StartPath)},
	mydlp_mnesia_write(I),
	populate_web_servers(Rest, RuleId);
populate_web_servers([], _RuleId) -> ok.
	

populate_rule_users(RuleOrigId, RuleId) -> 
	{ok, USQ} = psq(user_s_by_rule_id, [RuleOrigId]),
	populate_users_s(USQ, RuleId),

	{ok, UAUQ} = psq(user_ad_u_by_rule_id, [RuleOrigId]),
	populate_users_ad_u(UAUQ, RuleId),

	{ok, UAOQ} = psq(user_ad_o_by_rule_id, [RuleOrigId]),
	populate_users_ad_u(UAOQ, RuleId),

	{ok, OUQ} = psq(ou_id_by_rule_id, [RuleOrigId]),
	populate_ou(OUQ, RuleId),

	{ok, UAGQ} = psq(user_ad_g_by_rule_id, [RuleOrigId]),
	populate_users_ad_u(UAGQ, RuleId),

	ok.

populate_users_s([[Username]| Rows], RuleId) ->
	new_user(Username, RuleId),
	populate_users_s(Rows, RuleId);
populate_users_s([], _RuleId) -> ok.

populate_users_ad_u([[OrigId]| Rows], RuleId) ->
	Usernames = get_usernames(OrigId),
	lists:foreach(fun({Username}) -> new_user(Username, RuleId) end, Usernames),
	populate_users_ad_u(Rows, RuleId);
populate_users_ad_u([], _RuleId) -> ok.

populate_ou(OQ, RuleId) -> populate_ou(OQ, [], RuleId).

populate_ou([[OUId]|Rows], OUEx, RuleId) ->
	case lists:member(OUId, OUEx) of
		true -> populate_ou(Rows, OUEx, RuleId);
		false -> 
			{ok, UQ} = psq(user_ad_by_ou_id, [OUId]),
			populate_users_ad_u(UQ, RuleId),
			{ok, OUQ} = psq(ou_ad_by_ou_id, [OUId]),
			populate_ou(Rows ++ OUQ, [OUId|OUEx], RuleId) end;
populate_ou([], _Ex, _RuleId) -> ok.

populate_rule_dest_users(RuleOrigId, RuleId) -> 
	{ok, USQ} = psq(dest_user_s_by_rule_id, [RuleOrigId]),
	populate_dest_users_s(USQ, RuleId),

	{ok, UAUQ} = psq(dest_user_ad_u_by_rule_id, [RuleOrigId]),
	populate_dest_users_ad_u(UAUQ, RuleId),

	{ok, UAOQ} = psq(dest_user_ad_o_by_rule_id, [RuleOrigId]),
	populate_dest_users_ad_u(UAOQ, RuleId),

	{ok, OUQ} = psq(dest_ou_id_by_rule_id, [RuleOrigId]),
	populate_dest_ou(OUQ, RuleId),

	{ok, UAGQ} = psq(dest_user_ad_g_by_rule_id, [RuleOrigId]),
	populate_users_ad_u(UAGQ, RuleId),

	ok.

populate_dest_users_s([[Username]| Rows], RuleId) ->
	new_dest_user(Username, RuleId),
	populate_dest_users_s(Rows, RuleId);
populate_dest_users_s([], _RuleId) -> ok.

populate_dest_users_ad_u([[OrigId]| Rows], RuleId) ->
	Usernames = get_usernames(OrigId),
	lists:foreach(fun({Username}) -> new_dest_user(Username, RuleId) end, Usernames),
	populate_dest_users_ad_u(Rows, RuleId);
populate_dest_users_ad_u([], _RuleId) -> ok.

populate_dest_ou(OQ, RuleId) -> populate_dest_ou(OQ, [], RuleId).

populate_dest_ou([[OUId]|Rows], OUEx, RuleId) ->
	case lists:member(OUId, OUEx) of
		true -> populate_dest_ou(Rows, OUEx, RuleId);
		false -> 
			{ok, UQ} = psq(user_ad_by_ou_id, [OUId]),
			populate_dest_users_ad_u(UQ, RuleId),
			{ok, OUQ} = psq(ou_ad_by_ou_id, [OUId]),
			populate_dest_ou(Rows ++ OUQ, [OUId|OUEx], RuleId) end;
populate_dest_ou([], _Ex,  _RuleId) -> ok.

populate_remote_document_database() ->
	{ok, RDD} = psq(get_remote_document_databases),
	populate_each_remote_dd(RDD).

populate_each_remote_dd([[DDId, DDRSId]|Rest]) ->
	RS = get_remote_storage_with_type(DDRSId),
	Id = mydlp_mnesia:get_unique_id(remote_storage_dd),
	I = case RS of
		none -> #remote_storage_dd{id=Id, document_id=DDId, details=none, rs_id=none, exclude_files=none};
		{Type, D, RSId} -> {ok, EF} = psq(get_exclude_files, [DDRSId]),
				#remote_storage_dd{id=Id, document_id=DDId, details={Type, D}, rs_id=RSId, exclude_files=lists:flatten(EF)}
	end,
	mydlp_mnesia_write(I),
	populate_each_remote_dd(Rest);
populate_each_remote_dd([]) -> ok.
	
get_remote_storage_with_type(DDRSId) ->
	case psq(get_remote_sshfs, [DDRSId]) of
		{ok, [[Id|R]]} -> {sshfs, R, Id};
		_ ->
	case psq(get_remote_ftpfs, [DDRSId]) of
		{ok, [[Id1|R1]]} -> {ftpfs, R1, Id1};
		_ ->
	case psq(get_remote_nfs, [DDRSId]) of
		{ok, [[Id2|R2]]} -> {nfs, R2, Id2};
		_ ->
	case psq(get_remote_windows, [DDRSId]) of
		{ok, [[Id4|R4]]} -> {windows, R4, Id4};
		_ ->none end end end end.

get_usernames(OrigId) ->
	Users = get_users(OrigId),
	Domains = get_domains(OrigId),
	L = lists:map(fun(U) ->
		lists:map(fun(D) ->
			UU = unicode:characters_to_list(U),
			UD = unicode:characters_to_list(D),
			concat_username(UU,UD)
		end, Domains)
	end, Users),
	lists:flatten(L).

concat_username(<<>>, _D) -> []; % lists:flatten will drop this.
concat_username(U, <<>>) -> {U};
concat_username(U, D) -> {U ++ "@" ++ D}.

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

new_dest_user(Username, RuleId) ->
	Id = mydlp_mnesia:get_unique_id(destination_user),
	UsernameH = mydlp_api:hash_un(Username),
	User = #destination_user{id=Id, rule_id=RuleId, un_hash=UsernameH},
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
	{ok, MQ} = psq(match_by_id, [MatcherId]),
	MatchId = populate_match(MQ),
	F = #ifeature{id=IFeatureId, itype_id=ITypeId, match_id=MatchId, threshold=Threshold},
	mydlp_mnesia_write(F),
	populate_ifeatures(Rows, ITypeId);
populate_ifeatures([], _RuleId) -> ok.

%whitefile(Matches) -> whitefile(Matches, []).

%whitefile([#match{func=whitefile} = Match|Matches], Returns) -> 
%	L1 = lists:append([lists:reverse(Matches), Returns, [Match]]),
%	lists:reverse(L1);
%whitefile([Match|Matches], Returns) -> whitefile(Matches, [Match|Returns]);
%whitefile([], Returns) -> lists:reverse(Returns).

new_match(Func) -> new_match(Func, []).

new_match(Func, FuncParams) ->
	case find_match_id(Func, FuncParams) of
		none -> NewId = mydlp_mnesia:get_unique_id(match),
			M = #match{id=NewId, func=Func, func_params=FuncParams},
			mydlp_mnesia_write(M),
			NewId;
		Id ->	Id end.

write_regex(RegexGroupId, RegexS) ->
	RegexId = mydlp_mnesia:get_unique_id(regex),
	R = #regex{id=RegexId, group_id=RegexGroupId, plain=RegexS},
	mydlp_mnesia_write(R).

write_keyword(IsWholeWord, KeywordGroupId, KeywordS) ->
	Keyword = case IsWholeWord of
		false -> KeywordS;
		true -> <<" ", KeywordS/binary, " ">> end,
	KeywordId = mydlp_mnesia:get_unique_id(keyword),
	K = #keyword{id=KeywordId, group_id=KeywordGroupId, keyword=Keyword},
	mydlp_mnesia_write(K).

is_whole_word_by_mid(MId) ->
	{ok, SAQ} = psq(strarg_by_matcher_id, [MId]),
	case SAQ of
		[] -> false;
		[[<<"whole_word">>]] -> true;
		[[<<"partial">>]] -> false end.

populate_match([[OrigId, FuncName]]) -> populate_match(OrigId, FuncName).

populate_match(_Id, <<"encrypted_archive">>) ->
	Func = e_archive_match,
	new_match(Func);

populate_match(_Id, <<"encrypted_file">>) ->
	Func = e_file_match,
	new_match(Func);

populate_match(_Id, <<"trid">>) ->
	Func = trid_match,
	new_match(Func);

populate_match(_Id, <<"ssn">>) ->
	Func = ssn_match,
	new_match(Func);

populate_match(_Id, <<"iban">>) ->
	Func = iban_match,
	new_match(Func);

populate_match(_Id, <<"aba">>) ->
	Func = aba_match,
	new_match(Func);

populate_match(_Id, <<"cc">>) ->
	Func = cc_match,
	new_match(Func);

populate_match(_Id, <<"cc_track1">>) ->
	Func = cc_track1_match,
	new_match(Func);

populate_match(_Id, <<"cc_track2">>) ->
	Func = cc_track2_match,
	new_match(Func);

populate_match(_Id, <<"cc_track3">>) ->
	Func = cc_track3_match,
	new_match(Func);

populate_match(_Id, <<"ten_digit">>) ->
	Func = ten_digit_match,
	new_match(Func);

populate_match(_Id, <<"nine_digit">>) ->
	Func = nine_digit_match,
	new_match(Func);

populate_match(_Id, <<"fe_digit">>) ->
	Func = fe_digit_match,
	new_match(Func);

populate_match(_Id, <<"ip">>) ->
	Func = ip_match,
	new_match(Func);

populate_match(_Id, <<"mac">>) ->
	Func = mac_match,
	new_match(Func);

populate_match(_Id, <<"icd10">>) ->
	Func = icd10_match,
	new_match(Func);

populate_match(_Id, <<"canada_sin">>) ->
	Func = canada_sin_match,
	new_match(Func);

populate_match(_Id, <<"france_insee">>) ->
	Func = france_insee_match,
	new_match(Func);

populate_match(_Id, <<"uk_nino">>) ->
	Func = uk_nino_match,
	new_match(Func);

populate_match(_Id, <<"italy_fc">>) ->
	Func = italy_fc_match,
	new_match(Func);

populate_match(_Id, <<"spain_dni">>) ->
	Func = spain_dni_match,
	new_match(Func);

populate_match(_Id, <<"dna">>) ->
	Func = dna_match,
	new_match(Func);

populate_match(_Id, <<"said">>) ->
	Func = said_match,
	new_match(Func);

populate_match(_Id, <<"taiwan_nid">>) ->
	Func = taiwan_nid_match,
	new_match(Func);

populate_match(_Id, <<"pan">>) ->
	Func = pan_match,
	new_match(Func);

populate_match(_Id, <<"tan">>) ->
	Func = tan_match,
	new_match(Func);

populate_match(_Id, <<"cpf">>) ->
	Func = cpf_match,
	new_match(Func);

populate_match(_Id, <<"china_icn">>) ->
	Func = china_icn_match,
	new_match(Func);

populate_match(_Id, <<"chinese_name">>) ->
	Func = chinese_name_match,
	new_match(Func);

populate_match(_Id, <<"cc_edate">>) ->
	Func = cc_edate_match,
	new_match(Func);

populate_match(_Id, <<"gdate">>) ->
	Func = gdate_match,
	new_match(Func);

populate_match(_Id, <<"birthdate">>) ->
	Func = birthdate_match,
	new_match(Func);

populate_match(_Id, <<"scode">>) ->
	Func = scode_match,
	new_match(Func);

populate_match(_Id, <<"scode_ada">>) ->
	Func = scode_ada_match,
	new_match(Func);

populate_match(_Id, <<"all">>) ->
	Func = all,
	new_match(Func);

populate_match(Id, <<"keyword">>) ->
	Func = keyword_match,
	{ok, REQ} = psq(regex_by_matcher_id, [Id]),
	[[KeywordS]] = REQ,
	KeywordGroupId = mydlp_mnesia:get_unique_id(keyword_group_id),
	IsWholeWord = is_whole_word_by_mid(Id),
	write_keyword(IsWholeWord, KeywordGroupId, KeywordS),
	FuncParams=[{group_id, KeywordGroupId}],
	new_match(Func, FuncParams);

populate_match(Id, <<"keyword_group">>) ->
	Func = keyword_match,
	IsWholeWord = is_whole_word_by_mid(Id),
	{ok, BKGQ} = psq(kg_bundled_by_matcher_id, [Id]),
	FuncParams = case BKGQ of
		[] ->	KeywordGroupId = mydlp_mnesia:get_unique_id(keyword_group_id),
			{ok, REQ} = psq(kg_regexes_by_matcher_id, [Id]),
			lists:foreach(fun([KeywordS]) ->
				write_keyword(IsWholeWord, KeywordGroupId, KeywordS)
			end, REQ),
			{ok, REREQ} = psq(kg_rdbms_regexes_by_matcher_id, [Id]),
			lists:foreach(fun([KeywordS]) ->
				write_keyword(IsWholeWord, KeywordGroupId, KeywordS)
			end, REREQ),
			[{group_id, KeywordGroupId}];
		[[BundledFileName]] -> [{file, BundledFileName, IsWholeWord}] end,
	new_match(Func, FuncParams);

populate_match(Id, <<"regex">>) ->
	Func = regex_match,
	{ok, REQ} = psq(regex_by_matcher_id, [Id]),
	[[RegexS]] = REQ,
	RegexGroupId = mydlp_mnesia:get_unique_id(regex_group_id),
	write_regex(RegexGroupId, RegexS),
	FuncParams=[RegexGroupId],
	new_match(Func, FuncParams);

populate_match(Id, <<"document_hash">>) ->
	Func = md5_match,
	{ok, DDQ} = psq(dd_by_matcher_id, [Id]),
	[[DDId]] = DDQ,
	{ok, FHQ} = psq(filehash_by_dd_id, [DDId]),
	Filehashes = lists:append(FHQ),
	populate_filehashes(Filehashes, DDId),
	FuncParams=[DDId],
	new_match(Func, FuncParams);

populate_match(Id, <<"document_pdm">>) ->
	Func = pdm_match,
	{ok, DDQ} = psq(dd_by_matcher_id, [Id]),
	[[DDId]] = DDQ,
	{ok, FFQ} = psq(filefingerprint_by_dd_id, [DDId]),
	{ok, RFQ} = psq(rdbmsfingerprint_by_dd_id, [DDId]),
	Fingerprints = lists:flatten([FFQ,RFQ]),
	populate_filefingerprints(Fingerprints, DDId),
	FuncParams=[DDId],
	new_match(Func, FuncParams);

populate_match(Id, Matcher) -> throw({error, {unsupported_match, Id, Matcher} }).

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

populate_filehashes(FilehashesHEX, DDId) when is_list(FilehashesHEX) ->
	case has_mydlp_compile_item(file_hash, DDId) of
		false -> Filehashes = lists:map(fun(I) -> mydlp_api:hex2bytelist(I) end, FilehashesHEX),
			GBSet=gb_sets:from_list(Filehashes),
			F = #file_hash{group_id=DDId, gb_set=GBSet},
			mydlp_mnesia_write(F),
			mydlp_compile_add_item(file_hash, DDId);
		true -> ok end,
	ok.

populate_filefingerprints(Fingerprints, DDId) when is_list(Fingerprints) ->
	case has_mydlp_compile_item(file_fingerprint, DDId) of
		false -> GBSet=gb_sets:from_list(Fingerprints),
			F = #file_fingerprint{group_id=DDId, gb_set=GBSet},
			mydlp_mnesia_write(F),
			mydlp_compile_add_item(file_fingerprint, DDId);
		true -> ok end,
	ok.

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

get_user_ipr_rid(UserRIds, IprRIds) -> get_user_ipr_rid(UserRIds, IprRIds, []).

get_user_ipr_rid([RIds|UserRIds], IprRIds, Acc) ->
	A = lists:map(fun(I) -> lists:usort(RIds ++ I) end, IprRIds),
	get_user_ipr_rid(UserRIds, IprRIds, Acc ++ A);
get_user_ipr_rid([], _IprRIds, Acc) -> lists:usort(Acc).

populate_mc_modules() ->
	RDRIs = mydlp_mnesia:get_remote_default_rule_ids(),
	RURIs = mydlp_mnesia:get_remote_user_rule_ids(),
	RIRIs = mydlp_mnesia:get_remote_ipr_rule_ids(),
	RIDSs1 = get_user_ipr_rid(RURIs, RIRIs),
	RIDSs2 = [[]|RIDSs1], %% for default rule ids
	RIDSs = lists:map(fun(I) -> lists:usort(RDRIs ++ I) end, RIDSs2),
	populate_mc_modules([local|RIDSs]).
	
populate_mc_modules([T|Rest]) ->
	MC = mydlp_mc:mc_module(T),
	mydlp_mnesia_write(MC),
	populate_mc_modules(Rest);
populate_mc_modules([]) -> ok.

%get_rule_cid(RuleId) ->
%	case psq(cid_of_rule_by_id, [RuleId]) of
%		{ok, [[FilterId]]} -> FilterId;
%		_Else -> 0 end.

custom_action_type_to_atom(<<"SECLORE">>) -> seclore;
custom_action_type_to_atom(<<"seclore">>) -> seclore;
custom_action_type_to_atom(Else) -> throw({error, unsupported_custom_action_type, Else}).

rule_action_to_atom(<<"PASS">>) -> pass;
rule_action_to_atom(<<"LOG">>) -> log;
rule_action_to_atom(<<"BLOCK">>) -> block;
rule_action_to_atom(<<"QUARANTINE">>) -> quarantine;
rule_action_to_atom(<<"ARCHIVE">>) -> archive;
rule_action_to_atom(<<"ENCRYPT">>) -> encrypt;
rule_action_to_atom(<<"CUSTOM">>) -> custom;
rule_action_to_atom(<<"pass">>) -> pass;
rule_action_to_atom(<<"log">>) -> log;
rule_action_to_atom(<<"block">>) -> block;
rule_action_to_atom(<<"quarantine">>) -> quarantine;
rule_action_to_atom(<<"archive">>) -> archive;
rule_action_to_atom(<<"encrypt">>) -> encrypt;
rule_action_to_atom(<<"custom">>) -> custom;
rule_action_to_atom(<<"">>) -> pass;
rule_action_to_atom(Else) -> throw({error, unsupported_action_type, Else}).

rule_dtype_to_channel(<<"WebRule">>) -> web;
rule_dtype_to_channel(<<"MailRule">>) -> mail;
rule_dtype_to_channel(<<"RemovableStorageRule">>) -> removable;
rule_dtype_to_channel(<<"PrinterRule">>) -> printer;
rule_dtype_to_channel(<<"DiscoveryRule">>) -> discovery;
rule_dtype_to_channel(<<"RemoteStorageRule">>) -> remote_discovery;
rule_dtype_to_channel(<<"ApiRule">>) -> api;
rule_dtype_to_channel(<<"RemovableStorageInboundRule">>) -> inbound;
rule_dtype_to_channel(<<"RemovableStorageEncryptionRule">>) -> encryption;
rule_dtype_to_channel(<<"ScreenshotRule">>) -> screenshot;
rule_dtype_to_channel(Else) -> throw({error, unsupported_rule_type, Else}).

validate_action_for_channel(web, pass) -> ok;
validate_action_for_channel(web, log) -> ok;
validate_action_for_channel(web, block) -> ok;
validate_action_for_channel(web, archive) -> ok;
validate_action_for_channel(web, quarantine) -> ok;
validate_action_for_channel(mail, pass) -> ok;
validate_action_for_channel(mail, log) -> ok;
validate_action_for_channel(mail, block) -> ok;
validate_action_for_channel(mail, archive) -> ok;
validate_action_for_channel(mail, quarantine) -> ok;
validate_action_for_channel(mail, {custom, {seclore, pass, _, {HotFolderId, _}}}) 
		when is_integer(HotFolderId)-> ok;
validate_action_for_channel(removable, pass) -> ok;
validate_action_for_channel(removable, log) -> ok;
validate_action_for_channel(removable, block) -> ok;
validate_action_for_channel(removable, archive) -> ok;
validate_action_for_channel(removable, quarantine) -> ok;
validate_action_for_channel(printer, pass) -> ok;
validate_action_for_channel(printer, log) -> ok;
validate_action_for_channel(printer, block) -> ok;
validate_action_for_channel(printer, archive) -> ok;
validate_action_for_channel(printer, quarantine) -> ok;
validate_action_for_channel(discovery, pass) -> ok;
validate_action_for_channel(discovery, log) -> ok;
validate_action_for_channel(discovery, block) -> ok;
validate_action_for_channel(discovery, archive) -> ok;
validate_action_for_channel(discovery, quarantine) -> ok;
validate_action_for_channel(discovery, {custom, {seclore, pass, _, {HotFolderId, _}}}) 
		when is_integer(HotFolderId)-> ok;
validate_action_for_channel(remote_discovery, pass) -> ok;
validate_action_for_channel(remote_discovery, log) -> ok;
validate_action_for_channel(remote_discovery, archive) -> ok;
validate_action_for_channel(api, pass) -> ok;
validate_action_for_channel(api, log) -> ok;
validate_action_for_channel(api, block) -> ok;
validate_action_for_channel(api, archive) -> ok;
validate_action_for_channel(api, quarantine) -> ok;
validate_action_for_channel(inbound, pass) -> ok;
validate_action_for_channel(inbound, log) -> ok;
validate_action_for_channel(inbound, archive) -> ok;
validate_action_for_channel(screenshot, pass) -> ok;
validate_action_for_channel(screenshot, block) -> ok;
validate_action_for_channel(encryption, pass) -> ok;
validate_action_for_channel(encryption, encrypt) -> ok;
validate_action_for_channel(Channel, Action) -> throw({error, {unexpected_action_for_channel, Channel, Action}}).

mydlp_mnesia_write(I) when is_list(I) ->
	L = get(mydlp_mnesia_write),
	put(mydlp_mnesia_write, lists:append(I,L)),
	ok;

mydlp_mnesia_write(#match{} = M) ->
	L = get(mydlp_mnesia_write_match),
	put(mydlp_mnesia_write_match, [M|L]),
	ok;

mydlp_mnesia_write(I) when is_tuple(I) ->
	L = get(mydlp_mnesia_write),
	put(mydlp_mnesia_write, [I|L]),
	ok.

get_mydlp_mnesia_write() ->
	MW = get(mydlp_mnesia_write),
	MWM = get(mydlp_mnesia_write_match),
	lists:append(MW, MWM).

erase_mydlp_mnesia_write() ->
	erase(mydlp_mnesia_write),
	erase(mydlp_mnesia_write_match),
	erase(mydlp_compile_item),
	ok.

init_mydlp_mnesia_write() ->
	put(mydlp_mnesia_write, []),
	put(mydlp_mnesia_write_match, []),
	put(mydlp_compile_item, gb_sets:empty()),
	ok.

mydlp_compile_add_item(Tag, Id) ->
	S = get(mydlp_compile_item),
	put(mydlp_compile_item, gb_sets:add({Tag, Id}, S)),
	ok.

has_mydlp_compile_item(Tag, Id) ->
	S = get(mydlp_compile_item),
	gb_sets:is_member({Tag, Id}, S).
	
find_match_id(Func, FuncParam) ->
	MWM = get(mydlp_mnesia_write_match),
	find_match_id(MWM, Func, FuncParam).

find_match_id([#match{id=Id, func=Func, func_params=FuncParam}|_Rest], Func, FuncParam) -> Id;
find_match_id([_M|Rest], Func, FuncParams) -> find_match_id(Rest, Func, FuncParams);
find_match_id([], _Func, _FuncParams) -> none.

pre_push_log(RuleId, Ip, User, Destination, Action, Channel, Misc, GroupId) -> 
%	{FilterId, RuleId1} = case RuleId of
%		{dr, CId} -> {CId, 0};
%		-1 = RuleId -> {mydlp_mnesia:get_dfid(), RuleId};	% this shows default action had been enforeced 
							% this should be refined for multisite use
%		RId when is_integer(RId) -> {get_rule_cid(RId), RId} end,
	User1 = case User of
		nil -> null;
		unknown -> null;
		null -> null;
		undefined -> null;
		U when is_list(U) -> unicode:characters_to_binary(U);
		U when is_binary(U) -> U
	end,
	Ip1 = case ip_to_int(Ip) of
		nil -> null;
		Else2 -> Else2
	end,
	Destination1 = case Destination of
		nil -> null;
		Else3 -> unicode:characters_to_binary(Else3)
	end,
	ActionS = case Action of
		pass -> <<"P">>;
		block -> <<"B">>;
		log -> <<"L">>;
		quarantine -> <<"Q">>;
		archive -> <<"A">>;
		{custom, {seclore, pass, Name, {HotFolderId, _}}} ->
			list_to_binary([<<"CIS ">>, integer_to_list(HotFolderId), <<" ">>, unicode:characters_to_binary(Name)])
	end,
	ChannelS = case Channel of
		web -> <<"W">>;
		mail -> <<"M">>;
		removable -> <<"R">>;
		printer -> <<"P">>;
		discovery -> <<"D">>;
		remote_discovery -> <<"RD">>;
		api -> <<"A">> ;
		inbound -> <<"I">>
	end,
	Visible = case {Channel, RuleId} of
		{inbound, _} -> 0;
		{_, -1} -> 0;
		_Else -> 1 end,
	Misc1 = case Misc of
		M when is_list(M) -> unicode:characters_to_binary(M);
		Else4 -> Else4
	end,
	{0, RuleId, Ip1, User1, Destination1, ActionS, ChannelS, Misc1, GroupId, Visible}.

pre_push_opr_log(Time, Channel, RuleId, MessageKey, GroupId) ->
	ChannelS = case Channel of
		remote_discovery -> <<"RD">>;
		discovery -> <<"D">>;
		R -> ?ERROR_LOG("Unexpected Rule Type: ["?S"]", [R]),
			<<"U">>
	end,
	{Time, ChannelS, RuleId, MessageKey, GroupId, 1, 0}. % 1 for Visible column 0 for severity
		
pre_insert_log(Filename) ->
	Filename1 = case Filename of
		F when is_list(F) -> unicode:characters_to_binary(F);
		F when is_binary(F) -> F end,

	{Filename1}.

is_ep_discovery_finished(GroupId, Endpoint, Status) ->
	case lpsq(get_opr_with_group_id_and_ep, [GroupId, Endpoint, Status], 5000) of
		{ok, [[_]]} -> true;
		_ -> false
	end.

-endif.

