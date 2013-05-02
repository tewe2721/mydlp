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
%%% @copyright 2011, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-module(mydlp_discover_fs).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0,
	ql/1,
	q/3,
	q/4,
	continue_paused_discovery/1,
	is_discovery_finished/1,
	update_rule_status/2,
	stop/0]).

-ifdef(__MYDLP_ENDPOINT).

-export([start_discovery/2,
	stop_discovery/2,
	pause_discovery/2,
	continue_discovery/2
	]).

-endif.

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
	discover_queue,
	paused_queue,
	discover_inprog=false,
	is_new=false,
	timer
}).

-ifdef(__MYDLP_ENDPOINT).

-define(DISCOVERY_FINISHED, "ep_finished").
-define(DISCOVERY_PAUSED, "ep_paused").

-endif.

-ifdef(__MYDLP_NETWORK).

-define(DISCOVERY_FINISHED, "rfs_finished").
-define(DISCOVERY_PAUSED, "rfs_paused").

-endif.

-define(DISCOVERING, discovering).
-define(PAUSED, paused).
-define(STOPPED, stopped).

-ifdef(__PLATFORM_WINDOWS).

-define(EXCEPTIONS_FILE, [
	"ntuser.dat",
	"apache-tika-"
]).

is_substring(_FileName, []) -> false;
is_substring(FileName, [Head|Tail]) ->
	case string:str(FileName, Head) of
		1 -> true;
		_ -> is_substring(FileName, Tail)
	end.

is_exceptional(FilePath) ->
	FileName = filename:basename(FilePath, ""),
	FileName1 = string:to_lower(FileName),
	is_substring(FileName1, ?EXCEPTIONS_FILE).

-endif.

-ifdef(__PLATFORM_LINUX).

is_exceptional(_FilePath) -> false.

-endif.

%%%%%%%%%%%%%  API

is_discovery_finished(RuleId) -> gen_server:call(?MODULE, {is_discovery_finished, RuleId}).

ql(List) -> gen_server:cast(?MODULE, {ql, List}).

q(FilePath, RuleIndex, GroupId) -> q(none, FilePath, RuleIndex, GroupId).

q(ParentId, FilePath, RuleIndex, GroupId) -> gen_server:cast(?MODULE, {q, ParentId, FilePath, RuleIndex, GroupId}).

continue_paused_discovery(RuleId) ->
	gen_server:cast(?MODULE, {push_paused_to_proc_queue, RuleId}),
	consume().

update_rule_status(RuleId, Status) -> gen_server:cast(?MODULE, {update_rule_status, RuleId, Status}).

-ifdef(__MYDLP_ENDPOINT).

start_discovery(RuleId, GroupId) -> gen_server:cast(?MODULE, {start_discovery, RuleId, GroupId}).

stop_discovery(RuleId, _GroupId) -> gen_server:call(?MODULE, {stop_discovery_by_rule_id, RuleId}, 60000).

pause_discovery(RuleId, GroupId) -> gen_server:cast(?MODULE, {pause_discovery, RuleId, GroupId}).

continue_discovery(RuleId, GroupId) -> 
	gen_server:cast(?MODULE, {continue_discovery, RuleId, GroupId}),
	consume().

-endif.

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call({stop_discovery_by_rule_id, RuleId}, _From, #state{discover_queue=Q, paused_queue=PQ}=State) ->
	case get_discovery_status(RuleId) of
		{Status, GId} ->
			Q1 = drop_items_by_rule_id(RuleId, Q),
			PQ1 = drop_items_by_rule_id(RuleId, PQ),
			push_opr_log(RuleId, GId, ?DISCOVERY_FINISHED),
			case Status of
				?DISCOVERING -> mydlp_mnesia:del_fs_entries_by_rule_id(RuleId);
				?PAUSED -> mydlp_mnesia:del_fs_entries_by_rule_id(RuleId);
				_ -> ok
			end,
			mydlp_mnesia:remove_discovery_status(RuleId),
			filter_discover_cache(RuleId),
			{reply, ok, State#state{discover_queue=Q1, paused_queue=PQ1}};
		_ -> {reply, ok, State}
	end;

handle_call({is_discovery_finished, RuleId}, _From, #state{discover_queue=Q, paused_queue=PQ}=State) ->
	Reply = is_finished_by_rule_id(RuleId, Q),
	Reply1 = case Reply of
		true -> is_finished_by_rule_id(RuleId, PQ);
		false -> false end,
	{reply, Reply1, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({update_rule_status, RuleId, Status}, State) ->
	case get_discovery_status(RuleId) of
		none -> ok;
		{_, GId}  -> mydlp_mnesia:update_discovery_status(RuleId, Status, GId)
	end,
	{noreply, State};

handle_cast({ql, List}, State) ->
	[ q(FilePath, RuleIndex, GroupId) || {RuleIndex, FilePath, GroupId} <- List ],
	{noreply, State};

handle_cast({q, ParentId, FilePath, RuleIndex, _GroupId}, #state{discover_queue=Q, discover_inprog=false} = State) ->
	Q1 = queue:in({ParentId, FilePath, RuleIndex}, Q),
	consume(),
	set_discover_inprog(),
	{noreply, State#state{discover_queue=Q1, discover_inprog=true}};

handle_cast({q, ParentId, FilePath, RuleIndex, _GroupId}, #state{discover_queue=Q, discover_inprog=true} = State) ->
	Q1 = queue:in({ParentId, FilePath, RuleIndex}, Q),
	{noreply,State#state{discover_queue=Q1}};

handle_cast({push_paused_to_proc_queue, _RuleId}, #state{discover_queue=Q, paused_queue=PQ} = State) ->
	{noreply, State#state{discover_queue=queue:join(Q, PQ), paused_queue=queue:new()}};

handle_cast(consume, #state{discover_queue=Q, paused_queue=PQ, is_new=IsNew} = State) ->
	case queue:out(Q) of
		{{value, {ParentId, FilePath, RuleIndex}=Item}, Q1} ->
			case is_paused_or_stopped_by_rule_id(RuleIndex) of
				paused -> % rule is paused, push the item pause queue
					PQ1 = queue:in(Item, PQ),
					consume(),
					{noreply, State#state{discover_queue=Q1, paused_queue=PQ1, is_new=false}};
				stopped -> % rule is stopped, drop item
					consume(),
					{noreply, State#state{discover_queue=Q1, paused_queue=PQ, is_new=false}};
				_ ->
					try	case has_discover_rule() of
							true -> case is_exceptional(FilePath) of
									false -> discover(ParentId, FilePath, RuleIndex);
									true -> ok end;
							false -> ok end,
						consume(),
						{noreply, State#state{discover_queue=Q1, is_new=false}}
					catch Class:Error ->
						?ERROR_LOG("Discover Queue Consume: Error occured: "
								"Class: ["?S"]. Error: ["?S"].~n"
								"Stack trace: "?S"~n.FilePath: "?S"~nState: "?S"~n ",	
								[Class, Error, erlang:get_stacktrace(), FilePath, State]),
							consume(),
							{noreply, State#state{discover_queue=Q1, is_new=false}} end
			end;
		{empty, _} ->
			case IsNew of
				true -> ok;
				false -> mark_finished_rules(PQ) end,
			unset_discover_inprog(),
			{noreply, State#state{discover_inprog=false, is_new=false}}
	end;

handle_cast({stop_discovery, RuleId, GroupId}, State) ->
	mydlp_mnesia:update_discovery_status(RuleId, ?STOPPED, GroupId),
	{noreply, State};

handle_cast({start_discovery, RuleId, GroupId}, State) ->
	mydlp_mnesia:update_discovery_status(RuleId, ?DISCOVERING, GroupId),
	PathList = case mydlp_mnesia:get_discovery_directory(RuleId) of
		none -> [];
		L when is_list(L) ->
			lists:map(fun(P) -> 
				try unicode:characters_to_list(P)
					catch _:_ -> binary_to_list(P) end  %% TODO: log this case
				 end
			, L) end,
	filter_discover_cache(RuleId),
	lists:map(fun(P) -> q(P, RuleId, GroupId) end, PathList),
	{noreply, State#state{is_new=true}};

handle_cast({pause_discovery, RuleId, GroupId}, State) ->
	mydlp_mnesia:update_discovery_status(RuleId, ?PAUSED, GroupId),
	{noreply, State};

handle_cast({continue_discovery, RuleId, GroupId}, #state{discover_queue=Q, paused_queue=PQ}=State) ->
	%reset_discover_cache(),
	mydlp_mnesia:update_discovery_status(RuleId, ?DISCOVERING, GroupId),
	{noreply, State#state{discover_queue=queue:join(Q, PQ), paused_queue=queue:new()}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions


is_paused_or_stopped_by_rule_id(RuleId) -> 
	case get_discovery_status(RuleId) of
		{Status, _} -> Status;
		_ -> none
	end.

mark_finished_rules(PausedQ) ->
	DiscStatus = mydlp_mnesia:get_all_discovery_status(),
	lists:foreach(fun({RuleId, {GroupId, _Status}}) -> mark_finished_each_rule(RuleId, GroupId, PausedQ) end, DiscStatus).

mark_finished_each_rule(RuleId, GroupId, Q) ->
	case queue:out(Q) of
	 	{{value, {_ParentId, _FilePath, RuleIndex}}, Q1} -> 
			case RuleIndex of
				RuleId -> push_opr_log(RuleId, GroupId, ?DISCOVERY_PAUSED);
					%mydlp_mnesiai:update_discovery_status(RuleId, ?PAUSED, GroupId);
				_ -> mark_finished_each_rule(RuleId, GroupId, Q1)
			end;
		{empty, _Q2} ->
			push_opr_log(RuleId, GroupId, ?DISCOVERY_FINISHED),
			mydlp_mnesia:remove_discovery_status(RuleId)
	end.


-ifdef(__MYDLP_NETWORK).

has_discover_rule() -> true. % may be redundant

push_opr_log(RuleId, GroupId, Message) ->
	Time = erlang:universaltime(),
	OprLog = #opr_log{time=Time, channel=remote_discovery, rule_id=RuleId, message_key=Message, group_id=GroupId},
	?DISCOVERY_OPR_LOG(OprLog).


-endif.

-ifdef(__MYDLP_ENDPOINT).

has_discover_rule() ->
	true.
	%case mydlp_mnesia:get_rule_table(discovery) of
	%	none -> false;
	%	{_ACLOpts, {_Id, pass}, []} -> false;
	%	_Else -> true end.
	
push_opr_log(RuleId, GroupId, Message) ->
	Time = erlang:universaltime(),
	OprLog = #opr_log{time=Time, channel=discovery, rule_id=RuleId, message_key=Message, group_id=GroupId},
	?DISCOVERY_OPR_LOG(OprLog).

-endif.

consume() -> gen_server:cast(?MODULE, consume).

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) -> 
	reset_discover_cache(),
	{ok, #state{discover_queue=queue:new(), paused_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

get_discovery_status(RuleId) ->
	mydlp_mnesia:get_discovery_status(RuleId).

is_finished_by_rule_id(RuleId, Q) ->
	case queue:out(Q) of
	 	{{value, {_ParentId, _FilePath, RuleIndex}}, Q1} -> 
		case RuleIndex of
				RuleId -> false;
				_ -> is_finished_by_rule_id(RuleId, Q1)
			end;
		{empty, _Q2} -> true
	end.

drop_items_by_rule_id(RuleId, Q) -> drop_items_by_rule_id(RuleId, Q, queue:new()).

drop_items_by_rule_id(RuleId, Q, AccQ) ->
	case queue:out(Q) of
		 {{value, {_ParentId, _FilePath, RuleIndex}=Item}, Q1} -> 
			AccQ1 = case RuleIndex of
					RuleId -> AccQ;
					_ -> queue:in(Item, AccQ)
				end,
			drop_items_by_rule_id(RuleId, Q1, AccQ1);
		{empty, _Q2} -> AccQ
	end.

meta(FilePath) ->
	{ok, FileInfo} = file:read_file_info(FilePath),
	{FileInfo#file_info.mtime, FileInfo#file_info.size}.

is_changed(#fs_entry{file_id={FP, _RuleIndex}, file_size=FSize, last_modified=LMod} = E) ->
	{MTime, CSize} = meta(FP),
	case ( (LMod /= MTime) or (CSize /= FSize) ) of
		true -> mydlp_mnesia:add_fs_entry(E#fs_entry{file_size=CSize, last_modified=MTime}), % update mnesia entry
			true;
		false -> false end.

fs_entry(ParentId, FilePath, RuleIndex) ->
	FileId = {FilePath, RuleIndex},
	case mydlp_mnesia:get_fs_entry(FileId) of
		none -> Id = mydlp_mnesia:get_unique_id(fs_entry),
			E = #fs_entry{file_id=FileId, entry_id=Id, parent_id=ParentId},
			mydlp_mnesia:add_fs_entry(E), %% bulk write may improve performance
			E;
		#fs_entry{} = FS -> FS end.

-ifdef(__MYDLP_NETWORK).

set_prop_extra(ObjId) -> 
	ok = mydlp_container:setprop(ObjId, "channel", "remote_discovery"),
	ok = mydlp_container:setprop(ObjId, "drop_path", ?CFG(mount_dir)),
	ok.

-endif.

-ifdef(__MYDLP_ENDPOINT).

set_prop_extra(ObjId) ->
	ok = mydlp_container:setprop(ObjId, "channel", "discovery"),
	ok.

-endif.

discover_file(#fs_entry{file_id={FP, RuleIndex}}) ->
	try	timer:sleep(20),
		{ok, ObjId} = mydlp_container:new(),
		ok = mydlp_container:setprop(ObjId, "rule_index", RuleIndex),
		{_, GroupId} = get_discovery_status(RuleIndex),
		ok = mydlp_container:setprop(ObjId, "group_id", GroupId),
		set_prop_extra(ObjId),
		ok = mydlp_container:pushfile(ObjId, {raw, FP}),
		ok = mydlp_container:eof(ObjId),
		{ok, Action} = mydlp_container:aclq(ObjId),
		ok = mydlp_container:destroy(ObjId),
		case Action of
			block -> ok = file:delete(FP);
			pass -> ok end
	catch Class:Error ->
		?ERROR_LOG("DISCOVER FILE: Error occured: Class: ["?S"]. Error: ["?S"].~n"
				"Stack trace: "?S"~nFilePath: ["?S"].~n",
			[Class, Error, erlang:get_stacktrace(), FP])
	end,
	ok.

discover_dir(#fs_entry{file_id={FP, RuleIndex}, entry_id=EId}) ->
	CList = case file:list_dir(FP) of
		{ok, LD} -> LD;
		{error, _} -> [] end,
	OList = mydlp_mnesia:fs_entry_list_dir(EId),
	MList = lists:umerge([CList, OList]),
	{_, GroupId} = get_discovery_status(RuleIndex),
	[ q(EId, filename:absname(FN, FP), RuleIndex, GroupId) || FN <- MList ],
	ok.

discover_dir_dir(#fs_entry{file_id={FP, RuleIndex}, entry_id=EId}) ->
	OList = mydlp_mnesia:fs_entry_list_dir(EId),
	{_, GroupId} = get_discovery_status(RuleIndex),
	[ q(EId, filename:absname(FN, FP), RuleIndex, GroupId) || FN <- OList ],
	ok.

discover(ParentId, FilePath, RuleIndex) ->
	case is_cached({FilePath, RuleIndex}) of
		true -> ok;
		false -> discover1(ParentId, FilePath, RuleIndex) end.

discover1(ParentId, FilePath, RuleIndex) ->
	case filelib:is_regular(FilePath) of
		true -> E = fs_entry(ParentId, FilePath, RuleIndex),
			case is_changed(E) of
				true -> discover_file(E);
				false -> ok end;
	false -> case filelib:is_dir(FilePath) of
		true -> E = fs_entry(ParentId, FilePath, RuleIndex),
			case is_changed(E) of
				true -> discover_dir(E);
				false -> discover_dir_dir(E) end;
	false -> ?ERROR_LOG("DISCOVER: File or directory does not exists. Filename: "?S, [FilePath]),
		mydlp_mnesia:del_fs_entry(FilePath) end end, % Means file does not exists
	ok.


-ifdef(__MYDLP_ENDPOINT).

set_discover_inprog() ->
	mydlp_container:set_ep_meta("discover_inprog", "yes"),
	mydlp_sync:sync_now().

unset_discover_inprog() ->
	reset_discover_cache(),
	mydlp_container:set_ep_meta("discover_inprog", "no"),
	mydlp_sync:sync_now().

-endif.

-ifdef(__MYDLP_NETWORK).

set_discover_inprog() -> ok.

unset_discover_inprog() -> 
	reset_discover_cache().
-endif.

filter_discover_cache(RuleId) ->
	CS = get(cache),
	CS1 = gb_sets:filter(fun({_FP, RuleIndex}) -> RuleIndex /= RuleId end, CS),
	put(cache, CS1), ok.

reset_discover_cache() ->
	put(cache, gb_sets:new()), ok.

is_cached(Element) ->
	CS = get(cache),
	case gb_sets:is_element(Element, CS) of
		true -> true;
		false -> CS1 = gb_sets:add(Element, CS),
			put(cache, CS1),
			false end.
	
