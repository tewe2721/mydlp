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
	group_id_dict,
	discover_inprog=false,
	timer
}).


is_substring(_FileName, []) -> false;
is_substring(FileName, [Head|Tail]) ->
	case string:str(FileName, Head) of
		1 -> true;
		_ -> is_substring(FileName, Tail)
	end.

-ifdef(__PLATFORM_WINDOWS).

-define(EXCEPTIONS_FILE, [
	"ntuser.dat",
	"apache-tika-"
]).

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

stop_discovery(RuleId, GroupId) -> gen_server:cast(?MODULE, {stop_discovery_by_rule_id, RuleId, GroupId}).

pause_discovery(RuleId, GroupId) -> gen_server:cast(?MODULE, {pause_discovery, RuleId, GroupId}).

continue_discovery(RuleId, GroupId) -> gen_server:cast(?MODULE, {continue_discovery, RuleId, GroupId}).

-endif.

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call({stop_discovery_by_rule_id, RuleId}, _From, #state{discover_queue=Q, group_id_dict=GroupDict}=State) ->
	Q1 = drop_items_by_rule_id(RuleId, Q),
	GroupDict1 = dict:erase(RuleId, GroupDict),
	{reply, ok, State#state{discover_queue=Q1, group_id_dict=GroupDict1}};

handle_call({is_discovery_finished, RuleId}, _From, #state{discover_queue=Q, paused_queue=PQ}=State) ->
	Reply = is_finished_by_rule_id(RuleId, Q),
	Reply1 = case Reply of
		true -> is_finished_by_rule_id(RuleId, PQ);
		false -> false end,
	{reply, Reply1, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({update_rule_status, RuleId, Status}, #state{group_id_dict=GroupDict}=State) ->
	GroupDict1 = case dict:find(RuleId, GroupDict) of
			{ok, {GroupId, _}} -> dict:store(RuleId, {GroupId, Status}, GroupDict);
			error -> ?ERROR_LOG("Unknown RuleId: "?S"", [RuleId]), GroupDict
	end,
	{noreply, State#state{group_id_dict=GroupDict1}};

handle_cast({ql, List}, State) ->
	[ q(FilePath, RuleIndex, GroupId) || {RuleIndex, FilePath, GroupId} <- List ],
	{noreply, State};

handle_cast({q, ParentId, FilePath, RuleIndex, GroupId}, #state{discover_queue=Q, group_id_dict=GroupDict, discover_inprog=false} = State) ->
	Q1 = queue:in({ParentId, FilePath, RuleIndex}, Q),
	GroupDict1 = case dict:find(RuleIndex, GroupDict) of
			{ok, _Val} -> GroupDict;
			error -> dict:store(RuleIndex, {GroupId, disc}, GroupDict)
		end,
	consume(),
	set_discover_inprog(),
	{noreply, State#state{discover_queue=Q1, discover_inprog=true, group_id_dict=GroupDict1}};

handle_cast({q, ParentId, FilePath, RuleIndex, GroupId}, #state{discover_queue=Q, group_id_dict=GroupDict, discover_inprog=true} = State) ->
	Q1 = queue:in({ParentId, FilePath, RuleIndex}, Q),
	GroupDict1 = case dict:find(RuleIndex, GroupDict) of
			{ok, _Val} -> GroupDict;
			error -> dict:store(RuleIndex, {GroupId, disc}, GroupDict)
		end,
	{noreply,State#state{discover_queue=Q1, group_id_dict=GroupDict1}};

handle_cast({push_paused_to_proc_queue, RuleId}, #state{discover_queue=Q, paused_queue=PQ, group_id_dict=GroupDict} = State) ->
	reset_discover_cache(),
	GroupDict1 = case dict:find(RuleId, GroupDict) of
			{ok, {GId, _S}} -> dict:store(RuleId, {GId, disc}, GroupDict);
			error -> ?ERROR_LOG("Unknown Rule id: "?S"", [RuleId])
	end,
	{noreply, State#state{discover_queue=queue:join(Q, PQ), paused_queue=queue:new(), group_id_dict=GroupDict1}};

handle_cast({del_fs_entries, RuleIndex}, State) ->
	mydlp_mnesia:del_fs_entries_by_rule_id(RuleIndex),
	{noreply, State};

handle_cast(consume, #state{discover_queue=Q, paused_queue=PQ, group_id_dict=GroupDict} = State) ->
	case queue:out(Q) of
		{{value, {ParentId, FilePath, RuleIndex}=Item}, Q1} ->
			case is_paused_or_stopped_by_rule_id(RuleIndex, GroupDict) of
				paused -> % rule is paused, push the item pause queue
					PQ1 = queue:in(Item, PQ),
					consume(),
					{noreply, State#state{discover_queue=Q1, paused_queue=PQ1}};
				stopped -> % rule is stopped, drop item
					consume(),
					{noreply, State#state{discover_queue=Q1, paused_queue=PQ}};
				_ ->
					try	case has_discover_rule() of
							true -> case is_exceptional(FilePath) of
									false -> discover(ParentId, FilePath, RuleIndex, GroupDict);
									true -> ok end;
							false -> ok end,
						consume(),
						{noreply, State#state{discover_queue=Q1}}
					catch Class:Error ->
						?ERROR_LOG("Discover Queue Consume: Error occured: "
								"Class: ["?S"]. Error: ["?S"].~n"
								"Stack trace: "?S"~n.FilePath: "?S"~nState: "?S"~n ",	
								[Class, Error, erlang:get_stacktrace(), FilePath, State]),
							consume(),
							{noreply, State#state{discover_queue=Q1}} end
			end;
		{empty, _} ->
			unset_discover_inprog(),
			{noreply, State#state{discover_inprog=false}}
	end;

handle_cast(stop_discovery, State) ->
	NewQ = queue:new(),
	{noreply, State#state{discover_queue=NewQ}};

handle_cast({start_discovery, RuleId, GroupId}, #state{group_id_dict=GroupDict}=State) ->
	GroupDict1 = dict:store(RuleId, {GroupId, disc}, GroupDict),
	PathList = case mydlp_mnesia:get_discovery_directory(RuleId) of
		none -> [];
		L when is_list(L) ->
			lists:map(fun(P) -> 
				try unicode:characters_to_list(P)
					catch _:_ -> binary_to_list(P) end  %% TODO: log this case
				 end
			, L) end,
	erlang:display({pathList, PathList}),
	lists:map(fun(P) -> q(P, RuleId, GroupId) end, PathList),
	{noreply, State#state{group_id_dict=GroupDict1}};

handle_cast(schedule_discovery, State) -> handle_info(schedule_now, State);

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(schedule_now, State) ->
	State1 = cancel_timer(State),
	schedule(),
	{noreply, State1};

handle_info(schedule_startup, State) ->
	schedule(),
	{noreply, State};

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions


is_paused_or_stopped_by_rule_id(RuleId, GroupDict) -> 
	erlang:display({psne, dict:find(RuleId, GroupDict)}),
	case dict:find(RuleId, GroupDict) of
		{ok, {_GroupId, Status}} -> Status;
		_ -> none
	end.

-ifdef(__MYDLP_NETWORK).

has_discover_rule() -> true.

cancel_timer(State) -> State.

schedule_timer(State, _Interval) -> State.

schedule() -> ok.

-endif.

-ifdef(__MYDLP_ENDPOINT).


has_discover_rule() ->
	case mydlp_mnesia:get_rule_table(discovery) of
		none -> false;
		{_ACLOpts, {_Id, pass}, []} -> false;
		_Else -> true end.

cancel_timer(#state{timer=Timer} = State) ->
	case Timer of
		undefined -> ok;
		TRef ->	(catch timer:cancel(TRef)) end,
	State#state{timer=undefined}.
	
%schedule_timer(State, Interval) ->
%	State1 = cancel_timer(State),
%	Timer = case timer:send_after(Interval, schedule_now) of
%		{ok, TRef} -> TRef;
%		{error, _} = Error -> ?ERROR_LOG("Can not create timer. Reason: "?S, [Error]), undefined end,
%	State1#state{timer=Timer}.
%
schedule() ->
	PathsWithRuleIndex = mydlp_mnesia:get_discovery_directory(), % {Path, IndexInWhichRuleHasThisPath}
	PathList = case PathsWithRuleIndex of
		none -> [];
		L when is_list(L) ->
			lists:map(fun({P, Index}) -> 
				{try unicode:characters_to_list(P)
					catch _:_ -> binary_to_list(P) end,  %% TODO: log this case
				Index} end
			, L) end,	
	reset_discover_cache(),
	%lists:foreach(fun({P, I}) -> q(P, I) end, PathList),
	ok.
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
	{ok, #state{discover_queue=queue:new(), paused_queue=queue:new(), group_id_dict=dict:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

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
					RuleId -> erlang:display({dropped, Item}),AccQ;
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

discover_file(#fs_entry{file_id={FP, RuleIndex}}, GroupDict) ->
	try	timer:sleep(20),
		{ok, ObjId} = mydlp_container:new(),
		ok = mydlp_container:setprop(ObjId, "rule_index", RuleIndex),
		{ok, {GroupId, _}} = dict:find(RuleIndex, GroupDict),
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

discover_dir(#fs_entry{file_id={FP, RuleIndex}, entry_id=EId}, GroupDict) ->
	CList = case file:list_dir(FP) of
		{ok, LD} -> LD;
		{error, _} -> [] end,
	OList = mydlp_mnesia:fs_entry_list_dir(EId),
	MList = lists:umerge([CList, OList]),
	GroupId = dict:find(RuleIndex, GroupDict),
	[ q(EId, filename:absname(FN, FP), RuleIndex, GroupId) || FN <- MList ],
	ok.

discover_dir_dir(#fs_entry{file_id={FP, RuleIndex}, entry_id=EId}, GroupDict) ->
	OList = mydlp_mnesia:fs_entry_list_dir(EId),
	GroupId = dict:find(RuleIndex, GroupDict),
	[ q(EId, filename:absname(FN, FP), RuleIndex, GroupId) || FN <- OList ],
	ok.

discover(ParentId, FilePath, RuleIndex, GroupDict) ->
	case is_cached({FilePath, RuleIndex}) of
		true -> ok;
		false -> discover1(ParentId, FilePath, RuleIndex, GroupDict) end.

discover1(ParentId, FilePath, RuleIndex, GroupDict) ->
	case filelib:is_regular(FilePath) of
		true -> E = fs_entry(ParentId, FilePath, RuleIndex),
			case is_changed(E) of
				true -> discover_file(E, GroupDict);
				false -> ok end;
	false -> case filelib:is_dir(FilePath) of
		true -> E = fs_entry(ParentId, FilePath, RuleIndex),
			case is_changed(E) of
				true -> discover_dir(E, GroupDict);
				false -> discover_dir_dir(E, GroupDict) end;
	false -> mydlp_mnesia:del_fs_entry(FilePath) end end, % Means file does not exists
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

reset_discover_cache() ->
	put(cache, gb_sets:new()), ok.

is_cached(Element) ->
	CS = get(cache),
	case gb_sets:is_element(Element, CS) of
		true -> true;
		false -> CS1 = gb_sets:add(Element, CS),
			put(cache, CS1),
			false end.
	
