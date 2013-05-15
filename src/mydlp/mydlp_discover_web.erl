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

-ifdef(__MYDLP_NETWORK).

-module(mydlp_discover_web).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0,
	update_rule_status/2,
	test_web_server/1,
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
	head_requests,
	get_requests,
	rule_age,
	discover_queue,
	paused_queue,
	discover_inprog=false,
	timer_dict
}).

-define(DISCOVERY_FINISHED, "web_finished").
-define(DISCOVERY_PAUSED, "web_paused").


q(WebServerId, PagePath, RuleId) -> q(WebServerId, none, PagePath, RuleId).

q(WebServerId, ParentId, PagePath, RuleId) ->
	case mydlp_mnesia:get_web_server(WebServerId) of
		#web_server{dig_depth=Depth} -> q(WebServerId, ParentId, PagePath, RuleId, Depth);
		_Else -> ok end.

q(_WebServerId, _ParentId, _PagePath, _RuleId, 0) -> ok;
q(WebServerId, ParentId, PagePath, RuleId, Depth) -> gen_server:cast(?MODULE, {q, WebServerId, ParentId, PagePath, RuleId, Depth}).

test_web_server(URL) -> gen_server:call(?MODULE, {test_web_server, URL}).

%pause_discovery(RuleId) -> gen_server:cast(?MODULE, {pause_discovery, RuleId}).

update_rule_status(RuleId, Status) -> gen_server:cast(?MODULE, {update_rule_status, RuleId, Status}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call({test_web_server, URL}, _From, State) ->
	try
		URLS = binary_to_list(URL),
		R = case httpc:request(head, {URLS, []}, [], [{sync, true}]) of
			{ok, {{_, 200, _}, _, _}} -> "OK";
			{ok, {{_, C, Reason}, _, _}} when C > 400 -> integer_to_list(C) ++ " " ++ Reason;
			_ -> "Unknown Error Occured"
		end,
		{reply, R, State}
	catch _Class:_Error ->
		{reply, "Unknown Error Occured", State}
	end;

handle_call({stop_discovery, RuleId}, _From, #state{discover_queue=Q, paused_queue=PQ}=State) ->
	NewQ = drop_items_by_rule_id(RuleId, Q),
	NewPQ = drop_items_by_rule_id(RuleId, PQ),
	?FLE(fun() ->
		%GetT = gb_tree:empty(),
		%HeadT = gb_tree:empty(),
		case get_discovery_status(RuleId) of
			{Status, GId} -> 
				push_opr_log(RuleId, GId, ?DISCOVERY_FINISHED),
				case Status of
					discovering -> mydlp_mnesia:del_web_entries_by_rule_id(RuleId);
					on_demand_discovering -> mydlp_mnesia:del_web_entries_by_rule_id(RuleId);
					paused -> mydlp_mnesia:del_web_entries_by_rule_id(RuleId);
					user_paused -> mydlp_mnesia:del_web_entries_by_rule_id(RuleId);
					system_paused -> mydlp_mnesia:del_web_entries_by_rule_id(RuleId)
				end;
			_ -> ?ERROR_LOG("mydlp_discover web: Unknown Rule Id: ["?S"]", [RuleId])
		end,
		filter_discover_cache(RuleId)
	end)(),
	{reply, ok, State#state{discover_queue=NewQ, paused_queue=NewPQ}};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

%handle_cast({pause_discovery, RuleId}, #state{timer_dict=TimerDict, group_id_dict=GroupDict}) ->
%	case dict:find(RuleId, TimerDict) of
%		{ok, Timer} -> timer:cancel(Timer),
%				TimerDict1 = dict:store(RuleId, none, TimerDict),
%				GroupDict1 = case dict:find(RuleId, GroupDict) of
%						{ok, {GId, _Status}} -> dict:store(RuleId, {GId, paused});
%						_ -> GroupDict end,
%				{noreply, State#state{timer_dict=TimerDict1, group_id_dict=GroupDict}};
%		_ -> {noreply, State};
%	end;

handle_cast({continue_discovering, RuleId}, #state{discover_queue=Q, paused_queue=PQ, timer_dict=TimerDict}=State) ->
	case get_discovery_status(RuleId) of
		{_, _GId} ->
			{ok, Timer} = timer:send_after(60000, {is_finished, RuleId}),
			TimerDict1 = dict:store(RuleId, Timer, TimerDict),
			consume(),
			{noreply, State#state{discover_queue=queue:join(Q, PQ), paused_queue=queue:new(), timer_dict=TimerDict1}};
		_ -> ?ERROR_LOG("Unknown Rule id: "?S"", [RuleId]),
			{noreply, State}
	end;

handle_cast({q, _WebServerId, _ParentId, external, _RuleId, _Depth}, State) ->
	{noreply, State};

handle_cast({q, WebServerId, ParentId, PagePath, RuleId, Depth}, #state{discover_queue=Q, discover_inprog=false} = State) ->
	Q1 = queue:in({WebServerId, ParentId, PagePath, RuleId, Depth}, Q),
	consume(),
	set_discover_inprog(),
	{noreply, State#state{discover_queue=Q1, discover_inprog=true}};

handle_cast({q, WebServerId, ParentId, PagePath, RuleId, Depth}, #state{discover_queue=Q, discover_inprog=true} = State) ->
	Q1 = queue:in({WebServerId, ParentId, PagePath, RuleId, Depth}, Q),
	{noreply,State#state{discover_queue=Q1}};

handle_cast(consume, #state{discover_queue=Q, paused_queue=PQ, head_requests=HeadT} = State) ->
	case queue:out(Q) of
		{{value, {WebServerId, ParentId, PagePath, RuleId, Depth}=Item}, Q1} ->
			case is_paused_or_stopped_by_rule_id(RuleId) of
			paused -> % rule is paused push the item paused_queue
				PQ1 = queue:in(Item, PQ),
				consume(),
				{noreply, State#state{discover_queue=Q1, paused_queue=PQ1}};
			stopped -> % rule is stopped drop item
				consume(),
				{noreply, State#state{discover_queue=Q1, paused_queue=PQ}};
			_ ->
				try 	case is_cached({WebServerId, PagePath, RuleId}) of
						false -> case fetch_meta(WebServerId, PagePath) of
							{ok, RequestId} ->
								HeadT1 = gb_trees:enter(RequestId, {WebServerId, ParentId, PagePath, RuleId, Depth}, HeadT),
								consume(),
								{noreply, State#state{discover_queue=Q1, head_requests=HeadT1}};
							external -> consume(),
								{noreply, State#state{discover_queue=Q1}} end;
						true -> consume(),
							{noreply, State#state{discover_queue=Q1}} end
				catch Class:Error ->
					?ERROR_LOG("Web: Discover Queue Consume: Error occured: "
							"Class: ["?S"]. Error: ["?S"].~n"
							"Stack trace: "?S"~nWebServerId: "?S" PagePath: "?S"~nState: "?S"~n ",	
							[Class, Error, erlang:get_stacktrace(), WebServerId, PagePath, State]),
						consume(),
						{noreply, State#state{discover_queue=Q1}} end
			end;
		{empty, _} ->
			unset_discover_inprog(),
			{noreply, State#state{discover_inprog=false}}
	end;

handle_cast({update_rule_status, RuleId, _Status}, #state{timer_dict=TimerDict}=State) ->
	TimerDict1 = case dict:find(RuleId, TimerDict) of
			{ok, Timer} -> timer:cancel(Timer),
					dict:store(RuleId, none, TimerDict);
			_ -> TimerDict end,
	{noreply, State#state{timer_dict=TimerDict1}};


handle_cast({start_by_rule_id, OrigRuleId, GroupId}, #state{timer_dict=TimerDict}=State) ->
	filter_discover_cache(OrigRuleId),
	WebServers = try
		RuleId =  mydlp_mnesia:get_rule_id_by_orig_id(OrigRuleId),
		mydlp_mnesia:get_web_servers_by_rule_id(RuleId)
	catch Class:Error -> ?ERROR_LOG("Web: Can not get web servers  "
			"Class: ["?S"]. Error: ["?S"].~n"
			"Stack trace: "?S"~nState: "?S"~n",	
			[Class, Error, erlang:get_stacktrace(), State]),
		[] end,

	case WebServers of
		[] -> push_opr_log(OrigRuleId, GroupId, ?DISCOVERY_FINISHED),
			{noreply, State};
		_ -> 
			{ok, Timer} = timer:send_after(60000, {is_finished, OrigRuleId}),
			TimerDict1 = dict:store(OrigRuleId, Timer, TimerDict),
			lists:map(fun(W) -> q(W#web_server.id, W#web_server.start_path, OrigRuleId) end, WebServers),
			{noreply, State#state{timer_dict=TimerDict1}}
	end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({http, {RequestId, Result}}, #state{head_requests=HeadT, get_requests=GetT} = State) ->
	try	case gb_trees:is_defined(RequestId, HeadT) of
			true -> State1 =  handle_head(RequestId, Result, State), {noreply, State1};
		false -> case gb_trees:is_defined(RequestId, GetT) of
			true -> State1 = handle_get(RequestId, Result, State), {noreply, State1};
		false -> {noreply, State} end end
	catch Class:Error -> ?ERROR_LOG("Web: Discover Queue Consume: Error occured: "
		"Class: ["?S"]. Error: ["?S"].~n"
		"Stack trace: "?S"~nRequestId: "?S" Result: "?S"~nState: "?S"~n ",	
		[Class, Error, erlang:get_stacktrace(), RequestId, Result, State]),
		{noreply, State}
	end;

handle_info({is_finished, RuleId}, #state{timer_dict=TimerDict, paused_queue=PausedQ, rule_age=RuleAge}=State) ->
	case get_discovery_status(RuleId) of
		{_, GroupId} ->
			NowS = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
			Age = case gb_trees:is_defined(RuleId, RuleAge) of
				true -> gb_trees:get(RuleId, RuleAge);
				false -> 0 end, 
			case dict:find(RuleId, TimerDict) of
				{ok, Timer} -> (catch timer:cancel(Timer));
				_ -> ok end,
			case ((NowS - Age) > 180) of
				true -> 
					TimerDict1 = dict:store(RuleId, none, TimerDict),
					control_rule_status(RuleId, GroupId, PausedQ),
					{noreply, State#state{timer_dict=TimerDict1}};
				false -> {ok, Timer1} = timer:send_after(60000, {is_finished, RuleId}),
					{noreply, State#state{timer_dict=dict:store(RuleId, Timer1, TimerDict)}}
			end;
		_ -> {noreply, State}
	end;

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) -> 
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

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
	inets:start(),
	{ok, #state{discover_queue=queue:new(), paused_queue=queue:new(),
			head_requests=gb_trees:empty(), get_requests=gb_trees:empty(), rule_age=gb_trees:empty(), timer_dict=dict:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

get_discovery_status(RuleId) ->
        case catch mydlp_mnesia:get_discovery_status(RuleId) of
                none -> none;
                {_, _GroupId} = R -> R;
                _Else -> none end.

is_paused_or_stopped_by_rule_id(RuleId) ->
	case get_discovery_status(RuleId) of
		{system_paused, _} -> paused;
		{user_paused, _} -> paused;
		{system_stopped, _} -> stopped;
		{user_stopped, _} -> stopped;
		_ -> none
	end.

control_rule_status(RuleId, GroupId, Q) ->
	case queue:out(Q) of
		{{value, {_, _, _, RuleIndex, _}}, Q1} ->
			case RuleIndex of
				RuleId -> push_opr_log(RuleId, GroupId, ?DISCOVERY_PAUSED);
				_ -> control_rule_status(RuleId, GroupId, Q1)
			end;
		{empty, _Q2} ->
			push_opr_log(RuleId, GroupId, ?DISCOVERY_FINISHED)
	end.
push_opr_log(RuleId, GroupId, Message) ->
	Time = erlang:universaltime(),
	OprLog = #opr_log{time=Time, channel=remote_discovery, rule_id=RuleId, message_key=Message, group_id=GroupId},
	?DISCOVERY_OPR_LOG(OprLog).


drop_items_by_rule_id(RuleId, Q) -> drop_items_by_rule_id(RuleId, Q, queue:new()).

drop_items_by_rule_id(RuleId, Q, AccQ) ->
	case queue:out(Q) of
		 {{value, {_WebServerId, _ParentId, _PagePath, RuleIndex, _Depth}=Item}, Q1} -> 
			AccQ1 = case RuleIndex of
					RuleId -> AccQ;
					_ -> queue:in(Item, AccQ)
				end,
			drop_items_by_rule_id(RuleId, Q1, AccQ1);
		{empty, _Q2} -> AccQ
	end.

get_base_url(W) ->
	case W of
		#web_server{proto="https", port=443} -> 
			"https://" ++ W#web_server.address ++ "/";
		#web_server{proto="http", port=80} -> 
			"http://" ++ W#web_server.address ++ "/";
		_ -> W#web_server.proto ++ "://" ++ W#web_server.address ++ ":" ++ 
			integer_to_list(W#web_server.port) ++ "/"  end.

get_url(WebServerId, PagePath) ->
	W = mydlp_mnesia:get_web_server(WebServerId),
	BaseURL = get_base_url(W),
	Path = drop_base(PagePath, BaseURL),
	case Path of
		external -> external;
		P -> BaseURL ++ P end.

drop_base("http://" ++ _, _BaseUrl) -> external;
drop_base("https://" ++ _, _BaseUrl) -> external;
drop_base("/" ++ Path, _BaseUrl) -> Path;
drop_base(PagePath, BaseUrl) -> 
	case string:str(PagePath, BaseUrl) of
		1 -> string:substr(PagePath, length(BaseUrl) + 1);
		_ -> PagePath end.

add_link_to_path(WebServerId, PagePath, Link) ->
	W = mydlp_mnesia:get_web_server(WebServerId),
	BaseURL = get_base_url(W),
	case drop_base(Link, BaseURL) of
		external -> external;
		P -> add_link_to_path1(PagePath, P) end.

add_link_to_path1(PagePath, LinkPath) ->
	case string:rchr(PagePath, $/) of
		0 -> LinkPath;
		I -> string:substr(PagePath, 1, I) ++ LinkPath end.

web_entry(WebServerId, PagePath, ParentId, RuleId) ->
	EntryId = {WebServerId, PagePath, RuleId},
	case mydlp_mnesia:get_web_entry(EntryId) of
		none ->	E = #web_entry{entry_id=EntryId, parent_id=ParentId},
			mydlp_mnesia:add_web_entry(E), %% bulk write may improve performance
			E;
		#web_entry{} = WE -> WE end.

fetch_meta(WebServerId, PagePath) ->
	URL = get_url(WebServerId, PagePath),
	case URL of
		external -> external;
		_ -> httpc:request(head, {URL, []}, [], [{sync, false}]) end.

is_changed(WebServerId, PagePath, ParentId, RuleId, Headers) ->
	WE = web_entry(WebServerId, PagePath, ParentId, RuleId),
	IsChanged = is_changed(WE, Headers),
	WE1 = update_web_entry(WE, Headers),
	mydlp_mnesia:add_web_entry(WE1),
	IsChanged.

is_changed(#web_entry{last_modified=undefined}, _Headers) -> true;
is_changed(#web_entry{maxage=undefined} = WE, _Headers) -> is_expired(WE);
is_changed(#web_entry{maxage=MA, last_modified=LM} = WE, _Headers) ->
	NowS = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	LMS = calendar:datetime_to_gregorian_seconds(LM),
	case ( (NowS - LMS) > MA) of
		true -> is_expired(WE);
		false -> true end.

is_expired(#web_entry{expires=undefined}) -> true;
is_expired(#web_entry{expires=ED}) ->
	NowS = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	EDS =  calendar:datetime_to_gregorian_seconds(ED),
	(NowS > EDS).

update_web_entry(WE, [{"content-type", "text/html" ++ _}|Headers]) -> 
	update_web_entry(WE#web_entry{is_html=true}, Headers);
update_web_entry(WE, [{"content-length", Size}|Headers]) -> 
	S = try	list_to_integer(Size)
	catch _:_ -> undefined end,
	update_web_entry(WE#web_entry{size=S}, Headers);
update_web_entry(WE, [{"last-modified", LMD}|Headers]) ->
	LM = case catch httpd_util:convert_request_date(lists:flatten(LMD)) of
		bad_date -> undefined;
		{{_Year,_Month,_Date},{_Hour,_Min,_Sec}} = D -> D;
		_Else -> undefined end,
	update_web_entry(WE#web_entry{last_modified=LM}, Headers);
update_web_entry(WE, [{"expires", ED} |Headers]) ->
	E = case catch httpd_util:convert_request_date(lists:flatten(ED)) of
		bad_date -> undefined;
		{{_Year,_Month,_Date},{_Hour,_Min,_Sec}} = D -> D;
		_Else -> undefined end,
	update_web_entry(WE#web_entry{expires=E}, Headers);
update_web_entry(WE, [{"cache-control",CacheS}| Headers]) ->
	MA = case string:str(CacheS, "max-age=") of
		0 -> undefined;
		I -> 	NextStr = string:substr(CacheS, I + 8),
			case string:chr(NextStr, $\s) of
			0 -> undefined;
			I2 ->	case string:substr(NextStr, 1, I2 - 1) of
				"" -> undefined;
				Else -> try list_to_integer(Else)
					catch _:_ -> undefined end
				end
			end
		end,
	update_web_entry(WE#web_entry{maxage=MA}, Headers);
update_web_entry(WE, [_|Headers]) -> update_web_entry(WE, Headers);
update_web_entry(WE, []) -> WE.

update_rule_age(RuleId, RuleAge) ->
	Nows = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	case gb_trees:is_defined(RuleId, RuleAge) of
		false -> gb_trees:enter(RuleId, Nows, RuleAge);
		true -> gb_trees:update(RuleId, Nows, RuleAge) end.
	
fetch_data(WebServerId, PagePath) ->
	URL = get_url(WebServerId, PagePath),
	httpc:request(get, {URL, []}, [], [{sync, false}]).

handle_head(_RequestId, {error,no_scheme}, State) -> State;

handle_head(_RequestId, {{_, 404, _}, _Headers, _}, State) -> State;
	
handle_head(RequestId, {{_, 200, _}, Headers, _}, #state{head_requests=HeadT, get_requests=GetT, rule_age=RuleAge} = State) ->
	{WebServerId, ParentId, PagePath, RuleId, Depth} = gb_trees:get(RequestId, HeadT),
	RuleAge1 = update_rule_age(RuleId, RuleAge),
	HeadT1 = gb_trees:delete(RequestId, HeadT),
	EntryId = {WebServerId, PagePath, RuleId},
	GetT1 = case is_changed(WebServerId, PagePath, ParentId, RuleId, Headers) of
		true -> case has_exceed_maxobj_size(EntryId) of
			false -> {ok, RequestId2} = fetch_data(WebServerId, PagePath),
				gb_trees:enter(RequestId2, {WebServerId, ParentId, PagePath, RuleId, Depth}, GetT);
			true -> GetT end;
		false -> case is_html(EntryId) of
			true -> discover_cached_page(EntryId, RuleId, Depth - 1);
			false -> ok end,
			GetT end,
	State#state{head_requests=HeadT1, get_requests=GetT1, rule_age=RuleAge1}.

handle_get(RequestId, {{_, 200, _}, _Headers, Data}, #state{get_requests=GetT, rule_age=RuleAge} = State) ->
	{WebServerId, _ParentId, PagePath, RuleId, Depth} = gb_trees:get(RequestId, GetT),
	RuleAge1 = update_rule_age(RuleId, RuleAge),
	GetT1 = gb_trees:delete(RequestId, GetT),
	EntryId = {WebServerId, PagePath, RuleId},
	discover_item(EntryId, Data),
	case is_html(EntryId) of 
		true -> discover_links(EntryId, Data, Depth - 1);
		false -> ok end,
	State#state{get_requests=GetT1, rule_age=RuleAge1}.

get_fn(WebServerId, PagePath) -> 
	URL = get_url(WebServerId, PagePath),
	case string:rchr(URL, $/) of
		0 -> "data";
		I -> 	NextStr = string:substr(URL, I+1),
			case string:tokens(NextStr, "?=;&/") of
				[FN|_] -> case mydlp_api:prettify_uenc_data(FN) of
					{ok, PFN} -> mydlp_api:filename_to_list(PFN);
					_ -> "data" end;
				_ -> "data" 
				end
		end.

discover_item({WebServerId, PagePath, RuleId}, Data) ->
	try	timer:sleep(20),
		{ok, ObjId} = mydlp_container:new(),
		ok = mydlp_container:setprop(ObjId, "channel", "remote_discovery"),
		ok = mydlp_container:setprop(ObjId, "web_server_id", WebServerId),
		RuleIndex = mydlp_mnesia:get_rule_id_by_orig_id(RuleId),
		ok = mydlp_container:setprop(ObjId, "rule_index", RuleIndex),
		{_, GroupId} = get_discovery_status(RuleId),
		ok = mydlp_container:setprop(ObjId, "group_id", GroupId),
		ok = mydlp_container:setprop(ObjId, "page_path", PagePath),
		ok = mydlp_container:setprop(ObjId, "filename_unicode", get_fn(WebServerId, PagePath)),
		ok = mydlp_container:push(ObjId, Data),
		ok = mydlp_container:eof(ObjId),
		{ok, _Action} = mydlp_container:aclq(ObjId),
		ok = mydlp_container:destroy(ObjId)
	catch Class:Error ->
		?ERROR_LOG("DISCOVER FILE: Error occured: Class: ["?S"]. Error: ["?S"].~n"
				"Stack trace: "?S"~nWebServerId: "?S" PagePath: ["?S"].~n",
			[Class, Error, erlang:get_stacktrace(), WebServerId, PagePath])
	end,
	ok.

is_html(EntryId) ->
	W = mydlp_mnesia:get_web_entry(EntryId),
	case W#web_entry.is_html of
		true -> true;
		_Else -> false end.

has_exceed_maxobj_size(EntryId) ->
	W = mydlp_mnesia:get_web_entry(EntryId),
	
	case W#web_entry.size of
		undefined -> false;
		_ -> ( W#web_entry.size > ?CFG(maximum_object_size) ) end.

discover_links(_, _, 0) -> ok;
discover_links({_WebServerId, _PagePath, _RuleId} = EntryId, Data, Depth) ->
	Links = mydlp_tc:extract_links(Data),
	schedule_links(Links, EntryId, Depth).

schedule_links([L|Links], EntryId, Depth) when is_binary(L) ->
	schedule_links([binary_to_list(L)|Links],  EntryId, Depth);
schedule_links([L|Links], {WebServerId, PagePath, RuleId} = EntryId, Depth) when is_list(L) ->
	case add_link_to_path(WebServerId, PagePath, L) of
		external -> ok;
		LPath -> q(WebServerId, EntryId, LPath, RuleId, Depth) end,
	schedule_links(Links, EntryId, Depth);
schedule_links([], _, _) -> ok.

discover_cached_page(_, _, 0) -> ok;
discover_cached_page({_WebServerId, _PagePath, RuleId} = EntryId, RuleId, Depth) ->
	Links = mydlp_mnesia:web_entry_list_links(EntryId),
	schedule_links(Links, EntryId, Depth).

set_discover_inprog() -> ok.

unset_discover_inprog() ->
	reset_discover_cache(),
	ok.
filter_discover_cache(RuleId) ->
	CS = get(cache),
	CS1 = gb_sets:filter(fun({_, _, RuleIndex}) -> RuleIndex /= RuleId end, CS),
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

-endif.	
