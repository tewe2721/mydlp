%%%
%%%    Copyright (C) 2010 Ozgen Muzac <ozgen@mydlp.com>
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
%%% @author Ozgen Muzac <ozgen@mydlp.com>
%%% @copyright 2013, Ozgen Muzac
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-module(mydlp_discovery_manager).
-author("ozgen@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").


%% API
-export([start_link/0,
	start_on_demand_discovery/1,
	stop_discovery_on_demand/1,
	pause_discovery_on_demand/1
	]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
	discovery_dict
}).

-define(DISC, discovering).
-define(ON_DEMAND_DISC, on_demand_discovering).
-define(SYSTEM_PAUSED, system_paused).
-define(USER_PAUSED, user_paused).
-define(SYSTEM_STOPPED, system_stopped).
-define(USER_STOPPED, user_stopped).
-define(REMOTE_DISCOVERY, mydlp_discover_rfs).
-define(EP_DISCOVERY, hede).
-define(TRY_COUNT, 5).

%%%%%%%%%%%%%  API

start_on_demand_discovery(RuleOrigId) ->
	RuleId = mydlp_mnesia:get_rule_id_by_orig_id(RuleOrigId),
	gen_server:cast(?MODULE, {start_on_demand, RuleId}),
	ok.

stop_discovery_on_demand(RuleOrigId) ->
	RuleId = mydlp_mnesia:get_rule_id_by_orig_id(RuleOrigId),
	gen_server:cast(?MODULE, {stop_on_demand, RuleId}),
	ok.

pause_discovery_on_demand(RuleOrigId) ->
	RuleId = mydlp_mnesia:get_rule_id_by_orig_id(RuleOrigId),
	gen_server:cast(?MODULE, {pause_on_demand, RuleId}),
	ok.

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.


handle_cast({start_on_demand, RuleId}, #state{discovery_dict=Dict}=State) ->
	erlang:display({bs, dict:to_list(Dict)}),
	Dict2 = call_start_discovery_on_target(RuleId, Dict, true),
	erlang:display({as, dict:to_list(Dict2)}),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({pause_on_demand, RuleId}, #state{discovery_dict=Dict}=State) ->
	erlang:display({bp, dict:to_list(Dict)}),
	Dict2 = call_pause_discovery_on_target(RuleId, Dict, true),
	erlang:display({ap, dict:to_list(Dict2)}),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({stop_on_demand, RuleId}, #state{discovery_dict=Dict}=State) ->
	erlang:display(stop),
	Dict2 = case dict:find(RuleId, Dict) of
			{ok, {_, ReportId}} -> dict:store(RuleId, {?USER_STOPPED, ReportId}, Dict);
			_ -> Dict
		end, 
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({manage_schedules, Schedules}, #state{discovery_dict=Dict}=State) ->
	Dict2 = case Schedules of
		none -> Dict;
		_ -> edit_dictionary(Schedules, Dict)
	end,
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({start_discovery, RuleId}, #state{discovery_dict=Dict}=State) ->
	Dict2 = call_start_discovery_on_target(RuleId, Dict, false),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({stop_discovery, RuleId}, #state{discovery_dict=Dict}=State) ->
	Dict2 = case dict:find(RuleId, Dict) of
			{ok, {_, ReportId}} -> dict:store(RuleId, {?SYSTEM_STOPPED, ReportId});
			_ -> Dict
		end,	
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({pause_discovery, RuleId}, #state{discovery_dict=Dict}=State) ->
	Dict2 = call_pause_discovery_on_target(RuleId, Dict, false),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({continue_discovery, RuleId}, #state{discovery_dict=Dict}=State) ->

	{_, ReportId} = dict:fetch(RuleId, Dict),
	Dict2 = dict:store(RuleId, {?DISC, ReportId}, Dict),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({is_paused, RuleId}, #state{discovery_dict=Dict}=State) ->
	case dict:find(RuleId, Dict) of
		{ok, {?DISC, _}} -> true;
		{ok, {?ON_DEMAND_DISC, _}} -> true;
		_ -> false
	end,
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(startup, State) ->
	start_at_exact_hour(),
	{noreply, State};

handle_info(start_discovery_scheduling, State) ->
	start_discovery_scheduling(),
	{noreply, State};

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

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
	timer:send_after(60000, startup),
	{ok, #state{discovery_dict=dict:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

start_discovery(RuleId) -> gen_server:cast(?MODULE, {start_discovery, RuleId}).

stop_discovery(RuleId) -> gen_server:cast(?MODULE, {stop_discovery, RuleId}).

pause_discovery(RuleId) -> gen_server:cast(?MODULE, {pause_discovery, RuleId}).

continue_discovery(RuleId) -> gen_server:cast(?MODULE, {continue_discovery, RuleId}).

call_start_discovery_on_target(RuleId, Dict, IsOnDemand) ->
	case mydlp_mnesia:get_rule_channel(RuleId) of
		remote_discovery -> call_remote_storage_discovery(RuleId, Dict, IsOnDemand);
		discovery -> call_ep_discovery(RuleId);
		C -> ?ERROR_LOG("Unknown Rule Type: "?S"", [C]), 
			Dict
	end.

call_remote_storage_discovery(RuleId, Dict, IsOnDemand) -> 
	case get_discovery_status(RuleId, Dict) of
		{disc, _} -> Dict;
		{paused, ReportId} -> gen_server:cast(?REMOTE_DISCOVERY, {continue_discovering}),
			case IsOnDemand of
				true -> dict:store(RuleId, {?ON_DEMAND_DISC, ReportId}, Dict);
				false -> dict:store(RuleId, {?DISC, ReportId}, Dict)
			end;
		_ -> RId = generate_report_id(RuleId), % Discovering should be start with new Report id.
			gen_server:cast(?REMOTE_DISCOVERY, {start_by_rule_id, RuleId, RId}),
			case IsOnDemand of
				true -> dict:store(RuleId, {?ON_DEMAND_DISC, RId}, Dict);
				false -> dict:store(RuleId, {?DISC, RId}, Dict)
			end
	end.

call_ep_discovery(_RuleId) -> ok.

call_pause_discovery_on_target(RuleId, Dict, IsOnDemand) -> 
	case mydlp_mnesia:get_rule_channel(RuleId) of
		remote_discovery -> set_pause_remote_storage_discovery(RuleId, Dict, IsOnDemand);
		discovery -> call_pause_ep_discovery(RuleId)
	end.

set_pause_remote_storage_discovery(RuleId, Dict, IsOnDemand) ->
	case get_discovery_status(RuleId, Dict) of
		{disc, ReportId} -> 
			case IsOnDemand of 
				true -> dict:store(RuleId, {?USER_PAUSED, ReportId}, Dict);
				false -> dict:store(RuleId, {?SYSTEM_PAUSED, ReportId}, Dict) end;
		_ -> Dict
	end.

call_pause_ep_discovery(_RuleId) -> ok.
	
call_stop_discovery_on_target(_RuleId, _Dict, _IsOnDemand) -> ok.

generate_report_id(RuleId) ->
	integer_to_list(RuleId) ++ "_" ++ integer_to_list(calendar:datetime_to_gregorian_seconds(erlang:localtime())).

get_discovery_status(RuleId, Dict) ->
	case dict:find(RuleId, Dict) of
		{ok, {?DISC, ReportId}} -> {disc, ReportId};
		{ok, {?ON_DEMAND_DISC, ReportId}} -> {disc, ReportId};
		{ok, {?SYSTEM_PAUSED, ReportId}} -> {paused, ReportId};
		{ok, {?USER_PAUSED, ReportId}} -> {paused, ReportId};
		{ok, _} -> stop;
		_ -> none
	end.

edit_dictionary([RuleId|Rest], Dict) ->
	case mydlp_mnesia:get_availabilty_by_rule_id(RuleId) of
		true -> set_as_scheduling_available(RuleId, Dict);
		false -> ok
	end,
	edit_dictionary(Rest, Dict);
edit_dictionary([], Dict) -> control_already_scheduled_discoveries(Dict).

control_already_scheduled_discoveries(Dict) ->
	DiscoveryList = dict:to_list(Dict),
	edit_discoveries(DiscoveryList).

edit_discoveries([{RuleId, {?SYSTEM_PAUSED, _}}|R]) ->
	case mydlp_mnesia:get_availabilty_by_rule_id(RuleId) of
		true -> continue_discovery(RuleId);
		false -> ok
	end,
	edit_discoveries(R);
edit_discoveries([{RuleId, {?DISC, _}}|R]) ->
	case mydlp_mnesia:get_availabilty_by_rule_id(RuleId) of
		true -> ok;
		false -> pause_discovery(RuleId)
	end,
	edit_discoveries(R);
edit_discoveries([_|R]) -> edit_discoveries(R);
edit_discoveries([]) -> ok.

set_as_scheduling_available(RuleId, Dict) ->
	case dict:find(RuleId, Dict) of
		{ok, {?DISC, RuleId}} -> stop_discovery(RuleId);
		_ -> ok
	end,
	start_discovery(RuleId).

start_at_exact_hour() ->
	{_D, {_H, M, S}} = erlang:localtime(),
	erlang:display({exactHour, M, S}),
	case M of 
		0 -> timer:send_after(0, start_discovery_scheduling);
		_ -> Remaining = (((59-M)*60)+S)*1000,
			timer:send_after(Remaining, start_discovery_scheduling)
	end.

start_discovery_scheduling() ->
	{_D, {H, _M, _S}} = erlang:localtime(),
	Schedules = mydlp_mnesia:get_schedules_by_hour(H),
	gen_server:cast(?MODULE, {manage_schedules, Schedules}),
	timer:send_after(3600000, start_discovery_scheduling).








