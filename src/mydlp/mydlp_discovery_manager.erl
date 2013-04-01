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
	stop/0,
	start_on_demand_discovery/1,
	stop_discovery_on_demand/1,
	pause_discovery_on_demand/1,
	get_group_id/1,
	start_discovery/1,
	stop_discovery/1,
	pause_discovery/1,
	start_at_exact_hour/0,
	start_discovery_scheduling/0
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
	discovery_dict,
	timer_dict
}).

-define(DISC, discovering).
-define(ON_DEMAND_DISC, on_demand_discovering).
-define(SYSTEM_PAUSED, system_paused).
-define(USER_PAUSED, user_paused).
-define(SYSTEM_STOPPED, system_stopped).
-define(USER_STOPPED, user_stopped).
-define(FINISHED, finished).
-define(REMOTE_DISCOVERY, mydlp_discover_rfs).
-define(WEB_DISCOVERY, mydlp_discover_web).
-define(EP_DISCOVERY, hede).
-define(TRY_COUNT, 5).

-define(REPORT_STATUS_DISC, "running").
-define(REPORT_STATUS_STOPPED, "stopped").
-define(REPORT_STATUS_PAUSED_SYSTEM, "paused_system").
-define(REPORT_STATUS_PAUSED_USER, "paused_user").

-define(START_EP_COMMAND, start_discovery).
-define(STOP_EP_COMMAND, stop_discovery).
-define(PAUSE_EP_COMMAND, pause_discovery).
-define(CONTINUE_EP_COMMAND, continue_discovery).

-define(EP_CONTROL_TIME, ?CFG(sync_interval)).
-define(EP_DISC_FINISHED, "ep_finished").
-define(EP_DISC_PAUSED, "ep_paused").

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

get_group_id(RuleId) -> gen_server:call(?MODULE, {get_group_id, RuleId}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call({get_group_id, RuleId}, _From, #state{discovery_dict=Dict}=State) ->
	Reply = case dict:find(RuleId, Dict) of
			{ok, {_, GroupId}} -> GroupId;
			_ -> -1
		end,
	{reply, Reply, State};

handle_call({is_paused_or_stopped, RuleId}, _From, #state{discovery_dict=Dict}=State) ->
	Reply = case dict:find(RuleId, Dict) of
			{ok, {?SYSTEM_PAUSED, _}} -> paused;
			{ok, {?USER_PAUSED, _}} -> paused;
			{ok, {?SYSTEM_STOPPED, _}} -> stopped;
			{ok, {?USER_STOPPED, _}} -> stopped;
			_ -> none
		end,
	{reply, Reply, State};

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
	erlang:display({bst, dict:to_list(Dict)}),
	Dict2 = call_stop_discovery_on_target(RuleId, Dict, true),
	erlang:display({ast, dict:to_list(Dict2)}),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({manage_schedules, Schedules}, #state{discovery_dict=Dict}=State) ->
	erlang:display({manage_schedules, Schedules}),
	edit_dictionary(Schedules, Dict),
	{noreply, State};

handle_cast({start_discovery, RuleId}, #state{discovery_dict=Dict}=State) ->
	erlang:display({bs1, dict:to_list(Dict)}),
	Dict2 = call_start_discovery_on_target(RuleId, Dict, false),
	erlang:display({as1, dict:to_list(Dict2)}),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({stop_discovery, RuleId}, #state{discovery_dict=Dict}=State) ->
	erlang:display({bst1, dict:to_list(Dict)}),
	Dict2 = call_stop_discovery_on_target(RuleId, Dict, false),
	erlang:display({ast1, dict:to_list(Dict2)}),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({pause_discovery, RuleId}, #state{discovery_dict=Dict}=State) ->
	erlang:display({bp, dict:to_list(Dict)}),
	Dict2 = call_pause_discovery_on_target(RuleId, Dict, false),
	erlang:display({ap, dict:to_list(Dict2)}),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({continue_discovery, RuleId}, #state{discovery_dict=Dict}=State) ->
	Dict2 = call_continue_discovery_on_target(RuleId, Dict),
	{noreply, State#state{discovery_dict=Dict2}};

handle_cast({cancel_timer, RuleId}, #state{timer_dict=TimerDict}=State) ->
	case dict:find(RuleId, TimerDict) of
		{ok, TRef} -> (catch timer:cancel_timer(TRef));
		_ -> ok
	end,
	TimerDict1 = dict:erase(RuleId, TimerDict),
	{noreply, State#state{timer_dict=TimerDict1}};

handle_cast({create_timer, RuleId}, #state{timer_dict=TimerDict}=State) ->
	case dict:find(RuleId, TimerDict) of
		{ok, TRef} -> timer:cancel(TRef);
		_ -> ok
	end,

	TimerDict1 = case mydlp_mnesia:get_rule_channel(RuleId) of
			remote_discovery -> Timer = timer:send_after(60000, {is_discovery_finished, RuleId}),
					dict:store(RuleId, Timer, TimerDict);
			discovery -> erlang:display({discovery_timer, ?EP_CONTROL_TIME}),Timer = timer:send_after(?EP_CONTROL_TIME, {is_ep_discovery_finished, RuleId}),
					dict:store(RuleId, Timer, TimerDict)
		end,
	{noreply, State#state{timer_dict=TimerDict1}};

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

handle_info({is_discovery_finished, RuleId}, #state{discovery_dict=DiscDict, timer_dict=TimerDict}=State) ->
	erlang:display("TIMER IS ELAPSED"),
	erlang:display(dict:to_list(TimerDict)),
	case dict:find(RuleId, TimerDict) of
		error -> {noreply, State};
		{ok, Timer} ->
			try
			cancel_timer(Timer),
			Reply = mydlp_discover_fs:is_discovery_finished(RuleId),
			erlang:display({is_finished_resp, Reply}),
			case Reply of
				true ->	 
					TimerDict1 = dict:erase(RuleId, TimerDict),
					{ok, {_, GroupId}} = dict:find(RuleId, DiscDict),
					mydlp_discover_rfs:release_mount_by_rule_id(RuleId),
					update_report_as_finished(GroupId),
					DiscDict1 = case mydlp_mnesia:get_waiting_schedule_by_rule_id(RuleId) of
							none ->	dict:store(RuleId,{?FINISHED, GroupId}, DiscDict);
							GId -> call_start_discovery_by_rule_id(RuleId, GId, DiscDict, false)
						end,
					{noreply, State#state{discovery_dict=DiscDict1, timer_dict=TimerDict1}};
				false -> 
					Timer1 = timer:send_after(60000, {is_discovery_finished, RuleId}),
					TimerDict1 = dict:store(RuleId, Timer1, TimerDict),
					{noreply, State#state{timer_dict=TimerDict1}}
			end
			catch _Class:_Error ->
				erlang:display("SOME EXCEPTION IS OCCURED"),
				Timer2 = timer:send_after(60000, {is_discovery_finished, RuleId}),
				TimerDict2 = dict:store(RuleId, Timer2, TimerDict),
				{noreply, State#state{discovery_dict=DiscDict, timer_dict=TimerDict2}}
			end
	end;

handle_info({is_ep_discovery_finished, RuleId}, #state{discovery_dict=DiscDict, timer_dict=TimerDict}=State) ->
	erlang:display("EP TIMER IS ELAPSED"),
	case dict:find(RuleId, TimerDict) of
		error -> {noreply, State};
		{ok, Timer} ->
			try
			cancel_timer(Timer),
			{ok, {_, GroupId}} = dict:find(RuleId, DiscDict),
			[Endpoints] = mydlp_mnesia:get_endpoints_by_rule_id(RuleId), 
			erlang:display({endpoints, Endpoints}),
			case mydlp_mysql:is_all_ep_discovery_finished(GroupId, Endpoints, ?EP_DISC_FINISHED) of
				true -> TimerDict1 = dict:erase(RuleId, TimerDict),
					update_report_as_finished(GroupId),
					DiscDict1 = case mydlp_mnesia:get_waiting_schedule_by_rule_id(RuleId) of
							none -> dict:store(RuleId, {?FINISHED, GroupId}, DiscDict);
							GId -> call_start_discovery_on_ep(RuleId, GId, DiscDict, false)
						end,
					{noreply, State#state{discovery_dict=DiscDict1, timer_dict=TimerDict1}};
				false -> Timer1 = timer:send_after(?EP_CONTROL_TIME, {is_ep_discovery_finished, RuleId}),
					TimerDict1 = dict:store(RuleId, Timer1, TimerDict),
					{noreply, State#state{timer_dict=TimerDict1}}
			end
			catch _Class:_Error ->
				Timer2 = timer:send_after(?EP_CONTROL_TIME, {is_ep_discovery_finished, RuleId}),
				TimerDict2 = dict:store(RuleId, Timer2, TimerDict),
				{noreply, State#state{timer_dict=TimerDict2}}	
			end
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
	timer:send_after(6000, startup),
	{ok, #state{discovery_dict=dict:new(), timer_dict=dict:new()}}.

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
		discovery -> call_ep_discovery(RuleId, Dict, IsOnDemand);
		C -> ?ERROR_LOG("Unknown Rule Type: "?S"", [C]), 
			Dict
	end.

call_remote_storage_discovery(RuleId, Dict, IsOnDemand) -> 
	Resp = get_discovery_status(RuleId, Dict),
	erlang:display({status, Resp}),
	case Resp of
		{disc, GroupId} -> 
			case IsOnDemand of
				true -> Dict;
				false -> break_discovery(RuleId, GroupId, Dict)
			end;
		{paused, GroupId} -> call_continue_discovery_on_remote(RuleId),
			case IsOnDemand of
				true -> update_discovery_report(GroupId, ?REPORT_STATUS_DISC),
					dict:store(RuleId, {?ON_DEMAND_DISC, GroupId}, Dict);
				false -> break_discovery(RuleId, GroupId, Dict) % This case looks like impossible
			end;
		{user_paused, GroupId} ->
			case IsOnDemand of
				true -> % means that user paused discovery while ago and now starts again
					update_discovery_report(GroupId, ?REPORT_STATUS_DISC),
					call_continue_discovery_on_remote(RuleId),
					dict:store(RuleId, {?ON_DEMAND_DISC, GroupId}, Dict);
				false ->% means that user paused discovery while ago and now it is time to schedule
					% New discovery with a new report id. Ensure that last discovery is stopped.
					break_discovery(RuleId, GroupId, Dict)
			end; 
		{user_disc, _GroupId} ->
			case IsOnDemand of 
				true -> Dict; %looks impossible
				false -> register_schedules_for_future(RuleId, remote_discovery),% This will be stored and will be runned when discovery finish 
					Dict
			end;
		_ -> % Discovering should be start with new Report id.
			GId = generate_group_id(RuleId, remote_discovery),
			call_start_discovery_by_rule_id(RuleId, GId, Dict, IsOnDemand)
	end.

call_ep_discovery(RuleId, Dict, IsOnDemand) -> 
	Resp = get_discovery_status(RuleId, Dict),
	case Resp of
		{disc, GroupId} -> 
			case IsOnDemand of
				true -> Dict;
				false -> break_ep_discovery(RuleId, GroupId, Dict)
			end;
		{paused, GroupId} -> 
			case IsOnDemand of
				true -> update_discovery_report(GroupId, ?REPORT_STATUS_DISC),
					set_command_to_endpoints(RuleId, ?CONTINUE_EP_COMMAND, [{groupId, GroupId}]),
					dict:store(RuleId, {?ON_DEMAND_DISC, GroupId}, Dict);
				false -> break_ep_discovery(RuleId, GroupId, Dict)
			end;
		{user_paused, GroupId} -> 
			case IsOnDemand of
				true -> % means that user paused discovery while ago and now starts again
					update_discovery_report(GroupId, ?REPORT_STATUS_DISC),
					set_command_to_endpoints(RuleId, ?CONTINUE_EP_COMMAND, [{groupId, GroupId}]),
					dict:store(RuleId, {?ON_DEMAND_DISC, GroupId}, Dict);
				false ->% means that user paused discovery while ago and now it is time to schedule
					% New discovery with a new report id. Ensure that last discovery is stopped.
					break_ep_discovery(RuleId, GroupId, Dict)
			end;	
		{user_disc, _GroupId} ->
			case IsOnDemand of 
				true -> Dict; %looks impossible
				false -> register_schedules_for_future(RuleId, discovery),% This will be stored and will be runned when discovery finish 
					Dict
			end;
		_ -> GId = generate_group_id(RuleId, discovery),
			call_start_discovery_on_ep(RuleId, GId, Dict, IsOnDemand)
	end.

call_continue_discovery_on_remote(RuleId) ->
	gen_server:cast(?REMOTE_DISCOVERY, {continue_discovering, RuleId}),
	gen_server:cast(?WEB_DISCOVERY, {continue_discovering, RuleId}).

call_start_discovery_by_rule_id(RuleId, GroupId, Dict, IsOnDemand) -> 
	gen_server:cast(?REMOTE_DISCOVERY, {start_by_rule_id, RuleId, GroupId}),
	gen_server:cast(?WEB_DISCOVERY, {start_by_rule_id, RuleId, GroupId}),
	create_timer(RuleId),
	case IsOnDemand of
		true -> dict:store(RuleId, {?ON_DEMAND_DISC, GroupId}, Dict);
		false -> dict:store(RuleId, {?DISC, GroupId}, Dict)
	end.

call_start_discovery_on_ep(RuleId, GroupId, Dict, IsOnDemand) ->
	set_command_to_endpoints(RuleId, ?START_EP_COMMAND, [{groupId, GroupId}]),
	create_timer(RuleId),
	case IsOnDemand of
		true -> dict:store(RuleId, {?ON_DEMAND_DISC, GroupId}, Dict);
		false -> dict:store(RuleId, {?DISC, GroupId}, Dict)
	end.

call_pause_discovery_on_target(RuleId, Dict, IsOnDemand) -> 
	case mydlp_mnesia:get_rule_channel(RuleId) of
		remote_discovery -> call_pause_remote_storage_discovery(RuleId, Dict, IsOnDemand);
		discovery -> call_pause_ep_discovery(RuleId, Dict, IsOnDemand);
		C -> ?ERROR_LOG("Unknown Rule Type: "?S"", [C])
	end.

call_pause_remote_storage_discovery(RuleId, Dict, IsOnDemand) ->
	case get_discovery_status(RuleId, Dict) of
		{disc, GroupId} -> 
			cancel_timer(RuleId),
			mydlp_discover_fs:update_rule_status(RuleId, paused),
			case IsOnDemand of 
				true -> update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_USER),
					dict:store(RuleId, {?USER_PAUSED, GroupId}, Dict);
				false -> update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_SYSTEM),
					dict:store(RuleId, {?SYSTEM_PAUSED, GroupId}, Dict) end;
		{user_disc, GroupId} -> 
			case IsOnDemand of
				true -> cancel_timer(RuleId),
					mydlp_discover_fs:update_rule_status(RuleId, paused),
					update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_USER),
					dict:store(RuleId, {?USER_PAUSED, GroupId}, Dict);
				false -> Dict
			end;
		_ -> Dict
	end.

call_pause_ep_discovery(RuleId, Dict, IsOnDemand) ->
	case get_discovery_status(RuleId, Dict) of
		{disc, GroupId} ->
			set_command_to_endpoints(RuleId, ?PAUSE_EP_COMMAND, [{groupId, GroupId}]),
			case IsOnDemand of
				true -> update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_USER),
					dict:store(RuleId, {?USER_PAUSED, GroupId}, Dict);
				false -> update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_SYSTEM),	
					dict:store(RuleId, {?SYSTEM_PAUSED, GroupId}, Dict) end;
		{user_disc, GroupId} ->
			case IsOnDemand of
				true -> set_command_to_endpoints(RuleId, ?PAUSE_EP_COMMAND, [{groupId, GroupId}]),
					update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_USER),
					dict:store(RuleId, {?USER_PAUSED, GroupId}, Dict);
				false -> Dict
			end;
		_ -> Dict
	end.
	
call_stop_discovery_on_target(RuleId, Dict, IsOnDemand) ->
	case mydlp_mnesia:get_rule_channel(RuleId) of
		remote_discovery -> call_stop_remote_storage_discovery(RuleId, Dict, IsOnDemand);
		discovery -> call_stop_ep_discovery(RuleId, Dict, IsOnDemand);
		C -> ?ERROR_LOG("Unknown Rule Type: "?S"", [C])
	end. 

call_stop_remote_storage_discovery(RuleId, Dict, IsOnDemand) ->
	case get_discovery_status(RuleId, Dict) of
		none -> Dict;
		stop -> Dict;
		{_, GroupId} -> % discovering or paused
			mydlp_mnesia:del_fs_entries_by_rule_id(RuleId),
			update_report_as_finished(GroupId),
			%cancel_timer(RuleId),
			mydlp_discover_fs:update_rule_status(RuleId, stopped),
			%mydlp_discover_rfs:release_mount_by_rule_id(RuleId),
			case IsOnDemand of
				true -> dict:store(RuleId, {?USER_STOPPED, GroupId}, Dict);
				false -> dict:store(RuleId, {?SYSTEM_STOPPED, GroupId}, Dict) end
	end.

call_stop_ep_discovery(RuleId, Dict, IsOnDemand) ->
	case get_discovery_status(RuleId, Dict) of
		none -> Dict;
		stop -> Dict;
		{_, GroupId} ->
			update_report_as_finished(GroupId),
			set_command_to_endpoints(RuleId, ?STOP_EP_COMMAND, [{groupId, GroupId}]),
			case IsOnDemand of
				true -> dict:store(RuleId, {?USER_STOPPED, GroupId}, Dict);
				false -> dict:store(RuleId, {?SYSTEM_STOPPED, GroupId}, Dict) end
	end.
	
call_continue_discovery_on_target(RuleId, Dict) ->
	case mydlp_mnesia:get_rule_channel(RuleId) of
		remote_discovery -> call_continue_remote_storage_discovery(RuleId, Dict);
		discovery -> call_continue_ep_discovery(RuleId, Dict);
		C -> ?ERROR_LOG("Unknown Rule Type: "?S"", [C])
	end.

call_continue_remote_storage_discovery(RuleId, Dict) ->
	case get_discovery_status(RuleId, Dict) of
		{paused, GroupId} -> gen_server:cast(?REMOTE_DISCOVERY, {continue_discovering, RuleId}),
					dict:store(RuleId, {?DISC, GroupId}, Dict);
		_ -> Dict
	end.

call_continue_ep_discovery(RuleId, Dict) ->
	case get_discovery_status(RuleId, Dict) of
		{paused, GroupId} -> set_command_to_endpoints(RuleId, ?CONTINUE_EP_COMMAND, [{groupId, GroupId}]),
					dict:store(RuleId, {?DISC, GroupId}, Dict);
		_ -> Dict
	end.

set_command_to_endpoints(RuleId, Command, Args) ->
	case Command of
		?START_EP_COMMAND -> mydlp_mysql:populate_discovery_endpoint_schedules(RuleId);
		_ -> ok
	end,
	[Endpoints] = mydlp_mnesia:get_endpoints_by_rule_id(RuleId), 
	OrigRuleId = mydlp_mnesia:get_orig_id_by_rule_id(RuleId),
	set_each_endpoint_command(Endpoints, RuleId, Command, [{ruleId, OrigRuleId}|Args]).

set_each_endpoint_command([Alias|Endpoints], RuleId, Command, Args) ->
	mydlp_mnesia:save_endpoint_command(Alias, Command, Args),
	set_each_endpoint_command(Endpoints, RuleId, Command, Args);
set_each_endpoint_command([], _RuleId, _Command, _Args) -> ok.

update_report_as_finished(GroupId) -> 
	mydlp_mysql:update_report_as_finished(GroupId).

update_discovery_report(GroupId, NewStatus) ->
	mydlp_mysql:update_report_status(GroupId, NewStatus).

generate_group_id(RuleId, Channel) ->
	Time = erlang:universaltime(),
	GroupId = integer_to_list(RuleId) ++ "_" ++ integer_to_list(calendar:datetime_to_gregorian_seconds(erlang:localtime())),
	OrigRuleId = mydlp_mnesia:get_orig_id_by_rule_id(RuleId),
	erlang:display({rule_id, RuleId}),
	mydlp_mysql:push_discovery_report(Time, GroupId, OrigRuleId, ?REPORT_STATUS_DISC),
	OprLog = #opr_log{time=Time, channel=Channel, rule_id=RuleId, message_key=?SUCCESS_MOUNT_KEY, group_id=GroupId},%TODO: message key should be revised.
	?DISCOVERY_OPR_LOG(OprLog),
	GroupId .

register_schedules_for_future(RuleId, Channel) ->
	GroupId = generate_group_id(RuleId, Channel),
	WaitingSchedules = mydlp_mnesia:get_waiting_schedule_by_rule_id(RuleId),
	case WaitingSchedules of 
		none -> ok;
		GId -> update_report_as_finished(GId)
	end,
	mydlp_mnesia:register_schedule(RuleId, GroupId).

cancel_timer(RuleId) -> gen_server:cast(?MODULE, {cancel_timer, RuleId}).

create_timer(RuleId) -> gen_server:cast(?MODULE, {create_timer, RuleId}).

break_discovery(RuleId, GroupId, Dict) ->
	case gen_server:call(?REMOTE_DISCOVERY, {stop_discovery, RuleId}, 60000) of
		ok -> update_report_as_finished(GroupId),
			GId = generate_group_id(RuleId, remote_discovery),
			call_start_discovery_by_rule_id(RuleId, GId, Dict, false);
		R -> ?OPR_LOG("Failed to scheduling discovery: "?S"", [R]), 
			Dict
	end.

break_ep_discovery(RuleId, GroupId, Dict) ->
	set_command_to_endpoints(RuleId, ?STOP_EP_COMMAND, [{groupId, GroupId}]),
	update_report_as_finished(GroupId),
	GId = generate_group_id(RuleId, discovery),
	call_start_discovery_on_ep(RuleId, GId, Dict, false).

get_discovery_status(RuleId, Dict) ->
	case dict:find(RuleId, Dict) of
		{ok, {?DISC, GroupId}} -> {disc, GroupId};
		{ok, {?ON_DEMAND_DISC, GroupId}} -> {user_disc, GroupId};
		{ok, {?SYSTEM_PAUSED, GroupId}} -> {paused, GroupId};
		{ok, {?USER_PAUSED, GroupId}} -> {user_paused, GroupId};
		{ok, _} -> stop;
		_ -> none
	end.

edit_dictionary([RuleId|Rest], Dict) ->
	case mydlp_mnesia:get_availabilty_by_rule_id(RuleId) of
		true -> start_discovery(RuleId); 
		false -> ok
	end,
	edit_dictionary(Rest, Dict);
edit_dictionary(none, Dict) -> control_already_scheduled_discoveries(Dict);
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

start_at_exact_hour() -> % Remaining should be multiplied with 1000
	{_D, {_H, M, S}} = erlang:localtime(),
	erlang:display({exactHour, M, S}),
	case M of 
		0 -> timer:send_after(0, start_discovery_scheduling);
		_ -> Remaining = (((59-M)*60)+S),
			timer:send_after(Remaining, start_discovery_scheduling)
	end.

start_discovery_scheduling() ->
	{_D, {H, _M, _S}} = erlang:localtime(),
	Schedules = mydlp_mnesia:get_schedules_by_hour(H),
	gen_server:cast(?MODULE, {manage_schedules, Schedules}),
	timer:send_after(300000, start_discovery_scheduling).
