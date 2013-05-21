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
-define(FS_DISCOVERY, mydlp_discover_fs).
-define(REMOTE_DISCOVERY, mydlp_discover_rfs).
-define(WEB_DISCOVERY, mydlp_discover_web).
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
-define(RFS_DISC_FINISHED, "rfs_finished").
-define(RFS_DISC_PAUSED, "rfs_paused").

%%%%%%%%%%%%%  API

start_on_demand_discovery(RuleOrigId) ->
	%RuleId = mydlp_mnesia:get_rule_id_by_orig_id(RuleOrigId),
	gen_server:cast(?MODULE, {start_on_demand, RuleOrigId}),
	ok.

stop_discovery_on_demand(RuleOrigId) ->
	%RuleId = mydlp_mnesia:get_rule_id_by_orig_id(RuleOrigId),
	gen_server:cast(?MODULE, {stop_on_demand, RuleOrigId}),
	ok.

pause_discovery_on_demand(RuleOrigId) ->
	%RuleId = mydlp_mnesia:get_rule_id_by_orig_id(RuleOrigId),
	gen_server:cast(?MODULE, {pause_on_demand, RuleOrigId}),
	ok.

get_group_id(RuleId) -> gen_server:call(?MODULE, {get_group_id, RuleId}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call({get_group_id, RuleId}, _From, State) ->
	?REPLYGUARD(fun() ->
		Reply = case mydlp_mnesia:get_discovery_status(RuleId) of
				{_, GroupId} -> GroupId;
				_ -> -1
			end,
		{reply, Reply, State}
	end, -1, State);

handle_call({is_paused_or_stopped, RuleId}, _From, State) ->
	?REPLYGUARD(fun() ->
		Reply = case mydlp_mnesia:get_discovery_status(RuleId) of
				{?SYSTEM_PAUSED, _} -> paused;
				{?USER_PAUSED, _} -> paused;
				{?SYSTEM_STOPPED, _} -> stopped;
				{?USER_STOPPED, _} -> stopped;
				_ -> none
			end,
		{reply, Reply, State}
	end, none, State);

handle_call(_Msg, _From, State) ->
	{noreply, State}.


handle_cast({start_on_demand, RuleId}, State) ->
	catch call_start_discovery_on_target(RuleId, true),
	{noreply, State};

handle_cast({pause_on_demand, RuleId}, State) ->
	catch call_pause_discovery_on_target(RuleId, true),
	{noreply, State};

handle_cast({stop_on_demand, RuleId}, State) ->
	catch call_stop_discovery_on_target(RuleId, true),
	{noreply, State};

handle_cast({manage_schedules, Schedules}, State) ->
	catch edit_dictionary(Schedules),
	{noreply, State};

handle_cast({start_discovery, RuleId}, State) ->
	catch call_start_discovery_on_target(RuleId, false),
	{noreply, State};

handle_cast({stop_discovery, RuleId}, State) ->
	catch call_stop_discovery_on_target(RuleId, false),
	{noreply, State};

handle_cast({pause_discovery, RuleId}, State) ->
	catch call_pause_discovery_on_target(RuleId, false),
	{noreply, State};

handle_cast({continue_discovery, RuleId}, State) ->
	catch call_continue_discovery_on_target(RuleId),
	{noreply, State};

handle_cast({cancel_timer, RuleId}, #state{timer_dict=TimerDict}=State) ->
	?NOREPLYGUARD(fun() ->
		case dict:find(RuleId, TimerDict) of
			{ok, TRef} -> (catch timer:cancel(TRef));
			_ -> ok
		end,
		TimerDict1 = dict:erase(RuleId, TimerDict),
		{noreply, State#state{timer_dict=TimerDict1}}
	end, State);

handle_cast({create_timer, RuleId}, #state{timer_dict=TimerDict}=State) ->
	?NOREPLYGUARD(fun() ->
		case dict:find(RuleId, TimerDict) of
			{ok, TRef} -> timer:cancel(TRef);
			_ -> ok
		end,

		TimerDict1 = case mydlp_mnesia:get_rule_channel_by_orig_id(RuleId) of
				remote_discovery -> {ok, Timer} = timer:send_after(60000, {is_discovery_finished, RuleId}),
						dict:store(RuleId, Timer, TimerDict);
				discovery -> {ok, Timer} = timer:send_after(?EP_CONTROL_TIME, {is_ep_discovery_finished, RuleId}),
						dict:store(RuleId, Timer, TimerDict)
			end,
		{noreply, State#state{timer_dict=TimerDict1}}
	end, State);

handle_cast({start_timers, DiscoveryList}, State) ->
	?NOREPLYGUARD(fun() ->
		TimerList = lists:map(fun({RuleOrigId, _, _}) -> 
					{ok, T} = case mydlp_mnesia:get_rule_channel_by_orig_id(RuleOrigId) of
							remote_discovery -> timer:send_after(60000, {is_discovery_finished, RuleOrigId});
							discovery -> timer:send_after(?EP_CONTROL_TIME, {is_ep_discovery_finished, RuleOrigId});
							RT -> ?ERROR_LOG("Unknown Rule Type: ["?S"]", [RT]), 
								remove_discovery_status(RuleOrigId), {ok, none} end,
							{RuleOrigId, T} end, DiscoveryList),
		TimerList1 = lists:filter(fun({_, S}) -> S /= none end, TimerList),
		TimerDict = dict:from_list(TimerList1),
		{noreply, State#state{timer_dict=TimerDict}}
	end, State);

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(startup, State) ->
	?NOREPLYGUARD(fun() ->
		continue_discovery_after_init(),
		start_timers(), 
		start_at_exact_hour(),
		{noreply, State}
	end, State);

handle_info(start_discovery_scheduling, State) ->
	catch start_discovery_scheduling(),
	{noreply, State};

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info({is_discovery_finished, RuleId}, #state{timer_dict=TimerDict}=State) ->
	?NOREPLYGUARD(fun() ->
		case dict:find(RuleId, TimerDict) of
			error -> {noreply, State};
			{ok, Timer} ->
				try
				cancel_timer(Timer),
				%Reply = mydlp_discover_fs:is_discovery_finished(RuleId),
				{_, GroupId} = get_discovery_status(RuleId),
				Reply = mydlp_mysql:is_all_discovery_finished(GroupId),
				case Reply of
					true ->	 
						TimerDict1 = dict:erase(RuleId, TimerDict),
						mydlp_discover_rfs:release_mount_by_rule_id(RuleId),
						update_report_as_finished(GroupId),
						case mydlp_mnesia:get_waiting_schedule_by_rule_id(RuleId) of
							none ->	remove_discovery_status(RuleId);
							GId -> call_start_discovery_by_rule_id(RuleId, GId, false)
						end,
						{noreply, State#state{timer_dict=TimerDict1}};
					false -> 
						{ok, Timer1} = timer:send_after(60000, {is_discovery_finished, RuleId}),
						TimerDict1 = dict:store(RuleId, Timer1, TimerDict),
						{noreply, State#state{timer_dict=TimerDict1}}
				end
				catch _Class:_Error ->
					{ok, Timer2} = timer:send_after(60000, {is_discovery_finished, RuleId}),
					TimerDict2 = dict:store(RuleId, Timer2, TimerDict),
					{noreply, State#state{timer_dict=TimerDict2}}
				end
		end
	end, State);

handle_info({is_ep_discovery_finished, RuleId}, #state{timer_dict=TimerDict}=State) ->
	?NOREPLYGUARD(fun() ->
		case dict:find(RuleId, TimerDict) of
			error -> {noreply, State};
			{ok, Timer} ->
				try
				cancel_timer(Timer),
				{_, GroupId} = get_discovery_status(RuleId),
				[Endpoints] = mydlp_mnesia:get_endpoints_by_rule_id(RuleId), 
				case mydlp_mysql:is_all_ep_discovery_finished(GroupId, Endpoints, ?EP_DISC_FINISHED) of
					true -> TimerDict1 = dict:erase(RuleId, TimerDict),
						update_report_as_finished(GroupId),
						 case mydlp_mnesia:get_waiting_schedule_by_rule_id(RuleId) of
							none -> remove_discovery_status(RuleId);
							GId -> call_start_discovery_on_ep(RuleId, GId, false)
						end,
						{noreply, State#state{timer_dict=TimerDict1}};
					false -> {ok, Timer1} = timer:send_after(?EP_CONTROL_TIME, {is_ep_discovery_finished, RuleId}),
						TimerDict1 = dict:store(RuleId, Timer1, TimerDict),
						{noreply, State#state{timer_dict=TimerDict1}}
				end
				catch _Class:_Error ->
					{ok, Timer2} = timer:send_after(?EP_CONTROL_TIME, {is_ep_discovery_finished, RuleId}),
					TimerDict2 = dict:store(RuleId, Timer2, TimerDict),
					{noreply, State#state{timer_dict=TimerDict2}}	
				end
		end
	end, State);

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
	{ok, #state{timer_dict=dict:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

start_discovery(RuleId) -> gen_server:cast(?MODULE, {start_discovery, RuleId}).

stop_discovery(RuleId) -> gen_server:cast(?MODULE, {stop_discovery, RuleId}).

pause_discovery(RuleId) -> gen_server:cast(?MODULE, {pause_discovery, RuleId}).

continue_discovery(RuleId) -> gen_server:cast(?MODULE, {continue_discovery, RuleId}).

call_start_discovery_on_target(RuleId, IsOnDemand) ->
	case mydlp_mnesia:get_rule_channel_by_orig_id(RuleId) of
		remote_discovery -> call_remote_storage_discovery(RuleId, IsOnDemand);
		discovery -> call_ep_discovery(RuleId, IsOnDemand);
		C -> ?ERROR_LOG("Unknown Rule Type: "?S"", [C])
	end.

call_remote_storage_discovery(RuleId, IsOnDemand) -> 
	Resp = get_discovery_status(RuleId),
	case Resp of
		{disc, GroupId} -> 
			case IsOnDemand of
				true -> ok;
				false -> break_discovery(RuleId, GroupId)
			end;
		{paused, GroupId} -> call_continue_discovery_on_remote(RuleId),
			case IsOnDemand of
				true -> update_discovery_report(GroupId, ?REPORT_STATUS_DISC),
					update_discovery_status(RuleId, ?ON_DEMAND_DISC, GroupId);
				false -> break_discovery(RuleId, GroupId) % This case looks like impossible
			end;
		{user_paused, GroupId} ->
			case IsOnDemand of
				true -> % means that user paused discovery while ago and now starts again
					update_discovery_report(GroupId, ?REPORT_STATUS_DISC),
					call_continue_discovery_on_remote(RuleId),
					update_discovery_status(RuleId, ?ON_DEMAND_DISC, GroupId);
				false ->% means that user paused discovery while ago and now it is time to schedule
					% New discovery with a new report id. Ensure that last discovery is stopped.
					break_discovery(RuleId, GroupId)
			end; 
		{user_disc, _GroupId} ->
			case IsOnDemand of 
				true -> ok; %looks impossible
				false -> register_schedules_for_future(RuleId, remote_discovery)% This will be stored and will be runned when discovery finish 
			end;
		_ -> 	
			case mydlp_mnesia:get_waiting_schedule_by_rule_id(RuleId) of
				none -> GId = generate_group_id(RuleId, remote_discovery),
					call_start_discovery_by_rule_id(RuleId, GId, IsOnDemand);
				GIdW -> update_report_as_finished(GIdW),
					register_schedules_for_future(RuleId, remote_discovery) end
	end.

call_ep_discovery(RuleId, IsOnDemand) -> 
	Resp = get_discovery_status(RuleId),
	case Resp of
		{disc, GroupId} -> 
			case IsOnDemand of
				true -> ok;
				false -> break_ep_discovery(RuleId, GroupId)
			end;
		{paused, GroupId} -> 
			case IsOnDemand of
				true -> call_continue_discovery_on_ep(RuleId, GroupId);
				false -> break_ep_discovery(RuleId, GroupId)
			end;
		{user_paused, GroupId} -> 
			case IsOnDemand of
				true -> call_continue_discovery_on_ep(RuleId, GroupId);
				false ->% means that user paused discovery while ago and now it is time to schedule
					% New discovery with a new report id. Ensure that last discovery is stopped.
					break_ep_discovery(RuleId, GroupId)
			end;	
		{user_disc, _GroupId} ->
			case IsOnDemand of 
				true -> ok; %looks impossible
				false -> register_schedules_for_future(RuleId, discovery)% This will be stored and will be runned when discovery finish 
			end;
		_ -> 
			case mydlp_mnesia:get_waiting_schedule_by_rule_id(RuleId) of
				none -> GId = generate_group_id(RuleId, discovery),
					call_start_discovery_on_ep(RuleId, GId, IsOnDemand);
				GIdW -> update_report_as_finished(GIdW),
					register_schedules_for_future(RuleId, discovery) end
	end.

call_continue_discovery_on_remote(RuleId) ->
	create_timer(RuleId),
	mydlp_discover_fs:continue_paused_discovery(RuleId),
	gen_server:cast(?WEB_DISCOVERY, {continue_discovering, RuleId}).

call_continue_discovery_on_ep(RuleId, GroupId) ->
	create_timer(RuleId),
	update_discovery_report(GroupId, ?REPORT_STATUS_DISC),
	set_command_to_endpoints(RuleId, ?CONTINUE_EP_COMMAND, [{groupId, GroupId}]),
	update_discovery_status(RuleId, ?ON_DEMAND_DISC, GroupId).

call_start_discovery_by_rule_id(RuleId, GroupId, IsOnDemand) -> 
	gen_server:cast(?REMOTE_DISCOVERY, {start_by_rule_id, RuleId, GroupId}),
	gen_server:cast(?WEB_DISCOVERY, {start_by_rule_id, RuleId, GroupId}),
	create_timer(RuleId),
	case IsOnDemand of
		true -> update_discovery_status(RuleId, ?ON_DEMAND_DISC, GroupId);
		false -> update_discovery_status(RuleId, ?DISC, GroupId)
	end.

call_start_discovery_on_ep(RuleId, GroupId, IsOnDemand) ->
	set_command_to_endpoints(RuleId, ?START_EP_COMMAND, [{groupId, GroupId}]),
	create_timer(RuleId),
	case IsOnDemand of
		true -> update_discovery_status(RuleId, ?ON_DEMAND_DISC, GroupId);
		false -> update_discovery_status(RuleId, ?DISC, GroupId)
	end.

call_pause_discovery_on_target(RuleId, IsOnDemand) -> 
	case mydlp_mnesia:get_rule_channel_by_orig_id(RuleId) of
		remote_discovery -> call_pause_remote_storage_discovery(RuleId, IsOnDemand);
		discovery -> call_pause_ep_discovery(RuleId, IsOnDemand);
		C -> ?ERROR_LOG("Unknown Rule Type: "?S"", [C])
	end.

call_pause_remote_storage_discovery(RuleId, IsOnDemand) ->
	case get_discovery_status(RuleId) of
		{disc, GroupId} -> 
			cancel_timer(RuleId),
			case IsOnDemand of 
				true -> update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_USER),
					update_discovery_status(RuleId, ?USER_PAUSED, GroupId);
				false -> update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_SYSTEM),
					update_discovery_status(RuleId, ?SYSTEM_PAUSED, GroupId) end;
		{user_disc, GroupId} -> 
			case IsOnDemand of
				true -> cancel_timer(RuleId),
					update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_USER),
					update_discovery_status(RuleId, ?USER_PAUSED, GroupId);
				false -> ok
			end;
		_ -> ok
	end.

call_pause_ep_discovery(RuleId, IsOnDemand) ->
	case get_discovery_status(RuleId) of
		{disc, GroupId} ->
			cancel_timer(RuleId),
			set_command_to_endpoints(RuleId, ?PAUSE_EP_COMMAND, [{groupId, GroupId}]),
			case IsOnDemand of
				true -> update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_USER),
					update_discovery_status(RuleId, ?USER_PAUSED, GroupId);
				false -> update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_SYSTEM),	
					update_discovery_status(RuleId, ?SYSTEM_PAUSED, GroupId) end;
		{user_disc, GroupId} ->
			case IsOnDemand of
				true -> cancel_timer(RuleId),
					set_command_to_endpoints(RuleId, ?PAUSE_EP_COMMAND, [{groupId, GroupId}]),
					update_discovery_report(GroupId, ?REPORT_STATUS_PAUSED_USER),
					update_discovery_status(RuleId, ?USER_PAUSED, GroupId);
				false -> ok
			end;
		_ -> ok
	end.
	
call_stop_discovery_on_target(RuleId, IsOnDemand) ->
	case mydlp_mnesia:get_rule_channel_by_orig_id(RuleId) of
		remote_discovery -> call_stop_remote_storage_discovery(RuleId, IsOnDemand);
		discovery -> call_stop_ep_discovery(RuleId, IsOnDemand);
		C -> ?ERROR_LOG("Unknown Rule Type: "?S"", [C])
	end. 

call_stop_remote_storage_discovery(RuleId, _IsOnDemand) ->
	case get_discovery_status(RuleId) of
		none -> ok;
		stop -> ok;
		{_, GroupId} -> % discovering or paused
			mydlp_mnesia:del_fs_entries_by_rule_id(RuleId),
			mydlp_mnesia:del_web_entries_by_rule_id(RuleId),
			update_report_as_finished(GroupId),
			cancel_timer(RuleId),
			case mydlp_mnesia:get_waiting_schedule_by_rule_id(RuleId) of
				none ->	remove_discovery_status(RuleId),
					mydlp_discover_rfs:release_mount_by_rule_id(RuleId);
				GId -> call_start_discovery_by_rule_id(RuleId, GId, false) end
	end.

call_stop_ep_discovery(RuleId, _IsOnDemand) ->
	case get_discovery_status(RuleId) of
		none -> ok;
		stop -> ok;
		{_, GroupId} ->
			update_report_as_finished(GroupId),
			cancel_timer(RuleId),
			set_command_to_endpoints(RuleId, ?STOP_EP_COMMAND, [{groupId, GroupId}]),
			case mydlp_mnesia:get_waiting_schedule_by_rule_id(RuleId) of
				none ->	remove_discovery_status(RuleId);
				GId -> call_start_discovery_on_ep(RuleId, GId, false) end
	end.
	
call_continue_discovery_on_target(RuleId) ->
	case mydlp_mnesia:get_rule_channel_by_orig_id(RuleId) of
		remote_discovery -> call_continue_remote_storage_discovery(RuleId);
		discovery -> call_continue_ep_discovery(RuleId);
		C -> ?ERROR_LOG("Unknown Rule Type: "?S"", [C])
	end.

call_continue_remote_storage_discovery(RuleId) ->
	case get_discovery_status(RuleId) of
		{paused, GroupId} -> create_timer(RuleId),
					mydlp_discover_fs:continue_paused_discovery(RuleId),
					gen_server:cast(?WEB_DISCOVERY, {continue_discovering, RuleId}),
					update_discovery_status(RuleId, ?DISC, GroupId);
		_ -> ok
	end.

call_continue_ep_discovery(RuleId) ->
	case get_discovery_status(RuleId) of
		{paused, GroupId} -> create_timer(RuleId),
					set_command_to_endpoints(RuleId, ?CONTINUE_EP_COMMAND, [{groupId, GroupId}]),
					update_discovery_status(RuleId, ?DISC, GroupId);
		_ -> ok
	end.

set_command_to_endpoints(RuleId, Command, Args) ->
	case Command of
		?START_EP_COMMAND -> EndpointIds = mydlp_mnesia:get_endpoint_ids(),
				lists:map(fun(I) -> mydlp_mnesia:update_ep_schedules(I, RuleId) end, EndpointIds);
		_ -> ok
	end,
	[Endpoints] = mydlp_mnesia:get_endpoints_by_rule_id(RuleId), 
	set_each_endpoint_command(Endpoints, Command, [{ruleId, RuleId}|Args]).

set_each_endpoint_command([Alias|Endpoints], Command, Args) ->
	mydlp_mnesia:save_endpoint_command(Alias, Command, Args),
	set_each_endpoint_command(Endpoints, Command, Args);
set_each_endpoint_command([], _Command, _Args) -> ok.

update_report_as_finished(GroupId) -> 
	mydlp_mysql:update_report_as_finished(GroupId).

update_discovery_report(GroupId, NewStatus) ->
	mydlp_mysql:update_report_status(GroupId, NewStatus).

generate_group_id(RuleId, Channel) ->
	Time = erlang:universaltime(),
	GroupId = integer_to_list(RuleId) ++ "_" ++ integer_to_list(calendar:datetime_to_gregorian_seconds(erlang:localtime())),
	mydlp_mysql:push_discovery_report(Time, GroupId, RuleId, ?REPORT_STATUS_DISC),
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

break_discovery(RuleId, GroupId) ->
	case call_immediate_stop_on_remote(RuleId) of
		ok -> update_report_as_finished(GroupId),
			GId = generate_group_id(RuleId, remote_discovery),
			call_start_discovery_by_rule_id(RuleId, GId, false);
		R -> ?OPR_LOG("Failed to scheduling discovery: "?S"", [R])
	end.

call_immediate_stop_on_remote(RuleId) ->
	case gen_server:call(?FS_DISCOVERY, {stop_discovery_by_rule_id, RuleId}, 60000) of
		ok -> gen_server:call(?WEB_DISCOVERY, {stop_discovery, RuleId}, 60000);
		R -> R
	end.
	
break_ep_discovery(RuleId, GroupId) ->
	set_command_to_endpoints(RuleId, ?STOP_EP_COMMAND, [{groupId, GroupId}]),
	update_report_as_finished(GroupId),
	GId = generate_group_id(RuleId, discovery),
	call_start_discovery_on_ep(RuleId, GId, false).

get_discovery_status(RuleId) ->
	case mydlp_mnesia:get_discovery_status(RuleId) of
		{?DISC, GroupId} -> {disc, GroupId};
		{?ON_DEMAND_DISC, GroupId} -> {user_disc, GroupId};
		{?SYSTEM_PAUSED, GroupId} -> {paused, GroupId};
		{?USER_PAUSED, GroupId} -> {user_paused, GroupId};
		{ok, _} -> stop;
		_ -> none
	end.

update_discovery_status(RuleId, Status, GroupId) ->
	mydlp_mnesia:update_discovery_status(RuleId, Status, GroupId).

remove_discovery_status(RuleId) ->
	mydlp_mnesia:remove_discovery_status(RuleId).
	
edit_dictionary([{_RuleId, RuleOrigId}|Rest]) ->
	case mydlp_mnesia:get_availabilty_by_rule_id(RuleOrigId) of
		true -> start_discovery(RuleOrigId); 
		false -> ok
	end,
	edit_dictionary(Rest);
edit_dictionary(none) -> control_already_scheduled_discoveries();
edit_dictionary([]) -> control_already_scheduled_discoveries().

control_already_scheduled_discoveries() ->
	DiscoveryList = mydlp_mnesia:get_all_discovery_status(),
	edit_discoveries(DiscoveryList).

edit_discoveries([{RuleId, ?SYSTEM_PAUSED, _}|R]) ->
	case mydlp_mnesia:get_availabilty_by_rule_id(RuleId) of
		true -> continue_discovery(RuleId);
		false -> ok
	end,
	edit_discoveries(R);
edit_discoveries([{RuleId, ?DISC, _}|R]) ->
	case mydlp_mnesia:get_availabilty_by_rule_id(RuleId) of
		true -> ok;
		false -> pause_discovery(RuleId)
	end,
	edit_discoveries(R);
edit_discoveries([_|R]) -> edit_discoveries(R);
edit_discoveries([]) -> ok.

continue_discovery_after_init() ->
	Discoveries = mydlp_mnesia:get_all_discovery_status(),
	continue_discovery1(Discoveries).

continue_discovery1([{RuleId, Status, GroupId}|Rest]) ->
	case Status of
		stopped -> continue_discovery1(Rest);
		user_stopped -> continue_discovery1(Rest);
		_ -> mydlp_discover_rfs:start_discovery(RuleId, GroupId),
			mydlp_discover_web:start_discovery(RuleId, GroupId),
			continue_discovery1(Rest)
	end;
continue_discovery1([]) -> ok.

start_timers() ->
	DiscoveryList = mydlp_mnesia:get_all_discovery_status(),
	gen_server:cast(?MODULE, {start_timers, DiscoveryList}).

start_at_exact_hour() -> 
	{_D, {_H, M, S}} = erlang:localtime(),
	case M of 
		0 -> timer:send_after(0, start_discovery_scheduling);
		_ -> Remaining = (((59-M)*60)+(60-S)+10) * 1000, %10 is for safity
			timer:send_after(Remaining, start_discovery_scheduling)
	end.

start_discovery_scheduling() ->
	{_D, {H, M, S}} = erlang:localtime(),
	Schedules = mydlp_mnesia:get_schedules_by_hour(H),
	gen_server:cast(?MODULE, {manage_schedules, Schedules}),
	Time = (((59-M)*60)+(60-S)+10) * 1000,
	timer:send_after(Time, start_discovery_scheduling).
