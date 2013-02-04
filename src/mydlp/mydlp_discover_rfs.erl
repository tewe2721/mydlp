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

-module(mydlp_discover_rfs).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0,
	q/2,
	finished/0,
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
	discover_queue,
	discover_inprog=false,
	timer
}).

%%%%%%%%%%%%%  API

q(MountId, RuleIndex) -> gen_server:cast(?MODULE, {q, MountId, RuleIndex}).

finished() -> gen_server:cast(?MODULE, finished).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({q, MountId, RuleIndex}, #state{discover_queue=Q, discover_inprog=false} = State) ->
	Q1 = queue:in({MountId, RuleIndex}, Q),
	consume(),
	{noreply, State#state{discover_queue=Q1, discover_inprog=true}};

handle_cast({q, MountId, RuleIndex}, #state{discover_queue=Q, discover_inprog=true} = State) ->
	Q1 = queue:in({MountId, RuleIndex}, Q),
	{noreply,State#state{discover_queue=Q1}};

handle_cast(consume, #state{discover_queue=Q} = State) ->
	case queue:out(Q) of
		{{value, {MountId, RuleIndex}}, Q1} ->
			try	discover(MountId, RuleIndex),
				consume(),
				{noreply, State#state{discover_queue=Q1}}
			catch Class:Error ->
				?ERROR_LOG("Discover Queue Consume: Error occured: "
						"Class: ["?S"]. Error: ["?S"].~n"
						"Stack trace: "?S"~n.FilePath: "?S"~nState: "?S"~n ",	
						[Class, Error, erlang:get_stacktrace(), FilePath, State]),
					consume(),
					{noreply, State#state{discover_queue=Q1}} end;
		{empty, _} ->
			{noreply, State}
	end;

handle_cast(finished, State) ->
	release_mounts(),
	{noreply, State#state{discover_inprog=false}};

handle_info(schedule_startup, State) ->
        State1 = case ?CFG(discover_fs_on_startup) of
                true -> schedule(), State;
                false -> schedule_timer(State, ?CFG(discover_fs_interval)) end,
        {noreply, State1};

handle_cast(_Msg, State) ->
	{noreply, State}.

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
	release_mounts(),
	timer:send_after(60000, schedule_startup),
	{ok, #state{discover_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal
