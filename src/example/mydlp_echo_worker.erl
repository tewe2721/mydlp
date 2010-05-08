%%%
%%%    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@medra.com.tr>
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
%%% @author H. Kerem Cevahir <kerem@medratech.com>
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_echo_worker).
-author("kerem@medratech.com").
-behaviour(gen_server).

%% API
-export([start_cluster/0,
	echo_reply/1,
	async_echo_reply/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {id=0}).

start_cluster() ->
    gen_server_cluster:start(?MODULE, ?MODULE, [], []).

echo_reply(Request) ->
    gen_server_cluster:call(?MODULE, {echo_reply, Request}).

async_echo_reply(Request) ->
    gen_server_cluster:call(?MODULE, {async_echo_reply, Request}).

stop() ->
    gen_server:call({global,?MODULE}, stop).

init([]) ->
    {ok, #state{}}.

handle_call({echo_reply, Request}, _From, State) ->
	Value = State#state.id,
	Reply = Request,
	io:format("echo replied with id=~w by ~w at ~w...~n", [Value, self(), node()]),
    {reply, Reply, State#state{id=Value+1}};

handle_call({async_echo_reply, Request}, From, State) ->
	Worker = self(),
	spawn_link(fun() ->
			Value = State#state.id,
			Reply = Request,
			io:format("echo replied with id=~w by ~w at ~w...~n", [Value, self(), node()]),
			Worker ! {async_echo_reply, Reply, From, State#state{id=Value+1}}
		end),
    {noreply, State, 5000};

handle_call(stop, _From, State) ->
    {stop, normalStop, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({async_echo_reply, Reply, From, NewState}, _State) ->
	gen_server:reply(From, Reply),
	{noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

