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

-module(mydlp_document_trainer).
-author("ozgen@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").


%% API
-export([start_link/0,
	stop/0,
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

%%%%%%%%%%%%%  API


%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(startup, State) ->
	generate_fingerprints(),
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
	timer:send_after(6000, startup),
	{ok, #state{discovery_dict=dict:new(), timer_dict=dict:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

generate_fingerprints() ->
	RDDWithDocumentId = mydlp_mnesia:get_remote_document_databases(),

start_discovery_scheduling() ->
	{_D, {H, _M, _S}} = erlang:localtime(),
	Schedules = mydlp_mnesia:get_schedules_by_hour(H),
	gen_server:cast(?MODULE, {manage_schedules, Schedules}),
	timer:send_after(60*60*1000, start_discovery_scheduling).
