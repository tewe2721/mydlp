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
%%% @copyright 2011, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-ifdef(__MYDLP_ENDPOINT).

-module(mydlp_sync).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	sync/0,
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
	}).

%%%% API

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(sync_now, State) ->
	sync(),
	call_timer(),
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
	inets:start(),
	call_timer(5000),
	{ok, #state{}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

call_timer() -> call_timer(?CFG(sync_interval)).

call_timer(Interval) -> timer:send_after(Interval, sync_now).

sync() ->
	RevisionI = mydlp_api:get_client_policy_revision_id(),
	RevisionS = integer_to_list(RevisionI),
	Url = "https://" ++ ?CFG(management_server_address) ++ "/sync.php?rid=" ++ RevisionS,
	case catch http:request(Url) of
		{ok, {{_HttpVer, Code, _Msg}, _Headers, Body}} -> 
			case {Code, Body} of
				{200, <<"up-to-date", _/binary>>} -> ok;
				{200, CDBBin} -> mydlp_api:populate_client_policy(CDBBin);
				{Else1, _Data} -> ?ERROR_LOG("SYNC: An error occured during HTTP req: Code=~w~n", [Else1]) end;
		Else -> ?ERROR_LOG("SYNC: An error occured during HTTP req: Obj=~w~n", [Else]) end,
	ok.

-endif.

