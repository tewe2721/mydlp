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
-module(mydlp_tc).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	get_mime/1,
	get_text/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {thrift_server}).

%%%%%%%%%%%%% MyDLP Thrift RPC API

-define(MMLEN, 9300).

get_mime(Data) when is_list(Data) ->
	L = length(Data),
	Data1 = case L > ?MMLEN of
		true -> lists:sublist(Data, ?MMLEN);
		false -> Data
	end,
	gen_server:call(?MODULE, {thrift, getMagicMime, [Data1]});

get_mime(Data) when is_binary(Data) ->
	S = size(Data),
	Data1 = case S > ?MMLEN of
		true -> <<D:?MMLEN/binary, _/binary>> = Data, D;
		false -> Data
	end,
	gen_server:call(?MODULE, {thrift, getMagicMime, [Data1]}).

get_text(#file{mime_type=undefined, data=Data}) ->
	Data;

get_text(#file{mime_type= <<"application/x-empty">>, data=Data}) ->
	Data;

get_text(#file{mime_type= <<"text/plain">>, data=Data}) ->
	Data;

get_text(#file{mime_type= <<"application/pdf">>, data=Data}) ->
	gen_server:call(?MODULE, {thrift, getPdfText, [Data]});

get_text(#file{mime_type= <<"application/postscript">>, data=Data}) ->
	gen_server:call(?MODULE, {thrift, getPdfText, [Data]});

get_text(#file{mime_type= <<"application/vnd.ms-office">>, data=Data}) ->
	gen_server:call(?MODULE, {thrift, getOOoText, [Data]});

get_text(#file{data=Data}) ->
	Data.

%%%%%%%%%%%%%% gen_server handles

handle_call({thrift, Func, Params}, From, #state{thrift_server=TS} = State) ->
	Worker = self(),
	spawn_link(fun() ->
			{ok, Reply} = thrift_client:call(TS, Func, Params),
			Worker ! {async_thrift, Reply, From}
		end),
	{noreply, State, 15000};

handle_call(stop, _From, #state{thrift_server=TS} = State) ->
	thrift_client:close(TS),
	{stop, normalStop, State#state{thrift_server=undefined}};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_thrift, Reply, From}, State) ->
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
	{ok, TS} = thrift_client:start_link("localhost",9090, mydlp_thrift),
	{ok, #state{thrift_server=TS}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

