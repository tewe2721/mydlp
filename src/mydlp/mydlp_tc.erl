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
	is_valid_iban/1,
	html_to_text/1,
	bayes_score/1,
	bayes_train_confidential/1,
	bayes_train_public/1,
	bayes_reset/0,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {backend_py, backend_java}).

%%%%%%%%%%%%% MyDLP Thrift RPC API

-define(MMLEN, 4096).

get_mime(Data) when is_list(Data) ->
	L = length(Data),
	Data1 = case L > ?MMLEN of
		true -> lists:sublist(Data, ?MMLEN);
		false -> Data
	end,
	gen_server:call(?MODULE, {thrift, py, getMagicMime, [Data1]});

get_mime(Data) when is_binary(Data) ->
	S = size(Data),
	Data1 = case S > ?MMLEN of
		true -> <<D:?MMLEN/binary, _/binary>> = Data, D;
		false -> Data
	end,
	gen_server:call(?MODULE, {thrift, py, getMagicMime, [Data1]}).

is_valid_iban(IbanStr) ->
	gen_server:call(?MODULE, {thrift, py, isValidIban, [IbanStr]}).

html_to_text(Html) ->
	gen_server:call(?MODULE, {thrift, py, htmlToText, [Html]}).

bayes_score(Text) ->
	gen_server:call(?MODULE, {thrift, java, score, [Text]}).

bayes_train_confidential(Text) ->
	gen_server:call(?MODULE, {thrift, java, trainConfidential, [Text]}).

bayes_train_public(Text) ->
	gen_server:call(?MODULE, {thrift, java, trainPublic, [Text]}).

bayes_reset() ->
	gen_server:call(?MODULE, {thrift, java, reset, []}).

%%%%%%%%%%%%%% gen_server handles

handle_call({thrift, py, Func, Params}, From, #state{backend_py=TS} = State) ->
	Worker = self(),
	spawn_link(fun() ->
			{ok, Reply} = thrift_client:call(TS, Func, Params),
			Worker ! {async_thrift, Reply, From}
		end),
	{noreply, State, 15000};

handle_call({thrift, java, Func, Params}, From, #state{backend_java=TS} = State) ->
	Worker = self(),
	spawn_link(fun() ->
			{ok, Reply} = thrift_client:call(TS, Func, Params),
			Worker ! {async_thrift, Reply, From}
		end),
	{noreply, State, 15000};

handle_call(stop, _From, #state{backend_py=PY, backend_java=JAVA} = State) ->
	thrift_client:close(PY),
	thrift_client:close(JAVA),
	{stop, normalStop, State#state{backend_py=undefined, backend_java=undefined}};

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
	{ok, PY} = thrift_client:start_link("localhost",9090, mydlp_thrift),
	{ok, JAVA} = thrift_client:start_link("localhost",9091, mydlp_bz_thrift),
	{ok, #state{backend_py=PY, backend_java=JAVA}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

