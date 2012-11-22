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

-module(mydlp_quarantine).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	q/1,
	q/2,
	s/3,
	l/2,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {quarantine_dir, quarantine_uid, quarantine_gid}).

-define(QUARANTINE_TIMEOUT, 60000).

q(Data) -> gen_server:call(?MODULE, {q, Data}, 90000).

q(Hash, Data) -> gen_server:call(?MODULE, {q, Hash, Data}, ?QUARANTINE_TIMEOUT).

s(Cat, Id, Data) -> gen_server:call(?MODULE, {s, Cat, Id, Data}, ?QUARANTINE_TIMEOUT).

l(Cat, Id) -> gen_server:call(?MODULE, {l, Cat, Id}, ?QUARANTINE_TIMEOUT).

%%%%%%%%%%%%%% gen_server handles

handle_call({q, Data}, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
		Hash = mydlp_api:md5_hex(Data),
		Reply = q(Hash, Data),

		Worker ! {async_reply, Reply, From}
	end, 90000),
	{noreply, State};

handle_call({q, Hash, Data}, From, #state{quarantine_dir=Dir, quarantine_uid=Uid, quarantine_gid=Gid} = State) ->
	Worker = self(),
	?ASYNC0(fun() ->
		L1Dir = Dir ++ string:substr(Hash, 1, 1),
		case filelib:is_dir(L1Dir) of
			false -> file:make_dir(L1Dir),
				file:change_owner(L1Dir, Uid, Gid);
			true -> ok end,

		L2Dir = L1Dir ++ "/" ++ string:substr(Hash, 2, 2),
		case filelib:is_dir(L2Dir) of
			false -> file:make_dir(L2Dir),
				file:change_owner(L2Dir, Uid, Gid);
			true -> ok end,

		FilePath = L2Dir ++ "/" ++ Hash,
		case filelib:is_file(FilePath) of
			true -> ok;
			false -> file:write_file(FilePath, Data, [raw]), 
				file:change_owner(FilePath, Uid, Gid) end,

		Worker ! {async_reply, {ok, FilePath}, From}
	end),
	
	{noreply, State};

handle_call({s, Cat, Id, Data}, From, #state{quarantine_dir=Dir, quarantine_uid=Uid, quarantine_gid=Gid} = State) ->
	Worker = self(),
	?ASYNC0(fun() ->
		CName = case Cat of
			payload -> "payload";
			Else -> throw({ierror, {unkown_category, Else}}) end,
		IdS = case Id of
			I when is_integer(I) -> integer_to_list(I);
			I when is_binary(I) -> binary_to_list(I);
			I when is_list(I) -> I end,
		FilePath = Dir ++ CName ++ "/" ++ IdS,
		case filelib:is_file(FilePath) of
			true -> ok;
			false -> file:write_file(FilePath, Data, [raw]), 
				file:change_owner(FilePath, Uid, Gid) end,

		Worker ! {async_reply, {ok, FilePath}, From}
	end),
	
	{noreply, State};

handle_call({l, Cat, Id}, From, #state{quarantine_dir=Dir} = State) ->
	Worker = self(),
	?ASYNC0(fun() ->
		CName = case Cat of
			payload -> "payload";
			Else -> throw({ierror, {unkown_category, Else}}) end,
		IdS = case Id of
			I when is_integer(I) -> integer_to_list(I);
			I when is_binary(I) -> binary_to_list(I);
			I when is_list(I) -> I end,
		FilePath = Dir ++ CName ++ "/" ++ IdS,
		Reply = case filelib:is_file(FilePath) of
			true -> {ok, Data} = file:read_file(FilePath), file:delete(FilePath), {ok, Data};
			false -> {ierror, nofile} end,
		Worker ! {async_reply, Reply, From}
	end),
	
	{noreply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid} end.

stop() -> gen_server:call(?MODULE, stop).

init([]) ->
	Dir = ?CFG(quarantine_dir),
	Uid = ?CFG(quarantine_uid),
	Gid = ?CFG(quarantine_gid),

	PDir = Dir ++ "payload",

	case filelib:is_dir(PDir) of
		false -> file:make_dir(PDir),
			file:change_owner(PDir, Uid, Gid);
		true -> ok end,

	{ok, #state{quarantine_dir=Dir, quarantine_uid=Uid, quarantine_gid=Gid}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-endif.

