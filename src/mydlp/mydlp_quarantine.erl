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

-ifdef(__MYDLP_NETWORK).

-module(mydlp_quarantine).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	s/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {quarantine_dir, quarantine_uid, quarantine_gid}).

s(Data) -> gen_server:call(?MODULE, {s, Data}, 15000).

%%%%%%%%%%%%%% gen_server handles

handle_call({s, Data}, From, #state{quarantine_dir=Dir, quarantine_uid=Uid, quarantine_gid=Gid} = State) ->
	Worker = self(),
	spawn_link(fun() ->
		Hash = mydlp_api:md5_hex(Data),

		L1Dir = Dir ++ string:substr(Hash, 1, 1),
		case filelib:is_regular(L1Dir) of
			false -> file:make_dir(L1Dir),
				file:change_owner(L1Dir, Uid, Gid);
			true -> ok end,

		L2Dir = L1Dir ++ "/" ++ string:substr(Hash, 2, 2),
		case filelib:is_regular(L2Dir) of
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
	ConfList = case application:get_env(quarantine) of
		{ok, CL} -> CL;
		_Else -> ?QUARANTINE end,

	{dir, Dir} = lists:keyfind(dir, 1, ConfList),
	{uid, Uid} = lists:keyfind(uid, 1, ConfList),
	{gid, Gid} = lists:keyfind(gid, 1, ConfList),
	{ok, #state{quarantine_dir=Dir, quarantine_uid=Uid, quarantine_gid=Gid}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-endif.

