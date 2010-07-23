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
-module(mydlp_mysql).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	compile_filters/0,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {host, port, user, password, database, pool_size, master_pid, pool_pids}).

%%%%%%%%%%%%% MyDLP Thrift RPC API

compile_filters() ->
	gen_server:call(?MODULE, compile_filters).

%%%%%%%%%%%%%% gen_server handles

psq(PreparedKey) when is_atom(PreparedKey) ->
	psq(PreparedKey, []).

psq(PreparedKey, Params) when is_atom(PreparedKey), is_list(Params) ->
	case mysql:execute(p, PreparedKey, Params) of
		{data,{mysql_result,_,Result,_,_}} -> {ok, Result};
		Else -> {error, Else}
	end.

handle_call(compile_filters, _From, State) ->
	% under sweer construction :)
	erlang:display(psq(select_filters)),
	erlang:display(psq(select_rules_by_fid, [12])),

	{reply, ok, State};

handle_call(stop, _From,  State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info({'DOWN', _, _, MPid , _}, #state{master_pid=MPid} = State) ->
	{stop, normal, State};

handle_info({'DOWN', _, _, Pid , _}, #state{host=Host,
		user=User, password=Password, database=DB, 
		pool_pids=PoolPids} = State) ->
	PoolPids1 = lists:delete(Pid, PoolPids),
	{ok,NewPid} = mysql:connect(p, Host, undefined, User, Password, DB, true),
	{stop, normal, State#state{pool_pids=[NewPid|PoolPids1]}};

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
	ConfList = case application:get_env(mysql) of
		{ok, CL} -> CL;
		_Else -> ?MYSQL
	end,

	{host, Host} = lists:keyfind(host, 1, ConfList),
	{port, Port} = lists:keyfind(port, 1, ConfList),
	{user, User} = lists:keyfind(user, 1, ConfList),
	{password, Password} = lists:keyfind(password, 1, ConfList),
	{database, DB} = lists:keyfind(database, 1, ConfList),
	{pool_size, PoolSize} = lists:keyfind(pool_size, 1, ConfList),

	{ok, MPid} = mysql:start_link(p, Host, Port, User, Password, DB, fun(_,_,_,_) -> ok end),
	erlang:monitor(process, MPid), 
	
	PoolReturns = [ mysql:connect(p, Host, undefined, User, Password, DB, true) || _I <- lists:seq(1, PoolSize)],
	PPids = [ P || {ok, P} <- PoolReturns ],
	[ erlang:monitor(process, P) || P <- PPids ],

	[ mysql:prepare(Key, Query) || {Key, Query} <- [
		{select_filters, <<"SELECT id,name FROM sh_filter WHERE is_active=TRUE">>},
		{select_rules_by_fid, <<"SELECT id,action FROM sh_rule WHERE is_nw_active=TRUE and filter_id=?">>}
	]],

	{ok, #state{host=Host, port=Port, 
			user=User, password=Password, 
			database=DB, pool_size=PoolSize, 
			master_pid=MPid, pool_pids=PPids}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

