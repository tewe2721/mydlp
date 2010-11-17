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
%%% @doc Distribution Manager for MyDLP.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_distributor).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	is_distributed/0,
	find_authority/0,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {priority, init_epoch=unknown, nodes_in_conf}).

%%%%%%%%%%%%% API

is_distributed() -> lists:member(?MODULE, registered()).

find_authority() ->
	gen_server:call(?MODULE, find_authority, 15000).

%%%%%%%%%%%%%% gen_server handles

replies_to_nodes(Replies) -> replies_to_nodes(Replies, []).

replies_to_nodes([{Node,yes}|Replies], Nodes) -> 
	replies_to_nodes(Replies, [Node|Nodes]);
replies_to_nodes([{_Node,no}|Replies], Nodes) -> 
	replies_to_nodes(Replies, Nodes);
replies_to_nodes([], Nodes) -> Nodes.

handle_call(find_authority, _From, #state{priority=Priority, init_epoch=InitEpoch} = State) ->
	Reply = case nodes() of
		[] -> none;
		Nodes -> {Replies, _BadNodes} = gen_server:multi_call(Nodes, ?MODULE, 
					{are_you_my_authority, Priority, InitEpoch}),
			case replies_to_nodes(Replies) of 
				[] -> none ;
				NodeList -> RandomMax = length(NodeList),
					N = random:uniform(RandomMax),
					lists:nth(N, NodeList) end end,
	{reply, Reply, State};

handle_call({are_you_my_authority, PeerPriority, _PeerInitEpoch}, 
		_From, #state{priority=Priority} = State) 
		when Priority > PeerPriority ->
	{reply, yes, State};

handle_call({are_you_my_authority, PeerPriority, _PeerInitEpoch}, 
		_From, #state{priority=Priority} = State) 
		when Priority < PeerPriority ->
	{reply, no, State};

handle_call({are_you_my_authority, _PeerPriority, PeerInitEpoch}, 
		_From, #state{init_epoch=InitEpoch} = State) 
		when InitEpoch == unknown ; PeerInitEpoch == unknown ->
	{reply, no, State};

handle_call({are_you_my_authority, _PeerPriority, PeerInitEpoch}, 
		_From, #state{init_epoch=InitEpoch} = State) 
		when InitEpoch < PeerInitEpoch ->
	{reply, yes, State};

handle_call({are_you_my_authority, _PeerPriority, _PeerInitEpoch}, _From,  State) ->
	{reply, no, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->

	ConfList = case application:get_env(mydlp, auto_distribution) of
		{ok, CL} -> CL;
		_Else -> ?AUTO_DIST
	end,

        case lists:keyfind(activate, 1, ConfList) of
		{activate, true} ->
        		{all_nodes, AllNodes} = lists:keyfind(all_nodes, 1, ConfList),
			Priority = case lists:keyfind(priority, 1, ConfList) of
				{priority, P} -> P;
				_Else2 -> 100 end,

			case gen_server:start_link({local, ?MODULE}, ?MODULE, [Priority, AllNodes], []) of
				{ok, Pid} -> {ok, Pid};
				{error, {already_started, Pid}} -> {ok, Pid} end;

		_ -> ignore end.

stop() ->
	gen_server:call(?MODULE, stop).

init([Priority, AllNodes]) ->
	net_adm:world_list(AllNodes),
	InitEpoch = case file:read_file("/var/lib/mydlp/init_epoch") of
		{ok, Bin} -> list_to_integer(binary_to_list(Bin));
		_Else -> unknown end,

	{ok, #state{priority=Priority, init_epoch=InitEpoch, nodes_in_conf=AllNodes}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

