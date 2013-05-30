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
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Distribution Manager for MyDLP.
%%% @end
%%%-------------------------------------------------------------------

-ifdef(__MYDLP_NETWORK).

-module(mydlp_distributor).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	is_distributed/0,
	init_distribution/0,
	find_authority/0,
	bcast_cluster/0,
	flush_cache/1,
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

is_distributed() ->
	case lists:member(?MODULE, registered()) of
		true ->	gen_server:call(?MODULE, is_distributed);
		false -> false end.

init_distribution() ->
	gen_server:cast(?MODULE, init_distribution).

find_authority() ->
	gen_server:call(?MODULE, find_authority, 15000).

bcast_cluster() ->
	gen_server:cast(?MODULE, bcast_cluster).

flush_cache(ClusterNodes) ->
	gen_server:cast(?MODULE, {cluster_flush_cache, ClusterNodes}).

%%%%%%%%%%%%%% gen_server handles

handle_call(is_distributed, _From, #state{} = State) ->
	{reply, true, State};

handle_call(is_distributed, _From, undefined = State) ->
	{reply, false, State};

handle_call(find_authority, _From, #state{priority=Priority, init_epoch=InitEpoch} = State) ->
	Reply = find_an_authority(nodes(), Priority, InitEpoch),
	{reply, Reply, State};

handle_call({are_you_my_authority, PeerPriority, PeerInitEpoch}, 
		_From, #state{priority=Priority, init_epoch=InitEpoch} = State) ->
	Reply = is_authority({Priority, InitEpoch}, {PeerPriority, PeerInitEpoch}),
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(Msg, _From, State) ->
	?ERROR_LOG("DISTRIBUTOR: Unexpected message: "?S, [Msg]),
	{noreply, State}.

handle_cast(init_distribution, _ExState) ->
        State = case ?CFG(auto_distribution) of
		true ->
        		AllNodes = ?CFG(auto_distribution_nodes),
			Priority = ?CFG(auto_distribution_priority),
			net_adm:world_list(AllNodes),
			InitEpoch = case file:read_file("/var/lib/mydlp/init_epoch") of
				{ok, Bin} -> list_to_integer(binary_to_list(Bin));
				_Else -> unknown end,
			#state{priority=Priority, init_epoch=InitEpoch, nodes_in_conf=AllNodes};
		_ -> undefined end,
	{noreply, State};

handle_cast(bcast_cluster, #state{priority=Priority, init_epoch=InitEpoch} = State) ->
	ClusterNodes = mydlp_mnesia:get_mnesia_nodes(),
	OtherClusterNodes = ClusterNodes -- [node()],
	OtherNodes = nodes() -- OtherClusterNodes,
	case OtherNodes of 
		[] -> ok;
		_ -> case find_an_authority(OtherClusterNodes, Priority, InitEpoch) of
			none -> gen_server:abcast(OtherNodes, ?MODULE, 
					{we_are_up, ClusterNodes, Priority, InitEpoch});
			AuthorNode -> gen_server:abcast([AuthorNode], ?MODULE, 
					bcast_cluster) end end,
	{noreply, State};

handle_cast({we_are_up, ClusterNodes, PeerPriority, PeerInitEpoch}, 
			#state{priority=Priority, init_epoch=InitEpoch} = State) ->
	MnesiaNodes = mydlp_mnesia:get_mnesia_nodes(),
	case lists:any(fun(N) -> lists:member(N, MnesiaNodes) end, ClusterNodes) of 
		true -> ok;
		false -> case is_authority({PeerPriority, PeerInitEpoch}, 
					   {Priority, InitEpoch}) of
			yes -> mydlp_mnesia:new_authority(select_random(ClusterNodes));
			no -> case is_authority({Priority, InitEpoch}, 
						{PeerPriority, PeerInitEpoch}) of 
				yes -> bcast_cluster();
				no -> ok end end end,
	{noreply, State};

handle_cast({cluster_flush_cache, ClusterNodes}, State) ->
	OtherClusterNodes = ClusterNodes -- [node()],
	gen_server:abcast(OtherClusterNodes, ?MODULE, mnesia_flush_cache),
	{noreply, State};

handle_cast(mnesia_flush_cache, State) ->
	mydlp_mnesia:flush_cache(),
	{noreply, State};

handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid} end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	init_distribution(),
	{ok, undefined}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%% implicit

is_authority({Priority, _InitEpoch}, {PeerPriority, _PeerInitEpoch}) when Priority > PeerPriority -> yes;
is_authority({Priority, _InitEpoch}, {PeerPriority, _PeerInitEpoch}) when Priority < PeerPriority -> no;
is_authority({_Priority, InitEpoch}, {_PeerPriority, PeerInitEpoch}) when InitEpoch == unknown; PeerInitEpoch == unknown -> no;
is_authority({_Priority, InitEpoch}, {_PeerPriority, PeerInitEpoch}) when InitEpoch < PeerInitEpoch -> yes;
is_authority({_Priority, _InitEpoch}, {_PeerPriority, _PeerInitEpoch}) -> no.


replies_to_nodes(Replies) -> replies_to_nodes(Replies, []).

replies_to_nodes([{Node,yes}|Replies], Nodes) -> 
	replies_to_nodes(Replies, [Node|Nodes]);
replies_to_nodes([{_Node,no}|Replies], Nodes) -> 
	replies_to_nodes(Replies, Nodes);
replies_to_nodes([], Nodes) -> Nodes.

select_random(List) ->
	RandomMax = length(List),
	N = random:uniform(RandomMax),
	lists:nth(N, List).

find_an_authority(Nodes, Priority, InitEpoch) ->
	case Nodes of
		[] -> none;
		Nodes -> {Replies, _BadNodes} = gen_server:multi_call(Nodes, ?MODULE, 
					{are_you_my_authority, Priority, InitEpoch}, 12000),
			case replies_to_nodes(Replies) of 
				[] -> none ;
				NodeList -> select_random(NodeList) end end.

-endif.

