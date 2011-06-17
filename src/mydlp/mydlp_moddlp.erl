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

-module(mydlp_moddlp).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	init_entity/0,
	push_data/2,
	analyze/1,
	close_entity/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {entity_tree}).


init_entity() -> gen_server:call(?MODULE, i).

push_data(EntityId, Data) -> gen_server:call(?MODULE, {p, EntityId, Data}, 15000).

analyze(EntityId) -> gen_server:call(?MODULE, {a, EntityId}, 300000).

close_entity(EntityId) -> gen_server:call(?MODULE, {c, EntityId}).

%%%%%%%%%%%%%% gen_server handles

handle_def(i) -> 0;
handle_def({a, _}) -> true;
handle_def(_) -> ok.

handle(i, Tree) ->
	EntityId = random:uniform(100000000),
	Reply = EntityId,
	Tree1 = gb_trees:enter(EntityId, <<>>, Tree),
	{ok, Reply, Tree1};

handle({p, EntityId, Data}, Tree) ->
	ExData = gb_trees:get(EntityId, Tree),
	Tree1 = gb_trees:enter(EntityId, <<ExData/binary, Data/binary>>, Tree),
	{ok, ok, Tree1};

handle({a, EntityId}, Tree) ->
	Data = gb_trees:get(EntityId, Tree),
	Reply = case mydlp_acl:qm([#file{name="apache-response", dataref=?BB_C(Data)}]) of
		pass -> true;
		{Block = {block, _ }, AclR} -> log_req(Block, AclR), false;
		{Log = {log, _ }, AclR} -> log_req(Log, AclR), true; 
		{pass, AclR} -> log_req(pass, AclR), true;
		block -> false end,
	{ok, Reply, Tree};

handle({c, EntityId}, Tree) ->
	Tree1 = gb_trees:delete(EntityId, Tree),
	{ok, ok, Tree1};

handle(_,_) -> throw(undefined_handle).

handle_call({a, _} = Call, From, #state{entity_tree=Tree} = State) ->
	Worker = self(),
	spawn_link(fun() ->
			{ok, Reply, _NoTreeChange} = try handle(Call, Tree) 
				catch _:_ -> {ok, handle_def(Call), Tree} end,
			Worker ! {async_reply, Reply, From}
		end),
	{noreply, State};

handle_call(Call, _From, #state{entity_tree=Tree} = State) ->
	{ok, Reply, Tree1} = try handle(Call, Tree) 
		catch _:_ -> {ok, handle_def(Call), Tree} end,
	{reply, Reply, State#state{entity_tree=Tree1}};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
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
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	{ok, #state{entity_tree=gb_trees:empty()}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

log_req(Action, {{rule, RuleId}, {file, File}, {matcher, Matcher}, {misc, Misc}}) ->
        ?ACL_LOG(mod_dlp, RuleId, Action, nil, nil, nil, Matcher, File, Misc).

-endif.

