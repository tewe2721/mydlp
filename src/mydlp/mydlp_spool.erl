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

-module(mydlp_spool).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	create_spool/1,
	register_consumer/2,
	consume_next/1,
	consume_all/1,
	push/2,
	lock/1,
	release/1,
	pop/1,
	poppush/1,
	poppush_all/1,
	delete/1,
	is_empty/1,
	total_size/1,
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
	spools = dict:new()
	}).

-record(spool, {
	name = "",
	consume_fun,
	consume_prog = false
	}).

-define(SPOOL_DIR(SpoolName), filename:absname(SpoolName, ?CFG(spool_dir)) ).

%%%% API

create_spool(SpoolName) -> gen_server:cast(?MODULE, {create_spool, SpoolName}).

delete(Ref) -> gen_server:cast(?MODULE, {delete, Ref}).

register_consumer(SpoolName, ConsumeFun) -> gen_server:cast(?MODULE, {register_consumer, SpoolName, ConsumeFun}).

consume_next(SpoolName) -> gen_server:cast(?MODULE, {consume_next, SpoolName}).

consume_all(SpoolName) -> gen_server:cast(?MODULE, {consume_all, SpoolName}).

lock(Item) -> gen_server:cast(?MODULE, {lock, Item}).

release(Item) -> gen_server:cast(?MODULE, {release, Item}).

push(SpoolName, Item) -> gen_server:call(?MODULE, {push, SpoolName, Item}, 30000).

is_empty(SpoolName) -> gen_server:call(?MODULE, {is_empty, SpoolName}, 30000).

pop(SpoolName) -> gen_server:call(?MODULE, {pop, SpoolName}, 30000).

poppush(SpoolName) -> gen_server:call(?MODULE, {poppush, SpoolName}, 30000).

poppush_all(SpoolName) -> gen_server:call(?MODULE, {poppush_all, SpoolName}, 60000).

total_size(SpoolName) -> gen_server:call(?MODULE, {total_size, SpoolName}, 30000).

%%%%%%%%%%%%%% gen_server handles

handle_call({pop, SpoolName}, _From, #state{spools = Spools} = State) ->
	Reply = ?FLER(fun() -> case dict:is_key(SpoolName, Spools) of
		true ->	case file:list_dir(?SPOOL_DIR(SpoolName)) of
				{ok, []} -> {ierror, spool_is_empty};
				{ok, [FN0|_]} -> FN = filename:absname(FN0, ?SPOOL_DIR(SpoolName)),
						{ok, Bin} = file:read_file(FN),
						Item = erlang:binary_to_term(Bin),
						ok = file:delete(FN),
						{ok, Item};
				{error, Error} -> {ierror, Error} end;
		false -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]),
			{ierror, spool_does_not_exist} end
	end, pop),
	{reply, Reply, State};

handle_call({is_empty, SpoolName}, _From, #state{spools = Spools} = State) ->
	Reply = ?FLER(fun() -> case dict:is_key(SpoolName, Spools) of
		true ->	case file:list_dir(?SPOOL_DIR(SpoolName)) of
				{ok, []} -> {ok, true};
				{ok, _Else} -> {ok, false};
				{error, Error} -> {ierror, Error} end;
		false -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]),
			{ierror, spool_does_not_exist} end
	end, is_empty),
	{reply, Reply, State};

handle_call({poppush, SpoolName}, _From, #state{spools = Spools} = State) ->
	Reply = ?FLER(fun() -> case dict:is_key(SpoolName, Spools) of
		true ->	case file:list_dir(?SPOOL_DIR(SpoolName)) of
				{ok, []} -> {ierror, spool_is_empty};
				{ok, [_|_] = FNs} -> 
						case acquire_fn(FNs) of
							none -> {ierror, spool_is_empty};
							FN0 -> renew_ref(SpoolName,FN0) end;
				{error, Error2} -> {ierror, Error2} end;
		false -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]),
			{ierror, spool_does_not_exist} end
	end, poppush),
	{reply, Reply, State};

handle_call({poppush_all, SpoolName}, _From, #state{spools = Spools} = State) ->
	Reply = ?FLER(fun() -> case dict:is_key(SpoolName, Spools) of
		true ->	case file:list_dir(?SPOOL_DIR(SpoolName)) of
				{ok, []} -> {ierror, spool_is_empty};
				{ok, [_|_] = FNs} ->	RefItemPL = [ case renew_ref(SpoolName, FN) of
									{ok, Ref, Item} -> {Ref, Item};
									Else -> ?ERROR_LOG("Error with spool file: Error: "?S, [Else]),
										[] end
									|| FN <- FNs, lock_item(FN) == true ],
							{ok, lists:flatten(RefItemPL)};
				{error, Error2} -> {ierror, Error2} end;
		false -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]),
			{ierror, spool_does_not_exist} end
	end, poppush_all),
	{reply, Reply, State};

handle_call({push, SpoolName, Item}, _From, #state{spools = Spools} = State) ->
	Reply = ?FLER(fun() -> case dict:is_key(SpoolName, Spools) of
		true ->	Bin = erlang:term_to_binary(Item, [compressed]),
			NRef = now(),
			Ref = {SpoolName, NRef},
			FP = mydlp_api:ref_to_fn(?SPOOL_DIR(SpoolName), "item", NRef),
			ok = file:write_file(FP, Bin),
			{ok, Ref};
		false -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]),
			{ierror, spool_does_not_exist} end
	end, push),
	{reply, Reply, State};

handle_call({total_size, SpoolName}, _From, #state{spools = Spools} = State) ->
	Reply = ?FLER(fun() -> case dict:is_key(SpoolName, Spools) of
		true ->	get_dir_size(?SPOOL_DIR(SpoolName));
		false -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]),
			{ierror, spool_does_not_exist} end
	end, total_size),
	{reply, Reply, State};

handle_call({lock, Item}, _From, State) ->
	Reply = ?FLER(fun() -> lock_item(Item) end, lock),
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({delete, {SpoolName, NRef}}, #state{spools = Spools} = State) ->
	?ASYNC0(fun() -> case dict:is_key(SpoolName, Spools) of
		true ->	FP = mydlp_api:ref_to_fn(?SPOOL_DIR(SpoolName), "item", NRef),
			case file:delete(FP) of
				ok ->  ok;
				Error -> ?ERROR_LOG("Can not delete spool ref. SpoolName: "?S"~nRefPath: "?S"~nError: "?S"", 
					[SpoolName, FP, Error]) end;
		false -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]) end
	end),
	{noreply, State};

handle_cast({create_spool, SpoolName}, #state{spools = Spools} = State) ->
	case dict:is_key(SpoolName, Spools) of
		true -> {noreply, State};
		false -> case filelib:ensure_dir(?SPOOL_DIR(SpoolName)) of
				ok -> NewSpool = #spool{name=SpoolName},
					{noreply, State#state{spools=dict:store(SpoolName, NewSpool, Spools)}};
				Error -> ?ERROR_LOG("Can not create spool directory. Name: "?S"~nDir: "?S"~nError: "?S"", 
						[SpoolName, ?SPOOL_DIR(SpoolName), Error]),
					{noreply, State}
				end
		end;

handle_cast({register_consumer, SpoolName, ConsumeFun}, #state{spools = Spools} = State) ->
	case dict:find(SpoolName, Spools) of
		{ok, Spool} -> NewSpool = Spool#spool{consume_fun=ConsumeFun},
				{noreply, State#state{spools=dict:store(SpoolName, NewSpool, Spools)}};
		error -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]),
				{noreply, State}
		end;

handle_cast({consume_next, SpoolName}, #state{spools = Spools} = State) ->
	Worker = self(),
	case dict:find(SpoolName, Spools) of
		{ok, #spool{consume_prog = false, consume_fun=ConsumeFun} = Spool} ->
			?ASYNC(fun() -> case mydlp_spool:is_empty(SpoolName) of
					{ok, false} -> 
						{ok, Ref, Item} = mydlp_spool:poppush(SpoolName),
						ConsumeFun(Ref, Item);
					{ok, true} -> ok end,
				Worker ! {consume_completed, SpoolName}
			end, 120000),
			{noreply, State#state{spools=dict:store(SpoolName, Spool#spool{consume_prog = true}, Spools)}};
		{ok, #spool{consume_prog = true}} ->
			{noreply, State};
		error -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]),
			{noreply, State} end;

handle_cast({consume_all, SpoolName}, #state{spools = Spools} = State) ->
	Worker = self(),
	case dict:find(SpoolName, Spools) of
		{ok, #spool{consume_prog = false, consume_fun=ConsumeFun} = Spool} ->
			?ASYNC(fun() -> case mydlp_spool:is_empty(SpoolName) of
					{ok, false} -> 
						{ok, RefItemPL} = mydlp_spool:poppush_all(SpoolName),
						%% TODO: lock
						[ConsumeFun(Ref, Item) || {Ref, Item} <- RefItemPL],
						ok;
					{ok, true} -> ok end,
				Worker ! {consume_completed, SpoolName}
			end, 1200000),
			{noreply, State#state{spools=dict:store(SpoolName, Spool#spool{consume_prog = true}, Spools)}};
		{ok, #spool{consume_prog = true}} ->
			{noreply, State};
		error -> ?ERROR_LOG("Spool does not exist: Name: "?S" Dir: "?S, 
				[SpoolName, ?SPOOL_DIR(SpoolName)]),
			{noreply, State} end;

handle_cast({release, Item}, State) ->
	release_item(Item),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info({consume_completed, SpoolName}, #state{spools = Spools} = State) ->
	case dict:find(SpoolName, Spools) of
		{ok, #spool{consume_prog = true} = Spool} ->
			{noreply, State#state{spools=dict:store(SpoolName, Spool#spool{consume_prog = false}, Spools)}};
		_Else -> {noreply, State} end;

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid};
		Else -> Else
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	Ret = case filelib:ensure_dir(?CFG(spool_dir) ++ "/") of
		ok -> {ok, #state{}};
		Error -> Error end,

	create_spools(),
	Ret.


-ifdef(__MYDLP_NETWORK).

create_spools() ->
        ConsumeFun = fun(Ref, Item) ->
                mydlp_smtpc:mail(Ref, Item)
        end,
        mydlp_spool:create_spool("smtp"),
        mydlp_spool:register_consumer("smtp", ConsumeFun),
	ok.
	
-endif.

-ifdef(__MYDLP_ENDPOINT).

create_spools() ->
        ConsumeFun = fun(Ref, Item) ->
                mydlp_item_push:p(Ref, Item)
        end,
        mydlp_spool:create_spool("log_push"),
        mydlp_spool:register_consumer("log_push", ConsumeFun),
	ok.
	
-endif.


terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

renew_ref(SpoolName, FN0) -> 
	FN = filename:absname(FN0, ?SPOOL_DIR(SpoolName)),
	{ok, Bin} = file:read_file(FN),
	Item = erlang:binary_to_term(Bin),
	NRef = now(),
	Ref = {SpoolName, NRef},
	FP = mydlp_api:ref_to_fn(?SPOOL_DIR(SpoolName), "item", NRef),
	case file:rename(FN, FP) of
		ok -> 	release_item(FN),
			lock_item(FP),
			{ok, Ref, Item};
		{error, Error} -> {ierror, Error} end.

lock_item(FilePath) when is_list(FilePath)->
	Reply = global:set_lock({FilePath, spool}, nodes(), 0),
	timer:apply_after(600000, global, del_lock, [{FilePath, spool}, nodes()]),
	Reply;

lock_item({SpoolName, NRef}) ->
	FP = mydlp_api:ref_to_fn(?SPOOL_DIR(SpoolName), "item", NRef),
	lock_item(FP).

acquire_fn([FN|Rest]) ->
	case lock_item(FN) of
		true -> FN;
		false -> acquire_fn(Rest) end;
acquire_fn([]) -> none.

release_item(FilePath) when is_list(FilePath)->
	global:del_lock({FilePath, spool}, nodes()),
	ok;

release_item({SpoolName, NRef}) ->
	FP = mydlp_api:ref_to_fn(?SPOOL_DIR(SpoolName), "item", NRef),
	release_item(FP).

get_dir_size(Dir) ->
	case file:list_dir(Dir) of
		{ok, FNs} -> 	FSs = [ get_file_size(FN) || FN <- FNs],
				lists:sum(FSs);
		{error, Reason} -> throw({error, Reason}) end.

get_file_size(FN) ->
	case filelib:is_regular(FN) of
		true -> filelib:file_size(FN);
		false -> throw({error, is_not_a_regular_file}) end.

