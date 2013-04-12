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

-ifdef(__MYDLP_ENDPOINT).

-module(mydlp_trapper).
-author("kerem@mydlp.com").
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

-include_lib("eunit/include/eunit.hrl").

-record(state, {
	item_queue,
	consume_inprog=false
}).

%%%%%%%%%%%%%  API

s(Item) -> gen_server:cast(?MODULE, {s, Item}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({s, Item}, #state{item_queue=Q, consume_inprog=InProg} = State) ->
	Q1 = queue:in(Item, Q),
	case InProg of
		true -> ok;
		false -> consume_item() end,
	{noreply,State#state{item_queue=Q1, consume_inprog=true}};

handle_cast(consume_item, #state{item_queue=Q} = State) ->
	case queue:out(Q) of
                {{value, Item}, Q1} ->
                        try     process(Item)
                        catch Class:Error ->
                                ?ERROR_LOG("Scheduler Queue Consume: Error occured: "
                                                "Class: ["?S"]. Error: ["?S"].~n"
                                                "Stack trace: "?S"~n.Item: "?S"~nState: "?S"~n ",
                                                [Class, Error, erlang:get_stacktrace(), Item, State]) end,
                                %%% TODO: for some errors we many return original queue
                        consume_item(),
                        {noreply, State#state{item_queue=Q1}};
                {empty, _} ->
                        {noreply, State#state{consume_inprog=false}}
        end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(consume_now, State) ->
        consume_item(),
        {noreply, State};

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

consume_item(Interval) -> timer:send_after(Interval, consume_now).

consume_item() -> gen_server:cast(?MODULE, consume_item).

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	consume_item(60000),
	{ok, #state{item_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

process(ItemId) -> process(ItemId, 0).

process(_ItemId, I) when I > 15 -> ok;
process(ItemId, TryCount) ->
	TrapPid = mydlp_container:get_trap_pid(),
	case TrapPid of
		P when is_pid(P) -> case is_process_alive(P) of
			true -> process_func(TrapPid, ItemId);
			false -> timer:sleep(4000), process(ItemId, TryCount + 1) end;
		_ -> timer:sleep(4000), process(ItemId, TryCount + 1) end.
	

process_func(TrapPid, ItemId) ->
	PropDict = case mydlp_container:getpropdict(ItemId) of
		{ok, PD} -> PD;
		Err -> throw({error, {cannot_get_propdict, Err, ItemId}}) end,
	Command = case dict:find("command", PropDict) of
		{ok, V} -> V;
		Err2 -> throw({error, {cannot_find_key_command, Err2, PropDict}}) end,
	PropDict1 = dict:erase("command", PropDict),
	PropDictStr = string:join([K ++ "=" ++ V || {K,V} <- dict:to_list(PropDict1)], " "),
	
	Data = case mydlp_container:getdata(ItemId) of
		{ok, D} -> D;
		Err3 -> throw({error, {cannot_get_data, Err3, ItemId}}) end,
	{ok, TempFile} = mydlp_workdir:tempfile(),
	case file:write_file(TempFile, Data) of
		ok -> ok;
		Err4 -> throw({error, {cannot_write_to_tmpfile, Err4, TempFile, Data}}) end,

	TrapMessage = Command ++ " " ++ TempFile ++ " " ++ PropDictStr,
	TrapPid ! {trap_message, TrapMessage},
	
	ok.


-endif.

