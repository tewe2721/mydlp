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

-module(mydlp_item_push).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	p/1,
	p/2,
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
	queue_size = 0,
	max_queue_size = 0
}).

%%%%%%%%%%%%%  API

p(Item) -> 
	gen_server:cast(?MODULE, {p, {i, Item}}).

p(Ref, Item) ->
	gen_server:cast(?MODULE, {p, {i, Ref, Item}}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({p, Item}, #state{item_queue=Q, queue_size=QS, max_queue_size=MQS} = State) 
		when QS < MQS ->
	Q1 = queue:in(Item, Q),
	ItemSize = predict_serialized_size(Item),
	NextQS = QS+ItemSize,
	case NextQS > MQS of
		true -> consume_item();
		false -> ok end,
	{noreply,State#state{item_queue=Q1, queue_size=NextQS}};

handle_cast({p, Item}, #state{item_queue=Q, queue_size=QS} = State) ->
	Q1 = queue:in(Item, Q),
	ItemSize = predict_serialized_size(Item),
	{noreply, State#state{item_queue=Q1, queue_size=QS+ItemSize}};

handle_cast(consume_item, #state{item_queue=Q} = State) ->
	case queue:is_empty(Q) of
		false -> try 	
				ItemList = queue:to_list(Q),
				Refs = process_item(ItemList),
				lists:foreach(fun(R) -> mydlp_spool:delete(R) end, Refs),
				mydlp_spool:consume_all("push"),
				consume_item(?CFG(sync_interval)),
				{noreply, State#state{item_queue=queue:new(), queue_size=0}}
			catch Class:Error ->
			?ERROR_LOG("Push Item Consume: Error occured: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n.~nState: "?S"~n ",
				[Class, Error, erlang:get_stacktrace(), State]),
				% temporary change for test deployment
				ItemList1 = queue:to_list(Q),
				lists:foreach(fun(I) -> case I of
							{i, Item} -> mydlp_spool:push("push", Item);
				       			_Else -> ok end
					end, ItemList1),
				consume_item(?CFG(sync_interval)),
				{noreply, State#state{item_queue=queue:new(), queue_size=0}} end; 
				%consume_item(15000),
				%{noreply, State} end;
		true -> consume_item(?CFG(sync_interval)),
			{noreply, State} end;

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
	ConsumeFun = fun(Ref, Item) ->
		mydlp_item_push:p(Ref, Item)
	end,
	mydlp_spool:create_spool("push"),
	mydlp_spool:register_consumer("push", ConsumeFun),
	{ok, #state{item_queue=queue:new(), max_queue_size=?CFG(maximum_push_size)}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

process_item(TList) ->
	process_item(TList, [], []).

process_item([{i, Item}|Rest], RefList, ItemList) ->
	process_item(Rest, RefList, [Item|ItemList]);

process_item([{i, Ref, Item}|Rest], RefList, ItemList) ->
	process_item(Rest, [Ref|RefList], [Item|ItemList]);

process_item([], RefList, ItemList) ->
	send_item(ItemList),
	RefList.

send_item(Item)  ->  
	ItemBin = erlang:term_to_binary(Item, [compressed]),
	ItemId = new_item_id(),
	ItemSize = size(ItemBin),
	ChunkNumTotal = (ItemSize div ?CFG(maximum_push_size)) + 1,
	send_item(ItemId, ItemBin, ItemSize, 1, ChunkNumTotal), ok.

% send_item(_Item) -> ok. % TODO log unkown item.

send_item(_ItemId, _ItemBin, RemainingItemSize, _ChunkNumTotal, _ChunkNumTotal) when RemainingItemSize < 0 ->
	throw({error, negative_remaining_item_size});
send_item(ItemId, ItemBin, RemainingItemSize, ChunkNumTotal, ChunkNumTotal) ->
	ChunkSize = RemainingItemSize,
	<<ChunkData:ChunkSize/binary>> = ItemBin,
	push_chunk(ItemId, ChunkData, ChunkNumTotal, ChunkNumTotal);
send_item(ItemId, ItemBin, RemainingItemSize, ChunkNum, ChunkNumTotal) ->
	ChunkSize = ?CFG(maximum_push_size),
	<<ChunkData:ChunkSize/binary, ItemRestBin/binary >> = ItemBin,
	push_chunk(ItemId, ChunkData, ChunkNum, ChunkNumTotal),
	send_item(ItemId, ItemRestBin, RemainingItemSize - ChunkSize, ChunkNum + 1, ChunkNumTotal).

push_chunk(ItemId, ChunkData, ChunkNum, ChunkNumTotal) ->
	ItemIdS = integer_to_list(ItemId),
	ChunkNumS = integer_to_list(ChunkNum),
	ChunkNumTotalS = integer_to_list(ChunkNumTotal),
	Url = "https://" ++ ?CFG(management_server_address) ++ "/mydlp-web-manager/receive.php?o=push&" ++
			"i=" ++ ItemIdS ++ "&c=" ++ ChunkNumS ++ "&t=" ++ ChunkNumTotalS,
	case http_req(Url, ChunkData) of
		{ok, "error"} -> throw(http_returned_error);
		{ok, "ok"} -> ok;
		Else -> throw(Else) end.

new_item_id() ->
	Url = "https://" ++ ?CFG(management_server_address) ++ "/mydlp-web-manager/receive.php?o=begin",
	case http_req(Url) of
		{ok, "error"} -> throw(http_returned_error);
		{ok, Ret} -> list_to_integer(Ret);
		Else -> throw(Else) end.

http_req(Url) ->
        ReqRet = (catch http:request(Url)),
	http_req1(ReqRet).

http_req(Url, Data) when is_binary(Data) ->
	ReqRet = (catch http:request(post, {Url, [], "application/octet-stream", Data}, [], [])),
	http_req1(ReqRet).

http_req1(ReqRet) -> 
        case ReqRet of
                {ok, {{_HttpVer, Code, _Msg}, _Headers, Body}} -> 
                        case {Code, Body} of
                                {200, RetBody} -> {ok, RetBody};
                                {Else1, _Data} -> ?ERROR_LOG("ITEMPUSH: An error occured during HTTP req: Code="?S"~n", [Else1]),
						{error, {http_code, Else1}} end;
                Else -> ?ERROR_LOG("ITEMPUSH: An error occured during HTTP req: Obj="?S"~n", [Else]),
				{error, {http_req_not_ok, Else}} end.

predict_serialized_size({i, Item}) -> 
	predict_serialized_size0(Item);
predict_serialized_size({i, _Ref, Item}) -> 
	predict_serialized_size0(Item).
%predict_serialized_size(Item) -> predict_serialized_size0(Item).

predict_serialized_size0({seap_log, {_Proto, _RuleId, _Action, _Ip, _User, _To, _Matcher, #file{data=undefined}, _Misc}}) -> 128;
predict_serialized_size0({seap_log, {_Proto, _RuleId, _Action, _Ip, _User, _To, _Matcher, #file{data=Data}, _Misc}}) ->
	size(Data) + 128;
predict_serialized_size0({seap_log, _LogTerm}) -> 128;
predict_serialized_size0(Else) -> 
	?ERROR_LOG("PREDICTSIZE: Unknown item. Cannot predict. Return maximum_push_size+1 as size: Item="?S"~n", [Else]),
	?CFG(maximum_push_size) + 1.


-endif.

