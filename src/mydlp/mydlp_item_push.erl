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
	has_failure = false
}).

%%%%%%%%%%%%%  API

p(Item) -> gen_server:cast(?MODULE, {p, Item}).

p(Ref, Item) -> gen_server:cast(?MODULE, {p, Ref, Item}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({p, Item}, State) ->
	SpoolSize = mydlp_spool:total_size("log"),
	SoftLimit = ?CFG(endpoint_spool_soft_limit),
	HardLimit = ?CFG(endpoint_spool_hard_limit),
	case SpoolSize of
		{ierror, Error} -> ?ERROR_LOG("Error occured when getting queue size: Error: ["?S"].", [Error]);
		S when S < SoftLimit -> 
			{ok, Ref} = mydlp_spool:push("log", Item),
			p(Ref, Item);
		S when S < HardLimit -> 
			?ERROR_LOG("Spool soft limit had been reached, stripping logs.", []),
			StrippedItem = strip_item(Item),
			{ok, Ref} = mydlp_spool:push("log", StrippedItem),
			p(Ref, Item);
		_ ->	?ERROR_LOG("Spool hard limit had been reached, ignoring logs.", []),
			ok end,
	{noreply, State};

handle_cast({p, Ref, Item}, #state{item_queue=Q, queue_size=QS, has_failure=HF} = State) ->
	Q1 = queue:in({Ref, Item}, Q),
	ItemSize = predict_serialized_size(Item),
	NextQS = QS+ItemSize,
	case NextQS > ?CFG(maximum_push_size) of
		true -> consume_item();
		false -> ok end,
	case HF of
		false -> mydlp_spool:consume_next("log");
		true -> ok end,
	{noreply,State#state{item_queue=Q1, queue_size=NextQS}};

handle_cast(consume_item, #state{item_queue=Q} = State) ->
	case queue:is_empty(Q) of
		false -> HF = try RefItemList = queue:to_list(Q),
				process_ril(RefItemList),
				false
			catch Class:Error ->
				?ERROR_LOG("Push Item Consume: Error occured: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n.~nState: "?S"~n ",
					[Class, Error, erlang:get_stacktrace(), State]), true end,
			consume_item(?CFG(sync_interval)),
			{noreply, State#state{item_queue=queue:new(), queue_size=0, has_failure=HF}};
		true -> mydlp_spool:consume_next("log"),
			consume_item(?CFG(sync_interval)),
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
	consume_item(60000),
	{ok, #state{item_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

process_ril(RefItemList) ->
	RefList = [ R || {R,_I} <- RefItemList],
	ItemList = [ I || {_R,I} <- RefItemList],
	try	ok = process_item(ItemList),
		lists:foreach(fun(R) -> mydlp_spool:delete(R) end, RefList)
	catch Class:Error ->
		?ERROR_LOG("Process Item : Error occured: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n.~nRefItemList: "?S"~n ",
			[Class, Error, erlang:get_stacktrace(), RefItemList])
	after 	lists:foreach(fun(R) -> mydlp_spool:release(R) end, RefList) end.

process_item(Item)  ->  
	ItemBin = erlang:term_to_binary(Item, [compressed]),
	ItemId = new_item_id(),
	ItemSize = size(ItemBin),
	ChunkNumTotal = (ItemSize div ?CFG(maximum_push_size)) + 1,
	process_item(ItemId, ItemBin, ItemSize, 1, ChunkNumTotal).

% process_item(_Item) -> ok. % TODO log unkown item.

process_item(_ItemId, _ItemBin, RemainingItemSize, _ChunkNumTotal, _ChunkNumTotal) when RemainingItemSize < 0 ->
	throw({error, negative_remaining_item_size});
process_item(ItemId, ItemBin, RemainingItemSize, _ChunkNum = ChunkNumTotal, ChunkNumTotal) ->
	ChunkSize = RemainingItemSize,
	<<ChunkData:ChunkSize/binary>> = ItemBin,
	push_chunk(ItemId, ChunkData, ChunkNumTotal, ChunkNumTotal);
process_item(ItemId, ItemBin, RemainingItemSize, ChunkNum, ChunkNumTotal) ->
	ChunkSize = ?CFG(maximum_push_size),
	<<ChunkData:ChunkSize/binary, ItemRestBin/binary >> = ItemBin,
	push_chunk(ItemId, ChunkData, ChunkNum, ChunkNumTotal),
	process_item(ItemId, ItemRestBin, RemainingItemSize - ChunkSize, ChunkNum + 1, ChunkNumTotal).

push_chunk(ItemId, ChunkData, ChunkNum, ChunkNumTotal) ->
	ItemIdS = integer_to_list(ItemId),
	ChunkNumS = integer_to_list(ChunkNum),
	ChunkNumTotalS = integer_to_list(ChunkNumTotal),
	Url = "https://" ++ ?CFG(management_server_address) ++ "/receive?o=push&" ++
			"i=" ++ ItemIdS ++ "&c=" ++ ChunkNumS ++ "&t=" ++ ChunkNumTotalS,
	case http_req(Url, ChunkData) of
		{ok, <<"error">>} -> throw(http_returned_error);
                {ok, <<"invalid", _/binary>>} -> mydlp_api:delete_endpoint_key(), throw(http_returned_invalid);
		{ok, <<"ok">>} -> ok;
		Else -> throw(Else) end.

new_item_id() ->
	Url = "https://" ++ ?CFG(management_server_address) ++ "/receive?o=begin",
	case http_req(Url) of
		{ok, <<"error">>} -> throw(http_returned_error);
                {ok, <<"invalid", _/binary>>} -> mydlp_api:delete_endpoint_key(), throw(http_returned_invalid);
		{ok, <<"null">>} -> throw(http_returned_null);
		{ok, Ret} -> mydlp_api:binary_to_integer(Ret);
		Else -> throw(Else) end.

http_req(Url) -> http_req(Url, <<>>).

http_req(Url, Data) when is_binary(Data) ->
	Payload = case mydlp_api:encrypt_payload(Data) of
		retry -> throw({error, encrypt_payload_retry});
		P when is_binary(P) -> P end,
	ReqRet = (catch httpc:request(post, {Url, [], "application/octet-stream", Payload}, [], [])),
	http_req1(ReqRet).

http_req1(ReqRet) -> 
        case ReqRet of
                {ok, {{_HttpVer, Code, _Msg}, _Headers, Body0}} -> 
			Body = case Body0 of
				L when is_list(L) -> list_to_binary(L);
				B when is_binary(B) -> B end,
                        case {Code, Body} of
				{200, <<"error">>} -> throw(http_returned_error);
				{200, <<"invalid">>} -> mydlp_api:delete_endpoint_key(), throw(http_returned_invalid);
				{200, <<"null">>} -> throw(http_returned_null);
                                {200, RetBody} -> case mydlp_api:decrypt_payload(RetBody) of
					retry -> throw({error, decrypt_payload_retry});
					P when is_binary(P) -> {ok, P} end;
                                {Else1, _Data} -> ?ERROR_LOG("ITEMPUSH: An error occured during HTTP req: Code="?S"~n", [Else1]),
						{error, {http_code, Else1}} end;
                Else -> ?ERROR_LOG("ITEMPUSH: An error occured during HTTP req: Obj="?S"~n", [Else]),
				{error, {http_req_not_ok, Else}} end.

predict_serialized_size({endpoint_log, #log{file= Files}}) ->
	300 + lists:sum([predict_file_size(F)||F <- Files]);
predict_serialized_size({endpoint_log, _LogTerm}) -> 300;
predict_serialized_size(Else) -> 
	?ERROR_LOG("PREDICTSIZE: Unknown item. Cannot predict. Return maximum_push_size+1 as size: Item="?S"~n", [Else]),
	?CFG(maximum_push_size) + 1.

predict_file_size(#file{data=undefined}) -> 0;
predict_file_size(#file{data=Data}) -> size(Data).


strip_item({endpoint_log, #log{file=Files}=LogTerm}) ->
	StrippedFiles = mydlp_api:remove_all_data(Files),
	{endpoint_log, LogTerm#log{file=StrippedFiles}}.


-endif.

