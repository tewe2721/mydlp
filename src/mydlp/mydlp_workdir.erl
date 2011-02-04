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
%%% @doc Workdir Manager for MyDLP.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_workdir).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	tempfile/0,
	raw_to_obj/1,
	read_obj/1,
	delete_obj/1,
	get_obj_fp/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {workdir}).

%%%%%%%%%%%%% API

tempfile() -> gen_server:call(?MODULE, tempfile).

% removes unix file
raw_to_obj({unixfile, FilePath}) -> 
	case filelib:file_size(FilePath) > ?MAX_MEM_OBJ of
		true -> case gen_server:call(?MODULE, {cache_unixfile, FilePath}) of
			{error, Err} -> throw({error, Err});
			CacheRef -> CacheRef end;
		false -> case file:read_file(FilePath) of
			{ok, Bin} -> file:delete(FilePath), 
				{memory, Bin};
			Err -> throw(Err) end end;
raw_to_obj({memory, Bin}) -> raw_to_obj(Bin);
raw_to_obj({cacheref, Ref}) -> {cacheref, Ref};
raw_to_obj(RawData) -> 
	case mydlp_api:binary_size(RawData) > ?MAX_MEM_OBJ of
		true -> case gen_server:call(?MODULE, {cache, RawData}) of
			{error, Err} -> throw({error, Err});
			CacheRef -> CacheRef end;
		false -> {memory, list_to_binary([RawData])} end.

read_obj({memory, Bin}) -> Bin;
read_obj({cacheref, Ref}) -> 
	FN = ref_to_fn("obj", Ref),
	case file:read_file(FN) of
		{ok, Bin} -> Bin;
		Err -> throw(Err) end.

get_obj_fp({unixfile, FilePath}) -> FilePath;
get_obj_fp({cacheref, Ref}) -> ref_to_fn("obj", Ref);
get_obj_fp(_Else) -> throw({error, obj_type_no_fn}).

delete_obj({cacheref, Ref}) -> gen_server:cast(?MODULE, {delete_obj, Ref});
delete_obj(_Else) -> ok.

%%%%%%%%%%%%%% gen_server handles

ref_to_fn(Prefix, Ref) ->
	{A,B,C} = Ref,
	RN = lists:flatten(io_lib:format("~s-~p.~p.~p",[Prefix,A,B,C])),
	?WORK_DIR ++ "/" ++ RN.

handle_call(tempfile, _From, State) ->
	Ref = now(),
	FN = ref_to_fn("tmp", Ref),
	Reply = {ok, FN},
	{reply, Reply, State};

handle_call({cache_unixfile, FilePath} , _From, State) ->
	Ref = now(),
	FN = ref_to_fn("obj", Ref),
	Reply = case file:rename(FilePath, FN) of
		ok -> {cacheref, Ref};
		{error, Err} -> {error, Err} end,
	{reply, Reply, State};

handle_call({cache, RawData} , _From, State) ->
	Ref = now(),
	FN = ref_to_fn("obj", Ref),
	Reply = case file:write_file(FN, RawData, [raw]) of
		ok -> {cacheref, Ref};
		Err -> Err end,
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({delete_obj, Ref} , State) ->
	FN = ref_to_fn("obj", Ref),
	file:delete(FN),
	{noreply, State};

handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->

	Workdir = case application:get_env(mydlp, work_dir) of
		{ok, WD} -> WD;
		_Else -> ?WORK_DIR end,

	case gen_server:start_link({local, ?MODULE}, ?MODULE, [Workdir], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid} end.

stop() ->
	gen_server:call(?MODULE, stop).

init([Workdir]) ->
	{ok, #state{workdir=Workdir}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%% implicit

