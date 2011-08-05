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

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0,
	tempfile/0,
	raw_to_obj/1,
	read_obj/1,
	delete_obj/1,
	get_obj_fp/1,
	get_obj_size/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {}).

%%%%%%%%%%%%% API

tempfile() ->
	Ref = now(),
	FN = ref_to_fn("tmp", Ref),
	{ok, FN}.

raw_to_obj({unixfile, FilePath}) -> 
	case filelib:file_size(FilePath) > ?MAX_MEM_OBJ of
		true -> cache_unixfile(FilePath);
		false -> case file:read_file(FilePath) of
			{ok, Bin} -> file:delete(FilePath), 
				{memory, Bin};
			Err -> throw(Err) end end;
raw_to_obj({memory, Bin}) -> raw_to_obj(Bin);
raw_to_obj({cacheref, Ref}) -> {cacheref, Ref};
raw_to_obj(RawData) -> 
	case mydlp_api:binary_size(RawData) > ?MAX_MEM_OBJ of
		true -> cache(RawData);
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

get_obj_size({memory, Bin}) -> size(Bin);
get_obj_size(Ref) ->
	FP = get_obj_fp(Ref),
	filelib:file_size(FP).

delete_obj({cacheref, Ref}) -> delete_cacheref(Ref);
delete_obj(_Else) -> ok.

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(cleanup_now, State) ->
	cleanup(),
	call_timer(),
        {noreply, State};

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
	filelib:ensure_dir(?WORK_DIR ++ "/"),
	call_timer(),
	{ok, #state{}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%% implicit

cache(RawData) ->
	Ref = now(),
	FN = ref_to_fn("obj", Ref),
	case file:write_file(FN, RawData, [raw]) of
		ok -> {cacheref, Ref};
		{error, Err} -> throw({error, Err}) end.

cache_unixfile(FilePath) ->
	Ref = now(),
	FN = ref_to_fn("obj", Ref),
	case file:rename(FilePath, FN) of
		ok -> {cacheref, Ref};
		{error, Err} -> throw({error, Err}) end.

ref_to_fn(Prefix, Ref) ->
	{A,B,C} = Ref,
	RN = lists:flatten(io_lib:format("~s-~p.~p.~p",[Prefix,A,B,C])),
	?WORK_DIR ++ "/" ++ RN.

delete_cacheref(Ref) ->
	spawn(fun() ->
		FN = ref_to_fn("obj", Ref),
		file:delete(FN) 
	end), ok.

call_timer() -> timer:send_after(900000, cleanup_now).

is_old(Filename, LocalSeconds) ->
	{ok, FileInfo} = file:read_file_info(Filename),
	ATime = calendar:datetime_to_gregorian_seconds(FileInfo#file_info.atime),
	MTime = calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime),

	BTime = if 	ATime > MTime -> ATime;
			true -> MTime end,

	case (LocalSeconds - BTime) of
		Age when Age > 1800 -> true;
		_Else -> false end.

cleanup() ->
	LocalSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	{ok, FileList} = file:list_dir(?WORK_DIR),

	lists:foreach(fun(FN) -> 
		NFN = ?WORK_DIR ++ "/" ++ FN,
		case is_old(NFN, LocalSeconds) of
			true -> mydlp_api:rmrf(NFN);
			false -> ok end
		end, FileList), 
	ok.



