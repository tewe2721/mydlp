%%%
%%%    Copyright (C) 2013 Ozgen Muzac <ozgen@mydlp.com>
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
%%% @author Ozgen Muzac <ozgen@mydlp.com>
%%% @copyright 2013, Ozgen Muzac
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-module(mydlp_ocr).
-author("ozgen@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").


%% API
-export([start_link/0,
	ocr/1,
	test_file/0,
	stop/0
	]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
	number_of_active_job,
	waiting_queue
}).

-define(TIMEOUT, ?CFG(ocr_max_processing_age)).
-define(IDENTIFY_COMMAND, "/usr/bin/identify").
-define(IDENTIFY_ARGS, ["-units", "PixelsPerInch", "-format", "%x"]).
-define(CONVERT_COMMAND, "/usr/bin/convert").
-define(CONVERT_ARGS, ["-units", "PixelsPerInch", "-density", "320", "-quality", "100", "-resize"]).
-define(TESSERACT_COMMAND, "/usr/bin/tesseract").
-define(TESSERACT_ARGS, ["-lang=eng+tur+chi_sim+chi_tra"]).
-define(MAX_NUMBER_OF_THREAD, 2).
-define(MAX_WAITING_QUEUE_SIZE, 2).
-define(OCR_WORKDIR, "/var/lib/mydlp/ocr").
-define(TRY_COUNT, 5).


%% API
ocr(FileRef) -> 
	try
		gen_server:call(?MODULE, {ocr, FileRef}, ?TIMEOUT+10)
	catch Class:Error ->
		?ERROR_LOG("Internal OCR error. Class: ["?S"], Error: ["?S"].~nFile: ["?S"]~nStacktrace:["?S"]", 
				[Class, Error, FileRef, erlang:get_stacktrace()]),
		"error"
	end.

consume_waiting_process() -> gen_server:cast(?MODULE, consume_queue).

%This function is used for test. Not included in product
test_file() ->
	File = #file{filename="Untitled.png"},
	File1 = ?BF_C(File, {regularfile, "/home/ozgen/Untitled.png"}),
	ocr(File1).

%% Gen_server callbacks

handle_call({ocr, FileRef}, From, #state{waiting_queue=Q, number_of_active_job=N}=State) ->
	Worker = self(),
	SpawnOpts = get_spawn_opts(),
	FileKey = get_cache_key(FileRef),
	case cache_lookup(FileKey) of
		{hit, I} -> I, {reply, I, State};
		 _ -> 
		case N >= ?CFG(ocr_number_of_threads) of
			true -> Q1 = case queue:len(Q) >= ?CFG(ocr_waiting_queue_size) of 
					false -> queue:in({FileRef, From}, Q);
					true -> Q end,
				{noreply, State#state{waiting_queue=Q1}};
			false ->
				mydlp_api:mspawn(fun() ->
						Resp = extract_text(FileRef, From),
						Worker ! {async_convert_reply, {Resp, FileRef}, From}
						end, ?TIMEOUT, SpawnOpts),
				{noreply, State#state{number_of_active_job=N+1}} end end;

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(consume_queue, #state{waiting_queue=Q, number_of_active_job=N}=State) ->
	Worker = self(),
	SpawnOpts = get_spawn_opts(),
	case N >= ?CFG(ocr_number_of_threads) of
		true -> {noreply, State};
		false ->
		case queue:out(Q) of
			{{value, {FileRef, From}}, Q1} -> 
				mydlp_api:mspawn(fun() ->
					Resp = extract_text(FileRef, From),
					Worker ! {async_convert_reply, {Resp, FileRef}, From} end, ?TIMEOUT, SpawnOpts),
					{noreply, State#state{number_of_active_job=N+1, waiting_queue=Q1}};
			_ -> {noreply, State} end end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({port_timeout, Port, From}, #state{number_of_active_job=N}=State) ->
	port_close(Port),
	?SAFEREPLY(From, error),	
	%send_new_request
	consume_waiting_process(),
	{noreply, State#state{number_of_active_job=N-1}};

handle_info(cleanup_now, State) ->
	cache_cleanup_handle(),
	call_timer(),
	{noreply, State};

handle_info({async_convert_reply, {Resp, FileRef}, From}, #state{number_of_active_job=N}=State) ->
	FileKey = get_cache_key(FileRef),
	?SAFEREPLY(From, Resp),
	cache_insert(FileKey, Resp),
	consume_waiting_process(),
	{noreply, State#state{number_of_active_job=N-1}};

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
	cache_start(),
	filelib:ensure_dir(?OCR_WORKDIR),
	{ok, #state{number_of_active_job=0, waiting_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% initernal
get_cache_key(FileRef) ->
%	lists:flatten(FileRef#file.dataref++FileRef#file.md5_hash)
	FileRef#file.dataref.
	%Path.

cache_lookup(Query) ->
        case ets:lookup(ocr_cache, Query) of
                [] -> miss;
                [I|_] -> {hit, I} end.

cache_insert(Query, Return) ->
        ets:insert(ocr_cache, {Query, Return}),
        ok.

cache_clean() ->
        ets:delete_all_objects(ocr_cache),
	clean_workdir().

cache_cleanup_handle() ->
        MaxSize = ?CFG(ocr_cache_maximum_size),
        case ets:info(ocr_cache, memory) of
                I when I > MaxSize -> cache_clean();
                _Else -> ok end.

call_timer() -> call_timer(?CFG(ocr_cache_cleanup_interval)).
call_timer(Time) -> timer:send_after(Time, cleanup_now).

cache_start() ->
        case ets:info(ocr_cache) of
                undefined -> ets:new(ocr_cache, [
                                        public,
                                        named_table,
                                        {write_concurrency, true}
                                ]);
                _Else -> ok end.

clean_workdir() ->
        case file:list_dir(?OCR_WORKDIR) of
                {ok, FileList} -> remove_file(FileList);
                {error, E} -> ?ERROR_LOG("OCR: Error Occured listing files. Path: ["?S"]~n. Error: ["?S"]~n", [?OCR_WORKDIR, E])
        end.

remove_file([File|Rest]) ->
        FilePath = filename:join(?OCR_WORKDIR, File),
        remove(FilePath, ?TRY_COUNT),
        remove_file(Rest);
remove_file([]) -> ok.

remove(FilePath, TryCount) ->
	case file:delete(FilePath) of
		ok -> ok;
		_ -> 
		case TryCount of
                	1 -> ?ERROR_LOG("OCR: Error Occured when deleting file. FilePath: ["?S"]", [FilePath]) ;
			_ -> timer:sleep(1000),
				remove(FilePath, TryCount-1)
                end
        end.


extract_text(FileRef, From) ->
	%{regularfile, FilePath} = FileRef#file.dataref,
	Percentage = calculate_resizing(FileRef),
	EnhancedImage = get_unique_filename(),
	BinaryData = FileRef#file.data,
	ConvertingImage = <<BinaryData/binary,"\n">>,
	Port = mydlp_api:cmd_get_port(?CONVERT_COMMAND, lists:append(?CONVERT_ARGS, [Percentage, "-", EnhancedImage]), [], ConvertingImage),
	{ok, TRef} = timer:send_after(?TIMEOUT, {port_timeout, Port, From}),
	%image enhancement
	timer:sleep(5000),%work around
	port_close(Port),
	timer:sleep(5000),
	Resp = case filelib:is_regular(EnhancedImage) of
		true -> timer:cancel(TRef), ok;
		false -> error end,
	%Resp = case mydlp_api:get_port_resp(Port, []) of
%		{ok, _} -> timer:cancel(TRef), ok;
%		Error -> ?ERROR_LOG("Error calling image enhancement. Error: ["?S"]", [Error]), 
%			port_close(Port), none end,
	case Resp of
		ok ->
			TextFile = get_unique_filename(), 
			Port1 = mydlp_api:cmd_get_port(?TESSERACT_COMMAND, lists:append([EnhancedImage, TextFile], ?TESSERACT_ARGS)),
			{ok, TRef1} = timer:send_after(?TIMEOUT, {port_timeout, Port, From}),
			case mydlp_api:get_port_resp(Port1, []) of
				{ok, _} -> timer:cancel(TRef1), 
					file:delete(EnhancedImage),
					TextFile,
					F = lists:append(TextFile, ".txt"),
					{ok, Data} = file:read_file(F),
					File1 = ?BF_C(#file{}, Data),
					mydlp_api:load_file(File1);
				Error1 -> ?ERROR_LOG("Error in ocr operation. Error: ["?S"]", [Error1]), 
					port_close(Port), "error" end;	
		_ -> "error" end.

calculate_resizing(FileRef) ->
%	BinaryData = FileRef#file.data, TODO: Do This!
	Port = mydlp_api:cmd_get_port(?IDENTIFY_COMMAND, lists:append(?IDENTIFY_ARGS, ["/home/ozgen/Capture.JPG"])),
	Resize = case mydlp_api:get_port_resp(Port, []) of
		{ok, Data} -> DataS = binary_to_list(Data),
				CDPIL = lists:nth(1, string:tokens(DataS, " \t\n")),
				CDPI = case string:chr(CDPIL, $.) of
					0 -> list_to_integer(CDPIL);
					_ -> list_to_float(CDPIL) end,
				round((320/CDPI)*100);
		Error -> ?ERROR_LOG("Error occured when gettin image information. Error: ["?S"]", [Error]), 100 end,
	lists:flatten(integer_to_list(Resize)++"%").

get_spawn_opts() -> [{priority, low}].

get_unique_filename() -> 
	{A, B, C} = now(),
	Filename = lists:flatten(io_lib:format("~p.~p.~p", [A, B, C])),
	filename:join(?OCR_WORKDIR, Filename).

