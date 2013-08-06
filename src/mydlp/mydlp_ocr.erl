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


%% API
ocr(FileRef) -> 
	try
		gen_server:call(?MODULE, {ocr, FileRef}, ?TIMEOUT+10)
	catch Class:Error ->
		?ERROR_LOG("Internal OCR error. Class: ["?S"], Error: ["?S"].~nFile: ["?S"]~nStacktrace:["?S"]", 
				[Class, Error, FileRef, erlang:get_stacktrace()]),
		"error"
	end.

%This function is used for test. Not included in product
test_file() ->
	File = #file{filename="Untitled.png"},
	File1 = ?BF_C(File, {regularfile, "/home/ozgen/Untitled.png"}),
	ocr(File1).

%% Gen_server callbacks

handle_call({ocr, FileRef}, From, #state{waiting_queue=Q, number_of_active_job=N}=State) ->
	Worker = self(),
	SpawnOpts = get_spawn_opts(),
	case N >= ?CFG(ocr_number_of_threads) of
		true -> Q1 = case queue:len(Q) >= ?CFG(ocr_waiting_queue_size) of 
				false -> queue:in({FileRef, From}, Q);
				true -> Q end,
			{noreply, State#state{waiting_queue=Q1}};
		false ->
	mydlp_api:mspawn(fun() ->
				{regularfile, FilePath} = FileRef#file.dataref,
				Percentage = calculate_resizing(FilePath),
				Port = mydlp_api:cmd_get_port(?CONVERT_COMMAND, lists:append(?CONVERT_ARGS, [Percentage, FilePath, "/home/ozgen/Untitled2.png"])),
				{ok, TRef} = timer:send_after(?TIMEOUT, {port_timeout, Port, From}),
				%image enhancement
				Resp = case mydlp_api:get_port_resp(Port, []) of
					{ok, _} -> timer:cancel(TRef), ok;
					Error -> ?ERROR_LOG("Error calling image enhancement. Error: ["?S"]", [Error]), 
								port_close(Port), none end,
				Resp1 = case Resp of
					ok -> 
						Port1 = mydlp_api:cmd_get_port(?TESSERACT_COMMAND, lists:append(["/home/ozgen/Untitled2.png", "/home/ozgen/testtest"], ?TESSERACT_ARGS)),
						{ok, TRef1} = timer:send_after(?TIMEOUT, {port_timeout, Port, From}),
						case mydlp_api:get_port_resp(Port1, []) of
							{ok, _} -> timer:cancel(TRef1), ok;
							Error1 -> ?ERROR_LOG("Error in ocr operation. Error: ["?S"]", [Error1]), 
								port_close(Port), none end;	
					_ -> none end, 
				Worker ! {async_convert_reply, Resp1, From}
	end, ?TIMEOUT, SpawnOpts),
	{noreply, State#state{number_of_active_job=N+1}} end;

handle_call(_Msg, _From, State) ->
	{noreply, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({port_timeout, Port, _From}, #state{number_of_active_job=N, waiting_queue=Q}=State) ->
	port_close(Port),
	%send_new_request
	Q2 = case queue:out(Q) of
		{{value, Item}, Q1} -> ocr(Item), Q1;
		_ -> Q end,
	{noreply, State#state{number_of_active_job=N-1, waiting_queue=Q2}};

handle_info({async_convert_reply, Resp, From}, State) ->
	?SAFEREPLY(From, Resp),
	test_file(),
	erlang:display(geldi),
	{noreply, State};

handle_info(test, State) ->
	erlang:display(test),
	test_file(),
	timer:send_after(1000, test),
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
%	timer:send_after(1000, test),
	{ok, #state{number_of_active_job=0, waiting_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% initernal

calculate_resizing(FilePath) ->
	Port = mydlp_api:cmd_get_port(?IDENTIFY_COMMAND, lists:append(?IDENTIFY_ARGS, [FilePath])),
	Resize = case mydlp_api:get_port_resp(Port, []) of
		{ok, Data} -> DataS = binary_to_list(Data),
				Splitted = string:tokens(DataS, " \t\n"),
				CDPI = list_to_float(lists:nth(1, Splitted)),
				round((320/CDPI)*100);
		Error -> ?ERROR_LOG("Error occured when gettin image information. Error: ["?S"]", [Error]), 100 end,
	lists:flatten(integer_to_list(Resize)++"%").

get_spawn_opts() -> [{priority, low}].
