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
	waiting_queue
}).

-define(TIMEOUT, 600000).
-define(CONVERT_COMMAND, "/usr/bin/convert").
-define(CONVERT_ARGS, ["-units", "PixelsPerInch", "-density", "320", "-quality", "100", "-resize"]).
-define(TESSERACT_ARGS, ["-lang=eng+tur+chi_sim+chi_tra"]).

%% API
ocr(FileRef) -> gen_server:call(?MODULE, {ocr, FileRef}, ?TIMEOUT).

%% Gen_server callbacks

handle_call({ocr, FileRef}, From, State) ->
	Worker = self(),
	SpawnOpts = get_spawn_opts(),
	mydlp_api:mspawn(fun() ->
				%Port = open_port({spawn_executable, ?CONVERT}, 
				%		[{args, lists:append(?CONVERT_ARGS, ["/home/ozgen/Untitled.png", "/home/ozgen/Untitled2.png"])}, use_stdio, exit_status, stderr_to_stdout]),
				
				{ok, B} = file:read_file("/home/ozgen/Untitled.png"),
				Resp = mydlp_api:cmd(?CONVERT_COMMAND, lists:append(?CONVERT_ARGS, ["400%", B, "/home/ozgen/Untitled2.png"])),
				erlang:display({resp, Resp}),
				Worker ! {async_convert_reply, Resp, From}
	end, ?TIMEOUT+10),
	{noreply, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_convert_reply, Resp, From}, State) ->
	?SAFEREPLY(From, Resp),
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
	{ok, #state{waiting_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% initernal

get_spawn_opts() -> [{priority, lowi}].
