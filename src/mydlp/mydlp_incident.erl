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

-ifdef(__MYDLP_NETWORK).

-module(mydlp_incident).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	l/1,
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
	logger_queue,
	logger_inprog = false
}).

%%%%%%%%%%%%%  API

l(LogTuple) -> gen_server:cast(?MODULE, {l, LogTuple}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({l, Item}, #state{logger_queue=Q, logger_inprog=false} = State) ->
	Q1 = queue:in(Item, Q),
	consume(),
	{noreply, State#state{logger_queue=Q1, logger_inprog=true}};

handle_cast({l, Item}, #state{logger_queue=Q, logger_inprog=true} = State) ->
	Q1 = queue:in(Item, Q),
	{noreply,State#state{logger_queue=Q1}};

handle_cast(consume, #state{logger_queue=Q} = State) ->
	case queue:out(Q) of
		{{value, Item}, Q1} ->
			try	process_log_tuple(Item),
				consume(),
				{noreply, State#state{logger_queue=Q1}}
			catch Class:Error ->
				?ERROR_LOG("Logger Queue Consume: Error occured: "
						"Class: ["?S"]. Error: ["?S"].~n"
						"Stack trace: "?S"~n.Item: "?S"~nState: "?S"~n ",	
						[Class, Error, erlang:get_stacktrace(), Item, State]),
					{noreply, State#state{logger_queue=Q1}} end;
		{empty, _} ->
			{noreply, State#state{logger_inprog=false}}
	end;

handle_cast(Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

consume() -> gen_server:cast(?MODULE, consume).

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	{ok, #state{logger_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

process_log_tuple({Channel, RuleId, Action, Ip, User, To, ITypeId, Files, Misc}) ->
	IsLogData = case Action of
		quarantine -> true;
		archive -> true;
		_Else -> false end,
	LogId = mydlp_mysql:push_log(Channel, RuleId, Action, Ip, User, To, ITypeId, Misc),
	process_log_files(LogId, IsLogData, Files),
	ok.

process_log_files(LogId, false = IsLogData, [File|Files]) ->
	Filename = mydlp_api:file_to_str(File),
	mydlp_api:clean_files(File),
	mydlp_mysql:insert_log_file(LogId, Filename),
	process_log_files(LogId, IsLogData, Files);
process_log_files(LogId, true = IsLogData, [File|Files]) ->
	File1 = mydlp_api:load_files(File),

	Size = mydlp_api:binary_size(File1#file.data),
	MimeType = case File1#file.mime_type of
		undefined -> mydlp_tc:get_mime(File1#file.data);
		Else -> Else end,
	Filename = mydlp_api:file_to_str(File1),
	{ok, Path} = mydlp_api:quarantine(File1),

	mydlp_api:clean_files(File1),

	mydlp_mysql:insert_log_file(LogId, Filename, MimeType, Size, Path),
	process_log_files(LogId, IsLogData, Files);
process_log_files(_LogId, _IsLogData, []) -> ok.

-endif.

