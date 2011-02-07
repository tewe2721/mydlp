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
%%% @copyright 2011, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_archiver).
-author("kerem@mydlp.org").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	a/1,
	a/2,
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
	file_queue,
	file_inprog = false
}).

%%%%%%%%%%%%%  API

a(Term) ->
	File = term2file(Term),
	gen_server:cast(?MODULE, {a, File}).

a(Term, FileName) ->
	File = term2file(Term),
	gen_server:cast(?MODULE, {a, File#file{filename=FileName} }).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({a, File}, #state{file_queue=Q, file_inprog=false} = State) ->
	Q1 = queue:in(File, Q),
	consume_file(),
	{noreply, State#state{file_queue=Q1, file_inprog=true}};

handle_cast({a, File}, #state{file_queue=Q, file_inprog=true} = State) ->
	Q1 = queue:in(File, Q),
	{noreply,State#state{file_queue=Q1}};

handle_cast(consume_file, #state{file_queue=Q} = State) ->
	case queue:out(Q) of
		{{value, Item}, Q1} ->
			archive_file(Item),
			consume_file(),
			{noreply, State#state{file_queue=Q1}};
		{empty, _} ->
			{noreply, State#state{file_inprog=false}}
	end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

consume_file() -> gen_server:cast(?MODULE, consume_file).

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	{ok, #state{file_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

term2file(Term) when is_binary(Term) -> #file{dataref=?BB_C(Term)};
term2file(#file{} = Term) -> Term;
term2file(Term) when is_list(Term) ->
        {ok, Bin} = file:read_file(Term),
        #file{dataref=?BB_C(Bin)}.

archive_file(#file{} = File) ->
	% add to file hash 
	File1 = mydlp_api:load_files(File),
	% get text
	FileText = mydlp_api:concat_texts(File1),

	ok.

