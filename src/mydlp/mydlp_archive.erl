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

-module(mydlp_archive).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	a/2,
	a/3,
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

a(FileId, Term) ->
	File = mydlp_api:term2file(Term),
	gen_server:cast(?MODULE, {a, {FileId, File} }).

a(FileId, Term, <<FileName/binary>>) -> a(FileId, Term, binary_to_list(FileName));
a(FileId, Term, [] ) -> a(FileId, Term);
a(FileId, Term, [_|_] = FileName) ->
	File = mydlp_api:term2file(Term),
	a(FileId, File#file{filename=FileName}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({a, Item}, #state{file_queue=Q, file_inprog=false} = State) ->
	Q1 = queue:in(Item, Q),
	consume_file(),
	{noreply, State#state{file_queue=Q1, file_inprog=true}};

handle_cast({a, Item}, #state{file_queue=Q, file_inprog=true} = State) ->
	Q1 = queue:in(Item, Q),
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

archive_file({ AFileId, #file{} = File }) ->
	% add to file hash 
	File1 = mydlp_api:load_files(File),
	% get text
	ContentText = mydlp_api:concat_texts(File1),

	% get meta
	Size = mydlp_api:binary_size(File1#file.data),
	MimeType = case File1#file.mime_type of
		undefined -> mydlp_tc:get_mime(File1#file.data);
		Else -> Else end,
	Filename = mydlp_api:file_to_str(File1),

	% persist to filesystem
	{ok, ArchivePath} = mydlp_api:quarantine(File1),

	mydlp_api:clean_files(File1),

	% update afile
	mydlp_mysql:update_afile(AFileId, Filename, MimeType, Size, ArchivePath, ContentText),

	ok.

-endif.

