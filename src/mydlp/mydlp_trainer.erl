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
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-ifdef(__MYDLP_NETWORK).

-module(mydlp_trainer).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	confidential/1,
	confidential/2,
	confidential/3,
	public/1,
	public/2,
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
	cfile_queue,
	cfile_inprog = false,
	pfile_queue,
	pfile_inprog = false
}).

%%%%%%%%%%%%%  API

confidential(Term) ->
	File = mydlp_api:term2file(Term),
	gen_server:cast(?MODULE, {confidential, {File}}).

confidential(Term, FileId) ->
	File = mydlp_api:term2file(Term),
	gen_server:cast(?MODULE, {confidential, {File, FileId}}).

confidential(Term, FileId, GroupId) ->
	File = mydlp_api:term2file(Term),
	gen_server:cast(?MODULE, {confidential, {File, FileId, GroupId}}).

public(Term) ->
	File = mydlp_api:term2file(Term),
	gen_server:cast(?MODULE, {public, {File}}).

public(Term, FileId) ->
	File = mydlp_api:term2file(Term),
	gen_server:cast(?MODULE, {public, {File, FileId}}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({confidential, Item}, #state{cfile_queue=CQ, cfile_inprog=false} = State) ->
	CQ1 = queue:in(Item, CQ),
	consume_cfile(),
	{noreply, State#state{cfile_queue=CQ1, cfile_inprog=true}};

handle_cast({confidential, Item}, #state{cfile_queue=CQ, cfile_inprog=true} = State) ->
	CQ1 = queue:in(Item, CQ),
	{noreply,State#state{cfile_queue=CQ1}};

handle_cast(consume_cfile, #state{cfile_queue=CQ} = State) ->
	case queue:out(CQ) of
		{{value, Item}, CQ1} ->
			train_cfile(Item),
			consume_cfile(),
			{noreply, State#state{cfile_queue=CQ1}};
		{empty, _} ->
			{noreply, State#state{cfile_inprog=false}}
	end;

handle_cast({public, Item}, #state{pfile_queue=PQ, pfile_inprog=false} = State) ->
	PQ1 = queue:in(Item, PQ),
	consume_pfile(),
	{noreply, State#state{pfile_queue=PQ1, pfile_inprog=true}};

handle_cast({public, Item}, #state{pfile_queue=PQ, pfile_inprog=true} = State) ->
	PQ1 = queue:in(Item, PQ),
	{noreply,State#state{pfile_queue=PQ1}};

handle_cast(consume_pfile, #state{pfile_queue=PQ} = State) ->
	case queue:out(PQ) of
		{{value, Item}, PQ1} ->
			train_pfile(Item),
			consume_pfile(),
			{noreply, State#state{pfile_queue=PQ1}};
		{empty, _} ->
			{noreply, State#state{pfile_inprog=false}}
	end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

consume_cfile() -> gen_server:cast(?MODULE, consume_cfile).

consume_pfile() -> gen_server:cast(?MODULE, consume_pfile).

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	{ok, #state{cfile_queue=queue:new(), pfile_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

train_cfile({File}) -> train_cfile({File, undefined});

train_cfile({File, FileId}) ->
	CGID = mydlp_mnesia:get_cgid(),
	train_cfile({File, FileId, CGID});

train_cfile({#file{mime_type=undefined} = File, FileId, GroupId}) ->
	File1 = mydlp_api:load_files(File), %% Refine these
	MT = mydlp_tc:get_mime(File1#file.data),
	train_cfile({File1#file{mime_type = MT}, FileId, GroupId});

train_cfile({#file{} = File, FileId, GroupId}) ->
	% add to file hash 
	File1 = mydlp_api:load_files(File),
        MD5Hash = erlang:md5(File1#file.data),
        ok = mydlp_mnesia:add_fhash(MD5Hash, FileId, GroupId),

	% get text
	case mydlp_api:concat_texts(File1) of
		<<>> -> ok;
		Txt ->	% add to sentence hash
			SList = mydlp_api:get_nsh(Txt),
			ok = mydlp_mnesia:add_shash(SList, FileId, GroupId),
			% train bayes
			bayeserl:train_positive(Txt) end, ok.

train_pfile({File}) -> train_pfile({File, undefined});

train_pfile({File, FileId}) -> 
	% add to whitefiles
	PGID = mydlp_mnesia:get_pgid(),
        MD5Hash = erlang:md5(File#file.data),
        ok = mydlp_mnesia:add_fhash(MD5Hash, FileId, PGID),
	% train bayes
	Txt = mydlp_api:concat_texts(File),
	bayeserl:train_negative(Txt), ok.

-endif.

