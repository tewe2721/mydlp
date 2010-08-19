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
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_trainer).
-author("kerem@medra.com.tr").
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
	File = term2file(Term),
	gen_server:cast(?MODULE, {confidential, {File}}).

confidential(Term, FileId) ->
	File = term2file(Term),
	gen_server:cast(?MODULE, {confidential, {File, FileId}}).

confidential(Term, FileId, GroupId) ->
	File = term2file(Term),
	gen_server:cast(?MODULE, {confidential, {File, FileId, GroupId}}).

public(Term) ->
	File = term2file(Term),
	gen_server:cast(?MODULE, {public, {File}}).

public(Term, FileId) ->
	File = term2file(Term),
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
			mydlp_tc:bayes_persist_db(),
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
			mydlp_tc:bayes_persist_db(),
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
	%% initialize Java Bayessian Zembere Backend.
	mnesia:wait_for_tables([bayes_data], 5000),
	mydlp_tc:bayes_reset(),
	mydlp_tc:bayes_load_db(),

	{ok, #state{cfile_queue=queue:new(), pfile_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

term2file(Term) when is_binary(Term) -> #file{data=Term};
term2file(#file{} = Term) -> Term;
term2file(Term) when is_list(Term) ->
        {ok, Bin} = file:read_file(Term),
        #file{data=Bin}.

train_cfile({File}) -> train_cfile({File, undefined});

train_cfile({File, FileId}) ->
	CGID = mydlp_mnesia:get_cgid(),
	train_cfile({File, FileId, CGID});

train_cfile({#file{mime_type=undefined} = File, FileId, GroupId}) ->
	MT = mydlp_tc:get_mime(File#file.data),
	train_cfile({File#file{mime_type = MT}, FileId, GroupId});

train_cfile({#file{} = File, FileId, GroupId}) ->
	% add to file hash 
        MD5Hash = erlang:md5(File#file.data),
        ok = mydlp_mnesia:add_fhash(MD5Hash, FileId, GroupId),

	% get text
	case concat_texts(File) of
		<<>> -> ok;
		Txt ->
			% add to sentence hash
			SList = mydlp_api:get_nsh(Txt),
			ok = mydlp_mnesia:add_shash(SList, FileId, GroupId),
			% train bayes
			mydlp_tc:bayes_train_confidential(Txt)
	end,
	ok.

train_pfile({File}) -> train_pfile({File, undefined});

train_pfile({File, FileId}) -> 
	% add to whitefiles
	PGID = mydlp_mnesia:get_pgid(),
        MD5Hash = erlang:md5(File#file.data),
        ok = mydlp_mnesia:add_fhash(MD5Hash, FileId, PGID),
	% train bayes
	Txt = concat_texts(File),
	mydlp_tc:bayes_train_public(Txt),
	ok.

concat_texts(#file{} = File) -> concat_texts([File]);
concat_texts(Files) when is_list(Files) -> 
	Files1 = mydlp_api:df_to_files(Files),
	concat_texts(Files1, []).

concat_texts([File|Files], Returns) ->
	case mydlp_api:get_text(File) of
		{ok, Txt} -> concat_texts(Files, [<<"\n">>, Txt| Returns]);
		_Else -> concat_texts(Files, Returns)
	end;
concat_texts([], Returns) -> list_to_binary(lists:reverse(Returns)).

