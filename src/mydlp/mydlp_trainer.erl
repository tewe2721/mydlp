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
	public/1,
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
	gen_server:cast(?MODULE, {confidential, File}).

public(Term) ->
	File = term2file(Term),
	gen_server:cast(?MODULE, {public, File}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({confidential, File}, #state{cfile_queue=CQ, cfile_inprog=false} = State) ->
	CQ1 = queue:in(File, CQ),
	consume_cfile(),
	{noreply, State#state{cfile_queue=CQ1, cfile_inprog=true}};

handle_cast({confidential, File}, #state{cfile_queue=CQ, cfile_inprog=true} = State) ->
	CQ1 = queue:in(File, CQ),
	{noreply,State#state{cfile_queue=CQ1}};

handle_cast(consume_cfile, #state{cfile_queue=CQ} = State) ->
	case queue:out(CQ) of
		{{value, File}, CQ1} ->
			train_cfile(File),
			consume_cfile(),
			{noreply, State#state{cfile_queue=CQ1}};
		{empty, _} ->
			{noreply, State#state{cfile_inprog=false}}
	end;

handle_cast({public, File}, #state{pfile_queue=PQ, pfile_inprog=false} = State) ->
	PQ1 = queue:in(File, PQ),
	consume_pfile(),
	{noreply, State#state{pfile_queue=PQ1, pfile_inprog=true}};

handle_cast({public, File}, #state{pfile_queue=PQ, pfile_inprog=true} = State) ->
	PQ1 = queue:in(File, PQ),
	{noreply,State#state{pfile_queue=PQ1}};

handle_cast(consume_pfile, #state{pfile_queue=PQ} = State) ->
	case queue:out(PQ) of
		{{value, File}, PQ1} ->
			train_pfile(File),
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

term2file(Term) when is_binary(Term) -> #file{data=Term};
term2file(Term) when is_record(Term, file) -> Term;
term2file(Term) when is_list(Term) ->
        {ok, Bin} = file:read_file(Term),
        #file{data=Bin}.

train_cfile(#file{mime_type=undefined} = File) when is_record(File, file) ->
	MT = mydlp_tc:get_mime(File#file.data),
	train_cfile(File#file{mime_type = MT});
train_cfile(File) when is_record(File, file) ->
	% add to file hash 
        MD5Hash = erlang:md5(File#file.data),
        CGID = mydlp_mnesia:get_cgid(),
        ok = mydlp_mnesia:add_fhash_with_gid(MD5Hash, CGID),
	% add to sentence hash
	TR = case mydlp_api:has_text(File) of
		true -> {ok, File#file.text};
		false -> mydlp_api:get_text(File)
	end,

	case TR of
		{ok, Text} -> 
			SList = mydlp_api:get_nsh(Text),
			ok = mydlp_mnesia:add_shash_with_gid(SList, CGID);
		_Else -> err
	end, 

	% train bayes
	Txt = concat_texts(File),
	mydlp_tc:bayes_train_confidential(Txt),
	ok.

train_pfile(File) -> 
	% train bayes
	Txt = concat_texts(File),
	mydlp_tc:bayes_train_public(Txt),
	ok.

concat_texts(File) when is_record(File, file) -> concat_texts([File]);
concat_texts(Files) when is_list(Files) -> 
	Files1 = mydlp_api:df_to_files(Files),
	concat_texts(Files1, []).

concat_texts([File|Files], Returns) ->
	case mydlp_api:get_text(File) of
		{ok, Txt} -> concat_texts(Files, [<<"\n">>, Txt| Returns]);
		_Else -> concat_texts(Files, Returns)
	end;
concat_texts([], Returns) -> list_to_binary(lists:reverse(Returns)).



