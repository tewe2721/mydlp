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
-module(mydlp_tc).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	pre_init/1,
	get_mime/1,
	is_valid_iban/1,
	html_to_text/1,
	check_binary_integrity/1,
	check_archive_integrity/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {backend_py}).

%%%%%%%%%%%%% MyDLP Thrift RPC API

-define(MMLEN, 4096).

get_mime(Data) when is_list(Data) ->
	L = length(Data),
	Data1 = case L > ?MMLEN of
		true -> lists:sublist(Data, ?MMLEN);
		false -> Data
	end,
	get_mime(list_to_binary(Data1));

get_mime(<<"Rar!", _Rest/binary>>) -> <<"application/x-rar">>; % WinRAR
get_mime(<<202,254,186,190, _Rest/binary>>) -> <<"application/java-vm">>; % CAFE BABE -> java class
get_mime(<<	16#00,16#01,16#00,16#00,
		16#53,16#74,15#61,15#6E,
		16#64,16#61,16#72,16#64,
		16#20,16#4A,16#65,16#74,
		16#20,16#44,16#42, _Rest/binary>>) -> <<"application/msaccess">>;
get_mime(<<	16#00,16#01,16#00,16#00,
		16#53,16#74,16#61,16#6E,
		16#64,16#61,16#72,16#64,
		16#20,16#41,16#43,16#45,
		16#20,16#44,16#42, _Rest/binary>>) -> <<"application/msaccess">>;
get_mime(Data) when is_binary(Data) ->
	S = size(Data),
	Data1 = case S > ?MMLEN of
		true -> <<D:?MMLEN/binary, _/binary>> = Data, D;
		false -> Data
	end,
	TRet = try
		call_pool({thrift, py, getMagicMime, [Data1]})
	catch _:_Exception ->
		unknown_type end,

	case TRet of
		<<"application/zip">> -> get_mime_zip(Data);
		Else -> Else end.

get_mime_zip(Data) ->
	{ok, FL} = zip:list_dir(Data),
	case 		lists:keymember("word/document.xml", 2, FL) of true -> ?MIME_OOXML_WORD;
	false -> case	lists:keymember("xl/workbook.xml", 2, FL) of true -> ?MIME_OOXML_EXCEL;
	false -> case	lists:keymember("ppt/presentation.xml", 2, FL) of true -> ?MIME_OOXML_POWERPOINT;
	false -> <<"application/zip">> end end end.

is_valid_iban(IbanStr) ->
	call_pool({thrift, py, isValidIban, [IbanStr]}).

html_to_text(Html) ->
	call_pool({thrift, py, htmlToText, [Html]}).

check_binary_integrity(FileData) ->
	{ok, FilePath} = mydlp_api:mktempfile(),
	ok = file:write_file(FilePath, FileData, [raw]),
	Ret = call_pool({thrift, py, checkBinaryIntegrity, [FilePath]}),
	ok = file:delete(FilePath),
	Ret.

check_archive_integrity(FileData) ->
	{ok, FilePath} = mydlp_api:mktempfile(),
	ok = file:write_file(FilePath, FileData, [raw]),
	Ret = call_pool({thrift, py, checkArchiveIntegrity, [FilePath]}),
	ok = file:delete(FilePath),
	Ret.

%%%%%%%%%%%%%% gen_server handles

handle_call({thrift, py, Func, Params}, _From, #state{backend_py=TS} = State) ->
	{TS1, Reply} = try
		thrift_client:call(TS, Func, Params)
	catch _:{TSE, _Exception} ->
		?DEBUG("Error in thrift backend. \n", []),
		{TSE, {error, exception_at_backend}} end,
		
	{reply, Reply, State#state{backend_py=TS1}};

handle_call(stop, _From, #state{backend_py=PY} = State) ->
	thrift_client:close(PY),
	{stop, normalStop, State#state{backend_py=undefined}};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

pre_init(_Args) -> ok.

start_link() ->
	mydlp_pg_sup:start_link(?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	{ok, PY} = thrift_client_util:new("localhost",9090, mydlp_thrift, []),
	{ok, #state{backend_py=PY}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

call_pool(Req) ->
	Pid = pg2:get_closest_pid(?MODULE),
	case gen_server:call(Pid, Req, 60000) of
		{ok, Ret} -> Ret;
		{error, Reason} -> throw({error, Reason}) end.

