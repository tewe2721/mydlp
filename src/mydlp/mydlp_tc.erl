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

-include_lib("stdlib/include/zip.hrl").

%% API
-export([start_link/0,
	pre_init/1,
	get_mime/1,
	get_text/3,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {backend_java}).

%%%%%%%%%%%%% MyDLP Thrift RPC API

-define(MMLEN, 4096).

get_mime(Data) when is_list(Data) ->
	L = length(Data),
	Data1 = case L > ?MMLEN of
		true -> lists:sublist(Data, ?MMLEN);
		false -> Data
	end,
	get_mime(list_to_binary(Data1));

get_mime(<<>>) -> <<"application/x-empty">>;
get_mime(<<"Rar!", _Rest/binary>>) -> <<"application/x-rar">>; % WinRAR
%get_mime(<<202,254,186,190, _Rest/binary>>) -> <<"application/java-vm">>; % CAFE BABE -> java class
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
		call_pool({thrift, java, getMime, [Data1]})
	catch _:_Exception ->
		unknown_type end,

	A = case TRet of
		?MIME_TIKA_OOXML -> get_mime_zip(Data, ?MIME_TIKA_OOXML);
		Else -> Else end,
	erlang:display(binary_to_list(A)),
	A.

get_mime_zip(Data, Default) ->
	{ok, FL} = zip:list_dir(Data),
	get_mime_zip1(FL, Default).

get_mime_zip1([], Default) -> Default;
get_mime_zip1([#zip_file{name="word/document.xml"}|_Rest], ?MIME_TIKA_OOXML) -> ?MIME_OOXML_WORD;
get_mime_zip1([#zip_file{name="xl/workbook.xml"}|_Rest], ?MIME_TIKA_OOXML) -> ?MIME_OOXML_EXCEL;
get_mime_zip1([#zip_file{name="ppt/presentation.xml"}|_Rest], ?MIME_TIKA_OOXML) -> ?MIME_OOXML_POWERPOINT;
get_mime_zip1([_Else|Rest], Default) -> get_mime_zip1(Rest, Default).

get_text(undefined, MT, Data) -> get_text(<<>>, MT, Data);
get_text(Filename, MT, Data) when is_list(Filename) ->
	FilenameB = unicode:characters_to_binary(Filename),
	get_text(FilenameB, MT, Data);
get_text(Filename0, MT0, Data) ->
	{MT, Filename} = pre_call(Filename0, MT0),
	Text = case MT of
		?MIME_XPS ->
			F = #file{filename=Filename, mime_type=?MIME_ZIP, dataref=?BB_C(Data)},
			T = mydlp_api:concat_texts(F),
			mydlp_api:clean_files(F), T;
		_Else -> call_pool({thrift, java, getText, [Filename, MT, Data]}) end,
	erlang:display(binary_to_list(Text)),
	Text.

pre_call(Filename, MT) ->
	case binary:part(Filename,{byte_size(Filename), -4}) of
		<<".xps">> -> {?MIME_XPS, binary:part(Filename,{0, byte_size(Filename) - 4})};
		_Else -> {MT, Filename} end.

%%%%%%%%%%%%%% gen_server handles

handle_call({thrift, java, Func, Params}, _From, #state{backend_java=TS} = State) ->
	{TS1, Reply} = try
		thrift_client:call(TS, Func, Params)
	catch _:{TSE, _Exception} ->
			?ERROR_LOG("Error in thrift backend. \n", []),
			{TSE, {error, exception_at_backend}} end,
		
	{reply, Reply, State#state{backend_java=TS1}};

handle_call(stop, _From, #state{backend_java=Java} = State) ->
	thrift_client:close(Java),
	{stop, normalStop, State#state{backend_java=undefined}};

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
	{ok, Java} = thrift_client_util:new("localhost",9090, mydlp_thrift, []),
	{ok, #state{backend_java=Java}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

call_pool(Req) ->
	Pid = pg2:get_closest_pid(?MODULE),
	case gen_server:call(Pid, Req, 60000) of
		{ok, <<"mydlp-internal/error">>} -> throw({error, exception_at_backend});
		{ok, Ret} -> Ret;
		{error, Reason} -> throw({error, Reason});
		{ierror, Class, Error} -> mydlp_api:exception(Class, Error) end.

