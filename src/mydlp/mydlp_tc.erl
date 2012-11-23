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
-export([	start_link/0,
		get_mime/2,
		get_text/3,
		seclore_initialize/7,
		seclore_protect/3,
		seclore_terminate/0,
		load/0
	]).

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

start_link() -> mydlp_pool:start_link(?MODULE, get_thrift_pool_size()).

-define(MMLEN, 4096).

get_mime(undefined, Data) -> get_mime("noname", Data);
get_mime("", Data) -> get_mime("noname", Data);
get_mime(<<>>, Data) -> get_mime("noname", Data);

get_mime(Filename, Data) when is_list(Data) ->
	L = length(Data),
	Data1 = case L > ?MMLEN of
		true -> lists:sublist(Data, ?MMLEN);
		false -> Data
	end,
	get_mime(Filename,list_to_binary(Data1));

get_mime(_Filename, <<>>) -> <<"application/x-empty">>;
get_mime(_Filename, <<"Rar!", _Rest/binary>>) -> <<"application/x-rar">>; % WinRAR
%get_mime(<<202,254,186,190, _Rest/binary>>) -> <<"application/java-vm">>; % CAFE BABE -> java class
get_mime(_Filename,
	<<	16#00,16#01,16#00,16#00,
		16#53,16#74,15#61,15#6E,
		16#64,16#61,16#72,16#64,
		16#20,16#4A,16#65,16#74,
		16#20,16#44,16#42, _Rest/binary>>) -> <<"application/msaccess">>;
get_mime(_Filename,
	<<	16#00,16#01,16#00,16#00,
		16#53,16#74,16#61,16#6E,
		16#64,16#61,16#72,16#64,
		16#20,16#41,16#43,16#45,
		16#20,16#44,16#42, _Rest/binary>>) -> <<"application/msaccess">>;
get_mime(Filename, Data) when is_binary(Data) ->
	S = size(Data),
	Data1 = case S > ?MMLEN of
		true -> <<D:?MMLEN/binary, _/binary>> = Data, D;
		false -> Data
	end,

	Filename1 = prettify_filename(Filename),

	try 	call_pool({thrift, java, getMime, [Filename1, Data1]})
	catch Class:Error ->
		?ERROR_LOG("Error occured when extractiong text. Filename: "?S".~nData: ["?S"]~nClass: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Filename1, Data, Class, Error, erlang:get_stacktrace()]),
		?MIME_OCTET_STREAM end.

prettify_filename(Filename) -> mydlp_api:filename_to_bin(Filename).

get_text(undefined, MT, Data) -> get_text(<<>>, MT, Data);
get_text(Filename0, MT, Data) ->
	Filename = prettify_filename(Filename0),
	try	RawText = call_pool({thrift, java, getText, [Filename, MT, Data]}),
		Text = case MT of
			?MIME_TEXT -> try mydlp_api:remove_html_tags(RawText) catch _:_ -> RawText end;
			_Else -> RawText end,
		<<" ", Text/binary, " ">>
	catch Class:Error ->
		?ERROR_LOG("Error occured when extractiong text. Filename: "?S", Mimetype: "?S".~nData: ["?S"]~nClass: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Filename, MT, Data, Class, Error, erlang:get_stacktrace()]),
		{error, {Class, Error}} end.

seclore_initialize(SecloreAppPath, SecloreAddress, SeclorePort, SecloreAppName, SecloreHotFolderCabinetId, SecloreHotFolderCabinetPassphrase, SeclorePoolSize) ->
	try	call_pool({thrift, java, secloreInitialize, [SecloreAppPath, SecloreAddress, SeclorePort, SecloreAppName, SecloreHotFolderCabinetId, SecloreHotFolderCabinetPassphrase, SeclorePoolSize]})
	catch Class:Error ->
		?ERROR_LOG("Error occured when initializing seclore module. "
			"SecloreAppPath: "?S", SecloreAddress: "?S" SeclorePort: "?S", SecloreAppName: "?S", SecloreHotFolderCabinetId: "?S" SeclorePoolSize: "?S".~nClass: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[SecloreAppPath, SecloreAddress, SeclorePort, SecloreAppName, SecloreHotFolderCabinetId, SeclorePoolSize, Class, Error, erlang:get_stacktrace()]),
		"mydlp.backend.secloreInitialize.unexpectedException" end.
	
seclore_protect(FilePath, HotFolderId, ActivityComments) ->
	try	call_pool({thrift, java, secloreProtect, [FilePath, HotFolderId, ActivityComments]})
	catch Class:Error ->
		?ERROR_LOG("Error occured when protecting file with seclore. "
			"FilePath: "?S", HotFolderId: "?S", ActivityComments: "?S".~nClass: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[FilePath, HotFolderId, ActivityComments, Class, Error, erlang:get_stacktrace()]),
		"mydlp.backend.secloreProtect.unexpectedException" end.

seclore_terminate() ->
	try	call_pool({thrift, java, secloreTerminate, []})
	catch Class:Error ->
		?ERROR_LOG("Error occured when terminating seclore module.~nClass: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Class, Error, erlang:get_stacktrace()]),
		"mydlp.backend.secloreTerminate.unexpectedException" end.

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
	case mydlp_pool:call(?MODULE, Req, 180000) of
		{ok, <<"mydlp-internal/error">>} -> throw({error, exception_at_backend});
		{ok, Ret} -> Ret;
		{error, Reason} -> throw({error, Reason});
		{ierror, Class, Error} -> mydlp_api:exception(Class, Error) end.

-ifdef(__MYDLP_NETWORK).

load_seclore() -> 
	case ?CFG(seclore_fs_enable) of
		true ->	seclore_initialize(	?CFG(seclore_dir), 
						?CFG(seclore_fs_address),
						?CFG(seclore_fs_port),
						?CFG(seclore_fs_app_name),
						?CFG(seclore_fs_hot_folder_cabinet_id),
						?CFG(seclore_fs_hot_folder_cabinet_passphrase),
						?CFG(seclore_fs_server_pool_size)
					);
		false -> <<"ok">> end.

-endif.

-ifdef(__MYDLP_ENDPOINT).

load_seclore() -> case ?CFG(seclore_fs_enable) of
		true ->	seclore_initialize(	?CFG(seclore_dir), 
						?CFG(seclore_fs_address),
						?CFG(seclore_fs_port),
						?CFG(seclore_fs_app_name),
						?CFG(seclore_fs_hot_folder_cabinet_id),
						?CFG(seclore_fs_hot_folder_cabinet_passphrase),
						?CFG(seclore_fs_endpoint_pool_size)
					);
		false -> <<"ok">> end.

-endif.


-ifdef(__MYDLP_NETWORK).

get_thrift_pool_size() -> try ?CFG(thrift_server_pool_size) catch _:_ -> 4 end.

-endif.

-ifdef(__MYDLP_ENDPOINT).

get_thrift_pool_size() -> try ?CFG(thrift_endpoint_pool_size) catch _:_ -> 2 end.

-endif.

load() ->
	mydlp_pool:set_pool_size(?MODULE, get_thrift_pool_size()),
	load_seclore(),
	ok.

