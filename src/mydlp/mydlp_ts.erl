%
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
%%% @copyright 2010, H. Kerem Cevahir
%%% @doc Backend for mydlp ui functions.
%%% @end
%%%-------------------------------------------------------------------

-ifdef(__MYDLP_NETWORK).

-module(mydlp_ts).
-author("kerem@mydlp.com").

-include("mydlp.hrl").

-include("mydlp_ui_thrift.hrl").
-include("mydlp_ui_types.hrl").

-export([start_link/0,
	stop/1,
	handle_function/2
	]).

-export([trainConfidential/2,
	setConfidentialGroup/2,
	trainPublic/2,
	removeFile/1,
	removeGroup/1,
	removeFileFromGroup/2,
	compileFilters/0,
	compileCustomer/1,
        newAFileEntry/0,
        updateAFile/2,
        updateAFileFN/3,
        updateAFileFP/3,
	getRuletable/2,
	receiveBegin/1,
	receiveChunk/5
	]).

%%%%% EXTERNAL INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() -> thrift_server:start_link(9092, mydlp_ui_thrift, ?MODULE).

stop(Server) -> thrift_server:stop(Server), ok.

%%%%% THRIFT INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
	case apply(?MODULE, Function, tuple_to_list(Args)) of
		ok -> ok;
		Reply -> {reply, Reply}
	end.

%%%%% FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trainConfidential(Data, Fileid) -> mydlp_trainer:confidential(Data, Fileid).

setConfidentialGroup(Fileid, Groupid) -> mydlp_mnesia:set_gid_by_fid(Fileid, Groupid).

trainPublic(Data, Fileid) -> mydlp_trainer:public(Data, Fileid).

removeFile(Fileid) -> mydlp_mnesia:remove_file_entry(Fileid).

removeGroup(Groupid) ->	mydlp_mnesia:remove_group(Groupid).

removeFileFromGroup(Fileid, Groupid) -> mydlp_mnesia:remove_file_from_group(Fileid, Groupid).

compileFilters() -> mydlp_mysql:compile_filters().

compileCustomer(Customerid) -> mydlp_mysql:compile_customer(Customerid).

newAFileEntry() -> mydlp_mysql:new_afile().

updateAFile(Afileid, Adata) -> mydlp_archive:a(Afileid, Adata).

updateAFileFN(Afileid, Adata, Filename) -> mydlp_archive:a(Afileid, Adata, Filename).

updateAFileFP(Afileid, <<Afilepath/binary>>, Filename) ->
	updateAFileFP(Afileid, binary_to_list(Afilepath), Filename);
updateAFileFP(Afileid, [_|_] = Afilepath, Filename) -> 
	case filelib:is_regular(Afilepath) of
		true ->	{ok, Adata} = file:read_file(Afilepath),
			mydlp_archive:a(Afileid, Adata, Filename);
		false -> ?DEBUG("MyDLP TS: 'updateAFileFP'. Is not a regular file: ~p\n", [Afilepath]) end, ok.

getRuletable(Ipaddress, Revisionid) ->
	RevisionIdI = mydlp_api:binary_to_integer(Revisionid),
	ClientIpS = binary_to_list(Ipaddress),
	ClientIp = mydlp_api:str_to_ip(ClientIpS),
	mydlp_api:generate_client_policy(ClientIp, RevisionIdI).

receiveBegin(_Ipaddress) -> 
	{ok, Ret} = mydlp_container:new(), 
	RetL = integer_to_list(Ret),
	list_to_binary(RetL).

receiveChunk(Ipaddress, Itemid, Chunkdata, Chunknumtotal, Chunknumtotal) -> 
	ClientIpS = binary_to_list(Ipaddress),
	ClientIp = mydlp_api:str_to_ip(ClientIpS),
	case mydlp_container:push(Itemid, Chunkdata) of
		ok -> case mydlp_container:eof(Itemid) of
			ok -> case mydlp_container:getdata(Itemid) of
				{ok, Data} -> mydlp_item_receive:r(ClientIp, Data),
					mydlp_container:destroy(Itemid),
					<<"ok">>;
				_Else2 -> <<"error">> end;
			_Else1 -> <<"error">> end;
		_Else -> <<"error">> end;

receiveChunk(_Ipaddress, Itemid, Chunkdata, _Chunknum, _Chunknumtotal) -> 
	case mydlp_container:push(Itemid, Chunkdata) of
		ok -> <<"ok">>;
		_Else -> <<"error">> end.

-endif.

