%%
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
%%% @copyright 2010, H. Kerem Cevahir
%%% @doc Backend for mydlp ui functions.
%%% @end
%%%-------------------------------------------------------------------

-ifdef(__MYDLP_NETWORK).

-module(mydlp_ts).
-author("kerem@medra.com.tr").

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
	initEntity/0,
	pushData/2,
	analyze/1,
	closeEntity/1
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

initEntity() -> mydlp_moddlp:init_entity().

pushData(Entityid, Data) -> mydlp_moddlp:push_data(Entityid, Data).

analyze(Entityid) -> mydlp_moddlp:analyze(Entityid).

closeEntity(Entityid) -> mydlp_moddlp:close_entity(Entityid).

-endif.

