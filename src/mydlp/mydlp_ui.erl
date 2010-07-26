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
%%% @copyright 2010, H. Kerem Cevahir
%%% @doc Backend for mydlp ui functions.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_ui).
-author("kerem@medra.com.tr").

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
	compileFilters/0
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

removeFile(Fileid) -> 
	mydlp_mnesia:remove_fhash(Fileid),
	mydlp_mnesia:remove_shash(Fileid).

removeGroup(Groupid) ->
	mydlp_mnesia:remove_fhash_group(Groupid),
	mydlp_mnesia:remove_shash_group(Groupid).

compileFilters() -> ok.

