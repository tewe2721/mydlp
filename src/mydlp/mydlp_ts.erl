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

-export([
	generateFingerprints/3,
	compileCustomer/1,
	getRuletable/3,
	receiveBegin/1,
	receiveChunk/5,
	requeueIncident/1,
	registerUserAddress/3,
	saveLicenseKey/1,
	getLicense/0
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

compileCustomer(Customerid) -> mydlp_mysql:compile_customer(Customerid).

getRuletable(Ipaddress, Userh, Revisionid) ->
	RevisionIdI = mydlp_api:binary_to_integer(Revisionid),
	UserHI = mydlp_api:binary_to_integer(Userh),
	ClientIpS = binary_to_list(Ipaddress),
	ClientIp = mydlp_api:str_to_ip(ClientIpS),
	mydlp_api:generate_client_policy(ClientIp, UserHI, RevisionIdI).

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

generateFingerprints(DocumentId, Filename, Data) -> 
	F = #file{filename=Filename, dataref=?BB_C(Data)},
	Text = mydlp_api:concat_texts(F),
	FList = mydlp_pdm:fingerprint(Text),
	mydlp_api:clean_files(F),
	lists:usort(FList),
	mydlp_mysql:save_fingerprints(DocumentId, FList).

requeueIncident(Incidentid) ->
	try     case mydlp_quarantine:l(payload, Incidentid) of
			{ok, Data} -> 	MessageR = erlang:binary_to_term(Data),
					mydlp_smtpc:mail(MessageR),
					mydlp_mysql:requeued(Incidentid);
			{ierror, _} ->	mydlp_mysql:delete_log_requeue(Incidentid), 
					?ERROR_LOG("REQUEUE_INCIDENT: Payload cannot find. Deleting DB entry. IncidentId: ["?S"].~n", [Incidentid])
					end
        catch Class:Error ->
                ?ERROR_LOG("REQUEUE_INCIDENT: Error occured: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
                        [Class, Error, erlang:get_stacktrace()])
        end, ok.

registerUserAddress(Ipaddress, Userh, Data) -> 
	Usern0 = try 	[{username,Username}] = erlang:binary_to_term(Data), Username
		catch Class:Error ->
			?ERROR_LOG("REGISTER_USER_ADDRESS: Error occured when deserializing: Class: ["?S"]. Error: ["?S"].~n"
					"Data: ["?S"]. UserHash: ["?S"]. IPAddr: ["?S"]. ~nStack trace: "?S"~n",
				[Class, Error, Data, Userh, Ipaddress, erlang:get_stacktrace()]),
			"" end,

	Usern = try 	lists:filter(fun(C) -> (C =< 255) and (C >= 0) end, Usern0)
		catch Class2:Error2 ->
			?ERROR_LOG("REGISTER_USER_ADDRESS: Error occured when filtering: Class: ["?S"]. Error: ["?S"].~n"
					"Username: ["?S"]. UserHash: ["?S"]. IPAddr: ["?S"]. ~nStack trace: "?S"~n",
				[Class2, Error2, Usern0, Userh, Ipaddress, erlang:get_stacktrace()]),
			"" end,
	UserHI = mydlp_api:binary_to_integer(Userh),
	ClientIpS = binary_to_list(Ipaddress),
	ClientIp = mydlp_api:str_to_ip(ClientIpS),
	mydlp_mnesia:save_user_address(ClientIp, UserHI, Usern),
	Usern.

saveLicenseKey(LicenseKey) -> mydlp_license:save_license_key(LicenseKey).

getLicense() -> mydlp_license:license().

-endif.

