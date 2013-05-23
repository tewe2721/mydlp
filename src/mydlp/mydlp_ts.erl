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
	generateFingerprintsWithFile/3,
	compileCustomer/1,
	getCompileStatus/0,
	getRuletable/2,
	receiveBegin/1,
	receiveChunk/5,
	requeueIncident/1,
	registerUserAddress/4,
	saveLicenseKey/1,
	getLicense/0,
	apiQuery/3,
	startDiscoveryOnDemand/1,
	stopDiscoveryOnDemand/1,
	pauseDiscoveryOnDemand/1,
	getRemoteStorageDir/1,
	startFingerprinting/1,
	stopFingerprinting/1,
	testConnection/1,
	testWebServer/1
	]).

%%%%% EXTERNAL INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%start_link() -> thrift_server:start_link(9092, mydlp_ui_thrift, ?MODULE, [{framed,true}]).

start_link() -> thrift_socket_server:start([{handler, ?MODULE},
                                {service, mydlp_ui_thrift},
                                {port, 9092},
                                {framed, true},
                                {name, mydlp_ts},
				{socket_opts, [{recv_timeout, infinity}]}
				]).

stop(Server) -> thrift_server:stop(Server), ok.

%%%%% THRIFT INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
	case apply(?MODULE, Function, tuple_to_list(Args)) of
		ok -> ok;
		Reply -> {reply, Reply}
	end.

%%%%% FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compileCustomer(Customerid) -> mydlp_mysql:compile_customer(Customerid).

getCompileStatus() -> 
	Atom = mydlp_mysql:get_progress(),
	atom_to_list(Atom).

getRuletable(EndpointId, Revisionid) ->
	RevisionIdI = mydlp_api:binary_to_integer(Revisionid),
	mydlp_api:generate_client_policy(EndpointId, RevisionIdI).

apiQuery(Ipaddress, Filename, Data) ->
	{ok, Itemid} = mydlp_container:new(), 
	ClientIpS = binary_to_list(Ipaddress),
	try	ok = mydlp_container:setprop(Itemid, "channel", "api"),
		ok = mydlp_container:setprop(Itemid, "filename_unicode", Filename),
		ok = mydlp_container:setprop(Itemid, "ip_address", ClientIpS),
		ok = mydlp_container:push(Itemid, Data),
		ok = mydlp_container:eof(Itemid),
		{ok, QueryRet} = mydlp_container:aclq(Itemid),
		erlang:atom_to_binary(QueryRet, unicode)
	catch Class:Error ->
		?ERROR_LOG("API CALL: Error occured when processing api call: Class: ["?S"]. Error: ["?S"].~n"
				"IPAddr: ["?S"]. ~nStack trace: "?S"~n",
			[Class, Error,  Ipaddress, erlang:get_stacktrace()]),
	<<"error">> end.
	
	

receiveBegin(_EndpointId) -> 
	{ok, Ret} = mydlp_container:new(), 
	RetL = integer_to_list(Ret),
	list_to_binary(RetL).

receiveChunk(EndpointId, Itemid, Chunkdata, Chunknumtotal, Chunknumtotal) -> 
	case mydlp_container:push(Itemid, Chunkdata) of
		ok -> case mydlp_container:eof(Itemid) of
			ok -> case mydlp_container:getdata(Itemid) of
				{ok, Data} -> mydlp_item_receive:r(EndpointId, Data),
					mydlp_container:destroy(Itemid),
					<<"ok">>;
				_Else2 -> <<"error">> end;
			_Else1 -> <<"error">> end;
		_Else -> <<"error">> end;

receiveChunk(_EndpointId, Itemid, Chunkdata, _Chunknum, _Chunknumtotal) -> 
	case mydlp_container:push(Itemid, Chunkdata) of
		ok -> <<"ok">>;
		_Else -> <<"error">> end.

generateFingerprints(DocumentId, Filename, Data) -> 
	generateFingerprintsFunc(DocumentId, Filename, Data).

generateFingerprintsWithFile(DocumentId, Filename, Filepath) -> 
	generateFingerprintsFunc(DocumentId, Filename, {tmpfile, Filepath}).

generateFingerprintsFunc(DocumentId, Filename, Data) -> 
	F = ?BF_C(#file{filename=Filename}, Data),
	Text = mydlp_api:concat_texts(F),
	FList = mydlp_pdm:fingerprint(Text),
	mydlp_api:clean_files(F),
	FList1 = lists:usort(lists:map(fun(#kgram{hash=Hash}) -> Hash end, FList)),
	mydlp_mysql:save_fingerprints(DocumentId, FList1),
	ok.

requeueIncident(Incidentid) ->
	try     case mydlp_quarantine:l(payload, Incidentid) of
			{ok, Data} -> 	MessageR = erlang:binary_to_term(Data),
					mydlp_smtpc:mail(MessageR),
					Ret = mydlp_mysql:requeued(Incidentid),
					mydlp_smtp_fsm:requeue_msg(MessageR), Ret;
			{ierror, _} ->	mydlp_mysql:delete_log_requeue(Incidentid), 
					?ERROR_LOG("REQUEUE_INCIDENT: Payload cannot find. Deleting DB entry. IncidentId: ["?S"].~n", [Incidentid])
					end
        catch Class:Error ->
                ?ERROR_LOG("REQUEUE_INCIDENT: Error occured: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
                        [Class, Error, erlang:get_stacktrace()])
        end, ok.

get_arg_value(MetaDict, Arg) ->
	case dict:find(Arg, MetaDict) of
		error -> "";
		{ok, ""} -> "";
		{ok, nil} -> "";
		{ok, unknown} -> "";
		{ok, undefined} -> "";
		{ok, Val} -> Val end.

registerUserAddress(EndpointId, Ipaddress, Userh, Data) -> 
	MetaDict = try erlang:binary_to_term(Data)
		catch Class:Error ->
			?ERROR_LOG("REGISTER_USER_ADDRESS: Error occured when deserializing: Class: ["?S"]. Error: ["?S"].~n"
					"Data: ["?S"]. UserHash: ["?S"]. IPAddr: ["?S"]. ~nStack trace: "?S"~n",
				[Class, Error, Data, Userh, Ipaddress, erlang:get_stacktrace()]),
		dict:new() end,

	case get_arg_value(MetaDict, "has_enc_key") of
		"no" -> case (catch mydlp_api:get_encryption_key()) of
			EncKey when is_binary(EncKey), size(EncKey) == 64 -> 
				mydlp_mnesia:save_endpoint_command(EndpointId, {set_enc_key, EncKey});
			Else -> ?ERROR_LOG("Error occurred obtaining encryption key: "?S , [Else]), ok end;
		_Else -> ok end,

	Hostname = case get_arg_value(MetaDict, "hostname") of
		"" -> unknown;
		H when is_binary(H) -> mydlp_nlp:to_lower_bin(H);
		H when is_list(H) -> 
			try 	HBin = unicode:characters_to_binary(H),
				mydlp_nlp:to_lower_bin(HBin)
			catch Class2:Error2 -> 
				?ERROR_LOG("REGISTER_USER_ADDRESS: Error occured when normalizing hostname: Class: ["?S"]. Error: ["?S"].~n"
						"Stack trace: "?S"~n",
					[Class2, Error2, erlang:get_stacktrace()]),
				unknown 
			end end,

	Username = case get_arg_value(MetaDict, "user") of
		"" -> unknown;
		U -> U end,

	UserHI = mydlp_api:binary_to_integer(Userh),
	ClientIpS = binary_to_list(Ipaddress),
	ClientIp = mydlp_api:str_to_ip(ClientIpS),
	mydlp_mnesia:save_user_address(EndpointId, ClientIp, UserHI, Username, Hostname),
	MetaDict.

saveLicenseKey(LicenseKey) ->
	mydlp_license:save_license_key(LicenseKey),
	Msg = mydlp_license:sync_result(),
	list_to_binary(Msg).

getLicense() -> mydlp_license:license().

startDiscoveryOnDemand(RuleId) -> mydlp_discovery_manager:start_on_demand_discovery(RuleId).

stopDiscoveryOnDemand(RuleId) -> mydlp_discovery_manager:stop_discovery_on_demand(RuleId).

pauseDiscoveryOnDemand(RuleId) -> mydlp_discovery_manager:pause_discovery_on_demand(RuleId).

getRemoteStorageDir(RSId) ->  mydlp_document_trainer:get_remote_storage_dir(RSId).

startFingerprinting(DDId) -> mydlp_document_trainer:start_fingerprinting(DDId).

stopFingerprinting(DDId) -> mydlp_document_trainer:stop_fingerprinting(DDId).

testConnection(RSDict) -> mydlp_document_trainer:test_connection(RSDict).

testWebServer(URL) -> mydlp_discover_web:test_web_server(URL).

-endif.

