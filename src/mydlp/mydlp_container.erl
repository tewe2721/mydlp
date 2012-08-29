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
%%% @copyright 2011, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------


-module(mydlp_container).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	schedule_confupdate/0,
	confupdate/0,
	get_user/0,
	set_user/1,
	unset_user/0,
	new/0,
	setprop/3,
	getprop/2,
	push/2,
	pushfile/2,
	pushchunk/2,
	eof/1,
	aclq/1,
	aclq/2,
	getdata/1,
	destroy/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(object, {
	buffer=[],
	data,
	eof_flag=false,
	filepath=undefined,
	prop_dict=dict:new()
	}).

-record(state, {
	confupdate=true,
	username=unknown,
	object_tree
	}).

%%%% API

schedule_confupdate() -> gen_server:cast(?MODULE, schedule_confupdate).

confupdate() -> gen_server:call(?MODULE, confupdate).

get_user() -> gen_server:call(?MODULE, get_user).

set_user(Username) -> gen_server:cast(?MODULE, {set_user, Username}).

unset_user() -> gen_server:cast(?MODULE, {set_user, unknown}).

new() -> gen_server:call(?MODULE, new).

setprop(ObjId, Key, Value) -> gen_server:cast(?MODULE, {setprop, ObjId, Key, Value}).

getprop(ObjId, Key) -> gen_server:call(?MODULE, {getprop, ObjId, Key}).

push(ObjId, DataChunk) -> gen_server:cast(?MODULE, {push, ObjId, DataChunk}).

pushfile(ObjId, FilePath) -> gen_server:cast(?MODULE, {pushfile, ObjId, FilePath}).

pushchunk(ObjId, ChunkPath) -> gen_server:cast(?MODULE, {pushchunk, ObjId, ChunkPath}).

eof(ObjId) -> gen_server:cast(?MODULE, {eof, ObjId}).

getdata(ObjId) -> gen_server:call(?MODULE, {getdata, ObjId}).

aclq(ObjId) -> aclq(ObjId, 1500000).

aclq(ObjId, Timeout) ->
	case gen_server:call(?MODULE, {aclq, ObjId, Timeout}, Timeout) of
	{ierror, {Class, Error}} -> mydlp_api:exception(Class, Error);
	Else -> Else end.

destroy(ObjId) -> gen_server:cast(?MODULE, {destroy, ObjId}).

%%%%%%%%%%%%%% gen_server handles

handle_call(confupdate, _From, #state{confupdate=ConfUpdate} = State) ->
	{reply, ConfUpdate, State#state{confupdate=false}};

handle_call(get_user, _From, #state{username=Username} = State) ->
	{reply, Username, State};

handle_call(new, _From, #state{object_tree=OT} = State) ->
	{_MegaSecs, Secs, MicroSecs} = erlang:now(),
	% we do not include MegaSecs, because timeout cleanup will schedule within a period less than 1000000 seconds.
	ObjId = 1000000*Secs + MicroSecs,
	OT1 = gb_trees:enter(ObjId, #object{}, OT),
	Reply = {ok, ObjId},
	{reply, Reply, State#state{object_tree=OT1}};

handle_call({getprop, ObjId, Key}, _From, #state{object_tree=OT} = State) ->
	Reply = case gb_trees:lookup(ObjId, OT) of
		{value, #object{prop_dict=PD}} -> 
				case dict:find(Key, PD) of
					{ok, Value} -> {ok, Value};
					error -> {error, not_in_prop_dict} end;
		none -> {error, not_in_object_tree} end,
	{reply, Reply, State};

handle_call({aclq, ObjId, Timeout}, From, #state{object_tree=OT} = State) ->
	case gb_trees:lookup(ObjId, OT) of
		{value, #object{eof_flag=true} = Obj} -> 
			Worker = self(),
			mydlp_api:mspawn(fun() -> 
					Return = try 
						File = object_to_file(Obj),
						DFFiles = [File],
						Channel = get_channel(Obj),
						{QRet, Obj1} = case Channel of
							api ->	IpAddress = get_ip_address(Obj),
								{UserName, UserHash} = mydlp_mnesia:get_user_from_address(IpAddress),
								AclQ = #aclq{channel=Channel, src_addr=IpAddress, src_user_h=UserHash},
								{mydlp_acl:q(AclQ, DFFiles), set_api_user(Obj, UserName)};
							_Else -> { case ( ?CFG(archive_inbound) and is_inbound(Obj) ) of
									true -> mydlp_acl:qi(Channel, DFFiles);
									false -> mydlp_acl:qe(Channel, DFFiles) end,
								Obj } end,
						AclRet = acl_ret(QRet, Obj1, DFFiles),
						{ok, AclRet}
					catch	throw:{error, eacces} -> {ok, pass};
						Class:Error ->
							?ERROR_LOG("ACLQ: Error occured: Class: ["?S"]. Error: ["?S"].~n"
									"Stack trace: "?S"~nObjID: ["?S"].~nState: "?S"~n ",
								[Class, Error, erlang:get_stacktrace(), ObjId, State]),
								{ierror, {Class, Error}} end,
					Worker ! {async_reply, Return, From}
				end, Timeout);
		{value, #object{eof_flag=false} = Obj} -> 
			?ERROR_LOG("ACLQ: eof_flag is not true, can not ACLQ before EOF: ObjId="?S", Obj="?S" OT="?S"~n",
				[ObjId, Obj, OT]),
			gen_server:reply(From, {error, eof_flag_is_not_true});
		none -> gen_server:reply(From, {error, not_in_object_tree}) end,
	{noreply, State};

handle_call({getdata, ObjId}, _From, #state{object_tree=OT} = State) ->
	Reply = case gb_trees:lookup(ObjId, OT) of
		{value, #object{eof_flag=true, data=Data}} -> 
			{ok, Data};
		{value, #object{eof_flag=false} = Obj} -> 
			?ERROR_LOG("ACLQ: eof_flag is not true, can not GETDATA before EOF: ObjId="?S", Obj="?S" OT="?S"~n",
				[ObjId, Obj, OT]),
			{error, eof_flag_is_not_true};
		none -> {error, not_in_object_tree} end,
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(schedule_confupdate, State) ->
	{noreply, State#state{confupdate=true}};

handle_cast({set_user, Username}, State) ->
	UsernameU = qp_decode(Username),
	{noreply, State#state{username=UsernameU}};

handle_cast({setprop, ObjId, Key, Value}, #state{object_tree=OT} = State) ->
	case gb_trees:lookup(ObjId, OT) of
		{value, #object{prop_dict=PD} = Obj} -> 
			PD1 = dict:store(Key, Value, PD),
			OT1 = gb_trees:enter(ObjId, Obj#object{prop_dict=PD1}, OT),
			{noreply, State#state{object_tree=OT1}};
		none -> ?ERROR_LOG("SETPROP: Object not found in object_tree: ObjId="?S", Key="?S", Value="?S" ObjectTree="?S"~n",
				[ObjId, Key, Value, OT]),
			{noreply, State}
			end;

handle_cast({pushfile, ObjId, {raw, FilePath}}, #state{object_tree=OT} = State) ->
	case gb_trees:lookup(ObjId, OT) of
		{value, #object{eof_flag=false} = Obj} -> 
			OT1 = gb_trees:enter(ObjId, Obj#object{filepath=FilePath, buffer=[]}, OT),
			{noreply, State#state{object_tree=OT1}};
		{value, #object{eof_flag=true} = Obj} -> 
			?ERROR_LOG("PUSHFILE: eof_flag is true, not pushing file: ObjId="?S", FilePath="?S", Object="?S"~n",
				[ObjId, FilePath, Obj]),
			{noreply, State};
		none -> ?ERROR_LOG("PUSHFILE: Object not found in object_tree: ObjId="?S", ObjectTree="?S"~n",
				[ObjId, OT]),
			{noreply, State}
			end;

handle_cast({pushfile, ObjId, FilePath}, State) -> handle_cast({pushfile, ObjId, {raw, qp_decode(FilePath)}}, State);

handle_cast({pushchunk, ObjId, ChunkPath}, State) ->
	try	{ok, DataChunk} = file:read_file(ChunkPath),
		handle_cast({push, ObjId, DataChunk}, State)
	catch Class:Error ->
		?ERROR_LOG("PUSHCUNK: Error occured: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~nObjID: ["?S"]. ChunkPath: ["?S"]~nState: "?S"~n ",
			[Class, Error, erlang:get_stacktrace(), ObjId, ChunkPath, State]),
		{noreply, State}
	end;

% could use dataref appends in push after a certain threshold.
handle_cast({push, ObjId, DataChunk}, #state{object_tree=OT} = State) ->
	case gb_trees:lookup(ObjId, OT) of
		{value, #object{eof_flag=false, filepath=undefined, buffer=Buffer} = Obj} -> 
			OT1 = gb_trees:enter(ObjId, Obj#object{buffer=[DataChunk|Buffer]}, OT),
			{noreply, State#state{object_tree=OT1}};
		{value, #object{eof_flag=true} = Obj} -> 
			?ERROR_LOG("PUSH: eof_flag is true, not pushing: ObjId="?S", DataChunk="?S", Object="?S"~n",
				[ObjId, DataChunk, Obj]),
			{noreply, State};
		{value, #object{eof_flag=false, filepath=FilePath} = Obj} -> 
			?ERROR_LOG("PUSH: Already pushed a file, not pushing data chunk: ObjId="?S", FilePath="?S", DataChunk="?S", Object="?S"~n",
				[ObjId, FilePath, DataChunk, Obj]),
			{noreply, State};
		none -> ?ERROR_LOG("PUSH: Object not found in object_tree: ObjId="?S", DataChunk="?S" ObjectTree="?S"~n",
				[ObjId, DataChunk, OT]),
			{noreply, State}
			end;

handle_cast({eof, ObjId}, #state{object_tree=OT} = State) ->
	case gb_trees:lookup(ObjId, OT) of
		{value, #object{eof_flag=false, filepath=undefined, buffer=Buffer} = Obj} -> 
			Data = list_to_binary(lists:reverse(Buffer)),
			OT1 = gb_trees:enter(ObjId, Obj#object{buffer=[], eof_flag=true, data=Data}, OT),
			{noreply, State#state{object_tree=OT1}};
		{value, #object{eof_flag=false} = Obj} ->  % END after PUSHFILE
			OT1 = gb_trees:enter(ObjId, Obj#object{eof_flag=true}, OT),
			{noreply, State#state{object_tree=OT1}};
		{value, #object{eof_flag=true} = Obj} -> 
			?ERROR_LOG("EOF: eof_flag is already true, doing nothing: ObjId="?S", Object="?S"~n",
				[ObjId, Obj]),
			{noreply, State};
		none -> ?ERROR_LOG("EOF: Object not found in object_tree: ObjId="?S", ObjectTree="?S"~n",
				[ObjId, OT]),
			{noreply, State}
			end;

handle_cast({destroy, ObjId}, #state{object_tree=OT} = State) ->
	OT1 = gb_trees:delete_any(ObjId, OT),
	{noreply, State#state{object_tree=OT1}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(cleanup_now, #state{object_tree=OT} = State) ->
	OT1 = cleanup(OT),
	call_timer(),
        {noreply, State#state{object_tree=OT1}};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	call_timer(),
	{ok, #state{	object_tree=gb_trees:empty()
			}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

acl_ret(QRet, Obj, DFFiles) ->
	case case QRet of % TODO filepath as destination 
		pass -> {pass, mydlp_api:empty_aclr(DFFiles)};
		log -> {log, mydlp_api:empty_aclr(DFFiles)};
		archive -> {archive, mydlp_api:empty_aclr(DFFiles)};
		block -> {block, mydlp_api:empty_aclr(DFFiles)};
		quarantine -> {quarantine, mydlp_api:empty_aclr(DFFiles)};
		{pass, _AR} = T -> T;
		{log, _AR} = T -> T;
		{archive, _AR} = T -> T;
		{block, _AR} = T -> T;
		{quarantine, _AR} = T -> T
	end of
		{pass, _AclR} -> 	mydlp_api:clean_files(DFFiles),
					pass; 
		{log, AclR} -> 		log_req(Obj, log, AclR),
					mydlp_api:clean_files(DFFiles),
					pass; 
		{archive, AclR} -> 	archive_req(Obj, AclR, DFFiles),
					% mydlp_incident will clean files.
					pass;
		{block, AclR} -> 	log_req(Obj, block, AclR),
					mydlp_api:clean_files(DFFiles),
					block;
		{quarantine, AclR} -> 	log_req(Obj, quarantine, AclR),
					mydlp_api:clean_files(DFFiles),
					block
	end.

archive_req(Obj, {{rule, RId}, {file, _}, {itype, IType}, {misc, Misc}}, DFFiles) ->
        case DFFiles of
                [] -> ok;
                _Else -> log_req(Obj, archive, {{rule, RId}, {file, DFFiles}, {itype, IType}, {misc, Misc}}) end.

log_req(Obj, Action, {{rule, RuleId}, {file, File}, {itype, IType}, {misc, Misc}}) ->
	User = case get_channel(Obj) of
		api -> get_api_user(Obj);
		_Else -> get_user() end,
	Channel = get_channel(Obj),
	Time = erlang:localtime(),
	log_req1(Time, Channel, RuleId, Action, User, IType, File, Misc).

-ifdef(__MYDLP_ENDPOINT).

log_req1(Time, Channel, RuleId, Action, User, IType, File, Misc) ->
	case {Channel, Action, Misc, ?CFG(ignore_discover_max_size_exceeded)} of
		{discovery, log, max_size_exceeded, true} -> ok;
		_Else2 -> ?ACL_LOG(Time, Channel, RuleId, Action, nil, User, nil, IType, File, Misc) end.

-endif.

-ifdef(__MYDLP_NETWORK).

log_req1(Time, Channel, RuleId, Action, User, IType, File, Misc) ->
	?ACL_LOG(Time, Channel, RuleId, Action, nil, User, nil, IType, File, Misc).

-endif.

is_inbound(#object{prop_dict=PD}) ->
	case dict:find("direction", PD) of
		{ok, "in"} -> true;
		{ok, "out"} -> false;
		{ok, _Else} -> false;
		error -> false end.

get_channel(#object{prop_dict=PD}) ->
	case dict:find("channel", PD) of
		{ok, "discovery"} -> discovery;
		{ok, "api"} -> api;
	error -> case dict:find("printerName", PD) of
		{ok, _} -> printer;
		error -> endpoint end end.

get_type(#object{prop_dict=PD}) ->
	case dict:find("type", PD) of
		{ok, "usb_device"} -> usb_device;
		{ok, "regular"} -> regular;
		{ok, _Else} -> regular;
		error -> regular  end.

get_ip_address(#object{prop_dict=PD}) ->
	case dict:find("ip_address", PD) of
		{ok, ClientIpS} -> mydlp_api:str_to_ip(ClientIpS);
		error -> unknown  end.

set_api_user(#object{prop_dict=PD} = Obj, UserName) ->
	PD1 = dict:store("api_user", UserName, PD),
	Obj#object{prop_dict=PD1}.

get_api_user(#object{prop_dict=PD}) ->
	case dict:find("api_user", PD) of
		{ok, User} -> User;
		error -> nil  end.

%get_user(#object{prop_dict=PD}) ->
%	case dict:find("user", PD) of
%		{ok, User} -> User;
%		error -> nil  end.

object_to_file(Obj) ->
	Type = get_type(Obj),
	object_to_file(Type, Obj).

object_to_file(regular, #object{prop_dict=PD, filepath=undefined, data=Data}) ->
	Filename = case dict:find("filename", PD) of
		{ok, FN} -> qp_decode(FN);
		error -> case dict:find("filename_unicode", PD) of
			{ok, UFN} -> UFN;
			error -> "seap-data" end end,
	?BF_C(#file{filename=Filename}, Data);

object_to_file(regular, #object{prop_dict=PD, filepath=FilePath}) ->  % created with PUSHFILE
	Filename = case dict:find("filename", PD) of
		{ok, FN} -> qp_decode(FN);
		error -> filename:basename(FilePath) end,
	URef = case dict:find("burn_after_reading", PD) of
		{ok, "true"} ->	{tmpfile, FilePath};
		_Else -> {regularfile, FilePath} end,
	?BF_C(#file{filename=Filename}, URef);

object_to_file(usb_device, #object{prop_dict=PD}) ->
	DeviceId = case dict:find("device_id", PD) of
		{ok, DId} -> DId;
		error -> "unknown" end,
	DeviceIdB = list_to_binary(DeviceId),
	#file{name="USB Device, device_id=" ++ DeviceId,
		mime_type= <<"mydlp-internal/usb-device;id=", DeviceIdB/binary>>}.


call_timer() -> timer:send_after(1000000, cleanup_now).

cleanup(OT) ->
	{_MegaSecs, Secs, MicroSecs} = erlang:now(),
	TSecs = case Secs > 1000 of % TODO: should use object update age not id
		true -> Secs - 1000;
		false -> 0 end,
	
	MinObjId = 1000000*TSecs + MicroSecs,
	ObjIds = gb_trees:keys(OT),
	cleanup1(OT, MinObjId, ObjIds).
	
cleanup1(OT, MinObjId, [ObjId| Rest]) when ObjId < MinObjId ->
	OT1 = gb_trees:delete_any(ObjId, OT),
	cleanup1(OT1, MinObjId, Rest);
cleanup1(OT, _MinObjId, _ObjIds) -> OT.

qp_decode(Str) when is_list(Str) -> qp_decode(list_to_binary(Str));
qp_decode(Str) when is_binary(Str) ->
	DBin = mydlp_api:quoted_to_raw(Str),
	unicode:characters_to_list(DBin).
	%DList = unicode:characters_to_list(DBin, {utf16, little}),
	%DList = unicode:characters_to_list(DBin).
	%filename:nativename(DList).

	

