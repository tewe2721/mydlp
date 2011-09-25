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
%%% @copyright 2011, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------


-module(mydlp_container).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	new/0,
	setprop/3,
	getprop/2,
	push/2,
	pushfile/2,
	eof/1,
	aclq/1,
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
	object_tree,
	archive_inbound=false
	}).

%%%% API

new() -> gen_server:call(?MODULE, new).

setprop(ObjId, Key, Value) -> gen_server:cast(?MODULE, {setprop, ObjId, Key, Value}).

getprop(ObjId, Key) -> gen_server:call(?MODULE, {getprop, ObjId, Key}).

push(ObjId, DataChunk) -> gen_server:cast(?MODULE, {push, ObjId, DataChunk}).

pushfile(ObjId, FilePath) -> gen_server:cast(?MODULE, {pushfile, ObjId, FilePath}).

pushchunk(ObjId, ChunkPath) -> gen_server:cast(?MODULE, {pushchunk, ObjId, ChunkPath}).

eof(ObjId) -> gen_server:cast(?MODULE, {eof, ObjId}).

aclq(ObjId) -> 	Timeout = 1500000,
		case gen_server:call(?MODULE, {aclq, ObjId, Timeout}, Timeout) of
		{ierror, {Class, Error}} -> mydlp_api:exception(Class, Error);
		Else -> Else end.

destroy(ObjId) -> gen_server:cast(?MODULE, {destroy, ObjId}).

%%%%%%%%%%%%%% gen_server handles

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
		{value, Obj} -> 
			Worker = self(),
			mydlp_api:mspawn(fun() -> 
					Return = try 
						File = object_to_file(Obj),
						DFFiles = [File],
						QRet = case is_inbound(Obj) of
							true -> mydlp_acl:qi(DFFiles);
							false -> mydlp_acl:qe(DFFiles) end,
						AclRet = acl_ret(QRet, Obj, DFFiles),
						{ok, AclRet}
					catch Class:Error ->
						?ERROR_LOG("ACLQ: Error occured: Class: [~w]. Error: [~w].~nStack trace: ~w~nObjID: [~w].~nState: ~w~n ",
							[Class, Error, erlang:get_stacktrace(), ObjId, State]),
							{ierror, {Class, Error}} end,
					Worker ! {async_reply, Return, From}
				end, Timeout);
		none -> gen_server:reply(From, {error, not_in_object_tree}) end,
	{noreply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({setprop, ObjId, Key, Value}, #state{object_tree=OT} = State) ->
	case gb_trees:lookup(ObjId, OT) of
		{value, #object{prop_dict=PD} = Obj} -> 
			PD1 = dict:store(Key, Value, PD),
			OT1 = gb_trees:enter(ObjId, Obj#object{prop_dict=PD1}, OT),
			{noreply, State#state{object_tree=OT1}};
		none -> ?ERROR_LOG("SETPROP: Object not found in object_tree: ObjId=~w, Key=~w, Value=~w ObjectTree=~w~n",
				[ObjId, Key, Value, OT]),
			{noreply, State}
			end;

handle_cast({pushfile, ObjId, FilePath}, #state{object_tree=OT} = State) ->
	case gb_trees:lookup(ObjId, OT) of
		{value, #object{eof_flag=false} = Obj} -> 
			OT1 = gb_trees:enter(ObjId, Obj#object{filepath=FilePath, buffer=[]}, OT),
			{noreply, State#state{object_tree=OT1}};
		{value, #object{eof_flag=true} = Obj} -> 
			?ERROR_LOG("PUSHFILE: eof_flag is true, not pushing file: ObjId=~w, FilePath=~w, Object=~w~n",
				[ObjId, FilePath, Obj]),
			{noreply, State};
		none -> ?ERROR_LOG("PUSHFILE: Object not found in object_tree: ObjId=~w, ObjectTree=~w~n",
				[ObjId, OT]),
			{noreply, State}
			end;

handle_cast({pushchunk, ObjId, ChunkPath}, State) ->
	try	{ok, DataChunk} = file:read_file(ChunkPath),
		handle_cast({push, ObjId, DataChunk}, State)
	catch Class:Error ->
		?ERROR_LOG("PUSHCUNK: Error occured: Class: [~w]. Error: [~w].~nStack trace: ~w~nObjID: [~w]. ChunkPath: [~w]~nState: ~w~n ",
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
			?ERROR_LOG("PUSH: eof_flag is true, not pushing: ObjId=~w, DataChunk=~w, Object=~w~n",
				[ObjId, DataChunk, Obj]),
			{noreply, State};
		{value, #object{eof_flag=false, filepath=FilePath} = Obj} -> 
			?ERROR_LOG("PUSH: Already pushed a file, not pushing data chunk: ObjId=~w, FilePath=~w, DataChunk=~w, Object=~w~n",
				[ObjId, FilePath, DataChunk, Obj]),
			{noreply, State};
		none -> ?ERROR_LOG("PUSH: Object not found in object_tree: ObjId=~w, DataChunk=~w ObjectTree=~w~n",
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
			?ERROR_LOG("EOF: eof_flag is already true, doing nothing: ObjId=~w, Object=~w~n",
				[ObjId, Obj]),
			{noreply, State};
		none -> ?ERROR_LOG("EOF: Object not found in object_tree: ObjId=~w, ObjectTree=~w~n",
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
	{ok, #state{	object_tree=gb_trees:empty(),
			archive_inbound=?CFG(archive_inbound)
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
		quarantine -> {quanratine, mydlp_api:empty_aclr(DFFiles)};
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
					% mydlp_archive will clean files.
					pass;
		{block, AclR} -> 	log_req(Obj, block, AclR),
					mydlp_api:clean_files(DFFiles),
					block;
		{quarantine, AclR} -> 	log_req(Obj, quarantine, AclR),
					mydlp_api:clean_files(DFFiles),
					block
	end.

archive_req(Obj, {{rule, RId}, {file, _}, {matcher, _}, {misc, _}}, DFFiles) ->
        case DFFiles of
                [] -> ok;
                _Else -> log_req(Obj, archive, {{rule, RId}, {file, DFFiles}, {matcher, none}, {misc,""}}) end.

log_req(_Obj, Action, {{rule, RuleId}, {file, File}, {matcher, Matcher}, {misc, Misc}}) ->
        ?ACL_LOG(seap, RuleId, Action, nil, nil, nil, Matcher, File, Misc).

is_inbound(#object{prop_dict=PD}) ->
	case dict:find("direction", PD) of
		{ok, "in"} -> true;
		{ok, "out"} -> false;
		{ok, _Else} -> false;
		error -> false end.

object_to_file(#object{prop_dict=PD, filepath=undefined, data=Data}) ->
	Filename = case dict:find("filename", PD) of
		{ok, FN} -> FN;
		error -> "seap-data" end,
	#file{filename=Filename, dataref=?BB_C(Data)};

object_to_file(#object{filepath=FilePath}) ->  % created with PUSHFILE
	Filename = filename:basename(FilePath),
	#file{filename=Filename, dataref=?BB_C({regularfile, FilePath})}.

call_timer() -> timer:send_after(150000, cleanup_now).

cleanup(OT) ->
	{_MegaSecs, Secs, MicroSecs} = erlang:now(),
	TSecs = case Secs > 150 of
		true -> Secs - 150;
		false -> 0 end,
	
	MinObjId = 1000000*TSecs + MicroSecs,
	ObjIds = gb_trees:keys(OT),
	cleanup1(OT, MinObjId, ObjIds).
	
cleanup1(OT, MinObjId, [ObjId| Rest]) when ObjId < MinObjId ->
	OT1 = gb_trees:delete_any(ObjId, OT),
	cleanup1(OT1, MinObjId, Rest);
cleanup1(OT, _MinObjId, _ObjIds) -> OT.
	

