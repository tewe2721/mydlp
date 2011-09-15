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

-module(mydlp_seap_fsm).
-author('kerem@medratech.com').
-behaviour(gen_fsm).
-include("mydlp.hrl").

-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'SEAP_REQ'/2,
    'PUSH_DATA_RECV'/2
]).

-record(state, {
	socket,
	addr,
	obj_id,
	recv_size,
	recv_data=[]
}).

-define(BIN_OK, <<"OK">>).
-define(BIN_ERR, <<"ERR">>).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	{ok, 'WAIT_FOR_SOCKET', #state{}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket, _CommType}, State) when is_port(Socket) ->
	inet:setopts(Socket, [{active, once}, {packet, line}, list]),
	{ok, {IP, _Port}} = inet:peername(Socket),
	{next_state, 'SEAP_REQ', State#state{socket=Socket, addr=IP}, ?CFG(fsm_timeout)};
'WAIT_FOR_SOCKET'(Other, State) ->
	?DEBUG("ICAP FSM: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'SEAP_REQ'({data, "BEGIN" ++ _Else}, State) -> 
	'BEGIN_RESP'(State);
'SEAP_REQ'({data, "SETPROP" ++ Rest}, State) -> 
	{ ObjId, Key, Value } = get_setprop_args(Rest),
	'SETPROP_RESP'(State, ObjId, Key, Value);
'SEAP_REQ'({data, "GETPROP" ++ Rest}, State) -> 
	{ ObjId, Key } = get_getprop_args(Rest),
	'GETPROP_RESP'(State, ObjId, Key);
'SEAP_REQ'({data, "PUSH" ++ Rest}, #state{socket=Socket} = State) -> 
	{ ObjId, RecvSize} = get_req_args(Rest),
	inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
	{next_state, 'PUSH_DATA_RECV', State#state{obj_id=ObjId, recv_size=RecvSize}, ?CFG(fsm_timeout)};
'SEAP_REQ'({data, "PUSHFILE" ++ Rest}, State) -> 
	{ ObjId, FilePath } = get_getprop_args(Rest),
	'PUSHFILE_RESP'(State, ObjId, FilePath);
'SEAP_REQ'({data, "END" ++ Rest}, State) -> 
	{ ObjId } = get_req_args(Rest),
	'END_RESP'(State, ObjId);
'SEAP_REQ'({data, "ACLQ" ++ Rest}, State) -> 
	{ ObjId } = get_req_args(Rest),
	'ACLQ_RESP'(State, ObjId);
'SEAP_REQ'({data, "DESTROY" ++ Rest}, State) -> 
	{ ObjId } = get_req_args(Rest),
	'DESTROY_RESP'(State, ObjId);
'SEAP_REQ'({data, "HELP" ++ _Rest}, State) -> 
	'HELP_RESP'(State);
'SEAP_REQ'({data, _Else}, State) -> 
	'HELP_RESP'(State);
'SEAP_REQ'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'BEGIN_RESP'(State) ->
	{ok, ObjId} = mydlp_container:new(),
	send_ok(State, ObjId),
	{next_state, 'SEAP_REQ', State, ?CFG(fsm_timeout)}.

'SETPROP_RESP'(State, ObjId, Key, Value) ->
	ok = mydlp_container:setprop(ObjId, Key, Value),
	send_ok(State),
	{next_state, 'SEAP_REQ', State, ?CFG(fsm_timeout)}.

'GETPROP_RESP'(State, ObjId, Key) ->
	{ok, Value} = mydlp_container:getprop(ObjId, Key),
	send_ok(State, Value),
	{next_state, 'SEAP_REQ', State, ?CFG(fsm_timeout)}.

'PUSH_RESP'(State, ObjId, ObjData) ->
	ok = mydlp_container:push(ObjId, ObjData),
	send_ok(State),
	{next_state, 'SEAP_REQ', State, ?CFG(fsm_timeout)}.

'PUSHFILE_RESP'(State, ObjId, FilePath) ->
	ok = mydlp_container:pushfile(ObjId, FilePath),
	send_ok(State),
	{next_state, 'SEAP_REQ', State, ?CFG(fsm_timeout)}.

'END_RESP'(State, ObjId) ->
	ok = mydlp_container:eof(ObjId),
	send_ok(State),
	{next_state, 'SEAP_REQ', State, ?CFG(fsm_timeout)}.

'ACLQ_RESP'(State, ObjId) ->
	{ok, Action} = mydlp_container:aclq(ObjId),
	send_ok(State, Action),
	{next_state, 'SEAP_REQ', State, ?CFG(fsm_timeout)}.

'DESTROY_RESP'(State, ObjId) ->
	ok = mydlp_container:destroy(ObjId),
	send_ok(State),
	{next_state, 'SEAP_REQ', State, ?CFG(fsm_timeout)}.

'HELP_RESP'(State) ->
	Print = "\r\n" ++ "Commands:" ++ "\r\n" ++
		"\t" ++ "BEGIN -> OK Id" ++ "\r\n" ++
		"\t" ++ "SETPROP Id Key=Value-> OK" ++ "\r\n" ++
		"\t" ++ "GETPROP Id Key-> OK Value" ++ "\r\n" ++
		"\t" ++ "PUSH Id ChunkSize" ++ "\r\n" ++
		"\t\t" ++ "Chunk -> OK" ++ "\r\n" ++
		"\t" ++ "PUSHFILE Id FilePath" ++ "\r\n" ++
		"\t" ++ "END Id -> OK" ++ "\r\n" ++
		"\t" ++ "ACLQ Id -> OK Action" ++ "\r\n" ++
		"\t" ++ "DESTROY Id -> OK" ++ "\r\n" ++
		"\t" ++ "HELP -> This screen" ++ "\r\n" ++
		"Any other command prints this screen." ++ "\r\n" ++
		"If an internal error occurs, server respond with ERR instead of OK." ++ "\r\n",
	send_ok(State, Print),
	{next_state, 'SEAP_REQ', State, ?CFG(fsm_timeout)}.

'PUSH_DATA_RECV'({data, Data}, #state{socket=Socket, obj_id=ObjId, recv_size=RecvSize, recv_data=RecvData} = State) -> 
	DataSize = mydlp_api:binary_size(Data),
	NewSize = RecvSize - DataSize,
	case NewSize of
		-2 ->	case Data of
				<<DataC:RecvSize/binary, "\r\n">> -> 'PUSH_DATA_RECV'({data, DataC}, State);
				Else -> throw({error, {unexpected_binary_size, Else}}) end;
		0 -> 	RecvData1 = [Data|RecvData],
			ObjData = list_to_binary(lists:reverse(RecvData1)),
			inet:setopts(Socket, [{active, once}, {packet, line}, list]),
			'PUSH_RESP'(State#state{obj_id=undefined, recv_size=undefined, recv_data=[]}, ObjId, ObjData);
		NewSize when NewSize > 0 ->
			RecvData1 = [Data|RecvData],
			{next_state, 'PUSH_DATA_RECV', State#state{recv_size=NewSize, recv_data=RecvData1}, ?CFG(fsm_timeout)};
		_Else -> throw({error, {unexpected_binary_size, DataSize}}) end;
'PUSH_DATA_RECV'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(stop, _StateName, State) ->
	{stop, normal, State};
handle_event(Event, StateName, StateData) ->
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	Return = fsm_call(StateName, {data, Bin}, StateData),
	inet:setopts(Socket, [{active, once}]),
	Return;

handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket, addr=_Addr} = StateData) ->
	% ?ERROR_LOG("~p Client ~p disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
	{noreply, StateName, StateData}.

fsm_call(StateName, Args, StateData) -> 
	try ?MODULE:StateName(Args, StateData)
	catch Class:Error ->
		?ERROR_LOG("Error occured on FSM (~w) call (~w). Class: [~w]. Error: [~w].~nStack trace: ~w~n",
				[?MODULE, StateName, Class, Error, erlang:get_stacktrace()]),
		send_err(StateData),
		{next_state, 'SEAP_REQ', StateData, ?CFG(fsm_timeout)} end.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket} = _State) ->
	% @todo: close conenctions to message store
    (catch gen_tcp:close(Socket)),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

rm_trailing_crlf(Str) when is_list(Str) ->
	StrL = string:len(Str),
	"\r\n" = string:substr(Str, StrL - 1, 2),
	string:substr(Str, 1, StrL - 2);
rm_trailing_crlf(Bin) when is_binary(Bin) -> 
	BuffSize = size(Bin) - 2,
	<<Buff:BuffSize/binary, "\r\n">> = Bin,
	Buff.

get_req_args(Rest) ->
	Rest1 = rm_trailing_crlf(Rest),
	case string:tokens(Rest1, " ") of
		[ObjIdS] -> { list_to_integer(ObjIdS) };
		[ObjIdS, ChunkSize] -> { list_to_integer(ObjIdS), list_to_integer(ChunkSize) };
		_Else -> throw({error, {obj_id_not_found, Rest}}) end.

get_setprop_args(Rest) ->
	Rest1 = rm_trailing_crlf(Rest),
	[ObjIdS, KeyValuePairS] = string:tokens(Rest1, " "),
	[Key| ValueL] = string:tokens(KeyValuePairS, "=" ),
	Value = string:join(ValueL, "="),
	{list_to_integer(ObjIdS), Key, Value}.

get_getprop_args(Rest) ->
	Rest1 = rm_trailing_crlf(Rest),
	[ObjIdS, Key] = string:tokens(Rest1, " "),
	{list_to_integer(ObjIdS), Key}.

send(#state{socket=Socket}, Data) -> gen_tcp:send(Socket, <<Data/binary, "\r\n">>).

send_err(State) -> send(State, ?BIN_ERR).

send_ok(State) -> send(State, ?BIN_OK).

send_ok(State, Arg) when is_binary(Arg) -> send(State, <<?BIN_OK/binary, " ", Arg/binary>>);
send_ok(State, Arg) when is_integer(Arg) -> send_ok(State, integer_to_list(Arg));
send_ok(State, Arg) when is_atom(Arg) -> send_ok(State, atom_to_list(Arg));
send_ok(State, Arg) when is_list(Arg)-> send_ok(State, list_to_binary(Arg)).

