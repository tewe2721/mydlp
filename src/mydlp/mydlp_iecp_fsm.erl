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

-module(mydlp_iecp_fsm).
-author('kerem@mydlp.com').
-behaviour(gen_fsm).
-include("mydlp.hrl").

-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
	'WAIT_FOR_SOCKET'/2,
	'IECP_REQ'/2,
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
	inet:setopts(Socket, [{active, once}, {nodelay, true}, {packet, line}, list]),
	{ok, {IP, _Port}} = inet:peername(Socket),
	{next_state, 'IECP_REQ', State#state{socket=Socket, addr=IP}, ?CFG(fsm_timeout)};
'WAIT_FOR_SOCKET'(Other, State) ->
	?DEBUG("IECP FSM: 'WAIT_FOR_SOCKET'. Unexpected message: "?S, [Other]),
	%% Allow to receive async messages
	{next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'IECP_REQ'({data, "TOKEN" ++ Rest}, State) -> 
	{ Token } = get_req_str_arg(Rest),
	'TOKEN_RESP'(State, Token);
'IECP_REQ'({data, "SETPROP" ++ _Rest}, #state{obj_id=undefined} = State) ->
        send_err(State),
        {next_state, 'IECP_REQ', State, ?CFG(fsm_timeout)};
'IECP_REQ'({data, "SETPROP" ++ Rest}, State) ->
        { Key, Value } = get_setprop_args(Rest),
        'SETPROP_RESP'(State, Key, Value);
'IECP_REQ'({data, "PUSH" ++ _Rest}, #state{obj_id=undefined} = State) ->
        send_err(State),
        {next_state, 'IECP_REQ', State, ?CFG(fsm_timeout)};
'IECP_REQ'({data, "PUSH" ++ Rest}, #state{socket=Socket} = State) ->
        { RecvSize } = get_req_int_arg(Rest),
        inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
        {next_state, 'PUSH_DATA_RECV', State#state{recv_size=RecvSize}, ?CFG(fsm_timeout)};
'IECP_REQ'({data, "END" ++ _Rest}, #state{obj_id=undefined} = State) ->
        send_err(State),
        {next_state, 'IECP_REQ', State, ?CFG(fsm_timeout)};
'IECP_REQ'({data, "END" ++ _Rest}, State) -> 
	'END_RESP'(State);
'IECP_REQ'({data, "HELP" ++ _Rest}, State) -> 
	'HELP_RESP'(State);
'IECP_REQ'({data, _Else}, State) -> 
	'HELP_RESP'(State);
'IECP_REQ'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'PUSH_RESP'(State, ObjId, ObjData) ->
        ok = mydlp_container:push(ObjId, ObjData),
        send_ok(State),
        {next_state, 'IECP_REQ', State, ?CFG(fsm_timeout)}.

'TOKEN_RESP'(#state{socket=Socket} = State, Token) ->
	case catch mydlp_api:is_valid_token(Token) of
		true -> send_ok(State),
			{ok, ObjId} = mydlp_container:new(),
			SrcAddr = case inet:peername(Socket) of
				{ok, {{I1,I2,I3,I4}, _Port}} -> io_lib:format("~B.~B.~B.~B", [I1,I2,I3,I4]);
				_Else -> "" end,
			ok = mydlp_container:setprop(ObjId, "srcAddr", SrcAddr),
			{next_state, 'IECP_REQ', State#state{obj_id=ObjId}, ?CFG(fsm_timeout)};
		false -> send_err(State),
			?ERROR_LOG("IECP: Token control has failed. Closing connection.", []),
			{stop, normal, State};
		Else -> send_err(State),
			?ERROR_LOG("IECP: Error occurred at token control. Closing connection. Err: "?S , [Else]),
			{stop, normal, State} end.

'SETPROP_RESP'(#state{obj_id=ObjId} = State, Key, Value) ->
	ok = mydlp_container:setprop(ObjId, Key, Value),
	send_ok(State),
	{next_state, 'IECP_REQ', State, ?CFG(fsm_timeout)}.

'END_RESP'(#state{obj_id=ObjId} = State) ->
	ok = mydlp_container:eof(ObjId),
	ok = mydlp_scheduler:s(ObjId),
	send_ok(State),
	{stop, normal, State}.

'HELP_RESP'(State) ->
	Print = "\r\n" ++ "Commands:" ++ "\r\n" ++
		"\t" ++ "TOKEN Token-> OK" ++ "\r\n" ++
		"\t" ++ "PUSH ChunkSize" ++ "\r\n" ++
		"\t\t" ++ "Chunk -> OK" ++ "\r\n" ++
		"\t" ++ "END -> OK" ++ "\r\n" ++
		"\t" ++ "HELP -> This screen" ++ "\r\n" ++
		"Any other command prints this screen." ++ "\r\n" ++
		"If an internal error occurs, server respond with ERR instead of OK." ++ "\r\n",
	send_ok(State, Print),
	{next_state, 'IECP_REQ', State, ?CFG(fsm_timeout)}.

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
			'PUSH_RESP'(State#state{recv_size=undefined, recv_data=[]}, ObjId, ObjData);
		NewSize when NewSize > 0 ->
			RecvData1 = [Data|RecvData],
			{next_state, 'PUSH_DATA_RECV', State#state{recv_size=NewSize, recv_data=RecvData1}, ?CFG(fsm_timeout)};
		_Else -> throw({error, {unexpected_binary_size, DataSize}}) end;
'PUSH_DATA_RECV'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
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
	% ?ERROR_LOG(?S" Client "?S" disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
	% ?ERROR_LOG("IECP: Unexpected message: "?S"~nStateName: "?S", StateData: "?S, [Info, StateName, StateData]),
	{next_state, StateName, StateData}.

fsm_call(StateName, Args, StateData) -> 
	try ?MODULE:StateName(Args, StateData)
	catch Class:Error ->
		?ERROR_LOG("Error occured on FSM ("?S") call ("?S"). Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[?MODULE, StateName, Class, Error, erlang:get_stacktrace()]),
		send_err(StateData),
		{next_state, 'IECP_REQ', StateData, ?CFG(fsm_timeout)} end.

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

get_req_int_arg(Rest) ->
	Rest1 = mydlp_api:rm_trailing_crlf(Rest),
	case string:tokens(Rest1, " ") of
		[I] -> { list_to_integer(I) };
		_Else -> throw({error, {not_expected, Rest}}) end.

get_req_str_arg(Rest) ->
	Rest1 = mydlp_api:rm_trailing_crlf(Rest),
	case string:tokens(Rest1, " ") of
		[S] -> { S };
		_Else -> throw({error, {not_expected, Rest}}) end.

get_setprop_args(Rest) ->
        Rest1 = mydlp_api:rm_trailing_crlf(Rest),
        Rest2 = string:strip(Rest1),
        {Key, Value} = case string:chr(Rest2, $=) of
                0 -> throw({no_equal_sign_to_tokenize, Rest2});
                I -> KS = string:sub_string(Rest2, 1, I - 1),
                        VS = string:sub_string(Rest2, I + 1),
                        {KS, VS} end,
        {string:strip(Key), string:strip(Value)}.

send(#state{socket=Socket}, Data) -> gen_tcp:send(Socket, <<Data/binary, "\r\n">>).

send_err(State) -> send(State, ?BIN_ERR).

send_ok(State) -> send(State, ?BIN_OK).

send_ok(State, Arg) when is_binary(Arg) -> send(State, <<?BIN_OK/binary, " ", Arg/binary>>);
send_ok(State, Arg) when is_integer(Arg) -> send_ok(State, integer_to_list(Arg));
send_ok(State, Arg) when is_atom(Arg) -> send_ok(State, atom_to_list(Arg));
send_ok(State, Arg) when is_list(Arg)-> send_ok(State, list_to_binary(Arg)).

