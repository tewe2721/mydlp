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

-module(mydlp_fsm).

-author('kerem@medra.com.tr').

-behaviour(gen_fsm).

-export([start_link/1,
	set_socket/3]).

%% gen_fsm callbacks
-export([init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4]).

%% FSM States
-export([
	'WF_SOCKET'/2,
	'WF_DATA'/2
]).

-include("mydlp.hrl").

-record(state, {
		module, % name of FSM module
		module_state, % state obj for FSM module
		module_fsm_state, % FSM state of FSM module
		socket, % client socket
		peer_sock,      % remote socket
		comm_type, % whether socket uses ssl
		addr,      % client address
		in_buffer=[]
	}).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec (TargetModule) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%	  If init/1 fails with Reason, the function returns {error,Reason}.
%%	  If init/1 returns {stop,Reason} or ignore, the process is
%%	  terminated and the function returns {error,Reason} or ignore,
%%	  respectively.
%% @end
%%-------------------------------------------------------------------------
start_link(TargetModule) ->
	gen_fsm:start_link(?MODULE, [TargetModule], []).

set_socket(Pid, Socket, CommType) when is_pid(Pid) ->
	gen_fsm:send_event(Pid, {socket_ready, Socket, CommType}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}		  |
%%		  {ok, StateName, StateData, Timeout} |
%%		  ignore							  |
%%		  {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([TargetModule]) ->
	process_flag(trap_exit, true),
	{ok, FirstState, StateObj} = TargetModule:init(),
	{ok, 'WF_SOCKET', #state{
			module=TargetModule,
			module_fsm_state=FirstState,
			module_state=StateObj}
		}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WF_SOCKET'({socket_ready, Socket, CommType}, State) ->
	% Now we own the socket
	BackendOpts = backend_opts(CommType),
	BackendOpts:setopts(Socket, [{active, once}, {packet, 0}, binary]),
	{ok, {IP, _Port}} = BackendOpts:peername(Socket),
	{next_state, 'WF_DATA', 
			State#state{socket=Socket, comm_type=CommType, addr=IP}, 
		?TIMEOUT};

'WF_SOCKET'(Other, State) ->
	?DEBUG("State: 'WF_SOCKET'. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, 'WF_SOCKET', State}.

'WF_DATA'({data, Data}, #state{in_buffer=Buffer} = State) ->
	consume({data, Data}, State#state{in_buffer = [Data|Buffer]});

'WF_DATA'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State};

'WF_DATA'(Data, State) ->
	?DEBUG("~p Ignoring data: ~p\n", [self(), Data]),
	{next_state, 'WF_DATA', State, ?TIMEOUT}.

consume({data, Data}, #state{module=Module, module_fsm_state=ModuleFSMState, module_state=ModuleState} = State) ->
	ModuleReply = Module:ModuleFSMState({data, Data}, ModuleState),

	case ModuleReply of
		{next_state, ModuleFSMState1, ModuleState1, Return} ->
			State1 = State#state{module_state=ModuleState1, module_fsm_state=ModuleFSMState1},
			case Return of
				{req_ok, {files, Files}, {peer, {Host, Port}}} ->
					%%%% here should send buff to remote and reset buff
					'REQ_OK'(State1, Files, {Host, Port});
				continue ->
					{next_state, 'WF_DATA', State1, ?TIMEOUT}
                        end;
		{next_state, ModuleFSMState1, ModuleState1} ->
			{next_state, 'WF_DATA', 
				State#state{module_state=ModuleState1, module_fsm_state=ModuleFSMState1},
				?TIMEOUT};
                {error, Error} ->
                        {stop, {error, Error}, State}
        end.

'REQ_OK'(#state{addr=Addr} = State, Files, Peer) ->
	case mydlp_acl:q(Addr, dest, Files) of
		pass -> 'CONNECT_REMOTE'(connect, State, Peer);
		{quarantine, AclR} -> log_req(State, Peer, block, AclR), 'BLOCK_REQ'(block, State);
		{block, AclR} -> log_req(State, Peer, block, AclR), 'BLOCK_REQ'(block, State);
		{log, AclR} -> log_req(State, Peer, log, AclR), 'CONNECT_REMOTE'(connect, State, Peer); % refine this
		{pass, AclR} -> log_req(State, Peer, pass, AclR), 'CONNECT_REMOTE'(connect, State, Peer)
	end.


'CONNECT_REMOTE'(connect, #state{socket=Socket} = State, {Host, Port}) ->
	BackendOpts = backend_opts(State),
	BackendOpts:setopts(Socket, [{active, false}]),
	Backend = backend(State),
	{ok, PeerSock} = Backend:connect(Host, Port, [{active, false}]),
	'SEND_REMOTE'(send_req, State#state{peer_sock=PeerSock}).

'SEND_REMOTE'(send_req, #state{socket=Socket,
				peer_sock=PeerSock,
				in_buffer=Buffer
				} = State) ->
	Backend = backend(State),
	Backend:send(PeerSock, lists:reverse(Buffer)),

	BackendOpts = backend_opts(State),
	BackendOpts:setopts(PeerSock, [{packet, 0}, {active, once}]),
	BackendOpts:setopts(Socket, [{packet, 0}, {active, once}, binary]),

	{next_state, 'WF_DATA', State#state{in_buffer=[]}, ?TIMEOUT}.

'BLOCK_REQ'(block, #state{socket=Socket} = State) ->
	Backend = backend(State),
	Backend:send(Socket, gen_deny_page(State)),

	BackendOpts = backend_opts(State),
	BackendOpts:setopts(Socket, [{packet, 0}, {active, once}, binary]),

	{next_state, 'WF_DATA', State#state{in_buffer=[]}, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}			|
%%		  {next_state, NextStateName, NextStateData, Timeout}   |
%%		  {reply, Reply, NextStateName, NextStateData}		  |
%%		  {reply, Reply, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NewStateData}						  |
%%		  {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
%%%% TODO: this setopts active once s should be moved to states.
handle_info({tcp, Socket, Data}, StateName, 
		#state{socket=Socket, comm_type=plain} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Data}, StateData);

handle_info({ssl, Socket, Data}, StateName, 
		#state{socket=Socket, comm_type=ssl} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	ssl:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Data}, StateData);

%%% any data coming from peer sock should be immediatly passed to local socket.
handle_info({tcp, PeerSock, Data}, StateName, 
		#state{socket=Socket, peer_sock=PeerSock, comm_type=plain} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	gen_tcp:send(Socket, Data),
	inet:setopts(PeerSock, [{active, once}]),
	{next_state, StateName, StateData, ?TIMEOUT};

handle_info({ssl, PeerSock, Data}, StateName, 
		#state{socket=Socket, peer_sock=PeerSock, comm_type=ssl} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	ssl:send(Socket, Data),
	ssl:setopts(PeerSock, [{active, once}]),
	{next_state, StateName, StateData, ?TIMEOUT};

handle_info({tcp_closed, _}, _StateName, #state{comm_type=plain, addr=Addr} = StateData) ->
	?DEBUG("~p Client ~p disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info({ssl_closed, _}, _StateName, #state{comm_type=ssl, addr=Addr} = StateData) ->
	?DEBUG("~p Client ~p disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
	{noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, State) ->
	Backend = backend(State),
	(catch Backend:close(State#state.socket)),
	case State#state.peer_sock of
		undefined -> ok;
		PeerSock -> (catch Backend:close(PeerSock))
	end,
	ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%% internal

backend(State) when is_record(State, state) -> 
	case State#state.comm_type of
			plain -> gen_tcp;
			ssl -> ssl
	end;
backend(plain) -> gen_tcp;
backend(ssl) -> ssl.

backend_opts(State) when is_record(State, state) -> 
	case State#state.comm_type of
			plain -> inet;
			ssl -> ssl
	end;
backend_opts(plain) -> inet;
backend_opts(ssl) -> ssl.

gen_deny_page(#state{module=Module}) -> apply(Module, deny_page, []).

get_proto_name(#state{module=Module, comm_type=CommType}) ->
	apply(Module, proto_name, [CommType]).

log_req(#state{addr=Addr} = State, {DestHost, _Port}, Action,
		{{rule, RuleId}, {file, File}, {matcher, Matcher}, {misc, Misc}}) ->
	?ACL_LOG(get_proto_name(State), RuleId, Action, Addr, nil, DestHost, Matcher, File, Misc).

