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

-module(mydlp_http_fsm).

-author('kerem@medra.com.tr').

-behaviour(gen_fsm).

-export([behaviour_info/1]).

-export([start_link/0,
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
	'WAIT_FOR_SOCKET'/2,
	'HTTP_PACKET'/2,
	'HTTP_HEADER'/2,
	'HTTP_CONTENT'/2
]).

-record(state, {
		socket,	% client socket
		peer_sock,	% remote socket
		comm_type, % whether socket uses ssl
		addr,	   % client address
		http_packet,
		http_headers=[],
		http_content=[],
		tmp
	}).

-record(http_request, {
		method,
		path,
		version
	}).

-record(http_headers, {
		connection,
		host,
		cookie = [],
		keep_alive,
		content_length,
		content_type,
		content_encoding,
		transfer_encoding,
		other = []   %% misc other headers
	}).

-record(http_header, {
		num,
		key,
		reserved,
		value
	}).

-define(TIMEOUT, 120000).

behaviour_info(callbacks) ->
	[{init, 0}];

behaviour_info(_Other) ->
	undefined.

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%	  If init/1 fails with Reason, the function returns {error,Reason}.
%%	  If init/1 returns {stop,Reason} or ignore, the process is
%%	  terminated and the function returns {error,Reason} or ignore,
%%	  respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

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
init([]) ->
	process_flag(trap_exit, true),
	{ok, 'WAIT_FOR_SOCKET', #state{http_headers=#http_headers{}}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket, CommType}, State) ->
	% Now we own the socket
	BackendOpts = backend_opts(CommType),
	BackendOpts:setopts(Socket, [{active, once}, {packet, http}]),
	{ok, {IP, _Port}} = BackendOpts:peername(Socket),
	{next_state, 'HTTP_PACKET', 
			State#state{socket=Socket, comm_type=CommType, addr=IP}, 
		?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
	error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, 'WAIT_FOR_SOCKET', State}.

'HTTP_PACKET'({http, HttpReq}, State) when is_record(HttpReq, http_request) ->
	{next_state, 'HTTP_HEADER', State#state{http_packet=HttpReq}, ?TIMEOUT};

'HTTP_PACKET'(timeout, State) ->
	error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_HEADER'({http, http_eoh}, #state{http_headers=HttpHeaders} = State) ->
	Cookies = HttpHeaders#http_headers.cookie,
	Others = HttpHeaders#http_headers.other,
	HttpHeaders1 = HttpHeaders#http_headers{cookie=lists:reverse(Cookies), other=lists:reverse(Others)},
	State1 = State#state{http_headers=HttpHeaders1},

	case has_body(State) of
		true -> get_http_content(State1);
		false -> 'CONNECT_REMOTE'(connect, State1)
	end;

'HTTP_HEADER'({http, HttpHeader}, #state{http_headers=HttpHeaders} = State) 
		when is_record(HttpHeader, http_header)->
	{http_header, _, Key, _, Value} = HttpHeader,
	HttpHeaders1 = case Key of
		'Connection' -> HttpHeaders#http_headers{connection=Value};
		'Host' -> HttpHeaders#http_headers{host=Value};
		'Cookie' -> Cookies = HttpHeaders#http_headers.cookie, HttpHeaders#http_headers{cookie=[Value|Cookies]};
		'Keep-Alive' -> HttpHeaders#http_headers{keep_alive=Value};
		'Content-Length' -> HttpHeaders#http_headers{content_length=Value};
		'Content-Type' -> HttpHeaders#http_headers{content_type=Value};
		'Content-Encoding' -> HttpHeaders#http_headers{content_encoding=Value};
		'Transfer-Encoding' -> HttpHeaders#http_headers{transfer_encoding=Value};
		_ -> Others = HttpHeaders#http_headers.other, HttpHeaders#http_headers{other=[HttpHeader|Others]}   %% misc other headers
	end,
		
	{next_state, 'HTTP_HEADER', State#state{http_headers=HttpHeaders1}, ?TIMEOUT};

'HTTP_HEADER'(timeout, State) ->
	error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

get_http_content(#state{socket=Socket, http_headers=HttpHeaders} = State) ->
	%%% TODO: partial post size limit
	BackendOpts = backend_opts(State),
	case HttpHeaders#http_headers.content_length of
		undefined ->
			case HttpHeaders#http_headers.transfer_encoding of
				"chunked" -> implement_me; % get_chunked_client_data
				_ -> 'CONNECT_REMOTE'(connect, State#state{http_content = <<>>})
			end;
		Len ->
			LenI = list_to_integer(Len),
			case LenI of
				0 -> 'CONNECT_REMOTE'(connect, State#state{http_content = <<>>});
				_ -> BackendOpts:setopts(Socket, [{packet, 0}, binary, {active, once}]),
					{next_state, 'HTTP_CONTENT', State#state{tmp=LenI}, ?TIMEOUT}
			end
		end.

'HTTP_CONTENT'({data, Data}, #state{http_content=Content, tmp=Count} = State) ->
	Count1 = Count - size(Data),
	Content1 = [Data|Content],

	case Count1 > 0 of
		true -> {next_state, 'HTTP_CONTENT', 
				State#state{http_content=Content1, tmp=Count1}, ?TIMEOUT};
		false -> 'CONNECT_REMOTE'(connect, 
				State#state{http_content=lists:reverse(Content1), tmp=undefined})
	end.

'CONNECT_REMOTE'(connect, #state{socket=Socket, http_headers=HttpHeaders} = State) ->
	BackendOpts = backend_opts(State),
	BackendOpts:setopts(Socket, [{active, false}]),

	{Host, Port} = case string:tokens(HttpHeaders#http_headers.host, ":") of
		[Host1, Port1] -> {Host1, Port1};
		[Host1] -> case State#state.comm_type of
				plain -> {Host1, 80};
				ssl -> {Host1, 443}
			end
		end,

	Backend = backend(State),
	{ok, PeerSock} = Backend:connect(Host, Port, [{active, false}, {packet, http}]),
	'SEND_REMOTE'(send_req, State#state{peer_sock=PeerSock}).

'SEND_REMOTE'(send_req, #state{socket=Socket,
				peer_sock=PeerSock
				} = State) ->
	send_req(State),

	BackendOpts = backend_opts(State),
	BackendOpts:setopts(PeerSock, [{packet, 0}, {active, once}]),
	BackendOpts:setopts(Socket, [{packet, http}, {active, once}, list]),

	{next_state, 'HTTP_PACKET', 
		State#state{http_packet=undefined, 
				http_headers=#http_headers{},
				http_content=[]}, ?TIMEOUT}.


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
handle_info({http, Socket, HttpData}, StateName, 
		#state{socket=Socket, comm_type=plain} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({http, HttpData}, StateData);

handle_info({ssl, Socket, HttpData}, StateName, 
		#state{socket=Socket, comm_type=ssl} = StateData) 
		when is_record(HttpData, http_request) ->
	% Flow control: enable forwarding of next TCP message
	ssl:setopts(Socket, [{packet, httph}, {active, once}]),
	?MODULE:StateName({http, HttpData}, StateData);

handle_info({ssl, Socket, HttpData}, StateName, 
		#state{socket=Socket, comm_type=ssl} = StateData) 
		when is_record(HttpData, http_header) ; HttpData == http_eoh ->
	% Flow control: enable forwarding of next TCP message
	ssl:setopts(Socket, [{active, once}]),
	?MODULE:StateName({http, HttpData}, StateData);

handle_info({_, Socket, http_error}, _StateName,
			#state{socket=Socket, addr=Addr} = StateData) ->
	error_logger:info_msg("~p HTTP error client ip: ~p .\n", [self(), Addr]),
	{stop, normal, StateData};


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
	error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info({ssl_closed, _}, _StateName, #state{comm_type=ssl, addr=Addr} = StateData) ->
	error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
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
		undefined -> true;
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

has_body(State) ->
	HttpReq = State#state.http_packet,
	case HttpReq#http_request.method of
		'POST' -> true;
		'PUT' -> true;
		'PROPFIND' -> true;
		_ -> false
	end.

send_header(_Backend, _PeerSock, _Key, undefined) -> ignored;
send_header(Backend, PeerSock, Key, Value) ->
	Backend:send(PeerSock, [Key, ": ", Value, "\r\n"]).

send_headers(Backend, PeerSock, HttpHeaders) 
		when is_record(HttpHeaders, http_headers) ->
	send_header(Backend, PeerSock, "Connection", HttpHeaders#http_headers.connection),
	send_header(Backend, PeerSock, "Host", HttpHeaders#http_headers.host),
	send_cookies(Backend, PeerSock, HttpHeaders#http_headers.cookie),
	send_header(Backend, PeerSock, "Keep-Alive", HttpHeaders#http_headers.keep_alive),
	send_header(Backend, PeerSock, "Content-Length", HttpHeaders#http_headers.content_length),
	send_header(Backend, PeerSock, "Content-Type", HttpHeaders#http_headers.content_type),
	send_header(Backend, PeerSock, "Content-Encoding", HttpHeaders#http_headers.content_encoding),
	send_header(Backend, PeerSock, "Transfer-Encoding", HttpHeaders#http_headers.transfer_encoding),
	send_headers(Backend, PeerSock, HttpHeaders#http_headers.other),
	Backend:send(PeerSock, "\r\n");
	
send_headers(_Backend, _PeerSock, []) -> ok;
send_headers(Backend, PeerSock, [HttpHeader|HttpHeaders]) ->
	% {http_header,14,'Host',undefined,"www.mydlp.org"}
	{http_header, _, Key, _, Value} = HttpHeader,
	KeyS = case is_atom(Key) of
		true -> atom_to_list(Key);
		false -> Key
	end,
	send_header(Backend, PeerSock, KeyS, Value),
	send_headers(Backend, PeerSock, HttpHeaders).

send_cookies(_Backend, _PeerSock, []) -> ok;
send_cookies(Backend, PeerSock, [Cookie|Cookies]) ->
	send_header(Backend, PeerSock, "Cookie", Cookie),
	send_cookies(Backend, PeerSock, Cookies).

send_req(#state{peer_sock=PeerSock, 
		http_packet=HttpReq,
		http_headers=HttpHeaders
		} = State) ->
	% {http_request,'GET',{abs_path,"/"},{1,1}}
	{http_request, Method, {_, Uri}, {Majorv, Minorv}} = HttpReq,

	Backend = backend(State),
	Backend:send(PeerSock, 
		[atom_to_list(Method), " ", Uri, " HTTP/", 
			integer_to_list(Majorv), ".", 
			integer_to_list(Minorv), "\r\n"]),
	send_headers(Backend, PeerSock, HttpHeaders),
	case has_body(State) of
		true -> Backend:send(PeerSock, State#state.http_content);
		false -> ok
	end,
	ok.
