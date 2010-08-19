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

-export([start_link/0]).

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
	'HTTP_CONTENT'/2,
	'HTTP_CC_LINE'/2,
	'HTTP_CC_CHUNK'/2,
	'HTTP_CC_CRLF'/2
]).

-include("mydlp.hrl").

-include("mydlp_http.hrl").

-record(state, {
		socket,	% client socket
		peer_sock,	% remote socket
		comm_type, % whether socket uses ssl
		addr,	   % client address
		http_packet,
		http_headers=[],
		http_content=[],
		files=[],
		tmp
	}).

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
	?DEBUG("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, 'WAIT_FOR_SOCKET', State}.

'HTTP_PACKET'({http, HttpReq}, State) when is_record(HttpReq, http_request) ->
	{next_state, 'HTTP_HEADER', State#state{http_packet=HttpReq}, ?TIMEOUT};

'HTTP_PACKET'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_HEADER'({http, http_eoh}, #state{http_headers=HttpHeaders} = State) ->
	Cookies = HttpHeaders#http_headers.cookie,
	Others = HttpHeaders#http_headers.other,
	HttpHeaders1 = HttpHeaders#http_headers{cookie=lists:reverse(Cookies), other=lists:reverse(Others)},
	State1 = State#state{http_headers=HttpHeaders1},

	case has_body(State) of
		true -> get_http_content(State1);
		false -> 'REQ_OK'(State1)
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
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

get_http_content(#state{socket=Socket, http_headers=HttpHeaders} = State) ->
	%%% TODO: partial post size limit
	BackendOpts = backend_opts(State),
	case HttpHeaders#http_headers.content_length of
		undefined ->
			case HttpHeaders#http_headers.transfer_encoding of
				"chunked" -> BackendOpts:setopts(Socket, [{packet, line}, binary, {active, once}]),
					{next_state, 'HTTP_CC_LINE', State, ?TIMEOUT};
				_ -> 'REQ_OK'(State#state{http_content = <<>>})
			end;
		Len ->
			LenI = list_to_integer(Len),
			case LenI of
				0 -> 'REQ_OK'(State#state{http_content = <<>>});
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
		false -> 'READ_FILES'(State#state{http_content=lists:reverse(Content1), tmp=undefined})
	end;

'HTTP_CONTENT'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_CC_LINE'({data, Line}, #state{http_content=Content1} = State) ->
	CSize = mydlp_api:hex2int(Line),
	%Content1 = [Line|Content],
	case CSize of
		0 -> 'REQ_OK'(State#state{http_content=lists:reverse(Content1)});
		_ -> {next_state, 'HTTP_CC_CHUNK', State#state{http_content=Content1, tmp=CSize}, ?TIMEOUT}
	end;

'HTTP_CC_LINE'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_CC_CHUNK'({data, Line}, #state{http_content=Content, tmp=CSize} = State) ->
	CSize1 = CSize - size(Line),
	Content1 = [Line|Content],
	if
		CSize1 > 0 -> {next_state, 'HTTP_CC_CHUNK', 
                                State#state{http_content=Content1, tmp=CSize1}, ?TIMEOUT};
		CSize1 == 0 -> {next_state, 'HTTP_CC_CRLF',
				State#state{http_content=Content1, tmp=undefined}, ?TIMEOUT};
		CSize1 == -2 -> {next_state, 'HTTP_CC_LINE',
				State#state{http_content=Content1, tmp=undefined}, ?TIMEOUT}
	end;

'HTTP_CC_CHUNK'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_CC_CRLF'({data, <<"\r\n">> = CRLF}, #state{http_content=Content} = State) ->
	Content1 = [CRLF|Content],
	{next_state, 'HTTP_CC_LINE', State#state{http_content=Content1}, ?TIMEOUT};

'HTTP_CC_CRLF'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

% {Action, {{rule, Id}, {file, File}, {matcher, Func}, {misc, Misc}}}
'REQ_OK'(#state{files=Files,http_content=HttpContent, addr=Addr} = State) ->
	case mydlp_acl:q(Addr, dest, df_to_files(list_to_binary(HttpContent), Files)) of
		pass -> 'CONNECT_REMOTE'(connect, State);
		{block, AclR} -> log_req(State, block, AclR), 'BLOCK_REQ'(block, State);
		{log, AclR} -> log_req(State, log, AclR), 'CONNECT_REMOTE'(connect, State); % refine this
		{pass, AclR} -> log_req(State, pass, AclR), 'CONNECT_REMOTE'(connect, State)
	end.

'READ_FILES'(#state{http_headers=HttpHeaders} = State) ->
%	when HttpHeaders#http_headers.content_type == ''->
	Files = case mydlp_api:starts_with(HttpHeaders#http_headers.content_type, "multipart/form-data") of
		true ->	parse_multipart(State);
		false -> []
	end,

	'REQ_OK'(State#state{files=Files}).

%%% imported from yaws (may be refactored for binary operation)
parse_multipart(#state{http_content=HttpContent, http_headers=H, http_packet=Req}) ->
	CT = H#http_headers.content_type,
	Res = case Req#http_request.method of
		'POST' ->
			case CT of
				undefined ->
					?DEBUG("Can't parse multipart if we "
						"have no Content-Type header",[]), [];
				"multipart/form-data"++Line ->
					LineArgs = parse_arg_line(Line),
					{value, {_, Boundary}} = lists:keysearch(boundary, 1, LineArgs),
					parse_multipart(binary_to_list(un_partial(list_to_binary(HttpContent))), Boundary);
				_Other ->
					?DEBUG("Can't parse multipart if we "
						"find no multipart/form-data",[]), []
			end;
		Other ->
			?DEBUG("Can't parse multipart if get a ~p", [Other]), []
	end,
	result_to_files(Res).

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
	{ok, PeerSock} = Backend:connect(Host, Port, [{active, false}]),
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
				http_content=[], 
				files=[]}, ?KA_TIMEOUT}.

'BLOCK_REQ'(block, #state{socket=Socket} = State) ->
	Backend = backend(State),
	Backend:send(Socket, gen_deny_page(State)),

	BackendOpts = backend_opts(State),
	BackendOpts:setopts(Socket, [{packet, http}, {active, once}, list]),

	{next_state, 'HTTP_PACKET', 
		State#state{http_packet=undefined, 
				http_headers=#http_headers{},
				http_content=[], 
				files=[]}, ?KA_TIMEOUT}.

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
handle_info({http, Socket, http_eoh}, StateName, 
		#state{socket=Socket, comm_type=plain} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	?MODULE:StateName({http, http_eoh}, StateData);

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
		when is_record(HttpData, http_header) ->
	% Flow control: enable forwarding of next TCP message
	ssl:setopts(Socket, [{active, once}]),
	?MODULE:StateName({http, HttpData}, StateData);

handle_info({ssl, Socket, http_eoh}, StateName, 
		#state{socket=Socket, comm_type=ssl} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	?MODULE:StateName({http, http_eoh}, StateData);

handle_info({_, Socket, {http_error, _}}, _StateName,
			#state{socket=Socket, addr=Addr} = StateData) ->
	?DEBUG("~p HTTP error client ip: ~p .\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info({_, Socket, http_error}, _StateName,
			#state{socket=Socket, addr=Addr} = StateData) ->
	?DEBUG("~p HTTP error client ip: ~p .\n", [self(), Addr]),
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

%%%%% multipart parsing
un_partial({partial, Bin}) ->
    Bin;
un_partial(Bin) ->
    Bin.

parse_arg_line(Line) ->
    parse_arg_line(Line, []).

parse_arg_line([],Acc) -> Acc;
parse_arg_line([$ |Line], Acc) ->
    parse_arg_line(Line, Acc);
parse_arg_line([$;|Line], Acc) ->
    {KV,Rest} = parse_arg_key(Line, [], []),
    parse_arg_line(Rest, [KV|Acc]).

%%

parse_arg_key([], Key, Value) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_key([$;|Line], Key, Value) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_key([$ |Line], Key, Value) ->
    parse_arg_key(Line, Key, Value);
parse_arg_key([$=|Line], Key, Value) ->
    parse_arg_value(Line, Key, Value, false, false);
parse_arg_key([C|Line], Key, Value) ->
    parse_arg_key(Line, [C|Key], Value).

%%
%% We need to deal with quotes and initial spaces here.
%% parse_arg_value(String, Key, ValueAcc, InQuoteBool, InValueBool)
%%

parse_arg_value([], Key, Value, _, _) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_value([$\\,$"|Line], Key, Value, Quote, Begun) ->
    parse_arg_value(Line, Key, [$"|Value], Quote, Begun);
parse_arg_value([$"|Line], Key, Value, false, _) ->
    parse_arg_value(Line, Key, Value, true, true);
parse_arg_value([$"], Key, Value, true, _) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_value([$",$;|Line], Key, Value, true, _) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_value([$;|Line], Key, Value, false, _) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_value([$ |Line], Key, Value, false, true) ->
    make_parse_line_reply(Key, Value, Line);
parse_arg_value([$ |Line], Key, Value, false, false) ->
    parse_arg_value(Line, Key, Value, false, false);
parse_arg_value([C|Line], Key, Value, Quote, _) ->
    parse_arg_value(Line, Key, [C|Value], Quote, true).


%%

make_parse_line_reply(Key, Value, Rest) ->
    X = {{list_to_atom(mydlp_api:funreverse(Key, {mydlp_api, to_lowerchar})),
          lists:reverse(Value)}, Rest},
    X.
%%

isolate_arg(Str) -> isolate_arg(Str, []).

isolate_arg([$:,$ |T], L) -> {mydlp_api:funreverse(L, {mydlp_api, to_lowerchar}), T};
isolate_arg([H|T], L)     -> isolate_arg(T, [H|L]).

%%
%%% Stateful parser of multipart data - allows easy re-entry
%% States are header|body|boundary|is_end

parse_multipart(Data, St) ->
    case parse_multi(Data, St) of
        {cont, St2, Res} ->
            {cont, {cont, St2}, lists:reverse(Res)};
        {result, Res} ->
            {result, lists:reverse(Res)}
    end.

%% Re-entry
parse_multi(Data, {cont, {boundary, Start_data, PartBoundary,
                          Acc, {Possible,Boundary}}}) ->
    parse_multi(boundary, Start_data++Data, PartBoundary, Acc, [],
                {Possible++Data,Boundary});
parse_multi(Data, {cont, {State, Start_data, Boundary, Acc, Tmp}}) ->
    parse_multi(State, Start_data++Data, Boundary, Acc, [], Tmp);

%% Initial entry point
parse_multi(Data, Boundary) ->
    B1 = "\r\n--"++Boundary,
    D1 = "\r\n"++Data,
    parse_multi(boundary, D1, B1, start, [], {D1, B1}).

parse_multi(header, "\r\n\r\n"++Body, Boundary, Acc, Res, Tmp) ->
    Header = do_header(lists:reverse(Acc)),
    parse_multi(body, Body, Boundary, [], [{head, Header}|Res], Tmp);
parse_multi(header, "\r\n"++Body, Boundary, [], Res, Tmp) ->
    Header = do_header([]),
    parse_multi(body, Body, Boundary, [], [{head, Header}|Res], Tmp);
parse_multi(header, "\r\n\r", Boundary, Acc, Res, Tmp) ->
    {cont, {header, "\r\n\r", Boundary, Acc, Tmp}, Res};
parse_multi(header, "\r\n", Boundary, Acc, Res, Tmp) ->
    {cont, {header, "\r\n", Boundary, Acc, Tmp}, Res};
parse_multi(header, "\r", Boundary, Acc, Res, Tmp) ->
    {cont, {header, "\r", Boundary, Acc, Tmp}, Res};
parse_multi(header, [H|T], Boundary, Acc, Res, Tmp) ->
    parse_multi(header, T, Boundary, [H|Acc], Res, Tmp);
parse_multi(header, [], Boundary, Acc, Res, Tmp) ->
    {cont, {header, [], Boundary, Acc, Tmp}, Res};

parse_multi(body, [B|T], [B|T1], Acc, Res, _Tmp) ->
    parse_multi(boundary, T, T1, Acc, Res, {[B|T], [B|T1]}); %% store in case no match
parse_multi(body, [H|T], Boundary, Acc, Res, Tmp) ->
    parse_multi(body, T, Boundary, [H|Acc], Res, Tmp);
parse_multi(body, [], Boundary, [], Res, Tmp) ->  %% would be empty partial body result
    {cont, {body, [], Boundary, [], Tmp}, Res};
parse_multi(body, [], Boundary, Acc, Res, Tmp) ->        %% make a partial body result
    {cont, {body, [], Boundary, [], Tmp}, [{part_body, lists:reverse(Acc)}|Res]};

parse_multi(boundary, [B|T], [B|T1], Acc, Res, Tmp) ->
    parse_multi(boundary, T, T1, Acc, Res, Tmp);
parse_multi(boundary, [_H|_T], [_B|_T1], start, Res, {[D|T2], Bound}) -> %% false alarm
    parse_multi(body, T2, Bound, [D], Res, []);
parse_multi(boundary, [_H|_T], [_B|_T1], Acc, Res, {[D|T2], Bound}) -> %% false alarm
    parse_multi(body, T2, Bound, [D|Acc], Res, []);
parse_multi(boundary, [], [B|T1], Acc, Res, Tmp) -> %% run out of body
    {cont, {boundary, [], [B|T1], Acc, Tmp}, Res};
parse_multi(boundary, [], [], start, Res, {_, Bound}) ->
    {cont, {is_end, [], Bound, [], []}, Res};
parse_multi(boundary, [], [], Acc, Res, {_, Bound}) ->
    {cont, {is_end, [], Bound, [], []}, [{body, lists:reverse(Acc)}|Res]};
parse_multi(boundary, [H|T], [], start, Res, {_, Bound}) -> %% matched whole boundary!
    parse_multi(is_end, [H|T], Bound, [], Res, []);
parse_multi(boundary, [H|T], [], Acc, Res, {_, Bound}) -> %% matched whole boundary!
    parse_multi(is_end, [H|T], Bound, [], [{body, lists:reverse(Acc)}|Res], []);

parse_multi(is_end, "--"++_, _Boundary, _Acc, Res, _Tmp) ->
    {result, Res};
parse_multi(is_end, "-", Boundary, Acc, Res, Tmp) ->
    {cont, {is_end, "-", Boundary, Acc, Tmp}, Res};
parse_multi(is_end, "\r\n"++Next, Boundary, _Acc, Res, _Tmp) ->
    parse_multi(header, Next, Boundary, [], Res, []);
parse_multi(is_end, "\r", Boundary, Acc, Res, Tmp) ->
    {cont, {is_end, "\r", Boundary, Acc, Tmp}, Res}.

do_header([]) -> {[]};
do_header(Head) ->
    Fields = string:tokens(Head, "\r\n"),
    MFields = merge_lines_822(Fields),
    Header = lists:map(fun isolate_arg/1, MFields),
    case lists:keysearch("content-disposition", 1, Header) of
        {value, {_,"form-data"++Line}} ->
            Parameters = parse_arg_line(Line),
            {value, {_,Name}} = lists:keysearch(name, 1, Parameters),
            {Name, Parameters};
        _ ->
            {Header}
    end.

merge_lines_822(Lines) ->
    merge_lines_822(Lines, []).

merge_lines_822([], Acc) ->
    lists:reverse(Acc);
merge_lines_822([Line=" "++_|Lines], []) ->
    merge_lines_822(Lines, [Line]);
merge_lines_822([Line=" "++_|Lines], [Prev|Acc]) ->
    merge_lines_822(Lines, [Prev++Line|Acc]);
merge_lines_822(["\t"++Line|Lines], [Prev|Acc]) ->
    merge_lines_822(Lines, [Prev++[$ |Line]|Acc]);
merge_lines_822([Line|Lines], Acc) ->
    merge_lines_822(Lines, [Line|Acc]).

%%%% convert yaws multipart result to file records
result_to_files({result, Rest}) ->
	result_to_files(Rest, [], undefined).
result_to_files([], Files, File) ->
	lists:reverse([File|Files]);
result_to_files([{head,Head}|Rest], Files, File) ->
	Files1 = case File of
		undefined -> Files;
		Else -> [Else|Files]
	end,
	{_, Heads} = Head,
	result_to_files(Rest, Files1, heads_to_file(Heads));
result_to_files([{body,Data}|Rest], Files, File) ->
	result_to_files(Rest, Files, File#file{data=list_to_binary(Data)}).

heads_to_file(Heads) ->
	heads_to_file(Heads, #file{}).
heads_to_file([{filename,FileName}|Heads], File) ->
	heads_to_file(Heads, File#file{filename=FileName});
heads_to_file([{name,PostName}|Heads], File) ->
	heads_to_file(Heads, File#file{name=PostName});
heads_to_file([_|Heads], File) ->
	ignore,
	heads_to_file(Heads, File);
heads_to_file([], File) ->
	File.

gen_deny_page(#state{http_packet=HttpReq}) -> 
	{http_request, _, {_, _}, {Majorv, Minorv}} = HttpReq,
	Body = <<
	"<html>",
	"<head><title>Blocked by MyDLP</title></head>",
	"<body><center><strong>DENIED !!!</strong></center></body>",
	"</html>"
	>>,
	[
	"HTTP/", integer_to_list(Majorv), ".", integer_to_list(Minorv), " 403 Forbidden\r\n",
	"Connection : keep-alive\r\n"
	"Content-Type: text/html; charset=UTF-8\r\n",
	"Content-Length: ", integer_to_list(size(Body)), "\r\n",
	"\r\n",
	Body
	].

df_to_files(Data, Files) ->
        case length(Files) of
                0 ->    DFile = #file{name= "post-data", data=Data},
                        [DFile];
                _ ->    Files
        end.

log_req(#state{comm_type=plain} = State, Action, AclR) -> log_req1(http, State, Action, AclR);
log_req(#state{comm_type=ssl} = State, Action, AclR) -> log_req1(https, State, Action, AclR).

log_req1(Proto, #state{addr=Addr, http_headers=(#http_headers{host=DestHost})}, Action, 
		{{rule, RuleId}, {file, File}, {matcher, Matcher}, {misc, Misc}}) ->
	?ACL_LOG(Proto, RuleId, Action, Addr, nil, DestHost, Matcher, File, Misc).

