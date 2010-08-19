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

-module(mydlp_smtp_fsm).
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
    'ICAP_REQ_LINE'/2
    'WAIT_FOR_DATA'/2,
]).

-record(state, [
	socket,
	addr,
	icap_request,
	icap_headers,
	icap_body=[],
	icap_rencap,
	curr_http_packet,
	http_packets=[],
	tmp
]).

-record(icap_request,[
	method,
	uri,
	major_version,
	minor_version
]).

-record(icap_headers,[
	allow,
	connection,
	encapsulated,
	x_client_ip,
	others=[]
]).

-record(http_packet, [
	http_request,
	http_headers,
	http_content=[]
]).


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
	{next_state, 'ICAP_REQ_LINE', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
	?DEBUG("ICAP FSM: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, 'WAIT_FOR_SOCKET', State}.


%% Notification event coming from client
'ICAP_REQ_LINE'({data, Line}, #state{socket=Socket} = State) ->
	[MethodS, Uri, VersionS] = string:tokens(Line, " "),

	Method = case http_util:to_lower(Methods) of
		"options" -> options;
		"reqmod" -> reqmod;
		"respmod" -> respmod;
		_Else -> throw({error, bad_method}) end,

	[$I, $C, $A, $P, $/, MajorVersionC, $., MinorVersionC, $\r, $\n ] = 
			http_util:to_lower(VersionS),
	MajorVersion = MajorVersionC - $0,
	MinorVersion = MinorVersionC - $0,

	inet:setopts(Socket, [{packet, httph}]),
	{next_state, 'ICAP_HEADER', 
		State#state{icap_request=#icap_request{
			method=Method, uri=Uri, major_version=MajorVersion, minor_version=MinorVersion},
			icap_headers=#icap_headers{}
		}, ?TIMEOUT};

'ICAP_REQ_LINE'(timeout, State) ->
	error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

get_body(#state{icap_rencap=[]} = State) -> 'REQ_OK'(State);
get_body(#state{icap_rencap=undefined} = State) -> 'REQ_OK'(State);
get_body(#state{icap_rencap=[{null_body, 0}]} = State) -> 'REQ_OK'(State);

get_body(#state{socket=Socket, icap_rencap=[{req_hdr, _BI}|Rest]} = State) -> 
	inet:setopts(Socket, [{packet, http}]),
	{next_state, 'HTTP_REQ_LINE', State#state{icap_rencap=Rest, 
			curr_http_packet=#http_packet{}}, ?TIMEOUT};

get_body(#state{socket=Socket, icap_rencap=[{req_body, _BI}|Rest]} = State) -> 
	inet:setopts(Socket, [{packet, line}, binary]),
	{next_state, 'HTTP_CC_LINE', State#state{icap_rencap=Rest, 
			curr_http_packet=#http_packet{}}, ?TIMEOUT};

'HTTP_REQ_LINE'({http, HttpReq}, #state{curr_http_packet=CurrHP} = State) when is_record(HttpReq, http_request) ->
        {next_state, 'HTTP_HEADER', State#state{curr_http_packet=CurrHP#http_packet{http_request=HttpReq}}, ?TIMEOUT};

'HTTP_REQ_LINE'(timeout, State) ->
        ?DEBUG("~p Client connection timeout - closing.\n", [self()]),
        {stop, normal, State}.

'HTTP_HEADER'({http, http_eoh}, #state{curr_http_packet=(#http_packet{http_headers=HttpHeaders} = HttpPack), 
			http_packets=HttpPackets} = State) ->
	Cookies = HttpHeaders#http_headers.cookie,
	Others = HttpHeaders#http_headers.other,
	HttpHeaders1 = HttpHeaders#http_headers{cookie=lists:reverse(Cookies), other=lists:reverse(Others)},
	State1 = State#state{curr_http_packet=undefined, 
			http_packets=[HttpPack#http_packet{http_headers=HttpHeaders1}|HttpPackets]},
	get_body(State1);

'HTTP_HEADER'({http, HttpHeader}, #state{curr_http_packet=(#http_packet{http_headers=HttpHeaders}=HttpPack)} = State) 
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
		
	{next_state, 'HTTP_HEADER', State#state{curr_http_packet=HttpPack#http_packet{http_headers=HttpHeaders1}}, ?TIMEOUT};

'HTTP_HEADER'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_CC_LINE'({data, Line}, #state{curr_http_packet=#http_packet{http_content=Content1}, http_packets=[HP|HPS]} = State) ->
	CSize = mydlp_api:hex2int(Line),
	%Content1 = [Line|Content],
	case CSize of
		0 -> get_body(State#state{curr_http_packet=undefined, 
				http_packets=[HP#http_packet{http_content=lists:reverse(Content1)}|HPS]});
		_ -> {next_state, 'HTTP_CC_CHUNK', State#state{tmp=CSize}, ?TIMEOUT}
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



'ICAP_HEADER'({http, http_eoh}, #state{icap_headers=IcapHeaders, socket=Socket} = State) ->
        Others = IcapHeaders#icap_headers.other,
        IcapHeaders1 = IcapHeaders#icap_headers{cookie=lists:reverse(Cookies), other=lists:reverse(Others)},
        State1 = State#state{icap_headers=IcapHeaders1},

	HH = State1#state.icap_headers,
	E = HH#icap_headers.encapsulated,
	get_body(State1#state{icap_rencap=E});

'ICAP_HEADER'({http, IcapHeader}, #state{icap_headers=IcapHeaders} = State)
                when is_record(IcapHeader, http_header)->
        {http_header, _, Key, _, Value} = IcapHeader,
        IcapHeaders1 = case Key of
                'Connection' -> IcapHeaders#icap_headers{connection=Value};
                "Encapsulated" -> IcapHeaders#icap_headers{encapsulated=raw_to_encapsulatedh(Value)};
                "X-Client-IP" -> IcapHeaders#icap_headers{x_client_ip=raw_to_xciph(Value)};
                'Allow' -> IcapHeaders#icap_headers{allow=raw_to_allowh(Value)};
                _ -> Others = IcapHeaders#icap_headers.other, IcapHeaders#icap_headers{other=[IcapHeader|Others]}   %% misc other headers
        end,

        {next_state, 'ICAP_HEADER', State#state{http_headers=HttpHeaders1}, ?TIMEOUT};

'ICAP_HEADER'(timeout, State) ->
	error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{buff = Buff} = State) ->
	NewBuff = <<Buff/binary,Data/binary>>,
	case end_of_data(NewBuff) of
		0 -> {next_state, 'WAIT_FOR_DATA', State#state{buff = NewBuff}, ?TIMEOUT};
		Pos -> % wait for end of data and return data in state.
			<<Message:Pos/binary,13,10,46,13,10,NextBuff/binary>> = NewBuff,
			NewState = smtpd_cmd:read_message(Message,State),
			smtpd_cmd:send(NewState,250),
			'READ_FILES'(NewState#state{buff=NextBuff})
	end;

'WAIT_FOR_DATA'(timeout, State) ->
%    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

'READ_FILES'(#state{message_mime=MIME} = State) ->
	Files = mime_to_files(MIME),
	'REQ_OK'(State#state{files=Files}).


% {Action, {{rule, Id}, {file, File}, {matcher, Func}, {misc, Misc}}}
'REQ_OK'(#state{files=Files} = State) ->
	case mydlp_acl:qu(user, dest, Files) of
		pass -> 'CONNECT_REMOTE'(connect, State);
		{block, AclR} -> log_req(State, block, AclR), 'BLOCK_REQ'(block, State);
		{log, AclR} -> log_req(State, log, AclR), 'CONNECT_REMOTE'(connect, State);
		{pass, AclR} -> log_req(State, pass, AclR), 'CONNECT_REMOTE'(connect, State)
	end.

% refined this
'BLOCK_REQ'(block, #state{message_record=MessageR} = State) ->
	MailFrom = MessageR#message.mail_from,
	RepMessage = #message{mail_from=MailFrom, 
			rcpt_to=MailFrom,
			message=
				"From: <" ++ MailFrom ++ ">\r\n" ++
				"To: <" ++ MailFrom ++ ">\r\n" ++
				"Subject: Access denied!!!\r\n" ++
				"\r\n" ++
				"\r\n" ++
				"Your e-mail to \'" ++ MessageR#message.rcpt_to ++ "\' have been denied by MyDLP. \n" ++
				"\n" ++
				"Please contact to your administrator for details.\n"},
	mydlp_smtpc:mail(RepMessage),
	NextState = State#state{cmd   = undefined,
			param = undefined,
			mail  = undefined,
			rcpt  = undefined,
			to    = undefined,
			messagename = undefined,
			message_record = undefined,
		        message_mime = undefined,
		        files       = [],
			data  = undefined},
	{next_state, 'WAIT_FOR_CMD', NextState, ?TIMEOUT}.

'CONNECT_REMOTE'(connect, #state{message_record=MessageR} = State) ->
	mydlp_smtpc:mail(MessageR),
	NextState = State#state{cmd   = undefined,
			param = undefined,
			mail  = undefined,
			rcpt  = undefined,
			to    = undefined,
			messagename = undefined,
			message_record = undefined,
		        message_mime = undefined,
		        files       = [],
			data  = undefined},
	{next_state, 'WAIT_FOR_CMD', NextState, ?TIMEOUT}.

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
handle_info({http, Socket, HttpData}, StateName, #state{socket=Socket} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({http, HttpData}, StateData);

handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket, addr=_Addr} = StateData) ->
	%error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
	{noreply, StateName, StateData}.

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

raw_to_xciph(IpStr) -> 
	Tokens = string:tokens(IpStr,"."),
	[S1,S2,S3,S4] = Tokens,
	Ints = lists:map(fun(S) -> list_to_integer(S) end, Tokens),
	case lists:any(fun(I) -> ( I > 255 ) or ( I < 0 ) end, Ints) of
		true -> throw({error, {bad_ip, Ints}});
		false -> ok end,
	[I1,I2,I3,I4] = Ints,
	{I1,I2,I3,I4}.

raw_to_allowh(AllowStr) ->
	string:tokens(AllowStr, ", ").

raw_to_encapsulatedh(EncapStr) ->
	Tokens = string:tokens(AllowStr, ", "),
	lists:map(fun(S) -> 
		case string:tokens(S, "=") of
			["req-hdr", Val] -> {req_hdr, list_to_integer(Val)};
			["res-hdr", Val] -> {res_hdr, list_to_integer(Val)};
			["req-body", Val] -> {req_body, list_to_integer(Val)};
			["res-body", Val] -> {res_body, list_to_integer(Val)};
			["opt-body", Val] -> {opt_body, list_to_integer(Val)};
			["null-body", Val] -> {null_body, list_to_integer(Val)};
			Else -> throw({error, {bad_encap_part, Else}})
	end, Tokens),
