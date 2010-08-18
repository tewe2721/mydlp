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

-module(mydlp_icap_fsm).
-author('kerem@medratech.com').
-behaviour(gen_fsm).
-include("mydlp.hrl").
-include("mydlp_http.hrl").

-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'ICAP_REQ_LINE'/2,
    'ICAP_HEADER'/2,
    'HTTP_REQ_LINE'/2,
    'HTTP_HEADER'/2,
    'HTTP_CC_LINE'/2,
    'HTTP_CC_CHUNK'/2,
    'HTTP_CC_CRLF'/2,
    'HTTP_CC_TCRLF'/2
]).

-record(state, {
	socket,
	addr,
	icap_request,
	icap_headers,
	icap_body=[],
	icap_rencap,
	http_request,
	http_headers,
	http_content=[],
	files=[],
	max_connections,
	options_ttl,
	path,
	tmp
}).

-record(icap_request, {
	method,
	uri,
	major_version,
	minor_version
}).

-record(icap_headers, {
	allow,
	connection,
	encapsulated,
	x_client_ip,
	other=[]
}).

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

	ConfList = case application:get_env(icap) of
		{ok, CL} -> CL;
		_Else -> ?ICAP
	end,

	{path, P} = lists:keyfind(path, 1, ConfList),
	{max_connections, MC} = lists:keyfind(max_connections, 1, ConfList),
	{options_ttl, OT} = lists:keyfind(options_ttl, 1, ConfList),

	{ok, 'WAIT_FOR_SOCKET', #state{max_connections=MC, path=P, options_ttl=OT}}.

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
'ICAP_REQ_LINE'({data, Line}, #state{path=Path} = State) ->
	[MethodS, Uri, VersionS] = string:tokens(Line, " "),
	case get_path(Uri) of
		Path -> ok;
		Else -> throw({error, {path_does_not_match, {Path, Else}}}) end,

	Method = case http_util:to_lower(MethodS) of
		"options" -> options;
		"reqmod" -> reqmod;
		"respmod" -> respmod;
		_Else2 -> throw({error, bad_method}) end,

	[$i, $c, $a, $p, $/, MajorVersionC, $., MinorVersionC, $\r, $\n ] = 
			http_util:to_lower(VersionS),
	MajorVersion = MajorVersionC - $0,
	MinorVersion = MinorVersionC - $0,

	{next_state, 'ICAP_HEADER', 
		State#state{icap_request=#icap_request{
			method=Method, uri=Uri, major_version=MajorVersion, minor_version=MinorVersion},
			icap_headers=#icap_headers{}
		}, ?TIMEOUT};

'ICAP_REQ_LINE'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.


'ICAP_HEADER'({data, "\r\n"}, #state{icap_headers=IcapHeaders} = State) ->
        Others = IcapHeaders#icap_headers.other,
        IcapHeaders1 = IcapHeaders#icap_headers{other=lists:reverse(Others)},
        State1 = State#state{icap_headers=IcapHeaders1},

	HH = State1#state.icap_headers,
	E = HH#icap_headers.encapsulated,
	get_body(State1#state{icap_rencap=E});

'ICAP_HEADER'({data, IcapHeaderLine}, #state{icap_headers=IcapHeaders} = State) ->
	{Key, Value} = case string:chr(IcapHeaderLine, $:) of
		0 -> throw({error, {bad_header_line, IcapHeaderLine}});
		I -> K = string:substr(IcapHeaderLine, 1, I-1),
			K1 = http_util:to_lower(K),
			[$\s| V] = string:substr(IcapHeaderLine, I+1), {K1, rm_trailing_crlf(V)} end,
        IcapHeaders1 = case Key of
                "connection" -> IcapHeaders#icap_headers{connection=Value};
                "encapsulated" -> IcapHeaders#icap_headers{encapsulated=raw_to_encapsulatedh(Value)};
                "x-client-ip" -> IcapHeaders#icap_headers{x_client_ip=raw_to_xciph(Value)};
                "allow" -> IcapHeaders#icap_headers{allow=raw_to_allowh(Value)};
                _ -> Others = IcapHeaders#icap_headers.other, IcapHeaders#icap_headers{
				other=[#http_header{key=Key, value=Value}|Others]}   %% misc other headers
        end,

        {next_state, 'ICAP_HEADER', State#state{icap_headers=IcapHeaders1}, ?TIMEOUT};

'ICAP_HEADER'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

get_body(#state{icap_rencap=[]} = State) -> 'READ_FILES'(State);
get_body(#state{icap_rencap=undefined} = State) -> 'READ_FILES'(State);
get_body(#state{icap_rencap=[{null_body, _Val}]} = State) -> 'READ_FILES'(State);

get_body(#state{icap_rencap=[{req_hdr, _BI}|Rest]} = State) -> 
	{next_state, 'HTTP_REQ_LINE', State#state{icap_rencap=Rest}, ?TIMEOUT};

get_body(#state{socket=Socket, icap_rencap=[{req_body, _BI}|Rest]} = State) -> 
	inet:setopts(Socket, [{packet, line}, binary]),
	{next_state, 'HTTP_CC_LINE', State#state{icap_rencap=Rest}, ?TIMEOUT};

get_body(#state{icap_rencap=[{res_hdr, _BI}|_Rest]}) -> throw({error, {not_implemented, res_hdr}});
get_body(#state{icap_rencap=[{res_body, _BI}|_Rest]}) -> throw({error, {not_implemented, res_body}});
get_body(#state{icap_rencap=[{opt_body, _BI}|_Rest]}) -> throw({error, {not_implemented, opt_body}}).

'HTTP_REQ_LINE'({data, Line}, State) ->
	[MethodS, Uri, VersionS] = string:tokens(Line, " "),

	Method = case http_util:to_lower(MethodS) of
		"options" -> 'OPTIONS';
		"get" -> 'GET';
		"head" -> 'HEAD';
		"post" -> 'POST';
		"put" -> 'PUT';
		"delete" -> 'DELETE';
		"trace" -> 'TRACE';
		"connect" -> 'CONNECT';
		_Else -> throw({error, bad_method}) end,

	[$h, $t, $t, $p, $/, MajorVersionC, $., MinorVersionC, $\r, $\n ] = 
			http_util:to_lower(VersionS),
	MajorVersion = MajorVersionC - $0,
	MinorVersion = MinorVersionC - $0,
        {next_state, 'HTTP_HEADER', State#state{http_request=#http_request{
			method=Method, path=Uri, version={MajorVersion, MinorVersion}}
			, http_headers=#http_headers{}}, ?TIMEOUT};

'HTTP_REQ_LINE'(timeout, State) ->
        ?DEBUG("~p Client connection timeout - closing.\n", [self()]),
        {stop, normal, State}.

'HTTP_HEADER'({data, "\r\n"}, #state{http_headers=HttpHeaders} = State) ->
	Cookies = HttpHeaders#http_headers.cookie,
	Others = HttpHeaders#http_headers.other,
	HttpHeaders1 = HttpHeaders#http_headers{cookie=lists:reverse(Cookies), other=lists:reverse(Others)},
	State1 = State#state{http_headers=HttpHeaders1},
	get_body(State1);

'HTTP_HEADER'({data, HttpHeaderLine}, #state{http_headers=HttpHeaders} = State) ->
	{Key, Value} = case string:chr(HttpHeaderLine, $:) of
		0 -> throw({error, {bad_header_line, HttpHeaderLine}});
		I -> K = string:substr(HttpHeaderLine, 1, I-1),
			K1 = http_util:to_lower(K),
			[$\s| V] = string:substr(HttpHeaderLine, I+1), {K1, rm_trailing_crlf(V)} end,
	HttpHeaders1 = case Key of
		"connection" -> HttpHeaders#http_headers{connection=Value};
		"host" -> HttpHeaders#http_headers{host=Value};
		"cookie" -> Cookies = HttpHeaders#http_headers.cookie, HttpHeaders#http_headers{cookie=[Value|Cookies]};
		"keep-alive" -> HttpHeaders#http_headers{keep_alive=Value};
		"content-length" -> HttpHeaders#http_headers{content_length=Value};
		"content-type" -> HttpHeaders#http_headers{content_type=Value};
		"content-encoding" -> HttpHeaders#http_headers{content_encoding=Value};
		"transfer-encoding" -> HttpHeaders#http_headers{transfer_encoding=Value};
		_ -> Others = HttpHeaders#http_headers.other, HttpHeaders#http_headers{other=[
				#http_header{key=Key, value=Value}|Others]}   %% misc other headers
	end,
		
	{next_state, 'HTTP_HEADER', State#state{http_headers=HttpHeaders1}, ?TIMEOUT};

'HTTP_HEADER'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_CC_LINE'({data, Line}, #state{http_content=_Content1} = State) ->
	CSize = mydlp_api:hex2int(Line),
	%Content1 = [Line|Content],
	case CSize of
		0 -> {next_state, 'HTTP_CC_TCRLF', State, ?TIMEOUT};
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

'HTTP_CC_CRLF'({data, <<"\r\n">> = _CRLF}, #state{http_content=Content1} = State) ->
	%Content1 = [CRLF|Content],
	{next_state, 'HTTP_CC_LINE', State#state{http_content=Content1}, ?TIMEOUT};

'HTTP_CC_CRLF'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_CC_TCRLF'({data, <<"\r\n">>}, #state{http_content=Content1} = State) ->
	get_body(State#state{http_content=lists:reverse(Content1)});

'HTTP_CC_TCRLF'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'READ_FILES'(#state{icap_request=#icap_request{method=options}} = State) -> 'REQ_OK'(State);
'READ_FILES'(#state{http_headers=undefined} = State) -> 'REQ_OK'(State);
'READ_FILES'(#state{http_headers=HttpHeaders} = State) ->
%	when HttpHeaders#http_headers.content_type == ''->
	Files = case mydlp_api:starts_with(HttpHeaders#http_headers.content_type, "multipart/form-data") of
		true ->	parse_multipart(State);
		false -> []
	end,

	'REQ_OK'(State#state{files=Files}).

% {Action, {{rule, Id}, {file, File}, {matcher, Func}, {misc, Misc}}}
'REQ_OK'(#state{icap_request=#icap_request{method=options} } = State) -> 'REPLY_OK'(State);
'REQ_OK'(#state{files=Files, http_content=HttpContent, icap_headers=#icap_headers{x_client_ip=Addr} } = State) ->
	case mydlp_acl:q(Addr, dest, df_to_files(list_to_binary(HttpContent), Files)) of
		pass -> 'REPLY_OK'(State);
		{block, AclR} -> log_req(State, block, AclR), 'BLOCK_REQ'(block, State);
		{log, AclR} -> log_req(State, log, AclR), 'REPLY_OK'(State); % refine this
		{pass, AclR} -> log_req(State, pass, AclR), 'REPLY_OK'(State)
	end.

'REPLY_OK'(State) -> reply(ok, State).

'BLOCK_REQ'(block, State) -> reply(block, State).

reply(What, #state{socket=Socket, icap_request=IcapReq, http_request=HttpReq,
		max_connections=MC, options_ttl=OT} = State) ->
	IcapVer = ["ICAP/", integer_to_list(IcapReq#icap_request.major_version), ".", 
		integer_to_list(IcapReq#icap_request.minor_version)],
	StdIH = ["Date: ", httpd_util:rfc1123_date(), "\r\n",
		"Service: MyDLP ICAP Server\r\n", 
		"Connection: keep-alive\r\n", 
		"ISTag: \"mydlp-icap-rocks\"\r\n"],
	Reply = case {What, IcapReq#icap_request.method} of
		{ok, options} -> [IcapVer, " 200 OK\r\n",
				StdIH,
				"Methods: REQMOD\r\n",
				"Max-Connections: ", integer_to_list(MC), "\r\n",
				"Options-TTL: ", integer_to_list(OT), "\r\n",
				"Encapsulated: null-body=0\r\n",
				"Allow: 204\r\n\r\n"];
		{ok, reqmod} -> [IcapVer, " 204 No Content\r\n",
				StdIH,
				"Encapsulated: null-body=0\r\n\r\n"];
		{block, reqmod} -> 
				{http_request, _,  _, {HTTPMajorv, HTTPMinorv}} = HttpReq,
				Deny = <<"<html>",
				"<head><title>Blocked by MyDLP</title></head>",
				"<body><center><strong>DENIED !!!</strong></center></body>",
				"</html>">>,
				ReqModB = [httpd_util:integer_to_hexlist(size(Deny)), 
						"\r\n", Deny, "\r\n0\r\n"],
				ReqModH = list_to_binary(
					["HTTP/", integer_to_list(HTTPMajorv), ".", 
						integer_to_list(HTTPMinorv), " 403 Forbidden\r\n",
				"Content-Type: text/html; charset=UTF-8\r\n",
				"Content-Length: ", integer_to_list(size(Deny)), "\r\n",
				"\r\n"]),
				[IcapVer, " 200 OK\r\n",
				StdIH,
				"Encapsulated: res-hdr=0",
					", res-body=", integer_to_list(size(ReqModH)),"\r\n\r\n",
				ReqModH, ReqModB, "\r\n"] end,

	%io:format("~s~n",[binary_to_list(list_to_binary(Reply))]),
	gen_tcp:send(Socket, Reply),
	inet:setopts(Socket, [{packet, line}, list]),

	{next_state, 'ICAP_REQ_LINE', 
		State#state{icap_request=undefined,
				icap_headers=undefined,
				icap_body=[],
				icap_rencap=undefined,
				http_request=undefined, 
				http_headers=undefined,
				http_content=[], 
				files=[],
				tmp=unedefined}, ?KA_TIMEOUT}.

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

rm_trailing_crlf(Str) ->
	StrL = string:len(Str),
	"\r\n" = string:substr(Str, StrL - 1, 2),
	string:substr(Str, 1, StrL - 2).

raw_to_xciph(IpStr) -> 
	Tokens = string:tokens(IpStr,"."),
	[_,_,_,_] = Tokens,
	Ints = lists:map(fun(S) -> list_to_integer(S) end, Tokens),
	case lists:any(fun(I) -> ( I > 255 ) or ( I < 0 ) end, Ints) of
		true -> throw({error, {bad_ip, Ints}});
		false -> ok end,
	[I1,I2,I3,I4] = Ints,
	{I1,I2,I3,I4}.

raw_to_allowh(AllowStr) ->
	string:tokens(AllowStr, ", ").

raw_to_encapsulatedh(EncapStr) ->
	Tokens = string:tokens(EncapStr, ", "),
	lists:map(fun(S) -> 
		case string:tokens(S, "=") of
			["req-hdr", Val] -> {req_hdr, list_to_integer(Val)};
			["res-hdr", Val] -> {res_hdr, list_to_integer(Val)};
			["req-body", Val] -> {req_body, list_to_integer(Val)};
			["res-body", Val] -> {res_body, list_to_integer(Val)};
			["opt-body", Val] -> {opt_body, list_to_integer(Val)};
			["null-body", Val] -> {null_body, list_to_integer(Val)};
			Else -> throw({error, {bad_encap_part, Else}}) end
	end, Tokens).

parse_multipart(#state{http_content=HttpContent, http_headers=H, http_request=Req}) ->
	mydlp_api:parse_multipart(HttpContent, H, Req).

df_to_files(Data, Files) ->
        case length(Files) of
                0 ->    DFile = #file{name= "post-data", data=Data},
                        [DFile];
                _ ->    Files
        end.

log_req(#state{icap_headers=#icap_headers{x_client_ip=Addr}, 
		http_headers=(#http_headers{host=DestHost})}, Action, 
		{{rule, RuleId}, {file, File}, {matcher, Matcher}, {misc, Misc}}) ->
	?ACL_LOG(icap, RuleId, Action, Addr, nil, DestHost, Matcher, File, Misc).

get_path(("/" ++ _Str) = Uri) -> Uri;
get_path("icap://" ++ Str) ->
	case string:chr(Str, $/) of
		0 -> "/";
		I2 -> string:substr(Str, I2) end;
get_path(Uri) -> throw({error, {bad_uri, Uri}}).
