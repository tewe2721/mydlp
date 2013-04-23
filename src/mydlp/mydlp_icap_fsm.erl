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

-ifdef(__MYDLP_NETWORK).

-module(mydlp_icap_fsm).
-author('kerem@mydlp.com').
-behaviour(gen_fsm).
-include("mydlp.hrl").
-include("mydlp_http.hrl").

-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-compile(export_all).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'ICAP_REQ_LINE'/2,
    'ICAP_HEADER'/2,
    'HTTP_REQ_LINE'/2,
    'PARSE_REQ_LINE'/2,
    'HTTP_RES_LINE'/2,
    'PARSE_RES_LINE'/2,
    'HTTP_HEADER'/2,
    'PARSE_HEADER'/2,
    'HTTP_CC_LINE'/2,
    'HTTP_CC_PREVIEW'/2,
    'HTTP_CC_CHUNK'/2,
    'PARSE_CC_CHUNK'/2,
    'HTTP_CC_CRLF'/2,
    'HTTP_CC_TCRLF'/2
]).

-record(state, {
	socket,
	addr,
	username=nil,
	un_hash=unknown,
	icap_request,
	icap_headers,
	icap_body=[],
	icap_rencap,
	http_request,
	http_response,
	http_headers,
	http_req_headers,
	http_res_headers,
	http_content= <<>>,
	http_req_content= <<>>,
	http_res_content= <<>>,
	files=[],
	max_connections,
	options_ttl,
	path_reqmod,
	path_respmod,
	icap_mod_mode=reqmod, % icap modification mode
	http_message_type,
	buffer,
	ch_req_h=false,
	ch_req_b=false,
	ch_data_req_h= <<>>,
	ch_data_req_b= <<>>,
	ch_res_h=false,
	ch_res_b=false,
	ch_data_res_h= <<>>,
	ch_data_res_b= <<>>,
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
	allow204=false,
	connection,
	encapsulated,
	x_client_ip,
	preview,
	other=[]
}).

-define(ICAP_RESP_LINE_OK, <<"ICAP/1.0 200 OK\r\n">>).
-define(ICAP_RESP_LINE_204, <<"ICAP/1.0 204 No Content\r\n">>).
-define(ICAP_RESP_LINE_100, <<"ICAP/1.0 100 Continue\r\n">>).
-define(ICAP_RESP_STD_HEADERS, 
	<<	"Service: MyDLP ICAP Server\r\n", 
		"Connection: keep-alive\r\n", 
		"ISTag: \"mydlp-icap-rocks\"\r\n"
	>>).

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

	P = ?CFG(icap_reqmod_path),
	PR = ?CFG(icap_respmod_path),
	MC = ?CFG(icap_max_connections),
	OT = ?CFG(icap_options_ttl),

	{ok, 'WAIT_FOR_SOCKET', #state{max_connections=MC, 
		path_reqmod=P, path_respmod=PR, options_ttl=OT}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket, _CommType}, State) when is_port(Socket) ->
	inet:setopts(Socket, [{active, once}, {packet, line}, list]),
	case inet:peername(Socket) of
		{ok, {IP, _Port}} -> 
			{next_state, 'ICAP_REQ_LINE', State#state{socket=Socket, addr=IP}, ?CFG(fsm_timeout)};
		Else -> ?ERROR_LOG("Can not get ip address and port. Ret: "?S, [Else]),
			{stop, normal, State} end;
'WAIT_FOR_SOCKET'(Other, State) ->
	?DEBUG("ICAP FSM: 'WAIT_FOR_SOCKET'. Unexpected message: "?S, [Other]),
	%% Allow to receive async messages
	{next_state, 'WAIT_FOR_SOCKET', State}.

% Notification event coming from client
'ICAP_REQ_LINE'({data, Line}, #state{path_reqmod=ReqmodPath, path_respmod=RespmodPath} = State) ->
	Line1 = case Line of
		L when is_list(L) -> Line;
		L when is_binary(L) -> binary_to_list(Line) end,
	[MethodS, Uri, VersionS] = string:tokens(Line1, " "),

	ModMode = case get_path(Uri) of
		ReqmodPath -> reqmod;
		RespmodPath -> respmod;
		Else -> throw({error, {path_does_not_match, Else}}) end,

	Method = case {ModMode, http_util:to_lower(MethodS)} of
		{_, "options"} -> options;
		{reqmod, "reqmod"} -> reqmod;
		{respmod, "respmod"} -> respmod;
		_Else2 -> throw({error, bad_method}) end,

	[$i, $c, $a, $p, $/, MajorVersionC, $., MinorVersionC, $\r, $\n ] = 
			http_util:to_lower(VersionS),
	MajorVersion = MajorVersionC - $0,
	MinorVersion = MinorVersionC - $0,

	{next_state, 'ICAP_HEADER', 
		State#state{icap_request=#icap_request{
			method=Method, uri=Uri, major_version=MajorVersion, minor_version=MinorVersion},
			icap_headers=#icap_headers{}, icap_mod_mode=ModMode}, ?CFG(fsm_timeout)};

'ICAP_REQ_LINE'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.


'ICAP_HEADER'({data, "\r\n"}, #state{icap_headers=IcapHeaders} = State) ->
        Others = IcapHeaders#icap_headers.other,
        IcapHeaders1 = IcapHeaders#icap_headers{other=lists:reverse(Others)},
        State1 = State#state{icap_headers=IcapHeaders1},

	HH = State1#state.icap_headers,
	E = HH#icap_headers.encapsulated,
	encap_next(State1#state{icap_rencap=E});

'ICAP_HEADER'({data, IcapHeaderLine}, #state{icap_headers=IcapHeaders} = State) ->
	{Key, Value} = case string:chr(IcapHeaderLine, $:) of
		0 -> throw({error, {bad_header_line, IcapHeaderLine}});
		I -> K = string:substr(IcapHeaderLine, 1, I-1),
			K1 = http_util:to_lower(K),
			[$\s| V] = string:substr(IcapHeaderLine, I+1), {K1, mydlp_api:rm_trailing_crlf(V)} end,

        IcapHeaders1 = case Key of
                "connection" -> IcapHeaders#icap_headers{connection=Value};
                "encapsulated" -> IcapHeaders#icap_headers{encapsulated=raw_to_encapsulatedh(Value)};
                "x-client-ip" -> IcapHeaders#icap_headers{x_client_ip=mydlp_api:str_to_ip(Value)};
                "preview" -> IcapHeaders#icap_headers{preview=list_to_integer(Value)};
                "allow" ->	AllowH = raw_to_allowh(Value),
				IcapHeaders#icap_headers{allow=AllowH, 
					allow204=lists:member("204",AllowH)};
                _ -> Others = IcapHeaders#icap_headers.other, IcapHeaders#icap_headers{
				other=[#http_header{key=Key, value=Value}|Others]}   %% misc other headers
        end,

	{UserName, UserHash} = case IcapHeaders1#icap_headers.x_client_ip of
		undefined -> {nil, unknown};
		IP -> mydlp_mnesia:get_user_from_address(IP) end,
        {next_state, 'ICAP_HEADER', State#state{username=UserName, un_hash=UserHash, icap_headers=IcapHeaders1}, ?CFG(fsm_timeout)};

'ICAP_HEADER'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

encap_next(#state{icap_rencap=[]} = State) -> 'READ_FILES'(State#state{http_message_type=undefined});
encap_next(#state{icap_rencap=undefined} = State) -> 'READ_FILES'(State#state{http_message_type=undefined});
encap_next(#state{icap_rencap=[{null_body, _Val}]} = State) -> 'READ_FILES'(State#state{http_message_type=undefined});

encap_next(#state{icap_rencap=[{req_hdr, _BI}|Rest], 
		icap_headers=#icap_headers{allow204=false}} = State) -> 
	{next_state, 'HTTP_REQ_LINE', State#state{icap_rencap=Rest, http_message_type=request, ch_req_h=true}, ?CFG(fsm_timeout)};

encap_next(#state{icap_rencap=[{req_hdr, _BI}|Rest], 
		icap_headers=#icap_headers{allow204=true}} = State) -> 
	{next_state, 'HTTP_REQ_LINE', State#state{icap_rencap=Rest, http_message_type=request, ch_req_h=false}, ?CFG(fsm_timeout)};

encap_next(#state{icap_rencap=[{res_hdr, _BI}|Rest], 
		icap_headers=#icap_headers{allow204=false}} = State) -> 
	{next_state, 'HTTP_RES_LINE', State#state{icap_rencap=Rest, http_message_type=response, ch_res_h=true}, ?CFG(fsm_timeout)};

encap_next(#state{icap_rencap=[{res_hdr, _BI}|Rest], 
		icap_headers=#icap_headers{allow204=true}} = State) -> 
	{next_state, 'HTTP_RES_LINE', State#state{icap_rencap=Rest, http_message_type=response, ch_res_h=false}, ?CFG(fsm_timeout)};

encap_next(#state{socket=Socket, icap_rencap=[{req_body, _BI}|Rest],
		icap_headers=#icap_headers{allow204=false}} = State) -> 
	inet:setopts(Socket, [{packet, line}, binary]),
	{next_state, 'HTTP_CC_LINE', State#state{icap_rencap=Rest, http_message_type=request, ch_req_b=true}, ?CFG(fsm_timeout)};

encap_next(#state{socket=Socket, icap_rencap=[{req_body, _BI}|Rest],
		icap_headers=#icap_headers{allow204=true}} = State) -> 
	inet:setopts(Socket, [{packet, line}, binary]),
	{next_state, 'HTTP_CC_LINE', State#state{icap_rencap=Rest, http_message_type=request, ch_req_b=false}, ?CFG(fsm_timeout)};

encap_next(#state{socket=Socket, icap_rencap=[{res_body, _BI}|Rest],
		icap_headers=#icap_headers{allow204=false}} = State) -> 
	inet:setopts(Socket, [{packet, line}, binary]),
	{next_state, 'HTTP_CC_LINE', State#state{icap_rencap=Rest, http_message_type=response, ch_res_b=true}, ?CFG(fsm_timeout)};

encap_next(#state{socket=Socket, icap_rencap=[{res_body, _BI}|Rest],
		icap_headers=#icap_headers{allow204=true}} = State) -> 
	inet:setopts(Socket, [{packet, line}, binary]),
	{next_state, 'HTTP_CC_LINE', State#state{icap_rencap=Rest, http_message_type=response, ch_res_b=false}, ?CFG(fsm_timeout)};

%encap_next(#state{icap_rencap=[{res_hdr, _BI}|_Rest]}) -> throw({error, {not_implemented, res_hdr}});
%encap_next(#state{icap_rencap=[{res_body, _BI}|_Rest]}) -> throw({error, {not_implemented, res_body}});
encap_next(#state{icap_rencap=[{opt_body, _BI}|_Rest]}) -> throw({error, {not_implemented, opt_body}}).

'HTTP_REQ_LINE'({data, Line}, State) -> read_line(Line, State, 'HTTP_REQ_LINE', 'PARSE_REQ_LINE');

'HTTP_REQ_LINE'(timeout, State) ->
        ?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
        {stop, normal, State}.

'PARSE_REQ_LINE'({data, ReqLine}, State) ->
	[MethodS, Uri, VersionS] = string:tokens(ReqLine, " "),

	Method = case http_util:to_lower(MethodS) of
		"options" -> 'OPTIONS';
		"get" -> 'GET';
		"head" -> 'HEAD';
		"post" -> 'POST';
		"put" -> 'PUT';
		"delete" -> 'DELETE';
		"trace" -> 'TRACE';
		"propfind" -> 'PROPFIND';
		"pull" -> 'PULL';
		"poll" -> 'POLL';
		"search" -> 'SEARCH';
		"lock" -> 'LOCK';
		"subscribe" -> 'SUBSCRIBE';
		"connect" -> 'CONNECT';
		_Else -> throw({error, bad_method}) end,

	[$h, $t, $t, $p, $/, MajorVersionC, $., MinorVersionC, $\r, $\n ] = 
			http_util:to_lower(VersionS),
	MajorVersion = MajorVersionC - $0,
	MinorVersion = MinorVersionC - $0,
        {next_state, 'HTTP_HEADER', State#state{http_request=#http_request{
			method=Method, path=Uri, version={MajorVersion, MinorVersion}}
			, http_headers=#http_headers{}}, ?CFG(fsm_timeout)}.

'HTTP_RES_LINE'({data, Line}, State) -> read_line(Line, State, 'HTTP_RES_LINE', 'PARSE_RES_LINE');

'HTTP_RES_LINE'(timeout, State) ->
        ?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
        {stop, normal, State}.

'PARSE_RES_LINE'({data, ResLine}, State) ->
	[VersionS, CodeS|PhraseArr] = string:tokens(ResLine, " "),
	PhraseS = string:join(PhraseArr, " "),

	Code = try list_to_integer(CodeS)
		catch _:Err -> throw({error, {notint, CodeS, Err}}) end,
		
	[$h, $t, $t, $p, $/, MajorVersionC, $., MinorVersionC ] = 
			http_util:to_lower(VersionS),
	MajorVersion = MajorVersionC - $0,
	MinorVersion = MinorVersionC - $0,

        {next_state, 'HTTP_HEADER', State#state{http_response=#http_response{
			code=Code, phrase=PhraseS, version={MajorVersion, MinorVersion}}
			, http_headers=#http_headers{}}, ?CFG(fsm_timeout)}.

'HTTP_HEADER'({data, Line}, State) -> read_line(Line, State, 'HTTP_HEADER', 'PARSE_HEADER');

'HTTP_HEADER'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'PARSE_HEADER'({data, "\r\n"}, #state{http_headers=HttpHeaders} = State) ->
	Cookies = HttpHeaders#http_headers.cookie,
	Others = HttpHeaders#http_headers.other,
	HttpHeaders1 = HttpHeaders#http_headers{cookie=lists:reverse(Cookies), other=lists:reverse(Others)},
	State1 = case State#state.http_message_type of % Request headers or response headers
		request -> State#state{http_req_headers=HttpHeaders1, ch_req_h=false};
		response -> State#state{http_res_headers=HttpHeaders1, ch_res_h=false} end,
	encap_next(State1#state{http_headers=undefined});

'PARSE_HEADER'({data, HttpHeaderLine}, #state{http_headers=HttpHeaders} = State) ->
	{Key, Value} = case string:chr(HttpHeaderLine, $:) of
		0 -> throw({error, {bad_header_line, HttpHeaderLine}});
		I -> K = string:substr(HttpHeaderLine, 1, I-1),
			K1 = http_util:to_lower(K),
			[$\s| V] = string:substr(HttpHeaderLine, I+1), {K1, mydlp_api:rm_trailing_crlf(V)} end,
	HttpHeaders1 = case Key of
		"connection" -> HttpHeaders#http_headers{connection=Value};
		"host" -> HttpHeaders#http_headers{host=Value};
		"cookie" -> Cookies = HttpHeaders#http_headers.cookie, HttpHeaders#http_headers{cookie=[Value|Cookies]};
		"keep-alive" -> HttpHeaders#http_headers{keep_alive=Value};
		"content-length" -> HttpHeaders#http_headers{content_length=list_to_integer(Value)};
		"content-type" -> HttpHeaders#http_headers{content_type=Value};
		"content-disposition" -> HttpHeaders#http_headers{content_disposition=Value};
		"content-encoding" -> HttpHeaders#http_headers{content_encoding=Value};
		"transfer-encoding" -> HttpHeaders#http_headers{transfer_encoding=Value};
		_ -> Others = HttpHeaders#http_headers.other, HttpHeaders#http_headers{other=[
				#http_header{key=Key, value=Value}|Others]}   %% misc other headers
	end,
	{next_state, 'HTTP_HEADER', State#state{http_headers=HttpHeaders1}, ?CFG(fsm_timeout)}.

respond_preview(State, #http_headers{content_length=undefined}) ->
	send_continue(State);

respond_preview(State, #http_headers{content_length=ContentLength}) ->
	case ( not ?CFG(icap_ignore_big_requests) ) and ( ContentLength > ?CFG(maximum_object_size) ) of
		true -> reply(ok, State);
		false -> send_continue(State) end.

'HTTP_CC_LINE'({data, <<"0\r\n">>}, 
		#state{icap_headers=(#icap_headers{preview=Preview} = IcapHeaders)} = State)
		when is_integer(Preview) ->
	IcapHeaders1 = IcapHeaders#icap_headers{preview=done},
	{next_state, 'HTTP_CC_PREVIEW', State#state{icap_headers=IcapHeaders1}, ?CFG(fsm_timeout)};

'HTTP_CC_LINE'({data, <<"0; ieof\r\n">>}, 
		#state{icap_headers=(
			#icap_headers{preview=Preview} = IcapHeaders)
		} = State) 
		when is_integer(Preview) ->
	IcapHeaders1 = IcapHeaders#icap_headers{preview=ieof},
	{next_state, 'HTTP_CC_TCRLF', State#state{icap_headers=IcapHeaders1}, ?CFG(fsm_timeout)};

'HTTP_CC_LINE'({data, Line}, State) ->
	CSize = mydlp_api:hex2int(Line),
	case CSize of
		0 -> {next_state, 'HTTP_CC_TCRLF', State, ?CFG(fsm_timeout)};
		_ -> {next_state, 'HTTP_CC_CHUNK', State#state{tmp=CSize}, ?CFG(fsm_timeout)}
	end;

'HTTP_CC_LINE'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

drop_preview_bin(DataB)->
	case size(DataB) of
		5 -> <<>>;
		I when I > 5 -> Size = I - 5,
				<<B:Size/binary, "0\r\n\r\n">> = DataB, B end.

'HTTP_CC_PREVIEW'({data, <<"\r\n">>}, 
		#state{icap_mod_mode=respmod,
		ch_res_b=false,
                http_res_headers=HttpHeaders} = State) ->
	respond_preview(State, HttpHeaders);

'HTTP_CC_PREVIEW'({data, <<"\r\n">>}, 
		#state{icap_mod_mode=respmod,
		ch_res_b=true, ch_data_res_b=CacheDataResB,
                http_res_headers=HttpHeaders} = State) ->
	ResBody = drop_preview_bin(CacheDataResB),
	respond_preview(State#state{ch_data_res_b=ResBody}, HttpHeaders);

'HTTP_CC_PREVIEW'({data, <<"\r\n">>}, 
		#state{icap_mod_mode=reqmod,
		ch_req_b=false,
                http_req_headers=HttpHeaders} = State) ->
	respond_preview(State, HttpHeaders);

'HTTP_CC_PREVIEW'({data, <<"\r\n">>}, 
		#state{icap_mod_mode=reqmod,
		ch_req_b=true, ch_data_req_b=CacheDataReqB,
                http_req_headers=HttpHeaders} = State) ->
	ReqBody = drop_preview_bin(CacheDataReqB),
	respond_preview(State#state{ch_data_req_b=ReqBody}, HttpHeaders);

'HTTP_CC_PREVIEW'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_CC_CHUNK'({data, Line}, State) -> read_line(Line, State, 'HTTP_CC_CHUNK', 'PARSE_CC_CHUNK');

'HTTP_CC_CHUNK'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'PARSE_CC_CHUNK'({data, Line}, #state{http_content=Content, tmp=CSize} = State) ->
	CSize1 = CSize - size(Line),
	if
		CSize1 > 0 -> {next_state, 'HTTP_CC_CHUNK', 
                                State#state{http_content= <<Content/binary,Line/binary>>, tmp=CSize1}, ?CFG(fsm_timeout)};
		CSize1 == 0 -> {next_state, 'HTTP_CC_CRLF',
				State#state{http_content= <<Content/binary,Line/binary>>, tmp=undefined}, ?CFG(fsm_timeout)};
		CSize1 == -2 -> {next_state, 'HTTP_CC_LINE',
				State#state{http_content= <<Content/binary, (mydlp_api:rm_trailing_crlf(Line)) /binary>>, tmp=undefined}, ?CFG(fsm_timeout)}
	end.

'HTTP_CC_CRLF'({data, <<"\r\n">>}, State) ->
	{next_state, 'HTTP_CC_LINE', State, ?CFG(fsm_timeout)};

'HTTP_CC_CRLF'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'HTTP_CC_TCRLF'({data, <<"\r\n">>}, #state{http_content=Content} = State) ->
	State1 = case State#state.http_message_type of % Request payload or response payload
		request -> State#state{http_req_content=Content, ch_req_b=false};
		response -> State#state{http_res_content=Content, ch_res_b=false} end,
	
	encap_next(State1#state{http_content= <<>>});

'HTTP_CC_TCRLF'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'READ_FILES'(#state{icap_request=#icap_request{method=options}} = State) -> 'REQ_OK'(State);
'READ_FILES'(#state{http_req_headers=undefined} = State) -> 'REQ_OK'(State);
'READ_FILES'(#state{http_req_headers=HttpHeaders} = State) ->
%	when HttpHeaders#http_headers.content_type == ''->
	Files = case HttpHeaders#http_headers.content_type of 
			"multipart/form-data" ++ _Tail -> parse_multipart(State);
			"application/x-www-form-urlencoded" ++ _Tail -> parse_urlencoded(State);
			_Else -> [] end,

	'REQ_OK'(State#state{files=Files}).

% {Action, {{rule, Id}, {file, File}, {matcher, Func}, {misc, Misc}, {matching_details, MatchingDetails}}}
'REQ_OK'(#state{icap_request=#icap_request{method=options} } = State) -> 'REPLY_OK'(State);
'REQ_OK'(#state{icap_mod_mode=respmod} = State) ->
	DFFiles = df_to_files(State),
	%QRet = mydlp_acl:qi(web, DFFiles),
	QRet = respmod_query(State, DFFiles),
	acl_ret(QRet, DFFiles, State);
'REQ_OK'(#state{icap_headers=#icap_headers{x_client_ip=CAddr},
		http_request=#http_request{method=Method, path=Uri},
		http_req_headers=#http_headers{host=DestHost},
		un_hash=UserHash } = State) ->
	DFFiles = df_to_files(State),

	DestHost1 = case DestHost of
		undefined -> get_host(Method, Uri);
		DH -> mydlp_api:drop_host_port(DH) end,
	DestList = [list_to_binary(DestHost1)],

	AclQ = #aclq{channel=web, src_addr=CAddr, src_user_h=UserHash, destinations=DestList},
	QRet = mydlp_acl:q(AclQ, DFFiles),
	acl_ret(QRet, DFFiles, State).

respmod_query(_State, Files) ->
	case ?CFG(web_archive) of
		true ->	{archive, mydlp_api:empty_aclr(Files, web_archive)}; % We don't create redundant data refs because fsm will not query acl.
		false -> pass end.

acl_ret(QRet, DFFiles, State) -> 
	case case QRet of
		pass -> pass; 
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
		pass -> 'REPLY_OK'(State); 
		{pass, _AclR} -> 'REPLY_OK'(State); 
		{log, AclR} -> log_req(State, log, AclR),
					'REPLY_OK'(State); 
		{archive, AclR} -> log_req(State, archive, AclR),
					'REPLY_OK'(State);
		{block, AclR} -> log_req(State, block, AclR),
					'BLOCK_REQ'(block, State, AclR);
		{quarantine, AclR} -> log_req(State, quarantine, AclR),
					'BLOCK_REQ'(block, State, AclR)
	end.

'REPLY_OK'(State) -> reply(ok, State).

'BLOCK_REQ'(block, State, {{rule, OrigRuleId}, _, _, _, _}) -> reply({block, OrigRuleId}, State).

send_continue(#state{socket=Socket} = State) ->
	Reply = [?ICAP_RESP_LINE_100,
		icap_date_hdr_line(),
		?ICAP_RESP_STD_HEADERS,
		<<"Encapsulated: null-body=0\r\n\r\n">>],
	%io:format("~s~n",[binary_to_list(list_to_binary(Reply))]),
	gen_tcp:send(Socket, Reply),
	inet:setopts(Socket, [{packet, line}, binary]),
        {next_state, 'HTTP_CC_LINE', State, ?CFG(fsm_timeout)}.

reply(What, #state{socket=Socket, icap_request=IcapReq, http_request=HttpReq,
		icap_headers=#icap_headers{allow204=Allow204},
		ch_data_req_h=CacheDataReqH, ch_data_req_b=CacheDataReqB,
		ch_data_res_h=CacheDataResH, ch_data_res_b=CacheDataResB,
		max_connections=MC, options_ttl=OT, icap_mod_mode=ModMode} = State) ->

	Reply = case {What, Allow204, IcapReq#icap_request.method} of
		{ok, _, options} -> 
				PMS = case ModMode of 
					reqmod -> <<"REQMOD">>;
					respmod -> <<"RESPMOD">> end,
				[ ?ICAP_RESP_LINE_OK,
				icap_date_hdr_line(),
				?ICAP_RESP_STD_HEADERS,
				"Methods: ", PMS, "\r\n",
				case MC of 0 -> "";
					_ -> ["Max-Connections: ", integer_to_list(MC), "\r\n"] end,
				case OT of 0 -> "";
					_ -> ["Options-TTL: ", integer_to_list(OT), "\r\n"] end,
				"Encapsulated: null-body=0\r\n",
				"Preview: 0\r\n"
				"Transfer-Complete: \r\n"
				"Transfer-Ignore: \r\n"
				"Transfer-Preview: *\r\n"
				"X-Include: X-Client-IP\r\n" % X-Server-IP, X-Authenticated-User, X-Authenticated-Groups could be added also
				"Allow: 204\r\n\r\n"];
		{ok, true, _Reqmod_Or_Respmod} -> [
				?ICAP_RESP_LINE_204,
				icap_date_hdr_line(),
				?ICAP_RESP_STD_HEADERS,
				<<"Encapsulated: null-body=0\r\n\r\n">>];
		{ok, false, reqmod} -> 
				{EncapLine, Payload} = encap_pl_req(CacheDataReqH, CacheDataReqB),
				[ ?ICAP_RESP_LINE_OK,
				icap_date_hdr_line(),
				?ICAP_RESP_STD_HEADERS,
				EncapLine,
				<<"\r\n\r\n">>,
                                Payload,
				<<"\r\n">>];
		{ok, false, respmod} -> 
				{EncapLine, Payload} = encap_pl_res(CacheDataResH, CacheDataResB),
				[ ?ICAP_RESP_LINE_OK,
				icap_date_hdr_line(),
				?ICAP_RESP_STD_HEADERS,
				EncapLine,
				<<"\r\n\r\n">>,
                                Payload,
				<<"\r\n">>];
		{{block, OrigRuleId}, _, _Reqmod_Or_Respmod} -> 
				{http_request, _,  _, {HTTPMajorv, HTTPMinorv}} = HttpReq,
				Deny = mydlp_api:get_denied_page(OrigRuleId, html),
				ReqModB = [httpd_util:integer_to_hexlist(size(Deny)), 
						"\r\n", Deny, "\r\n0\r\n"],
				ReqModH = list_to_binary(
					["HTTP/", integer_to_list(HTTPMajorv), ".", 
						integer_to_list(HTTPMinorv), " 403 Forbidden\r\n",
				"Content-Type: text/html; charset=UTF-8\r\n",
				"Content-Length: ", integer_to_list(size(Deny)), "\r\n",
				"\r\n"]),
				[ ?ICAP_RESP_LINE_OK,
				icap_date_hdr_line(),
				?ICAP_RESP_STD_HEADERS,
				"Encapsulated: res-hdr=0",
					", res-body=", integer_to_list(size(ReqModH)),"\r\n\r\n",
				ReqModH, ReqModB, "\r\n"] end,

	%Print = binary_to_list(list_to_binary(Reply)),
	%case length(Print) > 1024 of
	%	true -> io:format("~s~n",[string:substr(Print, 1,1000) ++ "..."]);
	%	false -> io:format("~s~n",[Print]) end,
	%
	gen_tcp:send(Socket, Reply),
	inet:setopts(Socket, [{packet, line}, list]),

	{next_state, 'ICAP_REQ_LINE', 
		State#state{	username=nil,
				un_hash=unknown,
				icap_request=undefined,
				icap_headers=undefined,
				icap_body=[],
				icap_rencap=undefined,
				http_request=undefined, 
				http_response=undefined, 
				http_headers=undefined,
				http_req_headers=undefined,
				http_res_headers=undefined,
				http_content= <<>>, 
				http_req_content= <<>>, 
				http_res_content= <<>>, 
				files=[],
				http_message_type=undefined,
				buffer=undefined,
				ch_req_h=false,
				ch_req_b=false,
				ch_data_req_h= <<>>,
				ch_data_req_b= <<>>,
				ch_res_h=false,
				ch_res_b=false,
				ch_data_res_h= <<>>,
				ch_data_res_b= <<>>,
				tmp=undefined}, ?KA_TIMEOUT}.

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
handle_info({tcp, Socket, _Bin}, _StateName, #state{socket=Socket,
		ch_req_h=true, ch_req_b=true}) ->
	throw({error, should_not_cache_both_http_req_h_and_b});

handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket,
		ch_req_h=true, ch_data_req_h=CacheH} = StateData) ->
	CacheDataH = <<CacheH/binary, ( list_to_binary(Bin) )/binary>>,
	Return = fsm_call(StateName, {data, Bin}, StateData#state{ch_data_req_h=CacheDataH}),
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	Return;

handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket,
		ch_req_b=true, ch_data_req_b=CacheB} = StateData) ->
	CacheDataB = concat_cache_data(StateName, CacheB, Bin),
	Return = fsm_call(StateName, {data, Bin}, StateData#state{ch_data_req_b= CacheDataB }),
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	Return;

handle_info({tcp, Socket, _Bin}, _StateName, #state{socket=Socket,
		ch_res_h=true, ch_res_b=true}) ->
	throw({error, should_not_cache_both_http_res_h_and_b});

handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket,
		ch_res_h=true, ch_data_res_h=CacheH} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	CacheDataH = <<CacheH/binary, ( list_to_binary(Bin) )/binary>>,
	Return = fsm_call(StateName, {data, Bin}, StateData#state{ch_data_res_h=CacheDataH}),
	inet:setopts(Socket, [{active, once}]),
	Return;


handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket,
		ch_res_b=true, ch_data_res_b=CacheB} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	CacheDataB = concat_cache_data(StateName, CacheB, Bin),
	Return = fsm_call(StateName, {data, Bin}, StateData#state{ch_data_res_b=CacheDataB }),
	inet:setopts(Socket, [{active, once}]),
	Return;

handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	Return = fsm_call(StateName, {data, Bin}, StateData),
	inet:setopts(Socket, [{active, once}]),
	Return;

handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket, addr=_Addr} = StateData) ->
	% ?ERROR_LOG(?S" Client "?S" disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
	{noreply, StateName, StateData}.

fsm_call(StateName, Args, StateData) -> 
	try ?MODULE:StateName(Args, StateData)
	catch Class:Error ->
		?ERROR_LOG("Error occured on FSM ("?S") call ("?S"). Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[?MODULE, StateName, Class, Error, erlang:get_stacktrace()]),
		%(catch 'REPLY_OK'(StateData)),
		{stop, normalStop, StateData} end.

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

parse_multipart(#state{http_req_content=HttpContent, http_req_headers=H, http_request=Req}) ->
	mydlp_api:parse_multipart(HttpContent, H, Req).

parse_urlencoded(#state{http_req_content=HttpContent}) ->
	mydlp_api:uenc_to_file(HttpContent).

-define(MAX_FILENAME_LENGTH, 128).

df_to_files(#state{icap_mod_mode=reqmod, files=Files, 
		http_request=#http_request{path=Uri},
		http_req_headers=#http_headers{content_disposition=CDisposition},
		http_req_content=ReqData}) ->
	UFiles = case mydlp_api:uri_to_hr_file(Uri) of
		none -> [];
		F -> [F] end,
	
	OFiles = case Files of
		[] -> 	FN = case cd_to_fn1(CDisposition) of
				none -> "post-data";
				CDFN -> CDFN end,
			DFile = ?BF_C(#file{name=FN}, ReqData), [DFile];
		_ -> Files end,

	lists:append([UFiles, OFiles]);
df_to_files(#state{icap_mod_mode=respmod, http_res_content= <<>>}) -> [];
df_to_files(#state{icap_mod_mode=respmod,
		http_request=#http_request{path=Uri},
		http_res_headers=#http_headers{content_disposition=CDisposition},
		http_res_content=ResData}) ->

	FN = case cd_to_fn1(CDisposition) of
		none -> uri_to_fn(Uri);
		CDFN -> CDFN end,

	RFile = case FN of
		none -> ?BF_C(#file{name= "resp-data"}, ResData);
		FN -> FN1 = case string:len(FN) > ?MAX_FILENAME_LENGTH of
				true -> string:substr(FN, 1, ?MAX_FILENAME_LENGTH);
				false -> FN end,
			?BF_C(#file{filename=FN1}, ResData) end,
	[RFile];
df_to_files(#state{icap_mod_mode=Else}) -> throw({error, not_implemented, Else}).

cd_to_fn1(undefined) -> none;
cd_to_fn1(CDisposition) -> mydlp_api:cd_to_fn(CDisposition).

uri_to_fn(Uri) ->
	Length = string:len(Uri),
	case string:rchr(Uri, $/) of 
		0 -> none;
		Length -> none;
		I -> LC = string:substr(Uri, I + 1),
			case string:chr(LC, $?) of
				0 -> LC;
				1 -> none;
				I2 -> string:substr(LC, 1, I2 - 1) end end.

log_req(#state{icap_headers=#icap_headers{x_client_ip=Addr},
		http_request=#http_request{path=Uri},
		username=UserName}, Action,
		{{rule, RuleId}, {file, File}, {itype, IType}, {misc, Misc}, {matching_details, MatchingDetails}}) ->
	Time = erlang:universaltime(),
	?ACL_LOG(#log{time=Time, channel=web, rule_id=RuleId, action=Action, ip=Addr, user=UserName, destination=Uri, itype_id=IType, file=File, misc=Misc, matching_details=MatchingDetails}).

get_path(("/" ++ _Str) = Uri) -> Uri;
get_path("icap://" ++ Str) ->
	case string:chr(Str, $/) of
		0 -> "/";
		I2 -> string:substr(Str, I2) end;
get_path(Uri) -> throw({error, {bad_uri, Uri}}).

binary_last(Bin) when is_binary(Bin) ->
	JunkSize = size(Bin) - 1,
	<<_Junk:JunkSize/binary, Last/integer>> = Bin,
	Last.

read_line(Line, #state{buffer=undefined} = State, FSMState, ParseFunc) when is_list(Line) ->
	case lists:last(Line) of
		$\n -> ?MODULE:ParseFunc({data, Line}, State);
		_Else -> {next_state, FSMState, State#state{buffer=[Line]}, ?CFG(fsm_timeout)} end;

read_line(Line, #state{buffer=BuffList} = State, FSMState, ParseFunc) when is_list(Line) ->
	case lists:last(Line) of
		$\n -> ?MODULE:ParseFunc(
				{data, lists:append(lists:reverse([Line|BuffList]))}, 
				State#state{buffer=undefined});
		_Else -> {next_state, FSMState, State#state{buffer=[Line|BuffList]}, ?CFG(fsm_timeout)} end;

read_line(Line, #state{buffer=undefined} = State, FSMState, ParseFunc) when is_binary(Line) ->
	% case binary:last(Line) of % Erlang R14 bif
	case binary_last(Line) of
		$\n -> ?MODULE:ParseFunc({data, Line}, State);
		_Else -> {next_state, FSMState, State#state{buffer=[Line]}, ?CFG(fsm_timeout)} end;

read_line(Line, #state{buffer=BuffList} = State, FSMState, ParseFunc) when is_binary(Line) ->
	% case binary:last(Line) of % Erlang R14 bif
	case binary_last(Line) of
		$\n -> ?MODULE:ParseFunc(
				{data, list_to_binary(lists:reverse([Line|BuffList]))}, 
				State#state{buffer=undefined});
		_Else -> {next_state, FSMState, State#state{buffer=[Line|BuffList]}, ?CFG(fsm_timeout)} end.

icap_date_hdr_line() -> [<<"Date: ">>, httpd_util:rfc1123_date(), <<"\r\n">>].

concat_cache_data(StateName, CacheB, Bin) ->
	try <<CacheB/binary, Bin/binary>>
	catch Class:Error ->
		?ERROR_LOG("Error occured on ICAP FSM call ("?S") when caching. Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
			[StateName, Class, Error, erlang:get_stacktrace()]),
		Dummy = list_to_binary([CacheB]),
		<<Dummy/binary, Bin/binary>> end.

encap_pl_req(CacheDataReqH, CacheDataReqB) ->
	case CacheDataReqB of
		<<>> -> { [<<"Encapsulated: req-hdr=0">>], [CacheDataReqH] };
		_Else -> { [<<"Encapsulated: req-hdr=0, req-body=">>, 
			integer_to_list(size(CacheDataReqH))],
			[CacheDataReqH, CacheDataReqB] } end.

encap_pl_res(CacheDataResH, CacheDataResB) ->
	case CacheDataResB of
		<<>> -> { [<<"Encapsulated: res-hdr=0">>], [CacheDataResH] };
		_Else -> { [<<"Encapsulated: res-hdr=0, res-body=">>, 
			integer_to_list(size(CacheDataResH))],
			[CacheDataResH, CacheDataResB] } end.

get_host('CONNECT', Uri) -> mydlp_api:drop_host_port(Uri);
get_host(_Method, Uri) -> mydlp_api:get_host(Uri).


-endif.

