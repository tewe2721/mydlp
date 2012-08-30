%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       SMTP Server FSM for ErlMail
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.3
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stuart Jackson, Simple Enigma, Inc. All Righs Reserved
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%
%%%---------------------------------------------------------------------------------------

-ifdef(__MYDLP_NETWORK).

-module(mydlp_smtp_fsm).
-author('sjackson@simpleenigma.com').
-author('kerem@mydlp.com').
-behaviour(gen_fsm).
-include("mydlp_smtp.hrl").

-export([start_link/0,
	requeue_msg/1
	]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2,
    'PARSE_DATA'/2,
    'WAIT_FOR_CMD'/2,
    'PARSE_CMD'/2
]).

-compile(export_all).

-define(SMTP_LOG(Type, Param), smtp_msg(Type,Param)).


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

	BOF = ?CFG(smtp_bypass_on_fail),

	{ok, 'WAIT_FOR_SOCKET', #smtpd_fsm{bypass_on_fail=BOF}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket, _CommType}, State) when is_port(Socket) ->
	% Now we own the socket
	inet:setopts(Socket, [{active, once}, {packet, line}, binary]),
	{ok, {IP, _Port}} = inet:peername(Socket),
%	{ok,DNSBL} = erlmail_antispam:dnsbl(IP),
%	NewState = State#smtpd_fsm{socket=Socket, addr=IP, options = DNSBL, relay = smtpd_queue:checkip(IP)},
	NewState = State#smtpd_fsm{socket=Socket, addr=IP, relay = true},
	?SMTP_LOG(connect, IP),
	NextState = smtpd_cmd:command({greeting,IP},NewState),
	{next_state, 'WAIT_FOR_CMD', NextState, ?CFG(fsm_timeout)};
'WAIT_FOR_SOCKET'(Other, State) ->
	?DEBUG("SMTP State: 'WAIT_FOR_SOCKET'. Unexpected message: "?S, [Other]),
	%% Allow to receive async messages
	{next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_CMD'({data, Data}, State) -> read_line(Data, State, 'WAIT_FOR_CMD', 'PARSE_CMD');

'WAIT_FOR_CMD'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'PARSE_CMD'({data, Data}, State) ->
	LineSize = size(Data) - 2,
	<<Line:LineSize/binary,13,10>> = Data,
	NextState = smtpd_cmd:command(Line, State#smtpd_fsm{line = Line}),
	case NextState#smtpd_fsm.data of
		undefined -> {next_state, 'WAIT_FOR_CMD', NextState, ?CFG(fsm_timeout)};
		<<>> -> {next_state, 'WAIT_FOR_DATA', NextState, ?CFG(fsm_timeout)}
	end.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, State) -> read_line(Data, State, 'WAIT_FOR_DATA', 'PARSE_DATA');

'WAIT_FOR_DATA'(timeout, State) ->
	?DEBUG(?S" Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'PARSE_DATA'({data, Line}, #smtpd_fsm{buff = Buff} = State) ->
	NewBuff = <<Buff/binary, Line/binary>>,
	NextState = State#smtpd_fsm{buff = NewBuff},
	case Line of
		<<46,13,10>> -> % .CRLF
			Pos = size(NewBuff) - 5,
			case NewBuff of
				<<Message:Pos/binary,13,10,46,13,10>> -> % CRLF.CRLF smtpd data end.
					NextState1 = State#smtpd_fsm{message_bin=Message, buff= <<>>},
					'PROCESS_DATA'(ok, NextState1);
				_Else2 -> {next_state, 'WAIT_FOR_DATA', NextState, ?CFG(fsm_timeout)} end;
		_Else -> {next_state, 'WAIT_FOR_DATA', NextState, ?CFG(fsm_timeout)} end.

'PROCESS_DATA'(ok, #smtpd_fsm{message_bin=Message} = State) ->
	NewState = smtpd_cmd:read_message(Message,State),
	{ok, Ref} = mydlp_spool:push("smtp", NewState#smtpd_fsm.message_record),
	mydlp_spool:lock(Ref),
	smtpd_cmd:send(NewState,250),
	?SMTP_LOG(received, NewState#smtpd_fsm.message_record),
	'READ_FILES'(NewState#smtpd_fsm{spool_ref=Ref}).

'READ_FILES'(#smtpd_fsm{message_mime=MIME} = State) ->
	Files = mydlp_api:mime_to_files(MIME),
	'REQ_OK'(State#smtpd_fsm{files=Files}).

% {Action, {{rule, Id}, {file, File}, {matcher, Func}, {misc, Misc}}}
'REQ_OK'(#smtpd_fsm{files=Files, message_record=(#message{mail_from=MailFrom} = MessageR)} = State) ->
	UserH = mydlp_api:hash_un(MailFrom),
	Destinations = get_dest_domains(MessageR),
	AclQ = #aclq{channel=mail, src_user_h=UserH, destinations=Destinations},
	AclRet = mydlp_acl:q(AclQ, Files),
	process_aclret(AclRet, State).

process_aclret(AclRet, #smtpd_fsm{files=Files} = State) ->
	case case AclRet of
		pass -> {pass, mydlp_api:empty_aclr(Files)};
		log -> {log, mydlp_api:empty_aclr(Files)};
		archive -> {archive, mydlp_api:empty_aclr(Files)};
		block -> {block, mydlp_api:empty_aclr(Files)};
		quarantine -> {quarantine, mydlp_api:empty_aclr(Files)};
		{pass, _AR} = T -> T;
		{log, _AR} = T -> T;
		{archive, _AR} = T -> T;
		{block, _AR} = T -> T;
		{quarantine, _AR} = T -> T
	end of
		{pass, AclR} ->		post_query(State, AclR, Files),
					'CONNECT_REMOTE'(connect, State);
		{log, AclR} -> 		log_req(State, log, AclR),
					post_query(State, AclR, Files),
					'CONNECT_REMOTE'(connect, State);
		{archive, AclR} -> 	archive_req(State, AclR, Files),
					% mydlp_incident will clean files.
					'CONNECT_REMOTE'(connect, State);
		{block, AclR} -> 	log_req(State, block, AclR),
					post_query(State, AclR, Files),
					'BLOCK_REQ'(block, State);
		{quarantine, AclR} -> 	log_req(State, quarantine, AclR),
					post_query(State, AclR, Files),
					'BLOCK_REQ'(block, State)
	end.

post_query(State, AclR, Files) ->
	case ?CFG(mail_archive) of
		true -> archive_req(State, AclR, Files);
			% mydlp_incident will clean files.
		false -> mydlp_api:clean_files(Files) end.

archive_req(State, {{rule, RId}, {file, _}, {itype, IType}, {misc, Misc}}, Files) ->
	case Files of
		[] -> ok;
		_Else -> log_req(State, archive, {{rule, RId}, {file, Files}, {itype, IType}, {misc, Misc}}) end.

% refined this
'BLOCK_REQ'(block, #smtpd_fsm{spool_ref=Ref, message_record=MessageR} = State) ->
	MailFrom = MessageR#message.mail_from,
	RepMessage = #message{mail_from=MailFrom, 
			rcpt_to=MailFrom,
			message=
				"From: <" ++ MailFrom ++ ">\r\n" ++
				"To: <" ++ MailFrom ++ ">\r\n" ++
				"Content-Type: text/html; charset=\"utf8\"\r\n" ++
				"Content-Transfer-Encoding: base64\r\n" ++
				"Subject: Your e-mail to \'" ++ MessageR#message.rcpt_to ++ "\' has been denied!!!\r\n" ++
				"\r\n" ++
				"\r\n" ++
				mydlp_api:get_denied_page(html_base64_str)},
	mydlp_smtpc:mail(Ref, RepMessage),
	?SMTP_LOG(sent_deny, MessageR),
	NextState = reset_statedata(State),
	{next_state, 'WAIT_FOR_CMD', NextState, ?CFG(fsm_timeout)}.

'CONNECT_REMOTE'(connect, #smtpd_fsm{spool_ref=Ref, message_record=MessageR} = State) ->
	mydlp_smtpc:mail(Ref, MessageR),
	?SMTP_LOG(sent_ok, MessageR),
	NextState = reset_statedata(State),
	{next_state, 'WAIT_FOR_CMD', NextState, ?CFG(fsm_timeout)}.

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
handle_info({tcp, Socket, Bin}, StateName, #smtpd_fsm{socket=Socket} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	fsm_call(StateName, {data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName, #smtpd_fsm{socket=Socket} = StateData) ->
	{stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
	{noreply, StateName, StateData}.

fsm_call(StateName, Args, StateData) -> 
	try ?MODULE:StateName(Args, StateData)
	catch Class:Error ->
		?ERROR_LOG("Error occured on FSM ("?S") call ("?S"). Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~nState: "?S"~n",
				[?MODULE, StateName, Class, Error, erlang:get_stacktrace(), StateData]),
		(catch case is_bypassable(StateData) of
			true -> deliver_raw(StateData),
				NextStateData = reset_statedata(StateData),
				{next_state, 'WAIT_FOR_CMD', NextStateData, ?CFG(fsm_timeout)};
			false -> {stop, normal, StateData} end) end.

is_bypassable(#smtpd_fsm{bypass_on_fail=false}) -> false;
is_bypassable(#smtpd_fsm{bypass_on_fail=undefined}) -> false;
is_bypassable(#smtpd_fsm{mail=undefined}) -> false;
is_bypassable(#smtpd_fsm{rcpt=undefined}) -> false;
is_bypassable(#smtpd_fsm{message_bin=undefined}) -> false;
is_bypassable(#smtpd_fsm{}) -> true.

deliver_raw(#smtpd_fsm{mail=From, rcpt=Rcpt, message_bin=MessageS}) -> 
	mydlp_smtpc:mail(From, Rcpt, MessageS).

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #smtpd_fsm{socket=Socket, addr=Addr} = _State) ->
	?SMTP_LOG(disconnect, Addr),
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

reset_statedata(#smtpd_fsm{} = State) ->
	State#smtpd_fsm{cmd   = undefined,
			param = undefined,
			mail  = undefined,
			rcpt  = undefined,
			to    = undefined,
			messagename = undefined,
			message_record = undefined,
		        message_mime = undefined,
		        message_bin = undefined,
		        lbuff = undefined,
		        files       = [],
			data  = undefined}.

get_from(MessageR) -> MessageR#message.mail_from.

get_dest_addresses(MessageR) ->
	DestList = ["rcpt to: <" ++ MessageR#message.rcpt_to ++ ">"] ++ 
		["to: <" ++ A#addr.username ++ "@" ++ A#addr.domainname ++ ">"|| A <- MessageR#message.to] ++
		["cc: <" ++ A#addr.username ++ "@" ++ A#addr.domainname ++ ">"|| A <- MessageR#message.cc] ++
		["bcc: <" ++ A#addr.username ++ "@" ++ A#addr.domainname ++ ">"|| A <- MessageR#message.bcc],
	string:join(DestList, ", ").

log_req(#smtpd_fsm{message_record=MessageR}, Action,
                {{rule, RuleId}, {file, File}, {itype, IType}, {misc, Misc}}) ->
	Src = get_from(MessageR),
	Dest = {MessageR#message.rcpt_to, get_dest_addresses(MessageR)},
	Time = erlang:universaltime(),
	Payload = case Action of
		quarantine -> MessageR;
		_Else -> none end,
        ?ACL_LOG_P(Time, mail, RuleId, Action, nil, Src, Dest, IType, File, Misc, Payload).

get_dest_domains(#message{rcpt_to=RcptTo, to=ToH, cc=CCH, bcc=BCCH})->
	RcptToA = lists:map(fun(S) -> mime_util:dec_addr(S) end, RcptTo),
	DestList = RcptToA ++ ToH ++ CCH ++ BCCH,
	Domains = [list_to_binary(A#addr.domainname) || A <- DestList],
	lists:usort(Domains).

create_smtp_msg(connect, {Ip1,Ip2,Ip3,Ip4}) ->
	{
		"Connected to ~w.~w.~w.~w .",
		[Ip1,Ip2,Ip3,Ip4]
	};
create_smtp_msg(received, MessageR) ->
	From = get_from(MessageR),
	ToList = get_dest_addresses(MessageR),
	{
		"Recieved mail. FROM=~s TO='~s'",
		[From, ToList]
	};
create_smtp_msg(sent_ok, MessageR) ->
	From = get_from(MessageR),
	ToList = get_dest_addresses(MessageR),
	{
		"Transferred clean message to queue. FROM=~s TO='~s' ",
		[From, ToList]
	};
create_smtp_msg(sent_deny, MessageR) ->
	From = get_from(MessageR),
	{
		"Transfer deny message to queue. TO=~s ",
		[From]
	};
create_smtp_msg(disconnect, {Ip1,Ip2,Ip3,Ip4}) ->
	{
		"Disconnected from ~w.~w.~w.~w .",
		[Ip1,Ip2,Ip3,Ip4]
	};
create_smtp_msg(requeue_ok, MessageR) ->
	From = get_from(MessageR),
	ToList = get_dest_addresses(MessageR),
	{
		"Requeued message to send original destination. FROM=~s TO='~s' ",
		[From, ToList]
	};
create_smtp_msg(Type, Param) ->
	{
		"Type=~w Param=~w",
		[Type, Param]
	}.

requeue_msg(MessageR) -> ?SMTP_LOG(requeue_ok, MessageR).

smtp_msg(Type, Param) ->
	{Format, Args} = create_smtp_msg(Type, Param),
	Format1 = "PID=~w " ++ Format ++ "~n",
	Args1 = [self() | Args],
	mydlp_logger:notify(smtp_msg, Format1, Args1).

read_line(Line, #smtpd_fsm{lbuff=undefined} = State, FSMState, ParseFunc) when is_list(Line) ->
        case lists:last(Line) of
                $\n -> ?MODULE:ParseFunc({data, Line}, State);
                _Else -> {next_smtpd_fsm, FSMState, State#smtpd_fsm{lbuff=[Line]}, ?CFG(fsm_timeout)} end;

read_line(Line, #smtpd_fsm{lbuff=BuffList} = State, FSMState, ParseFunc) when is_list(Line) ->
        case lists:last(Line) of
                $\n -> ?MODULE:ParseFunc(
                                {data, lists:append(lists:reverse([Line|BuffList]))},
                                State#smtpd_fsm{lbuff=undefined});
                _Else -> {next_smtpd_fsm, FSMState, State#smtpd_fsm{lbuff=[Line|BuffList]}, ?CFG(fsm_timeout)} end;

read_line(Line, #smtpd_fsm{lbuff=undefined} = State, FSMState, ParseFunc) when is_binary(Line) ->
        % case binary:last(Line) of % Erlang R14 bif
        case binary_last(Line) of
                $\n -> ?MODULE:ParseFunc({data, Line}, State);
                _Else -> {next_smtpd_fsm, FSMState, State#smtpd_fsm{lbuff=[Line]}, ?CFG(fsm_timeout)} end;

read_line(Line, #smtpd_fsm{lbuff=BuffList} = State, FSMState, ParseFunc) when is_binary(Line) ->
	% case binary:last(Line) of % Erlang R14 bif
	case binary_last(Line) of
		$\n -> ?MODULE:ParseFunc(
				{data, list_to_binary(lists:reverse([Line|BuffList]))},
				State#smtpd_fsm{lbuff=undefined});
		_Else -> {next_smtpd_fsm, FSMState, State#smtpd_fsm{lbuff=[Line|BuffList]}, ?CFG(fsm_timeout)} end.

binary_last(Bin) when is_binary(Bin) ->
	JunkSize = size(Bin) - 1,
	<<_Junk:JunkSize/binary, Last/integer>> = Bin,
	Last.

-include_lib("eunit/include/eunit.hrl").

-endif.

