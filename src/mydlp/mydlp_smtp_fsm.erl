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

-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2,
    'WAIT_FOR_CMD'/2
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

	EFA = ?CFG(smtp_enable_for_all),
	BOF = ?CFG(smtp_bypass_on_fail),

	{ok, 'WAIT_FOR_SOCKET', #smtpd_fsm{enable_for_all=EFA, bypass_on_fail=BOF}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket, _CommType}, State) when is_port(Socket) ->
	% Now we own the socket
	inet:setopts(Socket, [{active, once}, binary]),
	{ok, {IP, _Port}} = inet:peername(Socket),
%	{ok,DNSBL} = erlmail_antispam:dnsbl(IP),
%	?D({relay,IP,smtpd_queue:checkip(IP)}),
%	NewState = State#smtpd_fsm{socket=Socket, addr=IP, options = DNSBL, relay = smtpd_queue:checkip(IP)},
	NewState = State#smtpd_fsm{socket=Socket, addr=IP, relay = true},
	?SMTP_LOG(connect, IP),
	NextState = smtpd_cmd:command({greeting,IP},NewState),
	{next_state, 'WAIT_FOR_CMD', NextState, ?CFG(fsm_timeout)};
'WAIT_FOR_SOCKET'(Other, State) ->
	?DEBUG("SMTP State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_CMD'({data, Data}, #smtpd_fsm{buff = Buff} = State) ->
	NewBuff = <<Buff/binary,Data/binary>>,
	case end_of_cmd(NewBuff) of
		0 -> {next_state, 'WAIT_FOR_CMD', State#smtpd_fsm{buff = NewBuff}, ?CFG(fsm_timeout)};
		Pos -> 
			<<Line:Pos/binary,13,10,NextBuff/binary>> = NewBuff,
			NextState = smtpd_cmd:command(Line,State#smtpd_fsm{line = Line}),
			case NextState#smtpd_fsm.data of
				undefined -> {next_state, 'WAIT_FOR_CMD', NextState#smtpd_fsm{buff = NextBuff}, ?CFG(fsm_timeout)};
				<<>> -> {next_state, 'WAIT_FOR_DATA', NextState#smtpd_fsm{buff = NextBuff}, ?CFG(fsm_timeout)}
			end
			
	end;

'WAIT_FOR_CMD'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #smtpd_fsm{buff = Buff} = State) ->
	NewBuff = <<Buff/binary,Data/binary>>,
	case end_of_data(NewBuff) of
		0 -> {next_state, 'WAIT_FOR_DATA', State#smtpd_fsm{buff = NewBuff}, ?CFG(fsm_timeout)};
		Pos -> % wait for end of data and return data in state.
			<<Message:Pos/binary,13,10,46,13,10,NextBuff/binary>> = NewBuff,
			gen_fsm:send_all_state_event(self(), ok),
			{next_state, 'PROCESS_DATA', State#smtpd_fsm{message_bin=Message, buff=NextBuff}, ?CFG(fsm_timeout)} end;

'WAIT_FOR_DATA'(timeout, State) ->
	?DEBUG("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State}.

'PROCESS_DATA'(ok, #smtpd_fsm{message_bin=Message} = State) ->
	NewState = smtpd_cmd:read_message(Message,State),
	{ok, Ref} = mydlp_spool:push("mail", NewState#smtpd_fsm.message_record),
	smtpd_cmd:send(NewState,250),
	?SMTP_LOG(received, NewState#smtpd_fsm.message_record),
	'READ_FILES'(NewState#smtpd_fsm{spool_ref=Ref}).

'READ_FILES'(#smtpd_fsm{message_mime=MIME} = State) ->
	Files = mydlp_api:mime_to_files(MIME),
	'REQ_OK'(State#smtpd_fsm{files=Files}).

% {Action, {{rule, Id}, {file, File}, {matcher, Func}, {misc, Misc}}}
'REQ_OK'(#smtpd_fsm{enable_for_all=true, files=Files, message_record=MessageR} = State) ->
	AclRet = mydlp_acl:qa(get_dest_domains(MessageR), Files),
	process_aclret(AclRet, State);
'REQ_OK'(#smtpd_fsm{enable_for_all=false, files=Files, 
		message_record=(#message{mail_from=MailFrom} = MessageR)} = State) ->
	AclRet = mydlp_acl:qu(list_to_binary([MailFrom]), get_dest_domains(MessageR), Files),
	process_aclret(AclRet, State).

process_aclret(AclRet, #smtpd_fsm{files=Files} = State) ->
	case case AclRet of
		pass -> {pass, mydlp_api:empty_aclr(Files)};
		log -> {log, mydlp_api:empty_aclr(Files)};
		archive -> {archive, mydlp_api:empty_aclr(Files)};
		block -> {block, mydlp_api:empty_aclr(Files)};
		quarantine -> {quanratine, mydlp_api:empty_aclr(Files)};
		{pass, _AR} = T -> T;
		{log, _AR} = T -> T;
		{archive, _AR} = T -> T;
		{block, _AR} = T -> T;
		{quarantine, _AR} = T -> T
	end of
		{pass, _AclR} ->	mydlp_api:clean_files(Files),
					'CONNECT_REMOTE'(connect, State);
		{log, AclR} -> log_req(State, log, AclR),
					mydlp_api:clean_files(Files),
					'CONNECT_REMOTE'(connect, State);
		{archive, AclR} -> archive_req(State, AclR, Files),
					% mydlp_archive will clean files.
					'CONNECT_REMOTE'(connect, State);
		{block, AclR} -> log_req(State, block, AclR),
					mydlp_api:clean_files(Files),
					'BLOCK_REQ'(block, State);
		{quarantine, AclR} -> log_req(State, quarantine, AclR),
					mydlp_api:clean_files(Files),
					'BLOCK_REQ'(block, State)
	end.

archive_req(State, {{rule, RId}, {file, _}, {matcher, _}, {misc, _}}, Files) ->
	case Files of
		[] -> ok;
		_Else -> log_req(State, archive, {{rule, RId}, {file, Files}, {matcher, none}, {misc,""}}) end.

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
handle_event(ok, 'PROCESS_DATA' = StateName, StateData) ->
	fsm_call(StateName, ok, StateData);

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
		?ERROR_LOG("Error occured on FSM ("?S") call ("?S"). Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[?MODULE, StateName, Class, Error, erlang:get_stacktrace()]),
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

end_of_cmd(Bin) ->
	% Refine this with compiled version for %20 performance improvement
	case re:run(Bin, ?CRLF_BIN, [{capture,first}]) of
		{match,[{Pos,2}]} -> Pos;
		nomatch -> 0 end.

end_of_data(Bin) ->
	% Refine this with compiled version for %20 performance improvement
	case re:run(Bin, ?SMTP_DATA_END_REGEX, [{capture,first}]) of
		{match,[{Pos,5}]} -> Pos;
		nomatch -> 0 end.

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
                {{rule, RuleId}, {file, File}, {matcher, Matcher}, {misc, Misc}}) ->
	Src = get_from(MessageR),
	Dest = get_dest_addresses(MessageR),
        ?ACL_LOG(smtp, RuleId, Action, nil, Src, Dest, Matcher, File, Misc).

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
create_smtp_msg(Type, Param) ->
	{
		"Type=~w Param=~w",
		[Type, Param]
	}.

smtp_msg(Type, Param) ->
	{Format, Args} = create_smtp_msg(Type, Param),
	Format1 = "PID=~w " ++ Format ++ "~n",
	Args1 = [self() | Args],
	mydlp_logger:notify(smtp_msg, Format1, Args1).

-include_lib("eunit/include/eunit.hrl").

end_of_cmd_test_() -> [
	?_assertEqual(12, end_of_cmd(<<"hello world!\r\nhello">>)),
	?_assertEqual(7, end_of_cmd(<<"goodbye\r\nbye\r\nbye">>)),
	?_assertEqual(0, end_of_cmd(<<"humbara rumbara rumbamba!!!">>))
	].

end_of_data_test_() -> [
	?_assertEqual(12, end_of_data(<<"hello world!\r\n.\r\nhello">>)),
	?_assertEqual(13, end_of_data(<<"hello\r\nworld!\r\n.\r\nhello">>)),
	?_assertEqual(13, end_of_data(<<"hello\r\nw\r\nld!\r\n.\r\nhello">>)),
	?_assertEqual(7, end_of_data(<<"goodbye\r\n.\r\nbye\r\n.\r\nbye">>)),
	?_assertEqual(0, end_of_data(<<"humbara\r\nrumbara\r\nrumbamba!!!">>)),
	?_assertEqual(0, end_of_data(<<"humbara rumbara rumbamba!!!">>))
	].

-endif.

