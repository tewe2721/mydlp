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
-module(mydlp_smtp_fsm).
-author('sjackson@simpleenigma.com').
-author('kerem@medratech.com').
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
    {ok, 'WAIT_FOR_SOCKET', #smtpd_fsm{}}.

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
	NextState = smtpd_cmd:command({greeting,IP},NewState),
    {next_state, 'WAIT_FOR_CMD', NextState, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    ?DEBUG("SMTP State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_CMD'({data, Data}, #smtpd_fsm{buff = Buff} = State) ->
	NewBuff = <<Buff/binary,Data/binary>>,
	case end_of_cmd(NewBuff) of
		0 -> {next_state, 'WAIT_FOR_CMD', State#smtpd_fsm{buff = NewBuff}, ?TIMEOUT};
		Pos -> 
			<<Line:Pos/binary,13,10,NextBuff/binary>> = NewBuff,
			NextState = smtpd_cmd:command(Line,State#smtpd_fsm{line = Line}),
			case NextState#smtpd_fsm.data of
				undefined -> {next_state, 'WAIT_FOR_CMD', NextState#smtpd_fsm{buff = NextBuff}, ?TIMEOUT};
				<<>> -> {next_state, 'WAIT_FOR_DATA', NextState#smtpd_fsm{buff = NextBuff}, ?TIMEOUT}
			end
			
	end;

'WAIT_FOR_CMD'(timeout, State) ->
    ?DEBUG("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #smtpd_fsm{buff = Buff} = State) ->
	NewBuff = <<Buff/binary,Data/binary>>,
	case end_of_data(NewBuff) of
		0 -> {next_state, 'WAIT_FOR_DATA', State#smtpd_fsm{buff = NewBuff}, ?TIMEOUT};
		Pos -> % wait for end of data and return data in state.
			<<Message:Pos/binary,13,10,46,13,10,NextBuff/binary>> = NewBuff,
			NewState = smtpd_cmd:read_message(Message,State),
			smtpd_cmd:send(NewState,250),
			'READ_FILES'(NewState#smtpd_fsm{buff=NextBuff})
	end;

'WAIT_FOR_DATA'(timeout, State) ->
    ?DEBUG("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State}.

'READ_FILES'(#smtpd_fsm{message_mime=MIME} = State) ->
	Files = mime_to_files(MIME),
	'REQ_OK'(State#smtpd_fsm{files=Files}).


% {Action, {{rule, Id}, {file, File}, {matcher, Func}, {misc, Misc}}}
'REQ_OK'(#smtpd_fsm{files=Files, message_record=#message{mail_from=MailFrom}} = State) ->
	case mydlp_acl:qu(list_to_binary([MailFrom]), dest, Files) of
		pass -> 'CONNECT_REMOTE'(connect, State);
		{quarantine, AclR} -> log_req(State, quarantine, AclR),
					mydlp_api:quarantine(Files),
					'BLOCK_REQ'(block, State);
		{block, AclR} -> log_req(State, block, AclR),
					'BLOCK_REQ'(block, State);
		{log, AclR} -> log_req(State, log, AclR),
					'CONNECT_REMOTE'(connect, State);
		{pass, AclR} -> log_req(State, pass, AclR),
					'CONNECT_REMOTE'(connect, State)
	end.

% refined this
'BLOCK_REQ'(block, #smtpd_fsm{message_record=MessageR} = State) ->
	MailFrom = MessageR#message.mail_from,
	RepMessage = #message{mail_from=MailFrom, 
			rcpt_to=MailFrom,
			message=
				"From: <" ++ MailFrom ++ ">\r\n" ++
				"To: <" ++ MailFrom ++ ">\r\n" ++
				"Content-Type: text/html; charset=\"utf8\"\r\n" ++
				"Content-Transfer-Encoding: base64\r\n" ++
				"Subject: Your e-mail to \'" ++ MessageR#message.rcpt_to ++ "\' have been denied!!!\r\n" ++
				"\r\n" ++
				"\r\n" ++
				mydlp_api:get_denied_page(html_base64_str)},
	mydlp_smtpc:mail(RepMessage),
	NextState = State#smtpd_fsm{cmd   = undefined,
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

'CONNECT_REMOTE'(connect, #smtpd_fsm{message_record=MessageR} = State) ->
	mydlp_smtpc:mail(MessageR),
	NextState = State#smtpd_fsm{cmd   = undefined,
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
handle_info({tcp, Socket, Bin}, StateName, #smtpd_fsm{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName, #smtpd_fsm{socket=Socket, addr=_Addr} = StateData) ->
%    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #smtpd_fsm{socket=Socket} = _State) ->
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

end_of_cmd(Bin) ->
	List = binary_to_list(Bin),
	case string:str(List,?CRLF) of
		0 -> 0;
		Pos -> Pos - 1
	end.

end_of_data(Bin) ->
	List = binary_to_list(Bin),
	case string:str(List,?SMTP_DATA_END) of
		0 -> 0;
		Pos -> Pos - 1
	end.

heads_to_file(Headers) -> heads_to_file(Headers, #file{}).

heads_to_file([{'content-disposition', "inline"}|Rest], #file{filename=undefined, name=undefined} = File) ->
	case lists:keysearch('content-type',1,Rest) of
		{value,{'content-type',"text/plain"}} -> heads_to_file(Rest, File#file{name="Inline text message"});
		{value,{'content-type',"text/html"}} -> heads_to_file(Rest, File#file{name="Inline HTML message"});
		_Else -> heads_to_file(Rest, File)
	end;
heads_to_file([{'content-disposition', CD}|Rest], #file{filename=undefined} = File) ->
	case string:str(CD, "filename=") of
		0 -> heads_to_file(Rest, File);
		I when is_integer(I) -> 
			FN = case string:strip(string:substr(CD, I + 9)) of
				"\\\"" ++ Str -> 
					Len = string:len(Str),
					"\"\\" = string:substr(Str, Len - 1),
					string:substr(Str, 1, Len - 2);
				"\"" ++ Str -> 
					Len = string:len(Str),
					"\"" = string:substr(Str, Len),
					string:substr(Str, 1, Len - 1);
				Str -> Str
			end,
			heads_to_file(Rest, File#file{filename=FN})
	end;
heads_to_file([{'content-type', "text/html"}|Rest], #file{filename=undefined, name=undefined} = File) ->
	case lists:keysearch('content-disposition',1,Rest) of
		{value,{'content-disposition',"inline"}} -> heads_to_file(Rest, File#file{name="Inline HTML message"});
		_Else -> heads_to_file(Rest, File)
	end;
heads_to_file([{'content-type', "text/plain"}|Rest], #file{filename=undefined, name=undefined} = File) ->
	case lists:keysearch('content-disposition',1,Rest) of
		{value,{'content-disposition',"inline"}} -> heads_to_file(Rest, File#file{name="Inline text message"});
		_Else -> heads_to_file(Rest, File)
	end;
heads_to_file([{'content-type', CT}|Rest], #file{filename=undefined} = F) ->
	GT = case string:chr(CT, $;) of
		0 -> CT;
		I when is_integer(I) -> string:substr(CT, 1, I - 1)
	end,
	File = F#file{given_type=GT},
	case string:str(CT, "name=") of
		0 -> heads_to_file(Rest, File);
		I2 when is_integer(I2) -> 
			FN = case string:strip(string:substr(CT, I2 + 5)) of
				"\\\"" ++ Str -> 
					Len = string:len(Str),
					"\"\\" = string:substr(Str, Len - 1),
					string:substr(Str, 1, Len - 2);
				"\"" ++ Str -> 
					Len = string:len(Str),
					"\"" = string:substr(Str, Len),
					string:substr(Str, 1, Len - 1);
				Str -> Str
			end,
			heads_to_file(Rest, File#file{filename=FN})
	end;
heads_to_file([{'content-id', CI}|Rest], #file{filename=undefined, name=undefined} = File) ->
	heads_to_file(Rest, File#file{name=CI});
heads_to_file([{subject, Subject}|Rest], #file{filename=undefined, name=undefined} = File) ->
	heads_to_file(Rest, File#file{name="Mail: " ++ Subject});
heads_to_file([_|Rest], File) ->
	ignore,	heads_to_file(Rest, File);
heads_to_file([], File) -> File.

mime_to_files(Mime) -> mime_to_files([Mime], []).

mime_to_files([#mime{content=Content, header=Headers, body=Body}|Rest], Acc) ->
	File = heads_to_file(Headers),
	CTE = case lists:keysearch('content-transfer-encoding',1,Headers) of
		{value,{'content-transfer-encoding',Value}} -> Value;
		_ -> '7bit'
	end,
	Data = mime_util:decode_content(CTE, Content),
	mime_to_files(lists:append(Body, Rest), [File#file{data=Data}|Acc]);
mime_to_files([], Acc) -> lists:reverse(Acc).

log_req(#smtpd_fsm{message_record=MessageR}, Action,
                {{rule, RuleId}, {file, File}, {matcher, Matcher}, {misc, Misc}}) ->
	Src = MessageR#message.mail_from,
	DestList = ["rcpt to: <" ++ MessageR#message.rcpt_to ++ ">"] ++ 
			["to: <" ++ A#addr.username ++ "@" ++ A#addr.domainname ++ ">"|| A <- MessageR#message.to] ++
			["cc: <" ++ A#addr.username ++ "@" ++ A#addr.domainname ++ ">"|| A <- MessageR#message.cc] ++
			["bcc: <" ++ A#addr.username ++ "@" ++ A#addr.domainname ++ ">"|| A <- MessageR#message.bcc],
	Dest = string:join(DestList, ", "),
        ?ACL_LOG(smtp, RuleId, Action, nil, Src, Dest, Matcher, File, Misc).

