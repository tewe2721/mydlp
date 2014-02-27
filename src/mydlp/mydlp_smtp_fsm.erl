%clq%%---------------------------------------------------------------------------------------
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
	case mydlp_mysql:get_progress() of
		compile ->
			?SMTP_LOG(not_ready, IP),
			NextState = smtpd_cmd:command(not_ready,NewState),
			{stop, normal, NextState};
		_ ->	?SMTP_LOG(connect, IP),
			NextState = smtpd_cmd:command({greeting,IP},NewState),
			{next_state, 'WAIT_FOR_CMD', NextState, ?CFG(fsm_timeout)} end;
			
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
	smtpd_cmd:send(NewState,250),
	?SMTP_LOG(received, NewState#smtpd_fsm.message_record),
	'READ_FILES'(NewState#smtpd_fsm{spool_ref=Ref}).

'READ_FILES'(#smtpd_fsm{message_mime=MIME} = State) ->
	Files = mydlp_api:mime_to_files(MIME),
	Files1 = mydlp_api:drop_duplicate_files(Files),
	'REQ_OK'(State#smtpd_fsm{files=Files1}).

% {Action, {{rule, Id}, {file, File}, {matcher, Func}, {misc, Misc}, {matching_details, MatchingDetails}}}
'REQ_OK'(#smtpd_fsm{files=Files, message_record=(#message{mail_from=MailFrom} = MessageR)} = State) ->
	IsRegistered = mydlp_mnesia:add_email_address_to_license(MailFrom),

	S = case IsRegistered of 
		true -> true;
		false -> case mydlp_license:is_acceptable() of
			true -> mydlp_mnesia:set_email_as_registered(MailFrom), true;
			false -> false end end,

	case S of
		false -> process_aclret(pass, State);
		true -> 

	SrcDomainName = get_from_domainname(MessageR),
	UserH = mydlp_api:hash_un(MailFrom),
	DestinationDomains = get_dest_domains(MessageR),
	DestinationUserHashes = get_dest_user_hashes(MessageR),
	Destinations = lists:usort(lists:append(DestinationDomains, DestinationUserHashes)),
	HasBCC = has_bcc(MessageR),

	OrigFilesCopy = mydlp_api:reconstruct_crs(Files),

	OrigFilesCopy1 = lists:map(fun(File) -> mydlp_api:sizefy(File) end, OrigFilesCopy),
	OrigFilesCopy2 = mydlp_api:hashify_files(OrigFilesCopy1),

	State1 = State#smtpd_fsm{files=OrigFilesCopy2},

	pre_query(State1, Files),

	AclQ = #aclq{channel=mail, src_domain = SrcDomainName, src_user_h=UserH, destinations=Destinations, has_hidden_destinations=HasBCC},
	AclRet = mydlp_acl:q(AclQ, Files),
	process_aclret(AclRet, State1) end.

process_aclret(AclRet, #smtpd_fsm{files=Files} = State) ->
	case case AclRet of
		pass -> {pass, mydlp_api:empty_aclr(Files)};
		log -> {log, mydlp_api:empty_aclr(Files)};
		archive -> {archive, mydlp_api:empty_aclr(Files)};
		block -> {block, mydlp_api:empty_aclr(Files)};
		quarantine -> {quarantine, mydlp_api:empty_aclr(Files)};
		{custom, _} = CA -> {CA, mydlp_api:empty_aclr(Files)};
		{pass, _AR} = T -> T;
		{log, _AR} = T -> T;
		{archive, _AR} = T -> T;
		{block, _AR} = T -> T;
		{quarantine, _AR} = T -> T;
		{{custom, _CD}, _AR} = T -> T
	end of
		{pass, _AclR} ->	'CONNECT_REMOTE'(connect, State);
		{log, AclR} -> 		log_req(State, log, AclR),
					'CONNECT_REMOTE'(connect, State);
		{archive, AclR} -> 	log_req(State, archive, AclR),
					'CONNECT_REMOTE'(connect, State);
		{block, AclR} -> 	log_req(State, block, AclR),
					'BLOCK_REQ'(block, State, AclR);
		{quarantine, AclR} -> 	log_req(State, quarantine, AclR),
					'BLOCK_REQ'(block, State, AclR);
		{{custom, {Type, PrimAction, _Name, Param}} = CustomAction, AclR} ->
					{Message, NewFiles} = execute_custom_action(Type, Param, AclR, Files),
					log_req(State, CustomAction, AclR, Message),
					MessageR = State#smtpd_fsm.message_record,
					NewMessage = mydlp_api:files_to_mime_encoded(NewFiles),
					State1 = State#smtpd_fsm{message_record=MessageR#message{message=NewMessage}},
					case PrimAction of
						pass -> 'CONNECT_REMOTE'(connect, State1);
						block -> 'BLOCK_REQ'(block, State1, AclR) end
	end.

execute_custom_action(seclore, {HotFolderId, ActivityComments}, {_, {file, #file{dataref=DRef}}, _, _}, Files) ->
	execute_custom_action(seclore, {HotFolderId, ActivityComments}, DRef, Files, none, []).

execute_custom_action(seclore, {HotFolderId, ActivityComments}, DRef, [#file{dataref=DRef} = F|RestOfFiles], Message, FilesAcc) ->
	{M, NewFile} = seclore_protect_file(F, HotFolderId, ActivityComments),
	Message1 = case M of
		none -> Message;
		_ -> M end,
	execute_custom_action(seclore, {HotFolderId, ActivityComments}, DRef, RestOfFiles, Message1, [NewFile|FilesAcc]);
execute_custom_action(seclore, Param, DRef, [F|RestOfFiles], Message, FilesAcc) ->
	execute_custom_action(seclore, Param, DRef, RestOfFiles, Message, [F|FilesAcc]);
execute_custom_action(seclore, _Param, _DRef, [], Message, FilesAcc) -> {Message, lists:reverse(FilesAcc)}.
	
seclore_protect_file(#file{filename=Filename, given_type=GT} = File, HotFolderId, ActivityComments) ->
	File1 = mydlp_api:load_file(File),
	{ok, TempDir} = mydlp_api:mktempdir(),
	FN = case {Filename, GT} of
		{undefined, undefined} -> "mail_content.txt";
		{undefined, "text/plain"} -> "mail_content.txt";
		{undefined, "text/html"} -> "mail_content.html";
		{"", undefined} -> "mail_content.txt";
		{"", "text/plain"} -> "mail_content.txt";
		{"", "text/html"} -> "mail_content.html";
		_ -> Filename end,
	File2 = File1#file{filename=FN},
	FilePath = filename:absname(FN, TempDir),
	ok = file:write_file(FilePath, File1#file.data),
	FPRet = case unicode:characters_to_binary(FilePath) of 
			Bin when is_binary(Bin) -> {ok, Bin};
			_ -> {error, "mydlp.error.canNotEncodeFilePathAsUnicode"} end,
	Message = case FPRet of
		{ok, FPB} -> case mydlp_tc:seclore_protect(FPB, HotFolderId, ActivityComments) of
				<<"ok ", Rest/binary>> -> <<"seclore.fileId ", Rest/binary>>;
				"ok" -> none;
				<<"ok">> -> none;
				Else -> Else end;
		{error, M} -> M end,
	{ok, NewData} = file:read_file(FilePath),
	File3 = mydlp_api:remove_all_data(File2),
	mydlp_api:rmrf_dir(TempDir),
	{Message, ?BF_C(File3, NewData)}.

create_redundant_dataref(File) ->
	File1 = mydlp_api:load_file(File),
	Data = File1#file.data,
	?BF_C(File1#file{data=undefined, dataref=undefined}, Data).

pre_query(State, Files) ->
	case ?CFG(mail_archive) of
		true ->	Files1 = lists:map(fun(F) -> create_redundant_dataref(F) end, Files),
			log_req(State, archive, mydlp_api:empty_aclr(Files1, mail_archive));
		false -> ok end.

% refined this
'BLOCK_REQ'(block, #smtpd_fsm{spool_ref=Ref, message_record=MessageR} = State, {{rule, OrigRuleId}, _, _, _}) ->
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
				mydlp_api:get_denied_page(OrigRuleId, html_base64_str)},
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
				?SMTP_LOG(bypassed, StateData#smtpd_fsm.message_record),
				NextStateData = reset_statedata(StateData),
				{next_state, 'WAIT_FOR_CMD', NextStateData, ?CFG(fsm_timeout)};
			false -> {stop, normal, StateData} end) end.

is_bypassable(#smtpd_fsm{bypass_on_fail=false}) -> false;
is_bypassable(#smtpd_fsm{bypass_on_fail=undefined}) -> false;
is_bypassable(#smtpd_fsm{mail=undefined}) -> false;
is_bypassable(#smtpd_fsm{rcpt=undefined}) -> false;
is_bypassable(#smtpd_fsm{message_bin=undefined}) -> false;
is_bypassable(#smtpd_fsm{}) -> true.

deliver_raw(#smtpd_fsm{mail=From, rcpt=Rcpt, message_bin=MessageS, spool_ref=undefined}) -> 
	mydlp_smtpc:mail(From, Rcpt, MessageS);
deliver_raw(#smtpd_fsm{mail=From, rcpt=Rcpt, message_bin=MessageS, spool_ref=Ref}) -> 
    try 
	    mydlp_spool:delete(Ref),
    	mydlp_spool:release(Ref)
    catch
        Class:Error -> ?ERROR_LOG("Error occured on FSM ("?S") in spool call. Class: ["?S"]. Error: ["?S"]. ~nStack trace: "?S"~n",
                [?MODULE, Class, Error, erlang:get_stacktrace()]) end,
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

get_from(MessageR) -> 
	{_, User, DomainName, _} = MessageR#message.from,
	case ((User /= undefined) and (DomainName /= undefined)) of
		true -> User ++ "@" ++ DomainName;
		false -> MessageR#message.mail_from
	end.

get_from_domainname(MessageR) -> 
	{_ , _ , DomainName, _} = MessageR#message.from,
	list_to_binary(DomainName).
			

get_dest_addresses(MessageR) ->
	% RCPTTO - TO - CC + BCC
	% Equality mydlp_api:hash_un
	ToS = [A#addr.username ++ "@" ++ A#addr.domainname || A <- MessageR#message.to],
	CcS = [A#addr.username ++ "@" ++ A#addr.domainname || A <- MessageR#message.cc],
	BccS = [A#addr.username ++ "@" ++ A#addr.domainname || A <- MessageR#message.bcc],

	RcptD = dict:from_list([{mydlp_api:hash_un(V), V} || V <- MessageR#message.rcpt_to]),
	RcptD1 = lists:foldl(fun(I, Dict) -> dict:erase(mydlp_api:hash_un(I), Dict) end, RcptD, ToS),
	RcptD2 = lists:foldl(fun(I, Dict) -> dict:erase(mydlp_api:hash_un(I), Dict) end, RcptD1, CcS),

	BccD = lists:foldl(fun(I, Dict) -> dict:store(mydlp_api:hash_un(I), I, Dict) end, RcptD2, BccS),
	BccS1 = [V || {_H, V} <- dict:to_list(BccD)],

	DestList = ["rcpt to: <" ++ R ++ ">"|| R <- MessageR#message.rcpt_to] ++ 
		["to: <" ++ A ++ ">"|| A <- ToS] ++
		["cc: <" ++ A ++ ">"|| A <- CcS] ++
		["bcc: <" ++ A ++ ">"|| A <- BccS1],
	string:join(DestList, ", ").

log_req(#smtpd_fsm{message_record=MessageR}, Action, {{rule, RuleId}, {file, File}, {itype, IType}, {misc, Misc}}, none) ->
	log_req(#smtpd_fsm{message_record=MessageR}, Action, {{rule, RuleId}, {file, File}, {itype, IType}, {misc, Misc}});
log_req(#smtpd_fsm{message_record=MessageR}, Action, {{rule, RuleId}, {file, File}, {itype, IType}, {misc, _Misc}}, Message) ->
	log_req(#smtpd_fsm{message_record=MessageR}, Action, {{rule, RuleId}, {file, File}, {itype, IType}, {misc, Message}}).


log_req(#smtpd_fsm{message_record=MessageR, files=OrigFiles}, Action, {{rule, RuleId}, {file, File}, {itype, IType}, {misc, Misc}}) ->
	Src = get_from(MessageR),
	Dest = {MessageR#message.rcpt_to, get_dest_addresses(MessageR)},
	Time = erlang:universaltime(),

	File1 = lists:map(fun(F) -> mydlp_api:sizefy(F) end, File),
	File2 = mydlp_api:hashify_files(File1),
	
	{MergedFiles, TrashedFiles} = mydlp_api:merge_files(OrigFiles, File2),

	Payload = case Action of
		quarantine -> MessageR;
		_Else -> none end,
	FilesToLog = case Action of
		pass -> ?ERROR_LOG("Unexpected action on mail channel", []),
			mydlp_api:clean_files(MergedFiles), File;
		_ -> mydlp_api:clean_files(TrashedFiles), MergedFiles end,
%		quarantine -> 	mydlp_api:clean_files(File), MergedFiles;
%		archive -> 	mydlp_api:clean_files(File), MergedFiles;
%		_ ->		mydlp_api:clean_files(OrigFiles), File end,
        ?ACL_LOG_P(#log{time=Time, channel=mail, rule_id=RuleId, action=Action, ip=nil, user=Src, destination=Dest, itype_id=IType, file=FilesToLog, misc=Misc, payload=Payload}).

get_dest_domains(#message{rcpt_to=RcptTo, to=ToH, cc=CCH, bcc=BCCH})->
	RcptToA = lists:map(fun(S) -> mime_util:dec_addr(S) end, RcptTo),
	DestList = RcptToA ++ ToH ++ CCH ++ BCCH,
	L = [{domain, list_to_binary(A#addr.domainname)} || A <- DestList],
	lists:usort(L).

get_dest_user_hashes(#message{rcpt_to=RcptTo, to=ToH, cc=CCH, bcc=BCCH})->
	RcptToA = lists:map(fun(S) -> mime_util:dec_addr(S) end, RcptTo),
	DestList = RcptToA ++ ToH ++ CCH ++ BCCH,
	L = [{user, mydlp_api:hash_un(A#addr.username ++ "@" ++ A#addr.domainname)} || A <- DestList],
	lists:usort(L).

has_bcc(#message{bcc=undefined} = MessageR)-> has_bcc_1(MessageR);
has_bcc(#message{bcc=BCCH} = MessageR) when is_list(BCCH)->
	case length(BCCH) of
		0 -> has_bcc_1(MessageR);
		I when is_integer(I), I > 0 -> true end.

has_bcc_1(#message{rcpt_to=RcptTo, to=ToHeader, cc=CCHeader} = MessageR) ->
	RcptToCount = case RcptTo of
		R when is_list(R) -> length(R);
		_Else -> ?ERROR_LOG("Encountered with an email with 0 rcptto addresses. MessageR: "?S , [MessageR]), 0 end,
	ToCount = case ToHeader of
		T when is_list(T) -> length(T);
		_Else2 -> 0 end,
	CCCount = case CCHeader of
		C when is_list(C) -> length(C);
		_Else3 -> 0 end,
	( RcptToCount > ( ToCount + CCCount ) ).
	

create_smtp_msg(connect, {Ip1,Ip2,Ip3,Ip4}) ->
	{
		"Connected to ~w.~w.~w.~w .",
		[Ip1,Ip2,Ip3,Ip4]
	};
create_smtp_msg(not_ready, {Ip1,Ip2,Ip3,Ip4}) ->
	{
		"Policy compile is on progress. Temporarily rejecting mail from ~w.~w.~w.~w .",
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
create_smtp_msg(bypassed, MessageR) ->
	From = get_from(MessageR),
	ToList = get_dest_addresses(MessageR),
	{
		"An error occurred and bypassed message. FROM=~s TO='~s' ",
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
	mydlp_api:smtp_msg(Format, Args).

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

