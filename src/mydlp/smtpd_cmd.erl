%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       SMTP server commands
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.5
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

-module(smtpd_cmd).
-author('sjackson@simpleenigma.com').
-author('kerem@mydlp.com').
-include("mydlp_smtp.hrl").

-export([command/2]).
-export([read_message/2]).
-export([send/2]).



command(Line,State) when is_binary(Line) -> command(parse(Line),State);

command({greeting,_},State) ->
	send(State,220,"MyDLP http://www.mydlp.com (NO UCE)"),
	State;

command({helo = _Command,Domain},State) when is_list(Domain), length(Domain) > 0 -> 
	send(State,250),
	State#smtpd_fsm{host=Domain};
command({ehlo = _Command,Domain},State) when is_list(Domain), length(Domain) > 0 -> 
	send(State,250),
	State#smtpd_fsm{host=Domain};
%% MAIL before HELO or EHLO
command({mail = _Command,_Param},#smtpd_fsm{host = undefined} = State) ->
	send(State,503),
	State;
command({mail = _Command,Param},State) when length(Param) > 0 ->
	From = clean_email(Param),
	send(State,250),
	State#smtpd_fsm{mail = From, rcpt = undefined, to = undefined, messagename = undefined, data = undefined};
%% RCPT before MAIL
command({rcpt = _Command,_Param},#smtpd_fsm{mail = undefined} = State) ->
	send(State,503),
	State;
%% Too many Rcpt
command({rcpt = _Command,_Param},#smtpd_fsm{rcpt = RcptList} = State) when is_list(RcptList), length(RcptList) >= 100 ->
	send(State,452,"Too many recipients"),
	State;
command({rcpt = _Command,Param},#smtpd_fsm{relay = Relay} = State) ->
	To = clean_email(Param),
	?DEBUG("Relay: "?S, [Relay]),
%	case check_user(erlmail_util:split_email(To),Relay) of
%		true ->
			NewRcptList = case State#smtpd_fsm.rcpt of
				undefined -> [To];
				RcptList -> [To|RcptList]
			end,
			send(State,250),
			?DEBUG("RCPT: "?S, [NewRcptList]),
			State#smtpd_fsm{rcpt=NewRcptList};
%		false ->
%			send(State,550),
%			State
%	end;

command({data = _Command,[]},#smtpd_fsm{rcpt = undefined} = State) ->
	send(State,503),
	State;
command({data = _Command,[]},State) ->
	send(State,354),
	State#smtpd_fsm{data = <<>>};

command({noop = _Command,[]},State) ->
	send(State,250),
	State;
command({vrfy = _Command,[]},State) ->
	send(State,502),
	State;
command({expn = _Command,[]},State) ->
	send(State,502),
	State;
command({help = _Command,_Param},State) ->
	send(State,250,"http://erlsoft.org"),
	State;
command({quit = _Command,[]},State) ->
	send(State,221),
	gen_fsm:send_all_state_event(self(),stop),
	State;
command({rset = _Command,[]},State) ->
	send(State,250),
	State#smtpd_fsm{cmd = undefined, param = undefined, mail = undefined, rcpt = undefined, to = undefined, messagename = undefined, data = undefined};

%% Obsolete
command({send = _Command,_Param},State) ->
	send(State,502),
	State;
%% Obsolete
command({soml = _Command,_Param},State) ->
	send(State,502),
	State;
%% Obsolete
command({saml = _Command,_Param},State) ->
	send(State,502),
	State;

command({Command,Param},State) ->
	?ERROR_LOG("SMTP: Unknown Command: "?S" "?S,[Command,Param]),
	send(State,500),
	State.

%% @todo cehck relay state and store messages according to local or outgoing status. Only real differene is in the message name.

read_message(Message,State) when is_list(Message) -> read_message(list_to_binary(Message),State);
read_message(#message{} = Message,State) ->
	MIME = mime_util:decode(Message#message.message),
        NewMessage = expand(Message,MIME),
	State#smtpd_fsm{message_record=NewMessage, message_mime=MIME};
read_message(Message,#smtpd_fsm{mail=From, rcpt=To} = State) when is_binary(Message)->
	read_message(#message{
		mail_from=From,
		rcpt_to=lists:reverse(To),
		message=Message},State).

send(State,Code) -> send(State,Code,resp(Code)).
send(State,Code,[]) -> send(State,Code,resp(Code));
send(#smtpd_fsm{} = State,Code,Message) -> send(State#smtpd_fsm.socket,Code,Message);
send(Socket,Code,Message) ->
	Last = string:right(Message,2),
	Msg = case Last of
		?CRLF -> [integer_to_list(Code),32,Message];
		_      -> [integer_to_list(Code),32,Message,?CRLF]
	end,
	gen_tcp:send(Socket,Msg).


parse(Bin)  when is_binary(Bin) -> parse(binary_to_list(Bin));
parse(Line) when is_list(Line)  ->
	case string:chr(Line,32) of
		0 -> {list_to_atom(http_util:to_lower(Line)),[]};
		Pos ->
			{Command,RespText} = lists:split(Pos-1,Line),
			{list_to_atom(http_util:to_lower(Command)),string:strip(RespText)}
	end.


clean_email(String0) -> 
	String = case re:run(String0,"^(\\s*[a-z][a-z]*:\\s*)",[caseless, {capture,[1]}]) of
		{match,[{Start0,Length0}]} -> string:substr(String0,Start0 + 1 + Length0);
		{match,[{-1,_Length0}]} -> String0;
		nomatch -> String0 end,

	Ret = case re:run(String,"<(.*)>",[{capture,[1]}]) of
		{match,[{Start,Length}]} -> string:substr(String,Start+1,Length);
		{match,[{-1,_Length}]} -> nomatch;
		nomatch -> nomatch end,

	Ret2 = case Ret of 
		nomatch -> case re:run(String,"([^ ]{1,}@[^ ]{1,})",[{capture,[1]}]) of
			{match,[{Start2,Length2}]} -> string:substr(String,Start2+1,Length2);
			{match,[{-1,_Length2}]} -> nomatch;
			nomatch -> nomatch end;
		_Else -> Ret end,

	case Ret2 of
		nomatch -> 
			?ERROR_LOG("Can not extract an email address from client command. Str: "?S, [String]),
			throw({error, not_a_clean_address});
		_Else2 -> Ret2 end.

resp(211) -> "System Status"; % Need more info
resp(214) -> "For help please go to http://erlsoft.org/modules/erlmail/";
resp(220) -> "ErlMail (NO UCE)";
resp(221) -> "SMTP server closing transmission channel";
%resp(250) -> "Requested mail action okay, completed";
resp(250) -> "OK";
resp(251) -> "User not local";
resp(252) -> "Cannot VRFY user, but will accept message and attempt deliver";
resp(354) -> "Mail input accepted";
resp(421) -> "Service Not avaiable, closing transmission channel";
resp(450) -> "Requestion action not taken: mailbox unavailable";
resp(451) -> "Requestion action aborted: local error in processing";
resp(452) -> "Requestion action not taken: insufficient system storage";
resp(500) -> "Syntax error, command unrecognized";
resp(501) -> "Syntax error in parameters or arguments";
resp(502) -> "Command not implimented";
resp(503) -> "Bad sequence of commands";
resp(504) -> "Command parameter not implimented";
resp(511) -> "No mailbox here by that name";
resp(550) -> "Reqested action not taken: mailbox unavailable";
resp(551) -> "User not local";
resp(552) -> "Requestion action not taken: insufficient system storage";
resp(553) -> "Requestion action not taken: mailbox name not allowed";
resp(554) -> "Transaction Failed".

expand(Message,MIME) -> expand(Message,MIME,record_info(fields,message)).

expand(Message,_MIME,[]) -> Message;
expand(#message{from = []} = Message,MIME,[from | Rest]) ->
	expand(Message#message{from = mime_util:get_header(from,MIME)},MIME,Rest);
expand(#message{to = []} = Message,MIME,[to | Rest]) ->
	expand(Message#message{to = mime_util:get_header(to,MIME)},MIME,Rest);
expand(#message{cc = []} = Message,MIME,[cc | Rest]) ->
	expand(Message#message{cc = mime_util:get_header(cc,MIME)},MIME,Rest);
expand(#message{bcc = []} = Message,MIME,[bcc | Rest]) ->
	expand(Message#message{bcc = mime_util:get_header(bcc,MIME)},MIME,Rest);
expand(#message{size = 0} = Message,MIME,[size | Rest]) ->
	expand(Message#message{size = size(Message#message.message)},MIME,Rest);
expand(#message{internaldate = []} = Message,MIME,[internaldate | Rest]) ->
	expand(Message#message{internaldate={date(),time()}},MIME,Rest);
expand(Message,MIME,[_Unknown | Rest]) ->
	expand(Message,MIME,Rest).

-endif.

