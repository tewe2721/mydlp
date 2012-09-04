%%
%%%    Copyright (C) 2012 Huseyin Kerem Cevahir <kerem@mydlp.com>
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

-module(mydlp_logger_syslog).
-author('kerem@mydlp.com').

-behaviour(gen_event).

-include("mydlp.hrl").

%% gen_event callbacks
-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([
%	test/0
]).

-record(state, {
	beam_pid,
	syslog_acl_fd,
	syslog_diag_fd,
	syslog_report_fd
}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Socket definitions
%%---------------------------------------------------------------------
start_socket(BeamPid, Tag, RHost, RPort, Fac, Level) ->
	case gen_udp:open(0) of
		{ok, Fd} ->
			syslog(BeamPid, {Fd, RHost, RPort}, Fac, Level, "MyDLP " ++ Tag ++ " logger started"),
			{ok, Fd};
		{error, Reason} -> {error, Reason}
	end.
	    
%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init(_) -> init().
init() ->
	AclHost = ?CFG(syslog_acl_host),
	AclPort = ?CFG(syslog_acl_port),
	AclFac = ?CFG(syslog_acl_facility),
	DiagHost = ?CFG(syslog_diag_host),
	DiagPort = ?CFG(syslog_diag_port),
	DiagFac = ?CFG(syslog_diag_facility),
	ReportHost = ?CFG(syslog_report_host),
	ReportPort = ?CFG(syslog_report_port),
	ReportFac = ?CFG(syslog_report_facility),
	BeamPid = list_to_binary(os:getpid()),
	{ok, AclFd} = start_socket(BeamPid, "ACL", AclHost, AclPort, AclFac, ?LOG_INFO),
	{ok, DiagFd} = start_socket(BeamPid, "Diagnostic", DiagHost, DiagPort, DiagFac, ?LOG_DEBUG),
	{ok, ReportFd} = start_socket(BeamPid, "Report", ReportHost, ReportPort, ReportFac, ?LOG_INFO),
	{ok, #state{beam_pid=BeamPid, syslog_acl_fd=AclFd, syslog_diag_fd=DiagFd, syslog_report_fd=ReportFd}}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event({ReportLevel, _, {FromPid, StdType, Report}}, State) when is_record(Report, report), is_atom(StdType) ->
	try	RL = case {ReportLevel,StdType} of
			{error_report, _} -> ?LOG_ERROR;
			{warning_report, _} -> ?LOG_WARNING;
			{info_report, _} -> ?LOG_INFO
		end,
		syslog_report(State,  RL,  io_lib:format ("~p: " ++ Report#report.format, [FromPid|Report#report.data]))
	catch Class:Error ->
                        ?ERROR_LOG("Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
                        [Class, Error, erlang:get_stacktrace()]) end,
	{ok, State};

handle_event({ReportLevel, _, {_FromPid, StdType, Report}}, State) when is_atom(StdType) ->
	try	RL = case {ReportLevel,StdType} of
			{error_report, _} -> ?LOG_ERROR;
			{warning_report, _} -> ?LOG_WARNING;
			{info_report, _} -> ?LOG_INFO
		end,
		syslog_report(State,  RL, io_lib:format ("~p", [Report]))
	catch Class:Error ->
                        ?ERROR_LOG("Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
                        [Class, Error, erlang:get_stacktrace()]) end,
	{ok, State};

handle_event({EventLevel, _, {_FromPid, Fmt, Data}}, State) ->
	try	Message = io_lib:format (Fmt, Data),
		case EventLevel of
			error -> syslog_err(State, Message);
			warning_msg -> syslog_syswarn(State, Message);
			info_msg -> syslog_debug(State, Message);
			acl_msg -> syslog_acl(State, Message);
			smtp_msg -> syslog_smtp(State, Message);
			Else -> syslog_err(State, io_lib:format("Unexpected event level: ~w ~nMessage: ~s~n", [Else, Message]))
		end
	catch Class:Error ->
                        ?ERROR_LOG("Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
                        [Class, Error, erlang:get_stacktrace()]) end,
	{ok, State};

handle_event(Event, State) ->
	try	syslog_syswarn(State, io_lib:format ("Unknown event [~p]", [Event]))
	catch Class:Error ->
                        ?ERROR_LOG("Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
                        [Class, Error, erlang:get_stacktrace()]) end,
	{ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(_Request, State) ->
	Reply = ok,
	{ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_info(Info, State) ->
	try	syslog_misc(State, io_lib:format ("Info [~p]", [Info]))
	catch Class:Error ->
                        ?ERROR_LOG("Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
                        [Class, Error, erlang:get_stacktrace()]) end,
	{ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%% Internal

syslog(BeamPid, {Fd, Host, Port}, Facility, Level, Message) ->
	M = unicode:characters_to_binary([Message]),
	P = list_to_binary(integer_to_list(Facility bor Level)),
	HeadLen1 = size(M) - 1,
	HeadLen2 = HeadLen1 - 1,
	M1 = case M of
		<<Head:HeadLen2/binary, "\r\n" >> -> Head;
		<<Head:HeadLen1/binary, "\n" >> -> Head;
		_Else -> M end,
	gen_udp:send(Fd, Host, Port, <<"<", P/binary, ">", "mydlp[", BeamPid/binary, "]: ", M1/binary>>).

syslog_acl(#state{beam_pid=BeamPid, syslog_acl_fd=AclFd}, Message) ->
	AclHost = ?CFG(syslog_acl_host),
	AclPort = ?CFG(syslog_acl_port),
	AclFac = ?CFG(syslog_acl_facility),
	syslog(BeamPid, {AclFd, AclHost, AclPort}, AclFac, ?LOG_INFO, Message).

syslog_diag(#state{beam_pid=BeamPid, syslog_diag_fd=DiagFd}, Level, Message) ->
	DiagHost = ?CFG(syslog_diag_host),
	DiagPort = ?CFG(syslog_diag_port),
	DiagFac = ?CFG(syslog_diag_facility),
	syslog(BeamPid, {DiagFd, DiagHost, DiagPort}, DiagFac, Level, Message).

syslog_report(#state{beam_pid=BeamPid, syslog_report_fd=ReportFd}, Level, Message) ->
	ReportHost = ?CFG(syslog_report_host),
	ReportPort = ?CFG(syslog_report_port),
	ReportFac = ?CFG(syslog_report_facility),
	syslog(BeamPid, {ReportFd, ReportHost, ReportPort}, ReportFac, Level, ["[MyDLP Report] " | [Message] ]).
	
syslog_err(State, Message) -> syslog_diag(State, ?LOG_ERROR, Message).

syslog_debug(State, Tag, Message) -> syslog_diag(State, ?LOG_DEBUG, ["[MyDLP ", Tag, "] " | [Message] ]).

syslog_smtp(State, Message) -> syslog_debug(State, "SMTP", Message).

syslog_misc(State, Message) -> syslog_debug(State, "Misc", Message).

%syslog_debug(State, Message) -> syslog_debug(State, "Debug", Message).
syslog_debug(_State, _Message) -> ok.

syslog_syswarn(State, Message) -> syslog_debug(State, "SysWarn", Message).

-endif.

