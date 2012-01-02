%%%----------------------------------------------------------------------
%%% File    : ejabberd_logger_h.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage Erlang logging.
%%% Created : 23 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

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

-module(mydlp_logger_h).
-author('kerem@mydlp.com').

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	 code_change/3, reopen_log/0, rotate_log/1]).

-record(state, {
	log_dir,
	info_fd,
	error_fd,
	acl_fd,
	smtp_fd
}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Log file definitions
%%---------------------------------------------------------------------
	    
open_files(#state{log_dir=LogDir} = State) ->
	{ok, InfoFd} = fopen(LogDir ++ "/info.log"),
	{ok, ErrorFd} = fopen(LogDir ++ "/error.log"),
	{ok, AclFd} = fopen(LogDir ++ "/acl.log"),
	{ok, SmtpFd} = fopen(LogDir ++ "/smtp.log"),
	State#state{acl_fd=AclFd, info_fd=InfoFd, error_fd=ErrorFd, smtp_fd=SmtpFd}.

close_files(#state{acl_fd=AclFd, info_fd=InfoFd, error_fd=ErrorFd, smtp_fd=SmtpFd} = State) ->
	file:close(InfoFd),
	file:close(ErrorFd),
	file:close(AclFd),
	file:close(SmtpFd),
	State#state{acl_fd=undefined, error_fd=undefined, smtp_fd=undefined}.

rotate_logs(LogDir) ->
	rotate_log(LogDir ++ "/error.log"),
	rotate_log(LogDir ++ "/info.log"),
	rotate_log(LogDir ++ "/acl.log"),
	rotate_log(LogDir ++ "/smtp.log").

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init(LogDir) ->
	{ok, open_files(#state{log_dir=LogDir})}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event(Event, State) ->
	write_event(State, {erlang:localtime(), Event}),
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
handle_info({'EXIT', _Fd, _Reason}, _State) ->
	remove_handler;
handle_info({emulator, _GL, reopen}, #state{log_dir=LogDir} = State) ->
	State1 = close_files(State),
	rotate_logs(LogDir),
	{ok, open_files(State1)};
handle_info({emulator, GL, Chars}, State) ->
	write_event(State, {erlang:localtime(), {emulator, GL, Chars}}),
	{ok, State};
handle_info(_Info, State) ->
	{ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

reopen_log() -> error_logger ! {emulator, noproc, reopen}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
% Copied from erlang_logger_file_h.erl
write_event(#state{acl_fd=Fd}, {Time, {acl_msg, _GL, {Pid, Format, Args}}}) ->
	write_event1(Fd, "ACL", Time, Pid, Format, Args);
write_event(#state{info_fd=Fd}, {Time, {info_msg, _GL, {Pid, Format, Args}}}) ->
	write_event1(Fd, "INFO", Time, Pid, Format, Args);
write_event(#state{smtp_fd=Fd}, {Time, {smtp_msg, _GL, {Pid, Format, Args}}}) ->
	write_event1(Fd, "SMTP", Time, Pid, Format, Args);
write_event(#state{error_fd=Fd}, {Time, {error, _GL, {Pid, Format, Args}}}) ->
	write_event1(Fd, "ERROR", Time, Pid, Format, Args);
write_event(#state{error_fd=Fd}, {Time, {error_report, _GL, {Pid, Format, Args}}}) ->
	write_event1(Fd, "ERROR", Time, Pid, Format, Args);
write_event(_, _) -> ok.

file_write(Fd, String) ->
	Len = string:len(String),
	String1 = case string:substr(String,Len) of
		"\n" -> String;
		_Else -> String ++ "\n" end,
	file:write(Fd, String1).

write_event1(Fd, Prefix, Time, Pid, Format, Args) when is_list(Format)->
	T = write_time(Time, Prefix),
	case catch io_lib:format(add_node(Format,Pid), Args) of
		S when is_list(S) ->
			file_write(Fd, io_lib:format(T ++ S, []));
		_ ->
			F = add_node("ERROR: ~p - ~p~n", Pid),
			file_write(Fd, io_lib:format(T ++ F, [Format,Args])) end;
write_event1(Fd, Prefix, Time, Pid, Format, Args) ->
	T = write_time(Time, Prefix),
	F = add_node("~p - ~p~n", Pid),
	file_write(Fd, io_lib:format(T ++ F, [Format,Args])).

add_node(X, Pid) when is_atom(X) ->
	add_node(atom_to_list(X), Pid);
add_node(X, Pid) when node(Pid) /= node() ->
	lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) -> X.

%write_time(Time) -> write_time(Time, "ERROR").

write_time({{Y,Mo,D},{H,Mi,S}}, Type) ->
	io_lib:format("~s: ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w -> ",
		[Type, Y, Mo, D, H, Mi, S]).

%% @doc Rename the log file if exists, to "*-old.log".
%% This is needed in systems when the file must be closed before rotation (Windows).
%% On most Unix-like system, the file can be renamed from the command line and
%% the log can directly be reopened.
%% @spec (Filename::string()) -> ok
rotate_log(Filename) ->
	case file:read_file_info(Filename) of
		{ok, _FileInfo} ->
			RotationName = filename:rootname(Filename),
			file:rename(Filename, [RotationName, "-old.log"]),
			ok;
		{error, _Reason} -> ok end.

fopen(Filename) -> file:open(Filename, [append, raw]).
