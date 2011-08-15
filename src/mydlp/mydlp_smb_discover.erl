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

%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@mydlp.org>
%%% @copyright 2010, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-ifdef(__MYDLP_NETWORK).

-module(mydlp_smb_discover).
-author("kerem@mydlp.org").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {interval, script_path}).

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(discover_now, #state{interval=Interval, script_path=ScriptPath} = State) ->
	case discover_smb(ScriptPath) of
		{ok, XMLResult} -> mydlp_mysql:push_smb_discover(XMLResult);
		{error, {retcode, R}} -> ?DEBUG("SMB DISCOVER: Error: Nmap return code: ~p\n", [R]);
		{error, Err} -> ?DEBUG("SMB DISCOVER: Error: ~p\n", [Err]) end,

	call_random(Interval),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case ?CFG(smb_discover) of 
		true ->
			Interval = ?CFG(smb_discover_interval),
			ScriptPath = ?CFG(smb_discover_script_path),
		
			case gen_server:start_link({local, ?MODULE}, ?MODULE, 
						[ScriptPath, Interval], []) of
				{ok, Pid} -> {ok, Pid};
				{error, {already_started, Pid}} -> {ok, Pid} end;
		false -> ignore end.


stop() ->
	gen_server:call(?MODULE, stop).

init([ScriptPath, Interval]) ->
	call_random(Interval),
	{ok, #state{interval=Interval, script_path=ScriptPath}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

call_random(Interval) ->
	WaitS = random:uniform(Interval*2),
	WaitMS = WaitS * 1000,
	timer:send_after(WaitMS, discover_now).

discover_smb(ScriptPath) ->
	{ok, ResultFN} = mydlp_api:mktempfile(),
	Port = open_port({spawn_executable, "/bin/bash"}, 
			[{args, [ScriptPath, ResultFN]},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	Ret = case mydlp_api:get_port_resp(Port) of
		ok -> {ok, XMLResult} = file:read_file(ResultFN), {ok, XMLResult};
		Else -> Else end,

	ok = file:delete(ResultFN), Ret.

-endif.

