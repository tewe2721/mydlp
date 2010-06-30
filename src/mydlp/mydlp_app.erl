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

-module(mydlp_app).

-author('kerem@medratech.com').

-behaviour(application).

-export([start/2,
	init/1,
	stop/1]).

-include("mydlp.hrl").

%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _Args) ->
	% Start dependencies
	mydlp_loglevel:set(4),
        application:start(ssl),
        application:start(crypto),
        application:load(thrift),
        application:load(sasl),
        application:load(mydlp),
        error_logger:add_report_handler(mydlp_logger_h, get_log_path()),

	% Read configuration
	{ok, Protocols} = application:get_env(protocols),

	% Start mydlp
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Protocols]).

init([Protocols]) ->
	{ok, SWorkers} = application:get_env(shared_workers),
	SWSpec = { shared_worker_sup,                                                    % Id       = internal id
		{supervisor, start_link,
			[{local, shared_worker_sup}, mydlp_worker_sup, [SWorkers]]
		},                                                                            % StartFun = {M, F, A}
		permanent,                                                            % Restart  = permanent | transient | temporary
		infinity,                                                                     % Shutdown = brutal_kill | int() >= 0 | infinity
		supervisor,                                                           % Type   = worker | supervisor
		[mydlp_worker_sup]                                            % Modules  = [Module] | dynamic
	},
	init(Protocols, [SWSpec]).

init([ProtoConf| Protocols], ChildSpecs) ->
	{Proto, _, _} = ProtoConf,
	SupName = list_to_atom(atom_to_list(Proto) ++ "_sup"),

	init(Protocols, 
		[
			{	SupName,								% Id	   = internal id
				{supervisor, start_link, 
					[{local, SupName}, mydlp_sup, [protocol_supervisor, ProtoConf]]
				},										% StartFun = {M, F, A}
				permanent,								% Restart  = permanent | transient | temporary
				infinity,								% Shutdown = brutal_kill | int() >= 0 | infinity
				supervisor,								% Type	 = worker | supervisor
				[mydlp_sup]							% Modules  = [Module] | dynamic
			}
		|ChildSpecs]
	);

init([], ChildSpecs) ->
	{ok,
		{
			_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
			lists:reverse(ChildSpecs)
		}
	}.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_S) ->
	ok.

%% @spec () -> string()
%% @doc Returns the full path to the ejabberd log file.
%% It first checks for application configuration parameter 'log_path'.
%% If not defined it checks the environment variable EJABBERD_LOG_PATH.
%% And if that one is neither defined, returns the default value:
%% "ejabberd.log" in current directory.
get_log_path() ->
        case application:get_env(mydlp, log_path) of
        {ok, Path} ->
                Path;
        undefined ->
                case os:getenv("MYDLP_LOG_PATH") of
                        false -> ?LOG_PATH;
                        Path -> Path
                end
        end.

