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

-module(mydlp_app).

-author('kerem@mydlp.com').

-behaviour(application).

-export([start/2,
	init/1,
	stop/1]).

-include("mydlp.hrl").

-ifdef(__MYDLP_ENDPOINT).

-define(SEAP, 
			{seap, % Simple endpoint agent protocol
				{acceptor, {9099, plain, seap} },
		                 {workers, []} 
			}
	).

-define(ENDPOINT_SWORKERS,
		[
			{mydlp_mnesia, start_link,[]},
			{mydlp_acl, start_link,[]},
			{mydlp_container, start_link,[]},
			{mydlp_regex, start_link,[]},
			{pg, {mydlp_tc, start_link,[]}, 4},
			{mydlp_workdir, start_link,[]},
			{mydlp_sync, start_link,[]},
			{mydlp_item_push, start_link,[]}
		]
	).

-endif.

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
	% Prestart Load of dynamic modules
	mydlp_dynamic:prestart_load(),
	% Start dependencies
	mydlp_loglevel:set(4),
	ssl:start(),
        application:load(thrift),
        application:load(sasl),
        application:load(mydlp),
        error_logger:add_report_handler(mydlp_logger_h, ?CFG(log_dir)),
	create_pid_file(),

	Protocols = get_protocols(),

	% Start mydlp
	SRet = supervisor:start_link({local, ?MODULE}, ?MODULE, [Protocols]),

	case SRet of
		{ok, _} -> 
			% If everything is allright,
			% Load dynamic modules
			mydlp_dynamic:load();
		_Else -> ok end,
	SRet.

init([Protocols]) ->
	SWorkers = get_sworkers(),
	SWSpec = { shared_worker_sup,                                                    % Id       = internal id
		{supervisor, start_link,
			[{local, shared_worker_sup}, mydlp_worker_sup, [SWorkers]]
		},                                                                            % StartFun = {M, F, A}
		permanent,                                                            % Restart  = permanent | transient | temporary
		infinity,                                                                     % Shutdown = brutal_kill | int() >= 0 | infinity
		supervisor,                                                           % Type   = worker | supervisor
		[mydlp_worker_sup]                                            % Modules  = [Module] | dynamic
	},
	Agents = get_agents(),
	ASpec = { agent_sup,                                                    % Id       = internal id
		{supervisor, start_link,
			[{local, agent_sup}, mydlp_worker_sup, [Agents]]
		},                                                                            % StartFun = {M, F, A}
		permanent,                                                            % Restart  = permanent | transient | temporary
		infinity,                                                                     % Shutdown = brutal_kill | int() >= 0 | infinity
		supervisor,                                                           % Type   = worker | supervisor
		[mydlp_worker_sup]                                            % Modules  = [Module] | dynamic
	},
	init(Protocols, [ASpec, SWSpec]).

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
			_SupFlags = {one_for_one, ?CFG(supervisor_max_restart_count), ?CFG(supervisor_max_restart_time)},
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
	application:stop(mydlp),
	mnesia:stop(),
	ok.

%% @spec () -> string()
%% @doc Returns the full path to the ejabberd log file.
%% It first checks for application configuration parameter 'log_path'.
%% If not defined it checks the environment variable EJABBERD_LOG_PATH.
%% And if that one is neither defined, returns the default value:
%% "ejabberd.log" in current directory.

create_pid_file() ->
	file:write_file(?CFG(pid_file), os:getpid()).

-ifdef(__MYDLP_NETWORK).

get_protocols() -> 
	{ok, Protocols} = application:get_env(protocols),
	Protocols.

get_sworkers() -> 
	{ok, SWorkers} = application:get_env(shared_workers),
	SWorkers.

get_agents() ->
	{ok, Agents} = application:get_env(agents),
	Agents.

-endif.

-ifdef(__MYDLP_ENDPOINT).

get_protocols() -> [ ?SEAP ].

get_sworkers() ->  ?ENDPOINT_SWORKERS.

get_agents() -> [].

-endif.
