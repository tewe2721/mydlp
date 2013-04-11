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

-module(mydlp_sup).

-author('kerem@mydlp.com').
-author('saleyn@gmail.com').

-behaviour(supervisor).

%% Internal API
-export([start_client/1]).

%% Application and Supervisor callbacks
-export([init/1]).

-include("mydlp.hrl").

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client(SocketSup) ->
	supervisor:start_child(SocketSup, []).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([protocol_supervisor, ProtoConf]) ->
	{	Proto,
		{acceptor, {Port, CommType, FsmModule}}, 
		{workers, Workers}
	} = ProtoConf,

	AcceptorName = list_to_atom(atom_to_list(Proto) ++ "_acceptor"),
	AcceptorSupName = list_to_atom(atom_to_list(Proto) ++ "_acceptor_sup"),
	WorkerSupName = list_to_atom(atom_to_list(Proto) ++ "_worker_sup"),
	SocketSupName = list_to_atom(atom_to_list(Proto) ++ "_socket_sup"),

	{ok,
		{_SupFlags = {one_for_one, ?CFG(supervisor_max_restart_count), ?CFG(supervisor_max_restart_time)},
			[
				% TCP Listener
			  {   AcceptorSupName,							% Id	   = internal id
				  {mydlp_acceptor, start_link,
				  	[AcceptorName, Port, CommType, SocketSupName]
				  },										% StartFun = {M, F, A}
				  permanent,								% Restart  = permanent | transient | temporary
				  ?CFG(supervisor_kill_timeout),							% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  [mydlp_acceptor]							% Modules  = [Module] | dynamic
			  },
				% Worker supervisor
			  {   WorkerSupName,							% Id	   = internal id
				  {supervisor, start_link,
				  	[{local, WorkerSupName}, mydlp_worker_sup, [Workers]]
				  },										% StartFun = {M, F, A}
				  permanent,								% Restart  = permanent | transient | temporary
				  infinity,									% Shutdown = brutal_kill | int() >= 0 | infinity
				  supervisor,								% Type	 = worker | supervisor
				  [mydlp_worker_sup]						% Modules  = [Module] | dynamic
			  },
				% Client instance supervisor
			  {   SocketSupName,							% Id       = internal id
				  {supervisor,start_link,[{local, SocketSupName}, ?MODULE, [socket, FsmModule]]},
				  permanent,								% Restart  = permanent | transient | temporary
				  infinity,									% Shutdown = brutal_kill | int() >= 0 | infinity
				  supervisor,								% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	};
init([socket, http]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?CFG(supervisor_max_restart_count), ?CFG(supervisor_max_restart_time)},
			[
				% TCP Client
			  {   undefined,								% Id	   = internal id
				  {mydlp_http_fsm,start_link,[]},		% StartFun = {M, F, A}
				  temporary,								% Restart  = permanent | transient | temporary
				  ?CFG(supervisor_kill_timeout),							% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	};
init([socket, smtp]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?CFG(supervisor_max_restart_count), ?CFG(supervisor_max_restart_time)},
			[
				% TCP Client
			  {   undefined,								% Id	   = internal id
				  {mydlp_smtp_fsm,start_link,[]},		% StartFun = {M, F, A}
				  temporary,								% Restart  = permanent | transient | temporary
				  ?CFG(supervisor_kill_timeout),							% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	};
init([socket, icap]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?CFG(supervisor_max_restart_count), ?CFG(supervisor_max_restart_time)},
			[
				% TCP Client
			  {   undefined,								% Id	   = internal id
				  {mydlp_icap_fsm,start_link,[]},		% StartFun = {M, F, A}
				  temporary,								% Restart  = permanent | transient | temporary
				  ?CFG(supervisor_kill_timeout),							% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	};
init([socket, seap]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?CFG(supervisor_max_restart_count), ?CFG(supervisor_max_restart_time)},
			[
				% TCP Client
			  {   undefined,								% Id	   = internal id
				  {mydlp_seap_fsm,start_link,[]},		% StartFun = {M, F, A}
				  temporary,								% Restart  = permanent | transient | temporary
				  ?CFG(supervisor_kill_timeout),							% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	};
init([socket, iecp]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?CFG(supervisor_max_restart_count), ?CFG(supervisor_max_restart_time)},
			[
				% TCP Client
			  {   undefined,								% Id	   = internal id
				  {mydlp_iecp_fsm,start_link,[]},		% StartFun = {M, F, A}
				  temporary,								% Restart  = permanent | transient | temporary
				  ?CFG(supervisor_kill_timeout),							% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	};
init([socket, FsmModule]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?CFG(supervisor_max_restart_count), ?CFG(supervisor_max_restart_time)},
			[
				% TCP Client
			  {   undefined,								% Id	   = internal id
				  {mydlp_fsm,start_link,[FsmModule]},		% StartFun = {M, F, A}
				  temporary,								% Restart  = permanent | transient | temporary
				  ?CFG(supervisor_kill_timeout),							% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	}.

