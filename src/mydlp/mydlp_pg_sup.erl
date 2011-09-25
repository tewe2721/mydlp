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

-module(mydlp_pg_sup).

-author('kerem@mydlp.com').

-behaviour(supervisor).

%% Application and Supervisor callbacks
-export([
	init/1,
	start_link/3
	]).

-include("mydlp.hrl").

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([{Module, Function, Args}, ProcessCount]) ->
	ChildSpecs = [  
		{
			list_to_atom(atom_to_list(Module) ++ "_w" ++ integer_to_list(I)), % Id       = internal id
			{Module, Function, Args},                                 % StartFun = {M, F, A}
			permanent,                              % Restart  = permanent | transient | temporary
			?CFG(supervisor_kill_timeout),                  % Shutdown = brutal_kill | int() >= 0 | infinity
			worker,                                 % Type   = worker | supervisor
			[Module]                                % Modules  = [Module] | dynamic
		}
	|| I <- lists:seq(1, ProcessCount)],

	Module:pre_init(Args),
	{ok,
		{
			_SupFlags = {one_for_one, ?CFG(supervisor_max_restart_count), ?CFG(supervisor_max_restart_time)},
			lists:reverse(ChildSpecs)
		}
	}.

start_link(Module, Args, Opts) ->
	{ok, P} = gen_server:start_link(Module, Args, Opts),
	pg2:create(Module),
	pg2:join(Module, P),
	{ok, P}.
