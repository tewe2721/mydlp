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

-module(mydlp_worker_sup).

-author('kerem@medratech.com').
-author('saleyn@gmail.com').

-behaviour(supervisor).

%% Application and Supervisor callbacks
-export([init/1]).

-include("mydlp.hrl").

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Workers]) ->
	init(Workers, []).

init([Worker|Workers], ChildSpecs) ->
	{Module, _Func, _Args} = Worker,
	init (Workers,
		[
			{
				Module,					% Id	   = internal id
				Worker,					% StartFun = {M, F, A}
				permanent,				% Restart  = permanent | transient | temporary
				?KILL_TIMEOUT,			% Shutdown = brutal_kill | int() >= 0 | infinity
				worker,					% Type	 = worker | supervisor
				[Module]				% Modules  = [Module] | dynamic
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
												 
