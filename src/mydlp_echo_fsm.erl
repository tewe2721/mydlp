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

-module(mydlp_echo_fsm).

-author('kerem@medratech.com').

-export([init/0]).

%% FSM States
-export([
	'WAIT_FOR_LINE'/2
]).

-record(state, {count}).

%% determining initial FSM state and creating state record.
init() ->
	{ok, 'WAIT_FOR_LINE', #state{count=0}}.

'WAIT_FOR_LINE'({data, Data}, State) ->
	%% This request will be processed by clustered workers. 
	%% ReplyData = mydlp_echo_worker:echo_reply(Data),
	ReplyData = mydlp_echo_worker:async_echo_reply(Data),
	Count=State#state.count,
	io:format("fsm count id=~w~n", [Count]),
	{next_state, 'WAIT_FOR_LINE', State#state{count=Count+1}, ReplyData}.
