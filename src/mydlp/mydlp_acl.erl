%
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
%%% @author H. Kerem Cevahir <kerem@medratech.com>
%%% @copyright 2010, H. Kerem Cevahir
%%% @doc ACL for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_acl).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	q/3,
	qu/3,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {}).

%%%%%%%%%%%%% MyDLP ACL API

q(Addr, _Dest, Files) ->
	gen_server:call(?MODULE, {acl_q, {Addr, Files}}, 60000).

qu(_User, _Dest, Files) ->
	gen_server:call(?MODULE, {acl_qu, {user, Files}}, 60000).

%%%%%%%%%%%%%% gen_server handles

handle_call({acl_q, {Addr, Files}}, From, State) ->
	Worker = self(),
	spawn_link(fun() ->
		Rules = mydlp_mnesia:get_rules(Addr),
		Files1 = mydlp_api:df_to_files(Files),
		Param = {Addr, drop_nodata(Files1)},

		Result = apply_rules(Rules, Param),
		Worker ! {async_acl_q, Result, From}
	end),
	{noreply, State, 60000};

handle_call({acl_qu, {User, Files}}, From, State) ->
	Worker = self(),
	spawn_link(fun() ->
		Rules = mydlp_mnesia:get_rules_by_user(User),
		Param = {User, mydlp_api:df_to_files(Files)},

		Result = apply_rules(Rules, Param),
		Worker ! {async_acl_q, Result, From}
	end),
	{noreply, State, 60000};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_acl_q, Res, From}, State) ->
	gen_server:reply(From, Res),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	{ok, #state{}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%% helper func
apply_rules([{Id, Action, Matchers}|Rules], Params) ->
	case execute_matchers(Matchers, Params) of
		{pos, {file, File}, {matcher, Func}, {misc, Misc}} -> 
			{Action, {{rule, Id}, {file, File}, {matcher, Func}, {misc, Misc}}};
		neg -> apply_rules(Rules, Params)
	end;
apply_rules([], _Params) -> pass.

execute_matchers(Matchers, Params) -> execute_matchers(Matchers, Params, false).

execute_matchers(Matchers, Params, true) -> apply_f(Matchers, Params, true);
execute_matchers([{Func,_}|_] = Matchers, {Addr, Files} = Params, false) ->
	{PLT, Params1} = case get_matcher_req(Func) of
		raw -> {false, Params};
		analyzed -> {true, {Addr, pl_text(Files)}};
		text -> {true, {Addr, pl_text(Files)}}
	end,
	apply_f(Matchers, Params1, PLT);
execute_matchers([], Params, PLT) -> apply_f([], Params, PLT).

apply_f([{whitefile, _FuncParams}|Matchers], {Addr, Files}, PLT) ->
	execute_matchers(Matchers, {Addr, drop_whitefile(Files)}, PLT);
apply_f([{Func, FuncParams}|Matchers], Params, PLT) ->
	case apply_m(Func, [FuncParams, Params]) of
                neg -> execute_matchers(Matchers, Params, PLT);
                {pos, {file, F}} -> {pos, {file, F}, {matcher, Func}, {misc, ""}};
                {pos, {file, F}, {misc, Misc}} -> {pos, {file, F}, {matcher, Func}, {misc, Misc}};
                pos -> {pos, {file, #file{name="unknown"}}, {matcher, Func}, {misc, ""}}
        end;
apply_f([], _Params, _PLT) -> neg.

apply_m(Func, [FuncParams, {Addr, Files}]) ->
	Args = case get_matcher_req(Func) of
		raw -> [FuncParams, {Addr, Files}];
		analyzed -> [FuncParams, {Addr, Files}];
		text -> [FuncParams, {Addr, drop_notext(Files)}]
	end,
	apply(mydlp_matchers, Func, Args).

get_matcher_req(whitefile) -> raw;
get_matcher_req(Func) -> apply(mydlp_matchers, Func, []).

pl_text(Files) -> pl_text(Files, []).
pl_text([#file{text=undefined} = File|Files], Rets) -> 
	File1 = case mydlp_api:get_text(File) of
		{ok, Text} -> File#file{text = Text};
		_Else -> File#file{is_encrypted=true}
	end,
	pl_text(Files, [ File1 |Rets]);
pl_text([File|Files], Rets) -> pl_text(Files, [File|Rets]);
pl_text([], Rets) -> lists:reverse(Rets).

is_whitefile(File) ->
	Hash = erlang:md5(File#file.data),
	mydlp_mnesia:is_fhash_of_gid(Hash, [mydlp_mnesia:get_pgid()]).

drop_whitefile(Files) -> lists:filter(fun(F) -> not is_whitefile(F) end, Files).

drop_notext(Files) -> lists:filter(fun(I) -> mydlp_api:has_text(I) end, Files).

drop_nodata(Files) -> lists:filter(fun(F) -> size(F#file.data) > 0 end, Files).
