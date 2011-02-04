
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
	q/4,
	qu/3,
	qa/2,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {is_multisite=false}).

%%%%%%%%%%%%% MyDLP ACL API

q(Site, Addr, _Dest, Files) ->
	gen_server:call(?MODULE, {acl_q, Site, {Addr, Files}}, 60000).

qu(User, _Dest, Files) ->
	gen_server:call(?MODULE, {acl_qu, site, {User, Files}}, 60000).

qa(Dest, Files) ->
	gen_server:call(?MODULE, {acl_qa, site, {Dest, Files}}, 60000).

%%%%%%%%%%%%%% gen_server handles

%acl_exec(Rules, Source, Files) ->
%	[{cid, CustomerId}|_] = Source,
%	Files1 = mydlp_api:df_to_files(Files),
%	Param = {Source, drop_nodata(Files1)},
%	DRules = mydlp_mnesia:get_default_rule(CustomerId),
%	apply_rules(lists:append(DRules, Rules), Param).

acl_exec(Rules, Source, Files) ->
	Files1 = drop_nodata(Files),
	acl_exec1(Rules, Source, Files1).

acl_exec1(_Rules, _Source, []) -> pass;
acl_exec1(Rules, Source, Files) ->
	[{cid, CustomerId}|_] = Source,
	DRules = mydlp_mnesia:get_default_rule(CustomerId),
	acl_exec2(lists:append(DRules, Rules), Source, Files).

acl_exec2(AllRules, Source, Files) ->
	Files1 = drop_nodata(Files),
	acl_exec3(AllRules, Source, Files1).

acl_exec3(_AllRules, _Source, []) -> pass;
acl_exec3(AllRules, Source, Files) ->
	{PFiles, NewFiles} = mydlp_api:analyze(Files),

	Param = {Source, drop_nodata(PFiles)},
	case apply_rules(AllRules, Param) of
		pass -> acl_exec2(AllRules, Source, NewFiles);
		Else -> Else end.

%% it needs refactoring for trusted domains
handle_call({acl_q, SAddr, {Addr, Files}}, From, #state{is_multisite=true} = State) ->
	Worker = self(),
	spawn_link(fun() ->
		Result = case mydlp_mnesia:get_cid(SAddr) of
			nocustomer -> block;
			CustomerId -> 
				Rules = mydlp_mnesia:get_rules_for_cid(CustomerId, Addr),
				acl_exec(Rules, [{cid, CustomerId}, {addr, Addr}], Files) end,
		Worker ! {async_acl_q, Result, From}
	end),
	{noreply, State, 60000};

handle_call({acl_q, _Site, {Addr, Files}}, From, #state{is_multisite=false} = State) ->
	Worker = self(),
	spawn_link(fun() ->
		%Rules = mydlp_mnesia:get_rules(Addr),
		CustomerId = mydlp_mnesia:get_dcid(),
		Rules = mydlp_mnesia:get_rules_for_cid(CustomerId, Addr),
		Result = acl_exec(Rules, [{cid, CustomerId}, {addr, Addr}], Files),
		Worker ! {async_acl_q, Result, From}
	end),
	{noreply, State, 60000};

%% now this is used for only SMTP, and in SMTP domain part of, mail adresses itself a siteid for customer.
%% it needs refactoring for both multisite and trusted domains
handle_call({acl_qu, _Site, {User, Files}}, From, State) ->
	Worker = self(),
	spawn_link(fun() ->
		Rules = mydlp_mnesia:get_rules_by_user(User),
		Result = acl_exec(Rules, [{cid, mydlp_mnesia:get_dcid()}, {user, User}], Files),
		Worker ! {async_acl_q, Result, From}
	end),
	{noreply, State, 60000};

handle_call({acl_qa, _Site, {Dest, Files}}, From, State) ->
	Worker = self(),
	spawn_link(fun() ->
		Rules = mydlp_mnesia:get_all_rules(Dest),
		Result = acl_exec(Rules, [{cid, mydlp_mnesia:get_dcid()}], Files),
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
	IsMS = mydlp_mysql:is_multisite(),
	{ok, #state{is_multisite=IsMS}}.

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
		{neg, NewParams} -> apply_rules(Rules, NewParams)
			% If neg is return, params may be modified because of whitefile matchers.
			% So, we should updated params.
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
	% Droping whitefiles to prevent execution of matchers with them
	execute_matchers(Matchers, {Addr, drop_whitefile(Files)}, PLT);
apply_f([{whitefile_dr, _FuncParams}|Matchers], {Source, Files}, PLT) ->
	[{cid, CustomerId}|_] = Source,
	% Droping whitefiles to prevent execution of matchers with them
	execute_matchers(Matchers, {Source, drop_whitefile_dr(Files, CustomerId)}, PLT);
apply_f([{Func, FuncParams}|Matchers], Params, PLT) ->
	case apply_m(Func, [FuncParams, Params]) of
                neg -> execute_matchers(Matchers, Params, PLT);
                {pos, {file, F}} -> {pos, {file, F}, {matcher, Func}, {misc, ""}};
                {pos, {file, F}, {misc, Misc}} -> {pos, {file, F}, {matcher, Func}, {misc, Misc}};
                pos -> {pos, {file, #file{name="unknown"}}, {matcher, Func}, {misc, ""}}
        end;
apply_f([], Params, _PLT) -> {neg, Params}.

apply_m(Func, [FuncParams, {Addr, Files}]) ->
	Args = case get_matcher_req(Func) of
		raw -> [FuncParams, {Addr, Files}];
		analyzed -> [FuncParams, {Addr, Files}];
		text -> [FuncParams, {Addr, drop_notext(Files)}]
	end,
	apply(mydlp_matchers, Func, Args).

get_matcher_req(whitefile) -> raw;
get_matcher_req(whitefile_dr) -> raw;
get_matcher_req(Func) -> apply(mydlp_matchers, Func, []).

pl_text(Files) -> pl_text(Files, []).
pl_text([#file{text=undefined} = File|Files], Rets) -> 
	File1 = case mydlp_api:get_text(File) of
		{ok, Text} -> File#file{text = Text};
		{error, cobject} -> File;
		{error, compression} -> File;
		_Else -> File#file{is_encrypted=true}
	end,
	pl_text(Files, [ File1 |Rets]);
pl_text([File|Files], Rets) -> pl_text(Files, [File|Rets]);
pl_text([], Rets) -> lists:reverse(Rets).

is_whitefile(File) ->
	Hash = erlang:md5(File#file.data),
	mydlp_mnesia:is_fhash_of_gid(Hash, [mydlp_mnesia:get_pgid()]).

drop_whitefile(Files) -> lists:filter(fun(F) -> not is_whitefile(F) end, Files).

is_whitefile_dr(File, CustomerId) ->
	Hash = erlang:md5(File#file.data),
	mydlp_mnesia:is_dr_fh_of_fid(Hash, {wl, CustomerId}).

drop_whitefile_dr(Files, CustomerId) -> lists:filter(fun(F) -> not is_whitefile_dr(F, CustomerId) end, Files).

drop_notext(Files) -> lists:filter(fun(I) -> mydlp_api:has_text(I) end, Files).

drop_nodata(Files) -> lists:filter(fun(F) -> size(F#file.data) > 0 end, Files).

