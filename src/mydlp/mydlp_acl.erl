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
	stop/0]).

-ifdef(__MYDLP_NETWORK).

-export([
	q/4,
	qu/3,
	qa/2,
	qm/1
	]).

-endif.

-ifdef(__MYDLP_ENDPOINT).

-export([
	qe/1
	]).

-endif.

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
	is_multisite=false,
	error_action=pass
	}).

%%%%%%%%%%%%% MyDLP ACL API

-ifdef(__MYDLP_NETWORK).

q(Site, Addr, DestList, Files) -> acl_call({q, Site, DestList, {Addr, Files}}).

qu(User, _Dest, Files) -> acl_call({qu, site, {User, Files}}).

qa(DestList, Files) -> acl_call({qa, site, {DestList, Files}}).

qm(Files) -> acl_call({qm, site, {Files}}).

-endif.

-ifdef(__MYDLP_ENDPOINT).

qe(Files) -> acl_call({qe, site, {Files}}).

-endif.

%acl_call(Query) -> acl_call(Query, 1500).
acl_call(Query) -> acl_call(Query, 1500000).
acl_call(Query, Timeout) -> gen_server:call(?MODULE, {acl, Query, Timeout}, Timeout).

%%%%%%%%%%%%%% gen_server handles

-ifdef(__MYDLP_NETWORK).

acl_exec(_RuleTables, _Source, []) -> pass;
acl_exec(RuleTables, Source, Files) ->
	[{cid, CustomerId}|_] = Source,
	DRules = mydlp_mnesia:get_default_rule(CustomerId),
	acl_exec2(head_dr(RuleTables, DRules), Source, Files).

-endif.

acl_exec2([], _Source, _Files) -> pass;
% acl_exec2([{{_Id, DefaultAction}, Rules}| Rest], Source, Files)  % Cannot be more than one filter
acl_exec2([{{_Id, DefaultAction}, Rules}], Source, Files) ->
	case { DefaultAction, acl_exec3(Rules, Source, Files) } of
		% {return, return}-> acl_exec2(Rest, Source, Files);  % Cannot be more than one filter
		{DefaultAction, return} -> DefaultAction;
		{_DefaultAction, Action} -> Action end.

acl_exec3([], _Source, _Files) -> return;
acl_exec3(_AllRules, _Source, []) -> return;
acl_exec3(AllRules, Source, Files) ->
	acl_exec3(AllRules, Source, Files, [], false).

acl_exec3(_AllRules, _Source, [], [], _CleanFiles) -> return;

acl_exec3(AllRules, Source, [], ExNewFiles, false) ->
	acl_exec3(AllRules, Source, [], ExNewFiles, true);

acl_exec3(AllRules, Source, [], ExNewFiles, CleanFiles) ->
	acl_exec3(AllRules, Source, ExNewFiles, [], CleanFiles);
	
acl_exec3(AllRules, Source, Files, ExNewFiles, CleanFiles) ->
	[{cid, CustomerId}|_] = Source,
	
	{InChunk, RestOfFiles} = mydlp_api:get_chunk(Files),
	Files1 = mydlp_api:load_files(InChunk),
	
	Files2 = drop_whitefile_dr(Files1, CustomerId), % these should be cleaned too
	Files3 = case has_wf(AllRules) of
		true -> drop_whitefile(Files2);
		false -> Files2 end,

	{PFiles, NewFiles} = mydlp_api:analyze(Files3),
	PFiles1 = case CleanFiles of
		true -> mydlp_api:clean_files(PFiles); % Cleaning newly created files.
		false -> PFiles end,

	Param = {Source, drop_nodata(PFiles1)},

	case apply_rules(AllRules, Param) of
		return -> acl_exec3(AllRules, Source, RestOfFiles,
				lists:append(ExNewFiles, NewFiles), CleanFiles);
		Else -> Else end.

-ifdef(__MYDLP_NETWORK).

%% it needs refactoring for trusted domains
handle_acl({q, SAddr, DestList, {Addr, Files}}, #state{is_multisite=true}) ->
	case mydlp_mnesia:get_cid(SAddr) of
		nocustomer -> block;
		CustomerId -> 
			Rules = mydlp_mnesia:get_rules_for_cid(CustomerId, DestList, Addr),
			acl_exec(Rules, [{cid, CustomerId}, {addr, Addr}], Files) end;

handle_acl({q, _Site, DestList, {Addr, Files}}, #state{is_multisite=false}) ->
	%Rules = mydlp_mnesia:get_rules(Addr),
	CustomerId = mydlp_mnesia:get_dcid(),
	Rules = mydlp_mnesia:get_rules_for_cid(CustomerId, DestList, Addr),
	acl_exec(Rules, [{cid, CustomerId}, {addr, Addr}], Files);

%% now this is used for only SMTP, and in SMTP domain part of, mail adresses itself a siteid for customer.
%% it needs refactoring for both multisite and trusted domains
handle_acl({qu, _Site, {User, Files}}, _State) ->
	Rules = mydlp_mnesia:get_rules_by_user(User),
	acl_exec(Rules, [{cid, mydlp_mnesia:get_dcid()}, {user, User}], Files);

handle_acl({qa, _Site, {DestList, Files}}, _State) ->
	Rules = mydlp_mnesia:get_all_rules(DestList),
	acl_exec(Rules, [{cid, mydlp_mnesia:get_dcid()}], Files);

handle_acl({qm, _Site, {Files}}, _State) ->
	Rules = mydlp_mnesia:get_all_rules(),
	acl_exec(Rules, [{cid, mydlp_mnesia:get_dcid()}], Files);

handle_acl(Q, _State) -> throw({error, {undefined_query, Q}}).

-endif.

-ifdef(__MYDLP_ENDPOINT).

handle_acl({qe, _Site, {Files}}, _State) ->
	FinalRuleTable = [],
	acl_exec2(FinalRuleTable, [{cid, 0}], Files);

handle_acl(Q, _State) -> throw({error, {undefined_query, Q}}).

-endif.


handle_call({acl, Query, Timeout}, From, State) ->
	Worker = self(),
	mydlp_api:mspawn(fun() ->
		Return = try 
			Result = handle_acl(Query, State),
			{ok, Result}
		catch Class:Error ->
			?ERROR_LOG("Error occured on ACL query: [~w]. Class: [~w]. Error: [~w].~nStack trace: ~w~n",
				[Query, Class, Error, erlang:get_stacktrace()]),
			{error, {Class,Error}} end,
		Worker ! {async_acl_q, Return, From} 
	end, Timeout),
	{noreply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_acl_q, Res, From}, #state{error_action=Action} = State) ->
	Reply = case Res of
		{ok, R} -> R;
		{error, _} -> Action end, % TODO conf

	gen_server:reply(From, Reply),
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

-ifdef(__MYDLP_NETWORK).

init([]) ->
	IsMS = mydlp_mysql:is_multisite(),
	{ok, #state{is_multisite=IsMS, error_action=?CFG(error_action)}}.

-endif.

-ifdef(__MYDLP_ENDPOINT).

init([]) ->
	{ok, #state{is_multisite=false}}.

-endif.

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
apply_rules([], _Params) -> return.

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

apply_f([], Params, _PLT) -> {neg, Params};
apply_f([{whitefile, _FuncParams}|Matchers], {Addr, Files}, PLT) ->
%	% Droping whitefiles to prevent execution of matchers with them
%	execute_matchers(Matchers, {Addr, drop_whitefile(Files)}, PLT);
	execute_matchers(Matchers, {Addr, Files}, PLT);
apply_f([{whitefile_dr, _FuncParams}|Matchers], {Source, Files}, PLT) ->
%	[{cid, CustomerId}|_] = Source,
%	% Droping whitefiles to prevent execution of matchers with them
	execute_matchers(Matchers, {Source, Files}, PLT);
apply_f(Matchers, Params, true) ->    % if pl_text is already called, we can go parallel
					% total file size check could be added 
	PAnyRet = mydlp_api:pany(fun({Func, FuncParams}) ->
					apply_m(Func, [FuncParams, Params]) end, Matchers),
	{FuncRet, Ret} = case PAnyRet of
		false -> {arbitrary_func, neg};
		{ok, {Func, _FuncParams}, R} -> {Func, R} end,

	apply_f_ret([], FuncRet, Params, true, Ret);
apply_f([{Func, FuncParams}|Matchers], Params, PLT) ->   % pl_text is not called already, matchers will be called sequential.
	Ret = apply_m(Func, [FuncParams, Params]),
	apply_f_ret(Matchers, Func, Params, PLT, Ret).

apply_f_ret(Matchers, _Func, Params, PLT, neg) -> 				execute_matchers(Matchers, Params, PLT);
apply_f_ret(_Matchers, Func, _Params, _PLT, {pos, {file, F}}) -> 		{pos, {file, F}, {matcher, Func}, {misc, ""}};
apply_f_ret(_Matchers, Func, _Params, _PLT, {pos, {file, F}, {misc, Misc}}) -> 	{pos, {file, F}, {matcher, Func}, {misc, Misc}};
apply_f_ret(_Matchers, Func, _Params, _PLT, pos) -> 				{pos, {file, #file{name="unknown"}}, {matcher, Func}, {misc, ""}}.

apply_m(Func, [FuncParams, {Addr, Files}]) ->
	Files1 = case get_matcher_req(Func) of
		raw -> Files;
		analyzed -> Files;
		text -> drop_notext(Files) end,
	FuncOpts = get_func_opts(Func, FuncParams),
	apply_mp(Func, FuncOpts, Addr, Files1).

apply_mp(Func, FuncOpts, Addr, Files) ->
	PanyRet = mydlp_api:pany(fun(I) -> apply(mydlp_matchers, Func, [FuncOpts, Addr, I]) end, Files),
	case PanyRet of
		false -> neg;
		{ok, _File, Result} -> Result end.

get_matcher_req(whitefile) -> raw;
get_matcher_req(whitefile_dr) -> raw;
get_matcher_req(Func) -> apply(mydlp_matchers, Func, []).

get_func_opts(Func, FuncParams) -> apply(mydlp_matchers, Func, [FuncParams]).

pl_text(Files) -> pl_text(Files, []).
pl_text([#file{text=undefined} = File|Files], Rets) -> 
	File1 = case mydlp_api:get_text(File) of
		{ok, Text} -> File#file{text = Text};
		{error, cobject} -> File;
		{error, compression} -> File;
		{error, binary_format} -> File;
		{error, image} -> File;
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

has_data(#file{dataref={cacheref, _Ref}}) -> true;
has_data(#file{dataref={memory, Bin}}) -> size(Bin) > 0;
has_data(#file{data=Data}) when is_binary(Data)-> size(Data) > 0;
has_data(Else) -> throw({error, unexpected_obj, Else}).

drop_nodata(Files) -> lists:filter(fun(F) -> has_data(F) end, Files).

has_wf(Rules) ->
	lists:any(fun({_Id, _Action, Matchers}) ->
		case lists:keyfind(whitefile, 1, Matchers) of
			false -> false;
			_Else -> true end end, 
	Rules).

-ifdef(__MYDLP_NETWORK).

head_dr([], []) -> [];
head_dr([{FilterKey, Rules}], DRules) -> [{FilterKey, lists:append(DRules,Rules)}];
head_dr([], DRules) -> [{{0, pass}, DRules}].

-endif.

