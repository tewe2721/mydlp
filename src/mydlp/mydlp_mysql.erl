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
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_mysql).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

%% API
-export([start_link/0,
	compile_filters/0,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {host, port, user, password, database, pool_size, master_pid, pool_pids}).

%%%%%%%%%%%%% MyDLP Thrift RPC API

compile_filters() ->
	gen_server:call(?MODULE, compile_filters).

%%%%%%%%%%%%%% gen_server handles

psq(PreparedKey) when is_atom(PreparedKey) ->
	psq(PreparedKey, []).

psq(PreparedKey, Params) when is_atom(PreparedKey), is_list(Params) ->
	case mysql:execute(p, PreparedKey, Params) of
		{data,{mysql_result,_,Result,_,_}} -> {ok, Result};
		Else -> {error, Else}
	end.

handle_call(compile_filters, _From, State) ->
	Reply = populate(),

	{reply, Reply, State};

handle_call(stop, _From,  State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info({'DOWN', _, _, MPid , _}, #state{master_pid=MPid} = State) ->
	{stop, normal, State};

handle_info({'DOWN', _, _, Pid , _}, #state{host=Host,
		user=User, password=Password, database=DB, 
		pool_pids=PoolPids} = State) ->
	PoolPids1 = lists:delete(Pid, PoolPids),
	{ok,NewPid} = mysql:connect(p, Host, undefined, User, Password, DB, true),
	{stop, normal, State#state{pool_pids=[NewPid|PoolPids1]}};

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
	ConfList = case application:get_env(mysql) of
		{ok, CL} -> CL;
		_Else -> ?MYSQL
	end,

	{host, Host} = lists:keyfind(host, 1, ConfList),
	{port, Port} = lists:keyfind(port, 1, ConfList),
	{user, User} = lists:keyfind(user, 1, ConfList),
	{password, Password} = lists:keyfind(password, 1, ConfList),
	{database, DB} = lists:keyfind(database, 1, ConfList),
	{pool_size, PoolSize} = lists:keyfind(pool_size, 1, ConfList),

	{ok, MPid} = mysql:start_link(p, Host, Port, User, Password, DB, fun(_,_,_,_) -> ok end),
	erlang:monitor(process, MPid), 
	
	PoolReturns = [ mysql:connect(p, Host, undefined, User, Password, DB, true) || _I <- lists:seq(1, PoolSize)],
	PPids = [ P || {ok, P} <- PoolReturns ],
	[ erlang:monitor(process, P) || P <- PPids ],

	[ mysql:prepare(Key, Query) || {Key, Query} <- [
		{filters, <<"SELECT id,name FROM sh_filter WHERE is_active=TRUE">>},
		{rules_by_fid, <<"SELECT id,action FROM sh_rule WHERE is_nw_active=TRUE and filter_id=?">>},
		{ipr_by_rule_id, <<"SELECT a.id,a.base_ip,a.subnet FROM sh_ipr AS i, sh_ipaddress AS a WHERE i.parent_rule_id=? AND i.sh_ipaddress_id=a.id">>},
		{match_by_rule_id, <<"SELECT m.id,m.func FROM sh_match AS m, sh_func_params AS p WHERE m.parent_rule_id=? AND p.match_id=m.id AND p.param <> \"0\" ">>},
		{params_by_match_id, <<"SELECT p.param FROM sh_match AS m, sh_func_params AS p WHERE m.id=? AND p.match_id=m.id AND p.param <> \"0\" ">>},
		{file_params_by_match_id, <<"SELECT DISTINCT m.enable_shash, m.enable_bayes, m.enable_whitefile FROM sh_match AS m, sh_func_params AS p WHERE m.id=? AND p.match_id=m.id AND p.param <> \"0\" ">>},
		{mimes, <<"SELECT m.id, c.group_id, m.mime, m.extension FROM nw_mime_type_cross AS c, nw_mime_type m WHERE c.mime_id=m.id">>},
		{regexes, <<"SELECT r.id, c.group_id, r.regex FROM sh_regex_cross AS c, sh_regex r WHERE c.regex_id=r.id">>}
	]],

	{ok, #state{host=Host, port=Port, 
			user=User, password=Password, 
			database=DB, pool_size=PoolSize, 
			master_pid=MPid, pool_pids=PPids}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%% internal

populate() -> 
	{ok, FQ} = psq(filters),
	populate_filters(FQ),
	{ok, MQ} = psq(mimes),
	populate_mimes(MQ),
	{ok, RQ} = psq(regexes),
	populate_regexes(RQ),
	ok.

populate_filters([[Id, Name]|Rows]) ->
	{ok, RQ} = psq(rules_by_fid, [Id]),
	populate_rules(RQ, Id),
	F = #filter{id=Id, name=Name},
	mydlp_mnesia:write(F),
	populate_filters(Rows);
populate_filters([]) -> ok.

populate_rules([[Id, <<"block">>]|Rows], FilterId) ->
	populate_rule(Id, block, FilterId),
	populate_rules(Rows, FilterId);
populate_rules([[Id, <<"log">>]|Rows], FilterId) ->
	populate_rule(Id, log, FilterId),
	populate_rules(Rows, FilterId);
populate_rules([[Id, <<"pass">>]|Rows], FilterId) ->
	populate_rule(Id, pass, FilterId),
	populate_rules(Rows, FilterId);
populate_rules([], _FilterId) -> ok.

populate_rule(Id, Action, FilterId) ->
	Parent = {rule, Id},
	{ok, IQ} = psq(ipr_by_rule_id, [Id]),
	populate_iprs(IQ, Parent),
	{ok, MQ} = psq(match_by_rule_id, [Id]),
	populate_matches(MQ, Parent),
	%{ok, MGQ} = psq(mgroup_by_rule_id, [Id]),
	%populate_matchGroups(MGQ, Parent),
	R = #rule{id=Id, action=Action, filter_id=FilterId},
	mydlp_mnesia:write(R).

populate_iprs([[Id, Base, Subnet]| Rows], Parent) ->
	B1 = int_to_ip(Base),
	S1 = int_to_ip(Subnet),
	I = #ipr{id=Id, parent=Parent, ipbase=B1, ipmask=S1},
	mydlp_mnesia:write(I),
	populate_iprs(Rows, Parent);
populate_iprs([], _Parent) -> ok.

int_to_ip(N4) ->
	I4 = N4 rem 256,
	N3 = N4 div 256,
	I3 = N3 rem 256,
	N2 = N3 div 256,
	I2 = N2 rem 256,
	N1 = N2 div 256,
	I1 = N1,
	I1 = N1 rem 256,
	{I1, I2, I3, I4}.

populate_matches([[Id, Func]| Rows], Parent) ->
	populate_match(Id, Func, Parent),
	populate_matches(Rows, Parent);
populate_matches([], _Parent) -> ok.

populate_match(Id, <<"e_archive">>, Parent) ->
	Func = e_archive_match,
	M = #match{id=Id, parent=Parent, func=Func},
	mydlp_mnesia:write(M);

populate_match(Id, <<"e_file">>, Parent) ->
	Func = e_file_match,
	M = #match{id=Id, parent=Parent, func=Func},
	mydlp_mnesia:write(M);

populate_match(Id, <<"trid">>, Parent) ->
	Func = trid_match,
	[CountS] = get_func_params(Id),
	FuncParams = [{count, binary_to_integer(CountS)}],
	M = #match{id=Id, parent=Parent, func=Func, func_params=FuncParams},
	mydlp_mnesia:write(M);

populate_match(Id, <<"ssn">>, Parent) ->
	Func = ssn_match,
	[CountS] = get_func_params(Id),
	FuncParams = [{count, binary_to_integer(CountS)}],
	M = #match{id=Id, parent=Parent, func=Func, func_params=FuncParams},
	mydlp_mnesia:write(M);

populate_match(Id, <<"iban">>, Parent) ->
	Func = iban_match,
	[CountS] = get_func_params(Id),
	FuncParams = [{count, binary_to_integer(CountS)}],
	M = #match{id=Id, parent=Parent, func=Func, func_params=FuncParams},
	mydlp_mnesia:write(M);

populate_match(Id, <<"cc">>, Parent) ->
	Func = cc_match,
	[CountS] = get_func_params(Id),
	FuncParams = [{count, binary_to_integer(CountS)}],
	M = #match{id=Id, parent=Parent, func=Func, func_params=FuncParams},
	mydlp_mnesia:write(M);

populate_match(Id, <<"mime">>, Parent) ->
	Func = mime_match,
	GroupsS = get_func_params(Id),
	FuncParams = [ binary_to_integer(G) || G <- GroupsS ],
	M = #match{id=Id, parent=Parent, func=Func, func_params=FuncParams},
	mydlp_mnesia:write(M);

populate_match(Id, <<"regex">>, Parent) ->
	Func = regex_match,
	GroupsS = get_func_params(Id),
	FuncParams = [ binary_to_integer(G) || G <- GroupsS ],
	M = #match{id=Id, parent=Parent, func=Func, func_params=FuncParams},
	mydlp_mnesia:write(M);

populate_match(Id, <<"file">>, Parent) ->
	Func = file_match,
	GroupsS = get_func_params(Id),
	GroupsI = [ binary_to_integer(G) || G <- GroupsS ],
	{ok, [[SentenceHashI, BayesI, WhiteFileI]]} = psq(file_params_by_match_id, [Id]),
	SentenceHash = case SentenceHashI of 0 -> false; _ -> true end,
	Bayes = case BayesI of 0 -> false; _ -> true end,
	WhiteFile = case WhiteFileI of 0 -> false; _ -> true end,

	FuncParams = [{shash,SentenceHash}, {bayes, Bayes}, {whitefile, WhiteFile}, {groups, GroupsI}],
	
	M = #match{id=Id, parent=Parent, func=Func, func_params=FuncParams},
	mydlp_mnesia:write(M);

populate_match(_, _, _) -> ok.

get_func_params(MatchId) ->
	{ok, PQ} = psq(params_by_match_id, [MatchId]),
	lists:append(PQ).

binary_to_integer(Bin) -> list_to_integer(binary_to_list(Bin)).

populate_mimes([[Id, GroupId, Mime, Ext]|Rows]) ->
	M = #mime_type{id=Id, group_id=GroupId, mime=Mime, extension=Ext},
	mydlp_mnesia:write(M),
	populate_mimes(Rows);
populate_mimes([]) -> ok.


populate_regexes(Rows) -> 
	populate_regexes1(Rows),
	mydlp_mnesia:compile_regex().

populate_regexes1([[Id, GroupId, Regex]|Rows]) ->
	R = #regex{id=Id, group_id=GroupId, plain=Regex},
	mydlp_mnesia:write(R),
	populate_regexes1(Rows);
populate_regexes1([]) -> ok.

