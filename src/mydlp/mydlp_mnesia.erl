%%
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
%%% @doc Persistency api for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_mnesia).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

%% API
-export([start_link/0,
	get_rules/1,
	dyn_query/2,
	get_record_fields/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(state, {}).

%%%%%%%%%%%%%%%% Table definitions

-define(TABLES, [filter, rule, ipr, match, match_group]).


get_record_fields(Record) -> 
        case Record of
		unique_ids -> record_info(fields, unique_ids);
		filter -> record_info(fields, filter);
		rule -> record_info(fields, rule);
		ipr -> record_info(fields, ipr);
		match -> record_info(fields, match);
		match_group -> record_info(fields, match_group)
	end.

%%%%%%%%%%%%% MyDLP Mnesia API

get_rules(Who) ->
	gen_server:call(?MODULE, {get_rules, Who}).

dyn_query(Def, Args) ->
	gen_server:call(?MODULE, {dyn_query, Def, Args}).

dyn_query1({_, Table, _} = Def, Args) ->
	{QS, BS} = dyn_query2(Def,Args),
	erlang:display(QS),
	erlang:display(BS),
	Q = qlc:string_to_handle(QS, [], BS),
	qlc:e(Q).

dyn_query2({list, Table, Ands}, Args) ->
	QS = "[ I || I <- L ",
	BS = erl_eval:add_binding('L', mnesia:table(Table), erl_eval:new_bindings()),
	{QS1,BS1} = dyn_query3(QS, BS, Table, Ands, Args),
	{QS1 ++ " ].", BS1}.

dyn_query3(QS, BS, Table, [And|Ands], [Arg|Args]) ->
	QS1 = QS ++ ", I#" ++ atom_to_list(Table) ++ "." ++ atom_to_list(And) ++ " == A" ++ atom_to_list(And) ++ " ",
	BS1 = erl_eval:add_binding(list_to_atom("A"++atom_to_list(And)), Arg, BS),
	dyn_query3(QS1, BS1, Table, Ands, Args);
dyn_query3(QS, BS, _Table, [], []) -> {QS, BS}.

%%%%%%%%%%%%%% gen_server handles

handle_call({dyn_query, Def, Args}, From, State) ->
	Worker = self(),
	spawn_link(fun() ->
		F = fun() ->
			dyn_query1(Def, Args)
		end,
		{atomic, Objects} = transaction(F),
		Worker ! {async_reply, Objects, From}
	end),
	{noreply, State, 5000};

handle_call({get_rules, Who}, From, State) ->
	Worker = self(),
	spawn_link(fun() ->
		F = fun() ->
			Q = qlc:q([I#ipr.parent || I <- mnesia:table(ipr),
					ip_band(I#ipr.ipbase, I#ipr.ipmask) == ip_band(Who, I#ipr.ipmask)
					]),
			Parents = qlc:e(Q),
			Rules = resolve_rules(Parents),
			resolve_funcs(Rules)
		end,
		{atomic, Objects} = transaction(F),
		Worker ! {async_reply, Objects, From}
	end),
	{noreply, State, 5000};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
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

init([]) ->
	mnesia:create_schema([node()]),
	mnesia:start(),

	start_table({unique_ids, set}),
	start_tables(?TABLES),

	{ok, #state{}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%%

start_table(RecordAtom) when is_atom(RecordAtom) ->
	start_table({RecordAtom, ordered_set});

start_table({RecordAtom, TableType}) ->
	start_table({RecordAtom, TableType, fun() -> ok end});

start_table({RecordAtom, TableType, InitFun}) ->
	try
		mnesia:table_info(RecordAtom, type)
	catch
		exit: _ ->
			mnesia:create_table(RecordAtom,
					[{attributes, 
						get_record_fields(RecordAtom) },
						{type, TableType},
						{disc_copies, [node()]}]),

			transaction(InitFun)
	end.

start_tables([RecordAtom|RAList]) ->
	start_table(RecordAtom),
	start_tables(RAList);
start_tables([]) -> ok.

%get_unique_id(TableName) ->
%	mnesia:dirty_update_counter(unique_ids, TableName, 1).

transaction(F) ->
	try {atomic, mnesia:activity(transaction, F)}
	catch
		exit:Reason ->
			{aborted, Reason}
	end.

ip_band({A1,B1,C1,D1}, {A2,B2,C2,D2}) -> {A1 band A2, B1 band B2, C1 band C2, D1 band D2}.

resolve_rules(PS) -> resolve_rules(PS,[]).
resolve_rules([P|PS], Rules) -> resolve_rules(PS, [resolve_rule(P)| Rules]);
resolve_rules([], Rules) -> lists:reverse(Rules).

resolve_rule({mgroup, Id}) ->
	Q = qlc:q([MG#match_group.parent || 
			MG <- mnesia:table(match_group),
			MG#match_group.id == Id
			]),
	[Parent] = qlc:e(Q),
	resolve_rule(Parent);
resolve_rule({rule, Id}) ->
	Q = qlc:q([{R#rule.id, R#rule.action} || 
			R <- mnesia:table(rule), 
			F <- mnesia:table(filter),
			R#rule.id == Id, 
			R#rule.is_active == true,
			F#filter.id == R#rule.filter_id, 
			F#filter.is_active == true
			]),
	[Rule] = qlc:e(Q), Rule.
	
resolve_funcs(Rules) -> resolve_funcs(Rules,[]).
resolve_funcs([{Id,Action}|Rules], Results) -> 
	resolve_funcs(Rules, [{Id, Action, find_funcs({rule, Id})}|Results]);
resolve_funcs([], Results) -> lists:reverse(Results).

find_funcss(Parents) -> find_funcss(Parents, []).
find_funcss([Parent|Parents], Funcs) ->
	find_funcss(Parents, [find_funcs(Parent)|Funcs]);
find_funcss([], Funcs) -> lists:reverse(Funcs).

find_funcs(ParentId) ->
	QM = qlc:q([{M#match.func, M#match.func_params} ||
			M <- mnesia:table(match),
			M#match.parent == ParentId
		]),
	QMG = qlc:q([{mgroup, MG#match_group.id} ||
			MG <- mnesia:table(match_group),
			MG#match_group.parent == ParentId
		]),
	lists:flatten([qlc:e(QM), find_funcss(qlc:e(QMG))]).
