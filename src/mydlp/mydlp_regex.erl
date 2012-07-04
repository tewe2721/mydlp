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

%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@mydlp.com>
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_regex).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	replace_bin/3,
	split_bin/2,
	match_bin/2,
	longest_bin/2,
	is_match_bin/2,
	score_suite/2,
	match_count/2,
%	clean/1,
%	clean/0,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {builtin_tree, builtin_suite_tree, u304}).

%%%%%%%%%%%%% MyDLP Thrift RPC API

replace_bin(BInKey, Data, Replace) -> async_re_call({rbin, BInKey, Replace}, Data).

match_bin(BInKey, Data) -> async_re_call({mbin, BInKey}, Data).

longest_bin(BInKey, Data) -> async_re_call({longest_bin, BInKey}, Data).

is_match_bin(BInKey, Data) -> async_re_call({i_mbin, BInKey}, Data).

split_bin(BInKey, Data) -> async_re_call({sbin, BInKey}, Data).

score_suite(BInKey, Data) -> async_re_call({score_suite, BInKey}, Data).

match_count(GIs, Data) -> async_re_call({match_count, GIs}, Data).

%clean() -> gen_server:cast(?MODULE, {clean}).
%clean(GroupId) -> gen_server:cast(?MODULE, {clean, GroupId}).

%%%%%%%%%%%%%% gen_server handles

handle_call({async_re, Call, Data, Timeout}, From, State) ->
	Worker = self(),
	mydlp_api:mspawn(fun() -> 
			Return = try 
				Data1 = preregex(Data, State),
				handle_re(Call, Data1, State)
			catch Class:Error ->
				?ERROR_LOG("Error occured on REGEX call: ["?S"]. Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
					[Call, Class, Error, erlang:get_stacktrace()]),
					{ierror, {Class, Error}} end,
			Worker ! {async_reply, Return, From}
		end, Timeout),
	{noreply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_re({match_count, GIs}, Data, _State) ->
	count_all(GIs, Data);

handle_re({mbin, BInKey}, Data, #state{builtin_tree=BT}) ->
	RE = gb_trees:get(BInKey, BT),
	case re:run(Data, RE, [global, {capture, all, list}]) of
		nomatch -> [];
		{match, Captured} -> lists:append(Captured) end;

handle_re({longest_bin, BInKey}, Data, #state{builtin_tree=BT}) ->
	RE = gb_trees:get(BInKey, BT),
	case re:run(Data, RE, [global, {capture, all, index}]) of
		nomatch -> 0;
		{match, Captured} ->  
			Lengths = [ L || [{_, L}|_] <- Captured],
			lists:max(Lengths) end;

handle_re({i_mbin, BInKey}, Data, #state{builtin_tree=BT}) ->
	RE = gb_trees:get(BInKey, BT),
	case re:run(Data, RE, [{capture, first}]) of
		nomatch -> false;
		{match, _Range} -> true end;

handle_re({sbin, BInKey}, Data, #state{builtin_tree=BT}) ->
	RE = gb_trees:get(BInKey, BT),
	re:split(Data, RE, [{return, list}, trim]);

handle_re({rbin, BInKey, Replace}, Data, #state{builtin_tree=BT}) ->
	RE = gb_trees:get(BInKey, BT),
	re:replace(Data, RE, Replace, [global, {return, list}]);

handle_re({score_suite, BInKey}, Data, #state{builtin_suite_tree=BST}) ->
	RES = gb_trees:get(BInKey, BST),
	score_regex_suite(RES, Data);

handle_re(Call, _Data, _State) -> throw({error,{unhandled_re_call,Call}}).

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid} end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	BInREs = [
		%{credit_card, rec("\\b(?:\\d[ -]{0,4}?){13,16}\\b")}
		{credit_card, rec("(?:\\d[ -]{0,4}){13,16}", [unicode])},
		{iban, rec("(?:[a-zA-Z][ -]{0,4}){2}(?:[0-9][ -]{0,4}){2}(?:[a-zA-Z0-9][ -]{0,4}){4}(?:[0-9][ -]{0,4}){7}(?:[a-zA-Z0-9][ -]{0,4}){0,16}", [unicode])},
		{aba, rec("\\d{4}-?\\d{4}-?\\d", [unicode])},
		{trid, rec("(?:\\d[ -]{0,2}){11}", [unicode])},
		{ssn, rec("\\s(?:\\d{3}-\\d{2}-\\d{4})\\s", [unicode])},
		%{ssn, rec("(?:\\d{3} ?-? ?\\d{2} ?-? ?\\d{4})")},
		{sin, rec("(?:\\d{3} ?-? ?\\d{3} ?-? ?\\d{3})", [unicode])},
		%{insee, rec("(?:\\d{1} ?-? ?\\d{2} ?-? ?\\d{2} ?-? ?\\d{5} ?-? ?\\d{3} ?-? ?\\d{2})")},
		{insee, rec("(?:\\d{1} ?\\d{2} ?\\d{2} ?\\d{5} ?\\d{3} ?(?:\\d{2})?)", [unicode])},
		{nino, rec("(?:[A-Za-z]{2}\\d{6}[A-Za-z]{0,1})", [unicode])},
		{said, rec("(?:\\d[ -]{0,1}){13}", [unicode])},
		%{nino, rec("(?:[A-Za-z]{2} ?-? ?\\d{2} ?-? ?\\d{2} ?-? ?\\d{2} ?-? ?[A-Za-z]{0,1})")},
		{nonwc, rec("[^A-Za-z0-9]+", [unicode])},
		{sentence, rec("[\\n\\r\\t\\.!?]+\\s{0,1}\\){0,1}\\s+", [unicode])},
		{word, rec("\\d*(?:[.,]\\d+)+|[\\w\\p{L}]+-[\\w\\p{L}]+|[\\w\\p{L}]+", [unicode])},
		{hexencoded, rec("[a-fA-F0-9\\r\\n]{512,}")},
		{base64encoded, rec("[\\+/a-zA-Z0-9\\r\\n]{256,}(?:={1,}|[\\r\\n])")},
		{pan, rec("\\s(?:[A-Z]){5}(?:[0-9]){4}(?:[A-Z]){1}\\s", [unicode])},
		{cpf, rec("(?:\\d{3}\\s{0,1}\.\\s{0,1}){2}(?:\\d{3}\\s{0,1}-\\s{0,1}\\d{2})", [unicode])},
		{icn, rec("\\s(?:\\d{17}[\\dX])\\s", [unicode])},
		{edate, rec("(?:\\d{2}[/-]\\d{2})", [unicode])},
		{birthdate, rec("(?:(?:\\d{2}[/-.]){2}(?:\\d{4}))|(?:\\d{2}[/-.\\s][A-Za-z]{3}[/-.\\s]\\d{4})|"
				"(?:[A-Za-z]{3}\\s{1,5}\\d{1,2}(?:,){0,1}\\s{1,5}\\d{4})|"
				"(?:\\d{4}[/-.]\\d{2}[\-.]\\d{2})", [unicode])}
		%{birthdate, rec("(?:(?:\\d{2}[/-]){2}(?:\\d{4}))|(?:[A-Za-z]{3} \\d{2}[-] \\d{4})", [unicode])}
	],
	BT = insert_all(BInREs, gb_trees:empty()),

	% Format: {key, [{regex, weigth}]}
	BInS = [
		%% Source code detection
		{scode, [

%% !=\|&&\|||\|==\|>>\|<<
{rec("!=|&&|\\|\\||==|>>|<<", [multiline, ungreedy]), 1},

%% int\b\|char\b\|void\b\|const\b\
{rec("(?:int|char|void|const|enum|typedef)\\s+[a-zA-Z0-9_]+", [multiline, ungreedy]), 2},

%% /\*\|\*/\|[^:]*//
{rec("/\\*.*\\*/", [multiline, ungreedy]), 2},

%% hrdLocations.get(classID)
{rec("[a-zA-Z0-9_()]+\\.[a-zA-Z0-9_]+\\([^).]*\\)", [multiline, ungreedy]), 4},

%% new FileErrorHandler(dfis->getLocation(), Encodings::ENC_UTF8, false) 
{rec("\\bnew\\b\\s*[a-zA-Z0-9_]+\\([^).]*\\)", [multiline, ungreedy]), 4},

%% TextHRDMapper *mapper = new TextHRDMapper() 
%% TextHRDMapper ^mapper = new TextHRDMapper() 
%{rec("([a-zA-Z0-9_]+)\\s*\\^\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*\\1\\([^).]*\\);"), 10},
%{rec("([a-zA-Z0-9_]+)\\s*\\*\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*\\1\\([^).]*\\);"), 10},
{rec("([a-zA-Z0-9_]+)\\s*[*^]\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*\\1\\([^).]*\\);", [multiline, ungreedy]), 10},


%% ParserFactory::ParserFactory()
%% path->startsWith(DString("file://") 
%{rec("[a-zA-Z0-9_]+::[a-zA-Z0-9_]+\\([^).]*\\)"), 6},
%{rec("[a-zA-Z0-9_]+->[a-zA-Z0-9_]+\\([^).]*\\)"), 6},
{rec("[a-zA-Z0-9_]+(?:->|::)[a-zA-Z0-9_]+\\([^).]*\\)", [multiline, ungreedy]), 6},

%% #ifndef\b\|#define\b\|#ifdef\b\|#include\s*[<\"]
{rec("#ifndef\\b|#define\\b|#ifdef\\b|#include\\s*[<\"]", [multiline, ungreedy]), 6},

%% package com.deneme.hibernate;
%% import org.hibernate.Session; 
%{rec("^[ ]*package \\s*[a-zA-Z0-9_\\.]+;"), 6},
%{rec("^[ ]*import \\s*[a-zA-Z0-9_\\.]+;"), 6},
{rec("^[ ]*(?:import|package) \\s*[a-zA-Z0-9_\\.]+;", [multiline, ungreedy]), 6},

%% public class Uygulama 
%% public interface Uygulama
%{rec("(?:(?:(?:public|private|protected|) \\s*){0,1}((static|abstract|) \\s*){0,1}|^\\s*)class \\s*[a-zA-Z0-9_<>]+"), 6},
%{rec("(?:(?:(?:public|private|protected|) \\s*){0,1}((static|abstract|) \\s*){0,1}|^\\s*)interface \\s*[a-zA-Z0-9_<>]+"), 6},
{rec("(?:(?:(?:public|private|protected|) \\s*){0,1}(?:(?:static|abstract|) \\s*){0,1}|^\\s*)(?:class|interface) \\s*[a-zA-Z0-9_<>]+", [multiline, ungreedy]), 6},

%% public static void main( String[] args )
{rec("(?:(?:public|private|protected)\\s+)?(?:(?:static|abstract)\\s+)?[a-zA-Z0-9_<>]+\\s+[a-zA-Z0-9_]+\\s*\\(\\s*[^'`][^).]*\\)", [multiline, ungreedy]), 10},

%% Stock<T> stock = new Stock<T>() - weight = 6
%% Stock stock = new Stock();
{rec("([a-zA-Z0-9_<>]+)\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*\\1\\([^).]*\\)", [multiline, ungreedy]), 10},

%% IStock<T> stock = new Stock<T>() - weight = 4
%% IStock stock = new Stock();
{rec("[a-zA-Z0-9_<>]+\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*[a-zA-Z0-9_<>]+\\([^).]*\\)", [multiline, ungreedy]), 10},

{rec("(?:public:|private:|protected:)", [multiline, ungreedy]), 4}

		]},
		{scode_ada, [

%% ADA support
{rec("package\\s+(?:body\\s+)?([\\w\\pP\\pS_]+)\\s+is[\\w\\s\\pP\\pS]*end\\s+\\1\\s*;", [multiline, caseless, ungreedy]), 22},

{rec("procedure\\s+([\\w\\pS\\pP_]+)\\s*(?:\\(\\s*(?:[\\w_]+\\s*:\\s*(?:in|in\\s+out)\\s*[\\w_]+\\s*;?[\\s]*)*\\))?(?:\\s+is\\s*[\\w\\s\\pP\\pS]*begin[\\w\\s\\pP\\pS]*end\\s+\\1)\\s*;", [multiline, caseless, ungreedy]), 59},

{rec("function\\s+([\\w\\pS\\pP_]+)\\s*(?:\\((?:\\s*[\\w_]+\\s*:\\s*(?:in|in\\s+out)\\s*[\\w_]+\\s*;?[\\s]*)*\\))?\\s*return\\s+[\\w_\\.]+(?:\\s+is\\s*[\\w\\s\\pP\\pS]*begin[\\w\\s\\pP\\pS]*end\\s+\\1)?\\s*;", [multiline, caseless, ungreedy]), 73},

{rec("while[\\w\\s\\pP\\pS]*loop[\\w\\s\\pP\\pS]*end\\s+loop\\s*;", [multiline, caseless, ungreedy]), 7}

%{rec("for\\s+[\\w\\s\\pP\\pS]+\\s+in\\s+[\\w\\s\\pP\\pS]+loop[\\w\\s\\pP\\pS]*end\\s+loop\\s*;", [multiline, caseless, ungreedy]), 10}

		]}
	],
	BST = insert_all(BInS, gb_trees:empty()),

	U304 = rec("\x{0130}", [unicode]),

	{ok, #state{builtin_tree=BT, builtin_suite_tree=BST, u304=U304}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

preregex(Data, #state{u304=U304}) ->
	re:replace(Data, U304, <<"i">>, [global, {return, binary}]).

count_all(GIs, Data) ->
	Regexes = lists:flatten(
		[[ mydlp_mnesia:get_regexes(GI) || GI <- GIs ]] ),
	RRMap = mydlp_api:pmap(fun(R) -> count_expr(R, Data) end, Regexes),
	lists:sum(RRMap).

-define(CNT_RE_OPTS, [global, {capture, all, index}]).

count_expr(RE, Data) ->
	case re:run(Data, RE, ?CNT_RE_OPTS) of
		nomatch -> 0;
		{match, Captured} -> length(Captured) end.

insert_all([{Key, Val}|Rest], Tree) -> insert_all(Rest, gb_trees:enter(Key, Val, Tree));
insert_all([], Tree) -> Tree.

rec(Regex) -> rec(Regex,[]).
rec(Regex, ReOpts) -> 
	{ok, Ret} = case ReOpts of
		[] -> re:compile(Regex);
		ReOpts -> re:compile(Regex, ReOpts) end,
	Ret.

%score_regex_suite(Regexes, Data) -> score_regex_suite(Regexes, Data, 0).

%score_regex_suite([{RE,Weight}|Regexes], Data, Score) ->
%	Count = case re:run(Data, RE, ?CNT_RE_OPTS) of
%		nomatch -> 0;
%		{match, Captured} -> length(Captured) end,
%	score_regex_suite(Regexes, Data, Score + (Weight * Count) );

%score_regex_suite([], _Data, Score) -> Score.

% Now we go parallel
score_regex_suite(Regexes, Data) ->
	Scores = mydlp_api:pmap(fun(I) -> score_regex_suite1(I, Data) end, Regexes, 180000),
	lists:sum(Scores).

score_regex_suite1({RE,Weight}, Data) ->
	Count = case re:run(Data, RE, ?CNT_RE_OPTS) of
		nomatch -> 0;
		{match, Captured} -> length(Captured) end,
	(Weight * Count).

async_re_call(Query, Data) -> async_re_call(Query, Data, 180000).

async_re_call(Query, Data, Timeout) -> 
	case gen_server:call(?MODULE, {async_re, Query, Data, Timeout}, Timeout) of
		{ierror, {Class, Reason}} -> mydlp_api:exception(Class, Reason);
		Else -> Else end.

