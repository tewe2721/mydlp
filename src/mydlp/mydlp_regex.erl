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
-module(mydlp_regex).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	replace_bin/3,
	split_bin/2,
	match_bin/2,
	is_match_bin/2,
	score_suite/2,
	match/2,
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

is_match_bin(BInKey, Data) -> async_re_call({i_mbin, BInKey}, Data).

split_bin(BInKey, Data) -> async_re_call({sbin, BInKey}, Data).

score_suite(BInKey, Data) -> async_re_call({score_suite, BInKey}, Data).

match([GI|GIs], Data) -> 
	case match1(GI, Data) of
		true -> true;
		false -> match(GIs, Data)
	end;
match([], _Data) -> false.

match1(GroupId, Data) -> async_re_call({match, GroupId}, Data).

%clean() -> gen_server:cast(?MODULE, {clean}).
%clean(GroupId) -> gen_server:cast(?MODULE, {clean, GroupId}).

%%%%%%%%%%%%%% gen_server handles

handle_call({async_re, Call, Data}, From, State) ->
	Worker = self(),
	spawn_link(fun() -> 
			Data1 = preregex(Data, State),
			Ret = handle_re(Call, Data1, State),
			Worker ! {async_reply, Ret, From}
		end),
	{noreply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_re({match, GroupId}, Data, _State) ->
	Regexes = mydlp_mnesia:get_regexes(GroupId),
	matches_any(Regexes, Data);

handle_re({mbin, BInKey}, Data, #state{builtin_tree=BT}) ->
	RE = gb_trees:get(BInKey, BT),
	case re:run(Data, RE, [global, {capture, all, list}]) of
		nomatch -> [];
		{match, Captured} -> lists:append(Captured) end;

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
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	BInREs = [
		%{credit_card, rec("\\b(?:\\d[ -]{0,4}?){13,16}\\b")}
		{credit_card, rec("(?:\\d[ -]{0,4}){13,16}")},
		{iban, rec("(?:[a-zA-Z][ -]{0,4}){2}(?:[0-9][ -]{0,4}){2}(?:[a-zA-Z0-9][ -]{0,4}){4}(?:[0-9][ -]{0,4}){7}(?:[a-zA-Z0-9][ -]{0,4}){0,16}")},
		{trid, rec("(?:\\d[ -]{0,2}){11}")},
		{ssn, rec("(?:\\d{3}-\\d{2}-\\d{4})")},
		%{ssn, rec("(?:\\d{3} ?-? ?\\d{2} ?-? ?\\d{4})")},
		{sin, rec("(?:\\d{3} ?-? ?\\d{3} ?-? ?\\d{3})")},
		%{insee, rec("(?:\\d{1} ?-? ?\\d{2} ?-? ?\\d{2} ?-? ?\\d{5} ?-? ?\\d{3} ?-? ?\\d{2})")},
		{insee, rec("(?:\\d{1} ?\\d{2} ?\\d{2} ?\\d{5} ?\\d{3} ?(?:\\d{2})?)")},
		{nino, rec("(?:[A-Za-z]{2}\\d{6}[A-Za-z]{0,1})")},
		%{nino, rec("(?:[A-Za-z]{2} ?-? ?\\d{2} ?-? ?\\d{2} ?-? ?\\d{2} ?-? ?[A-Za-z]{0,1})")},
		{nonwc, rec("[^A-Za-z0-9]+")},
		{sentence, rec("[\\n\\r\\t\\.!?]+\\s{0,1}\\){0,1}\\s+")},
		{word, rec("\\d*(?:[.,]\\d+)+|[\\w\\p{L}]+-[\\w\\p{L}]+|[\\w\\p{L}]+")},
		{hexencoded, rec("[a-fA-F0-9\\r\\n]{512,}")},
		{base64encoded, rec("[\\+/a-zA-Z0-9\\r\\n]{256,}(?:={1,}|[\\r\\n])")}
	],
	BT = insert_all(BInREs, gb_trees:empty()),

	% Format: {key, [{regex, weigth}]}
	BInS = [
		%% Source code detection
		{scode, [

%% !=\|&&\|||\|==\|>>\|<<
{rec("!=|&&|\\|\\||==|>>|<<"), 2},

%% int\b\|char\b\|void\b\|const\b\
{rec("int\\b|char\\b|void\\b|const\\b|enum\\b|typedef\\b"), 2},

%% /\*\|\*/\|[^:]*//
{rec("/\\*|^\\s*\\*+\\s|\\*/|^//|[^:]//"), 2},

%% hrdLocations.get(classID)
{rec("[a-zA-Z0-9_()]+\\.[a-zA-Z0-9_]+\\([^).]*\\)"), 4},

%% new FileErrorHandler(dfis->getLocation(), Encodings::ENC_UTF8, false) 
{rec("\\bnew\\b\\s*[a-zA-Z0-9_]+\\([^).]*\\)"), 4},

%% TextHRDMapper *mapper = new TextHRDMapper() 
%% TextHRDMapper ^mapper = new TextHRDMapper() 
%{rec("([a-zA-Z0-9_]+)\\s*\\^\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*\\1\\([^).]*\\);"), 10},
%{rec("([a-zA-Z0-9_]+)\\s*\\*\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*\\1\\([^).]*\\);"), 10},
{rec("([a-zA-Z0-9_]+)\\s*[*^]\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*\\1\\([^).]*\\);"), 10},


%% ParserFactory::ParserFactory()
%% path->startsWith(DString("file://") 
%{rec("[a-zA-Z0-9_]+::[a-zA-Z0-9_]+\\([^).]*\\)"), 6},
%{rec("[a-zA-Z0-9_]+->[a-zA-Z0-9_]+\\([^).]*\\)"), 6},
{rec("[a-zA-Z0-9_]+(?:->|::)[a-zA-Z0-9_]+\\([^).]*\\)"), 6},

%% #ifndef\b\|#define\b\|#ifdef\b\|#include\s*[<\"]
{rec("#ifndef\\b|#define\\b|#ifdef\\b|#include\\s*[<\"]"), 6},

%% package com.deneme.hibernate;
%% import org.hibernate.Session; 
%{rec("^[ ]*package \\s*[a-zA-Z0-9_\\.]+;"), 6},
%{rec("^[ ]*import \\s*[a-zA-Z0-9_\\.]+;"), 6},
{rec("^[ ]*(?:import|package) \\s*[a-zA-Z0-9_\\.]+;"), 6},

%% public class Uygulama 
%% public interface Uygulama
%{rec("(?:(?:(?:public|private|protected|) \\s*){0,1}((static|abstract|) \\s*){0,1}|^\\s*)class \\s*[a-zA-Z0-9_<>]+"), 6},
%{rec("(?:(?:(?:public|private|protected|) \\s*){0,1}((static|abstract|) \\s*){0,1}|^\\s*)interface \\s*[a-zA-Z0-9_<>]+"), 6},
{rec("(?:(?:(?:public|private|protected|) \\s*){0,1}(?:(?:static|abstract|) \\s*){0,1}|^\\s*)(?:class|interface) \\s*[a-zA-Z0-9_<>]+"), 6},

%% public static void main( String[] args )
{rec("(?:public|private|protected|) \\s*(?:static|abstract|) \\s*[a-zA-Z0-9_<>]+\\b\\s*[a-zA-Z0-9_]+\\s*\\([^).]*\\)"), 10},

%% Stock<T> stock = new Stock<T>() - weight = 6
%% Stock stock = new Stock();
{rec("([a-zA-Z0-9_<>]+)\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*\\1\\([^).]*\\)"), 10},

%% IStock<T> stock = new Stock<T>() - weight = 4
%% IStock stock = new Stock();
{rec("[a-zA-Z0-9_<>]+\\s*\\b[a-zA-Z0-9_]+\\b\\s*=\\s*\\bnew\\b\\s*[a-zA-Z0-9_<>]+\\([^).]*\\)"), 10},

{rec("(?:public:|private:|protected:)"), 4},

%% ADA support
{rec("package\\s+(?:body\\s+)?([\\w\\pP\\pS_]+)\\s+is[\\w\\s\\pP\\pS]*end\\s+\\1\\s*;"), 8, [multiline, caseless, ungreedy]},

{rec("procedure\\s+([\\w\\pS\\pP_]+)\\s+(?:\\(\\s*(?:[\\w_]+\\s*:\\s*(?:in|in\\s+out)\\s*[\\w_]+\\s*;?[\\s]*)*\\))?\\s*is\\s*[\\w\\s\\pP\\pS]*begin[\\w\\s\\pP\\pS]*end\\s+\\1\\s*;"), 29, [multiline, caseless, ungreedy]},

{rec("function\\s+([\\w\\pS\\pP_]+)\\s+(?:\\((?:\\s*[\\w_]+\\s*:\\s*(?:in|in\\s+out)\\s*[\\w_]+\\s*;?[\\s]*)*\\))?\\s*return\\s+[\\w_]+\\s+is\\s*[\\w\\s\\pP\\pS]*begin[\\w\\s\\pP\\pS]*end\\s+\\1\\s*;"), 33, [multiline, caseless, ungreedy]},

{rec("while[\\w\\s\\pP\\pS]*loop[\\w\\s\\pP\\pS]*end\\s+loop\\s*;"), 4, [multiline, caseless, ungreedy]},

{rec("for\\s+[\\w\\s\\pP\\pS]+\\s+in\\s+[\\w\\s\\pP\\pS]+loop[\\w\\s\\pP\\pS]*end\\s+loop\\s*;"), 6, [multiline, caseless, ungreedy]}

		]}
	],
	BST = insert_all(BInS, gb_trees:empty()),

	U304 = rec("\x{0130}"),

	{ok, #state{builtin_tree=BT, builtin_suite_tree=BST, u304=U304}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

preregex(Data, #state{u304=U304}) ->
	re:replace(Data, U304, <<"i">>, [global, {return, binary}]).

matches_any([R|RS], Data) ->
	case re:run(Data, R) of
		{match, _Captured} -> true;
		nomatch -> matches_any(RS, Data) end;
matches_any([], _Data) -> false.

insert_all([{Key, Val}|Rest], Tree) -> insert_all(Rest, gb_trees:enter(Key, Val, Tree));
insert_all([], Tree) -> Tree.

rec(Regex) -> {ok, Ret} = re:compile(Regex, [unicode]), Ret.

-define(SS_RE_OPTS, [global, {capture, all, index}]).

score_regex_suite(Regexes, Data) -> score_regex_suite(Regexes, Data, 0).

score_regex_suite([{RE,Weight}|Regexes], Data, Score) ->
		score_regex_suite([{RE,Weight,[]}|Regexes], Data, Score);
score_regex_suite([{RE,Weight,REOpts}|Regexes], Data, Score) ->
	Count = case re:run(Data, RE, lists:append(REOpts, ?SS_RE_OPTS)) of
		nomatch -> 0;
		{match, Captured} -> length(Captured) end,
	score_regex_suite(Regexes, Data, Score + (Weight * Count) );

score_regex_suite([], _Data, Score) -> Score.
	
async_re_call(Query, Data) -> gen_server:call(?MODULE, {async_re, Query, Data}, 15000).

