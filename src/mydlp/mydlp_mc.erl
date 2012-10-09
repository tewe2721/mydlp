%%
%%%    Copyright (C) 2012 Ozgen Muzac <ozgen@mydlp.com>
%%%    Copyright (C) 2012 Huseyin Kerem Cevahir <kerem@mydlp.com>
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

%%%---------------------:----------------------------------------------
%%% @author H. Kerem Cevahir <kerem@mydlp.com>
%%% @copyright 2012, H. Kerem Cevahir
%%% @doc MyDLP Partial Document Matching module
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_mc).
-author("kerem@mydlp.com").
-author("ozgen@mydlp.com").

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

%% API
-export([
	mc_load_mnesia/0,
	mc_load/1,
	mc_search/1,
	mc_search/2
]).

-ifdef(__MYDLP_NETWORK).
-export([
	mc_module/1,
	mc_print/2,
	mc_generate/2,
	mc_generate/3
]).

-endif.

-define(MODFILENAME, "mydlp_mc_dynamic.erl").

-define(STATE_CHUNK, 8192).

mc_load([]) -> ok;
mc_load(ModCodeTupleList) ->
	LoadedMods = get_loaded_mc_mod_names(),
	ModNames = get_mod_names(ModCodeTupleList),
	UnloadList = lists:subtract(LoadedMods, ModNames),
	lists:foreach(fun({M,C}) -> code:load_binary(M, ?MODFILENAME, C) end, ModCodeTupleList),
	lists:foreach(fun(M) -> unload_code(M) end, UnloadList),
	ok.

mc_load_mnesia() ->
	case mydlp_mnesia:get_mc_module() of
		[] -> mc_generate_empty();
		Mods -> mc_load(Mods) end.

mc_search(kw, Data) -> mydlp_mc_kw_dyn:mc_search(Data);
mc_search(pd, Data) -> mydlp_mc_pd_dyn:mc_search(Data).

mc_search(Data) -> 
	KWL = mc_search(kw, Data),
	PDL = mc_search(pd, Data),
	lists:keymerge(1, KWL, PDL).

mc_generate_empty() -> 
	mc_generate(kw, []),
	mc_generate(pd, []),
	ok.

-ifdef(__MYDLP_NETWORK).

mc_module(local) ->
	Matchers = mydlp_mnesia:get_matchers(),
	mc_module1(local, Matchers);
mc_module(RuleIds) ->
	Matchers = mydlp_mnesia:get_matchers(RuleIds),
	mc_module1(RuleIds, Matchers).

mc_module1(Target, Matchers0) ->
	Matchers = merge_duplicate_matchers(Matchers0),
	PDPatterns = get_pd_patterns(Matchers),
	KWPatterns = get_kw_patterns(Matchers),
	PDCodes = mc_generate(pd, PDPatterns, true),
	KWCodes = mc_generate(kw, KWPatterns, true),
	Codes = PDCodes ++ KWCodes,
	#mc_module{target=Target, modules=Codes}.

merge_duplicate_matchers(Matchers) -> merge_duplicate_matchers(Matchers, dict:new()).

merge_duplicate_matchers([{Id, Func, FuncParam}|RestOfMatchers], D) ->
	Key = {Func, FuncParam},
	CurrentList = case dict:find(Key, D) of
		{ok, L} -> L;
		error -> [] end,
	IdList = lists:umerge(CurrentList, [Id]),
	D1 = dict:store(Key, IdList, D),
	merge_duplicate_matchers(RestOfMatchers, D1);
merge_duplicate_matchers([], D) -> matcher_dict_to_list(D).

matcher_dict_to_list(D) -> [{IdList, Func, FuncParam} || {{Func, FuncParam}, IdList} <- dict:to_list(D)].

mc_print(Engine, Data) ->
	Results = mc_search(Engine, Data),
	Str = unicode:characters_to_list(Data),
	[io:format("R: ~w ~ts~n", [BI, lists:sublist(Str, CI - L, L)]) || {CI, BI, {L, _}} <- Results], ok.

mc_generate(kw, L, JustReturnCode) -> mc_generate1(kw, L, JustReturnCode);
mc_generate(pd, L, JustReturnCode) -> mc_generate1(pd, L, JustReturnCode).

-endif.

mc_generate(kw, L) -> mc_generate1(kw, L);
mc_generate(pd, L) -> mc_generate1(pd, L).

mc_generate1(Engine, ListOfKeywordGroups) -> mc_generate1(Engine, ListOfKeywordGroups, false).

mc_generate1(Engine, ListOfKeywordGroups, JustReturnCode) ->
	?DEBUG("Generation started...~n", []),
	reset_tables(),
	mc_gen(ListOfKeywordGroups, Engine),
	?DEBUG("States have been generated...~n", []),
	Codes = try
		mc_gen_compile(Engine, JustReturnCode)
	catch
		Class:Error -> 
			?ERROR_LOG("Exception at mc generation,~nClass: "?S", Error: "?S"~nStackTrace: "?S, 
					[Class, Error, erlang:get_stacktrace()]),
			throw({mc_compile, {Class, Error}})
	end,
	reset_tables(),
	?DEBUG("Generation finished...~n", []),
	Codes.

reset_tables() ->
	reset_table(mc_states, ordered_set), %% {state, acceptance}
	reset_table(mc_success), %% {{state, char}, next_state}
	reset_table(mc_suffix), %% {state, root_state, is_last}
	reset_table(mc_suffix_root, bag), %% {root_state, state}
	ok.

reset_table(TableName) -> reset_table(TableName, set).

reset_table(TableName, Type) ->
	case ets:info(TableName) of
		undefined -> ets:new(TableName, [Type, public, named_table,
						{write_concurrency,false}, {read_concurrency,true}]);
		_Else -> ets:delete_all_objects(TableName) end.

set_accept(State, MatcherConf) when is_list(MatcherConf) -> 
	MCL = case ets:match(mc_states, {State, '$1'}) of
                [] -> MatcherConf;
                [[undefined]] -> MatcherConf;
                [[MC]] -> lists:umerge(MC, MatcherConf) end,
	ets:insert(mc_states, {State, MCL});
set_accept(State, MatcherConf) -> 
	set_accept(State, [MatcherConf]).

is_accept_rec(root) -> false;
is_accept_rec(not_found) -> false;
is_accept_rec(State) ->
	case is_accept(State) of
		{ok, MC} -> {ok, MC};
		false -> is_accept_rec(get_failure(State)) end.

is_accept(State) -> 
	case ets:match(mc_states, {State, '$1'}) of
		[] -> state_not_found;
		[[undefined]] -> false;
		[[MatcherConf]] -> {ok, {get_ulength(State), MatcherConf}} end.

get_ulength(root) -> throw({error, root_can_not_be_accepted});
get_ulength(State) -> length(unicode:characters_to_list(State)).

new_state(State) -> 
	ets:insert(mc_states, {State, undefined}).

new_suffix(Suffix, Root) -> 
	case ets:lookup(mc_suffix, Suffix) of
		[] -> 	ets:insert(mc_suffix, {Suffix, Root, false}),
			ets:insert(mc_suffix_root, {Root, Suffix});
		[{Suffix, _, _}] -> ok end, ok.

set_suffix_end(Suffix) ->
	case ets:match(mc_suffix, {Suffix, '$1', '$2'}) of
		[] -> state_not_found;
		[[_Root, true]] -> ok;
		[[Root, false]] -> 
			ets:insert(mc_suffix, {Suffix, Root, true}),
			ok end.

get_suffix_root(Suffix) ->
	case ets:match(mc_suffix, {Suffix, '$1', '_'}) of
		[] -> state_not_found;
		[[Root]] -> Root end.

is_suffix_end(Suffix) ->
	case ets:match(mc_suffix, {Suffix, '_', '$1'}) of
		[] -> state_not_found;
		[[true]] -> true;
		[[false]] -> false end.

%has_suffix_multiple_leaf(Suffix) ->
%	case ets:match(mc_suffix_root, {Suffix, '$1'}) of
%		[] -> false;
%		[[_S]] -> false;
%		[[_S1],[_S2]|_R] -> true end.

get_failure(root) -> not_found;
get_failure(State) -> get_failure1(mydlp_nlp:reverse(State)).

get_failure1(ReversedState) ->
	SuffixRoot = get_suffix_root(ReversedState),
	get_failure2(SuffixRoot).

get_failure2(root) -> not_found;
get_failure2(SuffixRoot) ->
	case is_suffix_end(SuffixRoot) of
		true -> mydlp_nlp:reverse(SuffixRoot);
		false -> get_failure1(SuffixRoot) end.

add_transition(State, Char, NextState) -> ets:insert(mc_success, {{State, Char}, NextState}).

get_transition(State, Char) -> 
	case ets:match(mc_success, {{State, Char}, '$1'}) of
		[] -> not_found;
		[[NextState]] -> NextState end.

%is_valid_transition(State, Char) ->
%	case ets:match(mc_success, {{State, Char}, '$1'}) of
%		[] -> false;
%		[[_NextState]] -> true end.

next_state(root, Char) -> <<Char/utf8>>;
next_state(State, Char) -> <<State/binary, Char/utf8>>.

get_leafs(State) -> ets:match(mc_success, {{State, '$1'}, '$2'}). % [[Char, NextState]| ..... ]

start_state() -> root.

readlines(FileName) ->
	case file:open(FileName, [read, binary]) of
		{ok, Device} ->
			try get_all_lines(Device, [])
			after file:close(Device)
			end;
		Error -> ?ERROR_LOG("MC: Cannot open file for mc generation. Filename: "?S", Error: "?S, [FileName, Error]), []
	end.

get_all_lines(Device, Acc) ->
	case io:get_line(Device, "") of
		eof  -> Acc;
		Line -> Size1 = size(Line) - 1,
			<<Line1:Size1/binary, _/binary>> = Line,
			get_all_lines(Device, [Line1|Acc])
	end.

mc_gen([{file, FilePath, MatcherConf} = _KeywordGroup|RestOfKeywordGroups], Engine) -> 
	Keywords = readlines(FilePath),
	mc_gen([{list, Keywords, MatcherConf}|RestOfKeywordGroups], Engine);
mc_gen([{list, Keywords0, MatcherConf} = _KeywordGroup|RestOfKeywordGroups], Engine) -> 
	Keywords = lists:map(fun(I) ->
			case I of
				I when is_binary(I) -> I;
				I when is_list(I) -> unicode:characters_to_binary(I) end
		end, Keywords0),
	mc_gen_ss(Keywords, MatcherConf, Engine),
	mc_gen_sf(Keywords, Engine),
	mc_gen(RestOfKeywordGroups, Engine);
mc_gen([], _Engine) -> ok.
	

mc_gen_ss(Keywords, MatcherConf, Engine)  ->
	StartState = start_state(),
	new_state(StartState),
	mc_gen_ss_ks(StartState, Keywords, MatcherConf, Engine, false).

mc_gen_ss_ks(_State, [], _MatcherConf, _Engine, _IsNormalized) -> ok;
mc_gen_ss_ks(State, [<<>>|OtherKeywords], MatcherConf, Engine, _IsNormalized) ->
	set_accept(State, MatcherConf),
	StartState = start_state(),
	mc_gen_ss_ks(StartState, OtherKeywords, MatcherConf, Engine, false);
mc_gen_ss_ks(State, [Keyword|OtherKeywords], MatcherConf, kw = Engine, false = _IsNormalized) ->
	NormalizedKeyword = mydlp_nlp:normalize(Keyword, _IgnoreCases = true),
	mc_gen_ss_ks(State, [NormalizedKeyword|OtherKeywords], MatcherConf, Engine, true);
mc_gen_ss_ks(State, [Keyword|OtherKeywords], MatcherConf, pd = Engine, false = _IsNormalized) ->
	NormalizedKeyword = mydlp_nlp:normalize(Keyword, _IgnoreCases = false),
	mc_gen_ss_ks(State, [NormalizedKeyword|OtherKeywords], MatcherConf, Engine, true);
mc_gen_ss_ks(State, [Keyword|OtherKeywords], MatcherConf, Engine, true = IsNormalized) ->
	case get_uchar(Keyword) of
		none -> mc_gen_ss_ks(State, [<<>>|OtherKeywords], MatcherConf, Engine, IsNormalized);
		{Char, RestOfKeyword} ->
			NS = case get_transition(State, Char) of
				not_found -> 	NextState = next_state(State, Char),
						new_state(NextState),
						add_transition(State, Char, NextState),
						NextState;
				NextState -> 	NextState end,
			mc_gen_ss_ks(NS, [<<RestOfKeyword/binary>>|OtherKeywords], MatcherConf, Engine, IsNormalized) end.

get_suffixes(root) -> [root];
get_suffixes(Keyword) -> get_suffixes(Keyword, []).

get_suffixes(<<>>, Acc) -> Acc;
get_suffixes(Keyword, Acc) ->
	case get_uchar(Keyword) of
		none -> get_suffixes(<<>>, Acc);
		{_Char, RestOfKeyword} -> get_suffixes(RestOfKeyword, [Keyword|Acc]) end.

mc_gen_sf(Keywords, Engine)  ->
	StartState = start_state(),
	new_suffix(StartState, StartState),
	set_suffix_end(StartState),
	mc_gen_sf_ks(StartState, [], Keywords, Engine).

mc_gen_sf_ks(_State, [], [], _Engine) -> ok;
mc_gen_sf_ks(State, [], [Keyword|OtherKeywords], kw = Engine) ->
	NormalizedKeyword = mydlp_nlp:normalize(Keyword, true),
	ReversedKeyword = mydlp_nlp:reverse(NormalizedKeyword),
	Suffixes = get_suffixes(ReversedKeyword),
	mc_gen_sf_ks(State, Suffixes, OtherKeywords, Engine);
mc_gen_sf_ks(State, [], [Keyword|OtherKeywords], pd = Engine) ->
	NormalizedKeyword = mydlp_nlp:normalize(Keyword, false),
	ReversedKeyword = mydlp_nlp:reverse(NormalizedKeyword),
	Suffixes = get_suffixes(ReversedKeyword),
	mc_gen_sf_ks(State, Suffixes, OtherKeywords, Engine);
mc_gen_sf_ks(State, [<<>>|OtherSuffixes], OtherKeywords, Engine) ->
	set_suffix_end(State),
	StartState = start_state(),
	mc_gen_sf_ks(StartState, OtherSuffixes, OtherKeywords, Engine);
mc_gen_sf_ks(State, [Suffix|OtherSuffixes], OtherKeywords, Engine) ->
	case get_uchar(Suffix) of
		none -> mc_gen_sf_ks(State, [<<>>|OtherSuffixes], OtherKeywords, Engine);
		{Char, RestOfSuffix} ->
			NextSuffix = next_state(State, Char),
			new_suffix(NextSuffix, State),
			mc_gen_sf_ks(NextSuffix, [<<RestOfSuffix/binary>>|OtherSuffixes], OtherKeywords, Engine) end.
	

get_uchar(<<>>) -> none;
get_uchar(Bin) ->
	try 	<<C/utf8, Rest/binary>> = Bin,
		{C, Rest}
	catch _:_ -> 
		<<NUC:1/binary, Rest2/binary>> = Bin,
		?ERROR_LOG("Encountered a non-unicode Character at MC generation. Character: '"?S"'", [NUC]),
		get_uchar(Rest2) end.

p(Term) when is_integer(Term) -> integer_to_list(Term);
p(Term) -> lists:flatten(io_lib:format("~w", [Term])).

-define(PAGE_MODNAME(Engine, PageNum), "mydlp_mc_" ++ p(Engine) ++  "_dyn_p" ++ p(PageNum)).

-define(PAGE_HEADER(Engine, PageNum), 
"-module(" ++ ?PAGE_MODNAME(Engine, PageNum) ++").
").

-define(PAGE_HEAD, 
"-author('ozgen@mydlp.com').
-author('kerem@mydlp.com').

-export([mc_fsm/4]).

").

-define(PAGE_TAIL, 
"mc_fsm(S, D, I, A) -> {continue, S, D, I, A}.
").

-define(DYN_NONE(Engine), 
"-module(mydlp_mc_" ++ p(Engine) ++ "_dyn).
-author('ozgen@mydlp.com').
-author('kerem@mydlp.com').

-export([mc_search/1]).
mc_search(Data) when is_binary(Data) -> [].
").

-define(DYN_HEAD(Engine), 
"-module(mydlp_mc_" ++ p(Engine) ++ "_dyn).
-author('ozgen@mydlp.com').
-author('kerem@mydlp.com').

-export([mc_search/1]).
mc_search(Data) when is_binary(Data) -> mc_fsm(dict:new(), 1, root, Data, 1, []).
mc_fsm(_, _, _, <<>>, _I, A) -> lists:reverse(A);
").

-define(DYN_PAGEDEF(Engine, PageNum), 
"mc_fsm(PD, " ++ p(PageNum) ++ ", S, D, I, A) -> mc_fsm_handle(PD, " ++ p(PageNum) ++ ", " ++ ?PAGE_MODNAME(Engine, PageNum) ++ ":mc_fsm(S, D, I, A));
").

-define(DYN_TAIL(NumberOfPages), 
"mc_fsm(PD, _, S, D, I, A) -> mc_fsm(PD, 1, S, D, I, A).
mc_fsm_handle(_PD, _P, {continue, _S, <<>>, _I, A}) -> lists:reverse(A);
mc_fsm_handle(PD, P, {continue, S, <<_C/utf8, R/binary>> = D, I, A}) -> 
	NP = case P of
		" ++ p(NumberOfPages) ++ " -> 1;
		_ElseI -> P+1 end,

	case {S, dict:find(NP, PD)} of
		{root, {ok, I}} -> mc_fsm(dict:new(), 1, root, R, I + 1, A);
		{_, {ok, I}} -> mc_fsm(dict:new(), 1, root, D, I + 1, A);
		_Else -> PD1 = dict:store(P, I, PD),
			mc_fsm(PD1, NP, S, D, I, A) end.
").

% mc_gen_compile(Engine) -> mc_gen_compile(Engine, false).

mc_gen_compile(Engine, JustReturnCode) ->
	StateChunks = mc_gen_state_chunks(),
	PageCodes = compile(Engine, StateChunks, JustReturnCode),
	DynCode = case StateChunks of
		[] -> mc_compile_dyn(Engine, 0, JustReturnCode);
		[{MaxNum, _}|_] -> mc_compile_dyn(Engine, MaxNum, JustReturnCode) end,
	PageCodes ++ DynCode.

mc_compile_dyn(Engine, 0, JustReturnCode) -> %% generate place holder for none
	PageBin = list_to_binary([?DYN_NONE(Engine)]),
	compile(PageBin, JustReturnCode);
mc_compile_dyn(Engine, MaxNum, JustReturnCode) ->
	PageBin = list_to_binary([?DYN_HEAD(Engine), [?DYN_PAGEDEF(Engine, N)||N <- lists:seq(1, MaxNum)], ?DYN_TAIL(MaxNum)]),
	compile(PageBin, JustReturnCode).

mc_gen_state_chunks() -> 
	case ets:first(mc_states) of
		'$end_of_table' -> [];
		Key -> mc_gen_state_chunks(Key, 1, 1, []) end.

mc_gen_state_chunks('$end_of_table', _KeyIndex, _PageNum, Acc) -> Acc;
mc_gen_state_chunks(Key, KeyIndex, PageNum, Acc) ->
	case ( KeyIndex rem ?STATE_CHUNK ) of
		1 -> mc_gen_state_chunks(ets:next(mc_states, Key), KeyIndex + 1, PageNum + 1, [{PageNum, Key}| Acc]);
		_Else -> mc_gen_state_chunks(ets:next(mc_states, Key), KeyIndex + 1, PageNum , Acc) end.

mc_gen_source(FirstKey) -> mc_gen_source(FirstKey, [], 1).

mc_gen_source(_Key, Acc, Count) when Count > ?STATE_CHUNK -> lists:flatten(Acc);
mc_gen_source('$end_of_table', Acc, _Count) -> lists:flatten(Acc);
mc_gen_source(Key, Acc, Count) -> 
	[{State, _Acceptance}] = ets:lookup(mc_states, Key),
	{Acc1, HasFailure} = case get_failure(State) of
		not_found -> {Acc, false};
		FailureState -> 
			IsAccept = is_accept(State),
			{mc_gen_source_f(FailureState, State, IsAccept, Acc), true} end,
	IsAcceptRec = is_accept_rec(State),
	Acc2 = case {get_leafs(State), HasFailure} of
		{[], false} -> mc_gen_source_s(State, IsAcceptRec, Acc1);
		{Leafs, _} -> mc_gen_source_s(Leafs, State, IsAcceptRec, Acc1) end,
	mc_gen_source(ets:next(mc_states, Key), Acc2, Count + 1).

mc_gen_source_s(State, {ok, MC} = _IsAccept, Acc) ->
	SD = "mc_fsm(" ++ p(State) ++ ", D, I, A) ->",
	SB = "mc_fsm(root, D, I, [{I, -size(D), " ++ p(MC) ++ "}|A]);", %% for predefined we do nested matchings
	SLine = list_to_binary(SD ++ SB ++ [10]),
	[SLine|Acc];
mc_gen_source_s(_State, false = _IsAccept, Acc) -> Acc.

mc_gen_source_s([], _State, _IsAccept, Acc) -> Acc;
mc_gen_source_s([[$A, NextState]|Rest], State, {ok, MC} = IsAccept, Acc) -> %% alpha special char handle
	SD = "mc_fsm(" ++ p(State) ++ ", <<C:8/integer, R/binary>>, I, A) when C >= 97, C =< 122 ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, [{I, -1-size(R), " ++ p(MC) ++ "}|A]);", %% for predefined we do nested matchings
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]);
mc_gen_source_s([[$A, NextState]|Rest], State, false = IsAccept, Acc) -> %% alpha special char handle
	SD = "mc_fsm(" ++ p(State) ++ ", <<C:8/integer, R/binary>>, I, A) when C >= 97, C =< 122 ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, A);",
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]);
mc_gen_source_s([[$N, NextState]|Rest], State, {ok, MC} = IsAccept, Acc) -> %% Number special char handle
	SD = "mc_fsm(" ++ p(State) ++ ", <<C:8/integer, R/binary>>, I, A) when C >= 48, C =< 57 ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, [{I, -1-size(R), " ++ p(MC) ++ "}|A]);", %% for predefined we do nested matchings
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]);
mc_gen_source_s([[$N, NextState]|Rest], State, false = IsAccept, Acc) -> %% Number special char handle
	SD = "mc_fsm(" ++ p(State) ++ ", <<C:8/integer, R/binary>>, I, A) when C >= 48, C =< 57 ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, A);",
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]);
mc_gen_source_s([[Char, NextState]|Rest], State, {ok, MC} = IsAccept, Acc) ->
	SD = "mc_fsm(" ++ p(State) ++ ", <<" ++ p(Char) ++ "/utf8, R/binary>>, I, A) ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, [{I, -1-size(R), " ++ p(MC) ++ "}|A]);", %% for predefined we do nested matchings
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]);
mc_gen_source_s([[Char, NextState]|Rest], State, false = IsAccept, Acc) ->
	SD = "mc_fsm(" ++ p(State) ++ ", <<" ++ p(Char) ++ "/utf8, R/binary>>, I, A) ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, A);",
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]).

mc_gen_source_f(FailureState, State, false = _IsAccept, Acc) ->
	FD = "mc_fsm(" ++ p(State) ++ ", D, I, A) ->",
	FB = "mc_fsm(" ++ p(FailureState) ++ ", D, I, A);",
	FLine = list_to_binary(FD ++ FB ++ [10]),
	[FLine|Acc];
mc_gen_source_f(FailureState, State, {ok, MC} = _IsAccept, Acc) ->
	FD = "mc_fsm(" ++ p(State) ++ ", D, I, A) ->",
	FB = "mc_fsm(" ++ p(FailureState) ++ ", D, I, [{I, -size(D), " ++ p(MC) ++ "}|A]);",
	FLine = list_to_binary(FD ++ FB ++ [10]),
	[FLine|Acc].


compile(Page, JustReturnCode) when is_binary(Page) -> compile1(Page, JustReturnCode).

compile(Engine, {PageNum,FirstKey}, JustReturnCode) ->
	Source = mc_gen_source(FirstKey),
	PageBin = list_to_binary([?PAGE_HEADER(Engine, PageNum), ?PAGE_HEAD, Source, ?PAGE_TAIL]),
	compile(PageBin, JustReturnCode);
compile(_Engine, [], _JustReturnCode) -> [];
compile(Engine, Pages, JustReturnCode) when is_list(Pages) ->
	PCount = case erlang:system_info(logical_processors_available) of
		I when I > 16 -> I - 8;
		I when I > 8 -> I - 4;
		I when I > 4 -> I - 2;
		I when I > 2 -> I - 1;
		_Else -> 1 end,
	compile(Engine, Pages, length(Pages), PCount, JustReturnCode, []).

compile(Engine, Pages, PageCount, ProcessCount, JustReturnCode, Acc) when PageCount > ProcessCount ->
	Chunk = lists:sublist(Pages, 1, ProcessCount),
	Rest = lists:sublist(Pages, ProcessCount + 1, PageCount),
	{ok, Codes} = mydlp_api:pall(fun(P) -> compile(Engine, P, JustReturnCode) end, Chunk, 600000),
	Acc1 = Acc ++ Codes,
	compile(Engine, Rest, PageCount - ProcessCount, ProcessCount, JustReturnCode, Acc1);
compile(Engine, Pages, _PageCount, _ProcessCount, JustReturnCode, Acc) ->
	{ok, Codes} = mydlp_api:pall(fun(P) -> compile(Engine, P, JustReturnCode) end, Pages, 600000),
	lists:flatten(Acc ++ Codes).

compile1(Source, JustReturnCode) ->
	{ok, Tempfile0} = mydlp_workdir:tempfile(),
	Tempfile = Tempfile0 ++ ".erl",
	file:write_file(Tempfile, Source),
	{ok, Mod, Code} = compile:file(Tempfile ,[binary, compressed, verbose,report_errors,report_warnings] ),
	case JustReturnCode of
		false -> code:load_binary(Mod, ?MODFILENAME, Code);
		true -> ok end,
	?DEBUG("Dynamic module compiled, ByteCodeSize: "?S"...~n", [size(Code)]),
	file:delete(Tempfile),
	[{Mod, Code}].


%%%%% integration stuff

-ifdef(__MYDLP_NETWORK).

is_func_pd(Func) ->
        {_, {distance, _}, {pd, IsPD}, {kw, _}} = apply(mydlp_matchers, Func, []), IsPD.

is_func_kw(Func) ->
        {_, {distance, _}, {pd, _}, {kw, IsKW}} = apply(mydlp_matchers, Func, []), IsKW.

get_pd_patterns(Matchers) -> get_pd_patterns(Matchers, []).

get_pd_patterns([{_Id, all, _FuncParam}|Rest], Acc) -> get_pd_patterns(Rest, Acc);
get_pd_patterns([{Id, Func, FuncParam}|Rest], Acc) ->
	case is_func_pd(Func) of
		true -> get_pd_patterns(Rest, [{list, func_pd_pattern(Func, FuncParam), Id}|Acc]);
		false -> get_pd_patterns(Rest, Acc) end;
get_pd_patterns([], Acc) -> lists:reverse(Acc).

func_pd_pattern(Func, _FuncParam) ->
	% TODO: implement this using FuncParam
        apply(mydlp_matchers, Func, [{pd_patterns, "normal"}]).

get_kw_patterns(Matchers) -> get_kw_patterns(Matchers, []).

get_kw_patterns([{_Id, all, _FuncParam}|Rest], Acc) -> get_kw_patterns(Rest, Acc);
get_kw_patterns([{Id, Func, FuncParam}|Rest], Acc) ->
	case is_func_kw(Func) of
		true -> case FuncParam of
			[{group_id, KGIs}] -> get_kw_patterns(Rest, [{list, func_kw_pattern(Func, KGIs), Id}|Acc]);
			[{file, BundledFilename}] -> get_kw_patterns(Rest, [{file, filename:absname(BundledFilename, ?CFG(resources_dir)), Id}|Acc]) end;
		false -> get_kw_patterns(Rest, Acc) end;
get_kw_patterns([], Acc) -> lists:reverse(Acc).

func_kw_pattern(keyword_match, KGI) when is_integer(KGI) ->
	func_kw_pattern(keyword_match, [KGI]);
func_kw_pattern(keyword_match, KGIs) when is_list(KGIs) ->
	lists:append([mydlp_mnesia:get_keywords(GI) || GI <- KGIs]);
func_kw_pattern(Func, FuncParam) -> 
	?ERROR_LOG("Unexpected matcher definition, F: "?S" FP: "?S, [Func, FuncParam]), [].

-endif.

unload_code(ModName) -> unload_code(ModName, 10).

unload_code(ModName, 0) -> 
	code:purge(ModName), ok;
unload_code(ModName, TryCount) -> 
	case code:soft_purge(ModName) of
		false -> timer:sleep(TryCount*TryCount*10 + 100),
			unload_code(ModName, TryCount - 1);
		true -> unload_code(ModName, 0) end.

get_loaded_mc_mod_names() -> get_loaded_mc_mod_names(code:all_loaded(), []).

get_loaded_mc_mod_names([{Mod, ?MODFILENAME}|Rest], Acc) -> get_loaded_mc_mod_names(Rest, [Mod|Acc]);
get_loaded_mc_mod_names([_|Rest], Acc) -> get_loaded_mc_mod_names(Rest, Acc);
get_loaded_mc_mod_names([], Acc) -> Acc.

get_mod_names(ModCodeList) -> [Mod || {Mod, _Code} <- ModCodeList].

