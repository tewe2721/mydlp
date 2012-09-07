%%%
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

%% API
-export([
	mc_search/1,
	mc_generate/1,
	readlines/1
]).

-compile([export_all]).

-define(STATE_CHUNK, 8192).

mc_search(Data) -> mydlp_mc_dyn:mc_search(Data).

mc_generate(ListOfKeywordGroups) ->
	io:format("Generation started (~w)...~n", [erlang:localtime()]),
	reset_tables(),
	mc_gen(ListOfKeywordGroups),
	io:format("States have been generated (~w)...~n", [erlang:localtime()]),
	try
		mc_gen_compile(),
		ok
	catch
		Type:Error -> 
			io:format("Exception (~w:~w) ~w...~n", [Type, Error, erlang:get_stacktrace()]),
			throw({dyn_compile, {Type, Error}})
	end,
	reset_tables(),
	io:format("Generation finished (~w)...~n", [erlang:localtime()]),
	ok.

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

set_accept(State, MatcherConf) -> 
	ets:insert(mc_states, {State, MatcherConf}).

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
		[[MatcherConf]] -> {ok, MatcherConf} end.

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
get_failure(State) -> 
	F = get_failure1(mydlp_nlp:reverse(State, false)),
	io:format("FF: ~ts -> ~ts ~n", [State, F]),
	F.

get_failure1(ReversedState) ->
	SuffixRoot = get_suffix_root(ReversedState),
	get_failure2(SuffixRoot).

get_failure2(root) -> not_found;
get_failure2(SuffixRoot) ->
	case is_suffix_end(SuffixRoot) of
		true -> mydlp_nlp:reverse(SuffixRoot, false);
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
	{ok, Device} = file:open(FileName, [read, binary]),
	try get_all_lines(Device, [])
	after file:close(Device)
	end.

get_all_lines(Device, Acc) ->
	case io:get_line(Device, "") of
		eof  -> Acc;
		Line -> Size1 = size(Line) - 1,
			<<Line1:Size1/binary, _/binary>> = Line,
			get_all_lines(Device, [Line1|Acc])
	end.

mc_gen([{file, FilePath, MatcherConf} = _KeywordGroup|RestOfKeywordGroups]) -> 
	Keywords = readlines(FilePath),
	mc_gen([{list, Keywords, MatcherConf}|RestOfKeywordGroups]);
mc_gen([{list, Keywords, MatcherConf} = _KeywordGroup|RestOfKeywordGroups]) -> 
	mc_gen_ss(Keywords, MatcherConf),
	mc_gen_sf(Keywords),
	mc_gen(RestOfKeywordGroups);
mc_gen([]) -> ok.
	

mc_gen_ss(Keywords, MatcherConf)  ->
	StartState = start_state(),
	new_state(StartState),
	mc_gen_ss_ks(StartState, Keywords, MatcherConf, false).

mc_gen_ss_ks(_State, [], _MatcherConf, _IsNormalized) -> ok;
mc_gen_ss_ks(State, [<<>>|OtherKeywords], MatcherConf, _IsNormalized) ->
	set_accept(State, MatcherConf),
	StartState = start_state(),
	mc_gen_ss_ks(StartState, OtherKeywords, MatcherConf, false);
mc_gen_ss_ks(State, [Keyword|OtherKeywords], MatcherConf, false = _IsNormalized) ->
	NormalizedKeyword = mydlp_nlp:normalize(Keyword,false),
	NormalizedKeyword = Keyword,
	mc_gen_ss_ks(State, [NormalizedKeyword|OtherKeywords], MatcherConf, true);
mc_gen_ss_ks(State, [Keyword|OtherKeywords], MatcherConf, true = IsNormalized) ->
	case get_uchar(Keyword) of
		none -> mc_gen_ss_ks(State, [<<>>|OtherKeywords], MatcherConf, IsNormalized);
		{Char, RestOfKeyword} ->
			NS = case get_transition(State, Char) of
				not_found -> 	NextState = next_state(State, Char),
						new_state(NextState),
						add_transition(State, Char, NextState),
						NextState;
				NextState -> 	NextState end,
			mc_gen_ss_ks(NS, [<<RestOfKeyword/binary>>|OtherKeywords], MatcherConf, IsNormalized) end.

get_suffixes(root) -> [root];
get_suffixes(Keyword) -> get_suffixes(Keyword, []).

get_suffixes(<<>>, Acc) -> Acc;
get_suffixes(Keyword, Acc) ->
	case get_uchar(Keyword) of
		none -> get_suffixes(<<>>, Acc);
		{_Char, RestOfKeyword} -> get_suffixes(RestOfKeyword, [Keyword|Acc]) end.

mc_gen_sf(Keywords)  ->
	StartState = start_state(),
	new_suffix(StartState, StartState),
	set_suffix_end(StartState),
	mc_gen_sf_ks(StartState, [], Keywords).

mc_gen_sf_ks(_State, [], []) -> ok;
mc_gen_sf_ks(State, [], [Keyword|OtherKeywords]) ->
	NormalizedKeyword = mydlp_nlp:normalize(Keyword, false),
	ReversedKeyword = mydlp_nlp:reverse(NormalizedKeyword, false),
	Suffixes = get_suffixes(ReversedKeyword),
	mc_gen_sf_ks(State, Suffixes, OtherKeywords);
mc_gen_sf_ks(State, [<<>>|OtherSuffixes], OtherKeywords) ->
	set_suffix_end(State),
	StartState = start_state(),
	mc_gen_sf_ks(StartState, OtherSuffixes, OtherKeywords);
mc_gen_sf_ks(State, [Suffix|OtherSuffixes], OtherKeywords) ->
	case get_uchar(Suffix) of
		none -> mc_gen_sf_ks(State, [<<>>|OtherSuffixes], OtherKeywords);
		{Char, RestOfSuffix} ->
			NextSuffix = next_state(State, Char),
			new_suffix(NextSuffix, State),
			mc_gen_sf_ks(NextSuffix, [<<RestOfSuffix/binary>>|OtherSuffixes], OtherKeywords) end.
	

get_uchar(<<>>) -> none;
get_uchar(Bin) ->
	try 	<<C/utf8, Rest/binary>> = Bin,
		{C, Rest}
	catch _:_ -> 
		%% TODO: log this case
		<<_:1/binary, Rest2/binary>> = Bin,
		get_uchar(Rest2) end.

p(Term) when is_integer(Term) -> integer_to_list(Term);
p(Term) -> lists:flatten(io_lib:format("~w", [Term])).

-define(PAGE_MODNAME(PageNum), "mydlp_mc_dyn_p" ++ p(PageNum)).

-define(PAGE_HEADER(PageNum), 
"-module(" ++ ?PAGE_MODNAME(PageNum) ++").
").

-define(PAGE_HEAD, 
"-author('ozgen@mydlp.com').
-author('kerem@mydlp.com').

-export([mc_fsm/4]).

").

-define(PAGE_TAIL, 
"mc_fsm(S, D, I, A) -> {continue, S, D, I, A}.
").

-define(DYN_HEAD, 
"-module(mydlp_mc_dyn).
-author('ozgen@mydlp.com').
-author('kerem@mydlp.com').

-export([mc_search/1]).
mc_search(Data) when is_binary(Data) -> mc_fsm(1, root, Data, 1, []).
mc_fsm(_, _, <<>>, _I, A) -> lists:reverse(A);
").

-define(DYN_PAGEDEF(PageNum), 
"mc_fsm(" ++ p(PageNum) ++ ", S, D, I, A) -> mc_fsm_handle(" ++ p(PageNum) ++ ", " ++ ?PAGE_MODNAME(PageNum) ++ ":mc_fsm(S, D, I, A));
").

-define(DYN_TAIL, 
"mc_fsm(_, root, <<_C/utf8, R/binary>>, I, A) -> mc_fsm(1, root, R, I+1, A);
mc_fsm(_, _, D, I, A) -> mc_fsm(1, root, D, I, A).
mc_fsm_handle(P, {continue, S, D, I, A}) -> mc_fsm(P+1, S, D, I, A).
").

mc_gen_compile() ->
	StateChunks = mc_gen_state_chunks(),
	compile(StateChunks),
	case StateChunks of
		[] -> ok;
		[{MaxNum, _}|_] -> mc_compile_dyn(MaxNum) end,
	ok.

mc_compile_dyn(MaxNum) ->
	PageBin = list_to_binary([?DYN_HEAD, [?DYN_PAGEDEF(N)||N <- lists:seq(1, MaxNum)], ?DYN_TAIL]),
	compile(PageBin).

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
	IsAcceptRec = is_accept_rec(State),
	Acc1 = case get_failure(State) of
		not_found -> Acc;
		FailureState -> 
			IsAccept = is_accept(State),
			mc_gen_source_f(FailureState, State, IsAccept, Acc) end,
	Leafs = get_leafs(State),
	Acc2 = mc_gen_source_s(Leafs, State, IsAcceptRec, Acc1),
	mc_gen_source(ets:next(mc_states, Key), Acc2, Count + 1).

mc_gen_source_s([], _State, _IsAccept, Acc) -> Acc;
mc_gen_source_s([[$A, NextState]|Rest], State, {ok, MC} = IsAccept, Acc) -> %% Alpha special char handle
	SD = "mc_fsm(" ++ p(State) ++ ", <<C:8/integer, R/binary>>, I, A) when C >= 97, C =< 122 ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, [{I, " ++ p(State) ++ ", " ++ p(MC) ++ "}|A]);", %% for predefined we do nested matchings
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]);
mc_gen_source_s([[$A, NextState]|Rest], State, false = IsAccept, Acc) -> %% Alpha special char handle
	SD = "mc_fsm(" ++ p(State) ++ ", <<C:8/integer, R/binary>>, I, A) when C >= 97, C =< 122 ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, A);",
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]);
mc_gen_source_s([[$N, NextState]|Rest], State, {ok, MC} = IsAccept, Acc) -> %% Alpha special char handle
	SD = "mc_fsm(" ++ p(State) ++ ", <<C:8/integer, R/binary>>, I, A) when C >= 48, C =< 57 ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, [{I, " ++ p(State) ++ ", " ++ p(MC) ++ "}|A]);", %% for predefined we do nested matchings
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]);
mc_gen_source_s([[$N, NextState]|Rest], State, false = IsAccept, Acc) -> %% Alpha special char handle
	SD = "mc_fsm(" ++ p(State) ++ ", <<C:8/integer, R/binary>>, I, A) when C >= 48, C =< 57 ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, A);",
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, IsAccept, [SLine|Acc]);
mc_gen_source_s([[Char, NextState]|Rest], State, {ok, MC} = IsAccept, Acc) ->
	SD = "mc_fsm(" ++ p(State) ++ ", <<" ++ p(Char) ++ "/utf8, R/binary>>, I, A) ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, [{I, " ++ p(State) ++ ", " ++ p(MC) ++ "}|A]);", %% for predefined we do nested matchings
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
	FB = "mc_fsm(" ++ p(FailureState) ++ ", D, I, [{I, " ++ p(State) ++ ", " ++ p(MC) ++ "}|A]);",
	FLine = list_to_binary(FD ++ FB ++ [10]),
	[FLine|Acc].

compile({PageNum,FirstKey}) ->
	Source = mc_gen_source(FirstKey),
	PageBin = list_to_binary([?PAGE_HEADER(PageNum), ?PAGE_HEAD, Source, ?PAGE_TAIL]),
	compile(PageBin);
compile(Page) when is_binary(Page) -> compile1(Page);
compile(Pages) when is_list(Pages) ->
	PCount = case erlang:system_info(logical_processors_available) of
		I when I > 16 -> I - 8;
		I when I > 8 -> I - 4;
		I when I > 4 -> I - 2;
		I when I > 2 -> I - 1;
		_Else -> 1 end,
	compile(Pages, length(Pages), PCount).

compile(Pages, PageCount, ProcessCount) when PageCount > ProcessCount ->
	Chunk = lists:sublist(Pages, 1, ProcessCount),
	Rest = lists:sublist(Pages, ProcessCount + 1, PageCount),
	erlang:display(length(Chunk)),
	mydlp_api:pall(fun(P) -> compile(P) end, Chunk, 600000),
	compile(Rest, PageCount - ProcessCount, ProcessCount);
compile(Pages, _PageCount, _ProcessCount) ->
	mydlp_api:pall(fun(P) -> compile(P) end, Pages, 600000),
	ok.

compile1(Source) ->
	{ok, Tempfile0} = mydlp_workdir:tempfile(),
	Tempfile = Tempfile0 ++ ".erl",
	file:write_file(Tempfile, Source),
	io:format("Written to file, compiling (~w)...~n", [erlang:localtime()]),
	{ok, Mod, Code} = compile:file(Tempfile ,[binary, compressed, verbose,report_errors,report_warnings] ),
	io:format("Compiled source, now loading (~w)...~n", [erlang:localtime()]),
	code:load_binary(Mod, "mc_dynamic.erl", Code),
	io:format("Dynamic module compiled and loaded, ByteCodeSize: ~w (~w)...~n", [size(Code),erlang:localtime()]),
	%file:delete(Tempfile),
	ok.





