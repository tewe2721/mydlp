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

-define(PAGE_CHUNK_NOL, 8192).

mc_search(Data) -> mydlp_mc_dyn:mc_search(Data).

mc_generate(ListOfKeywordGroups) ->
	io:format("Generation started (~w)...~n", [erlang:localtime()]),
	reset_tables(),

	mc_gen_ss(ListOfKeywordGroups),
	io:format("States and transitions have been generated (~w)...~n", [erlang:localtime()]),
	mc_gen_ff(),
	io:format("Failure transitions have been generated (~w)...~n", [erlang:localtime()]),
	{DynSource, PageSources} = mc_gen_source(),
	try
		io:format("Generating dyn module (~w)...~n", [erlang:localtime()]),
		compile(DynSource),
		compile(PageSources),
		ok
	catch
		Type:Error -> throw({dyn_compile, {Type, Error}})
	end,
	reset_tables(),
	io:format("Generation finished (~w)...~n", [erlang:localtime()]),
	ok.

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
	Rest = lists:sublist(Pages, ProcessCount + 1),
	mydlp_api:pall(fun(P) -> compile(P) end, Chunk, 600000),
	compile(Rest, PageCount - ProcessCount, ProcessCount);
compile(Pages, _PageCount, _ProcessCount) ->
	mydlp_api:pall(fun(P) -> compile(P) end, Pages),
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
	file:delete(Tempfile),
	ok.

reset_tables() ->
	reset_table(mc_states), %% {state, acceptance}
	reset_table(mc_success), %% {{state, char}, next_state}
	reset_table(mc_failure), %% {state, failure_state}
	ok.

reset_table(TableName) ->
	case ets:info(TableName) of
		undefined -> ets:new(TableName, [private,
						named_table]);
		_Else -> ets:delete_all_objects(TableName) end.

set_failure(State, NextStateOnFailure) -> ets:insert(mc_failure, {State, NextStateOnFailure}).

get_failure(State) -> 
	case ets:match(mc_failure, {State, '$1'}) of
		[] -> not_found;
		[[NextStateOnFailure]] -> NextStateOnFailure end.

set_accept(State, MatcherConf) -> ets:insert(mc_states, {State, MatcherConf}).

is_accept(State) -> 
	case ets:match(mc_states, {State, '$1'}) of
		[] -> state_not_found;
		[[undefined]] -> false;
		[[MatcherConf]] -> {ok, MatcherConf} end.

new_state(State) -> ets:insert(mc_states, {State, undefined}).

add_transition(State, Char, NextState) -> ets:insert(mc_success, {{State, Char}, NextState}).

get_transition(State, Char) -> 
	case ets:match(mc_success, {{State, Char}, '$1'}) of
		[] -> not_found;
		[[NextState]] -> NextState end.

is_valid_transition(State, Char) ->
	case ets:match(mc_success, {{State, Char}, '$1'}) of
		[] -> false;
		[[_NextState]] -> true end.
	

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

mc_gen_ss([{file, FilePath, MatcherConf} = _KeywordGroup|RestOfKeywordGroups]) -> 
	Keywords = readlines(FilePath),
	mc_gen_ss([{list, Keywords, MatcherConf}|RestOfKeywordGroups]);
mc_gen_ss([{list, Keywords, MatcherConf} = _KeywordGroup|RestOfKeywordGroups]) -> 
	StartState = start_state(),
	new_state(StartState),
	mc_gen_ss_ks(StartState, Keywords, MatcherConf, false),
	mc_gen_ss(RestOfKeywordGroups);
mc_gen_ss([]) -> ok.
	

mc_gen_ss_ks(_State, [], _MatcherConf, _IsNormalized) -> ok;
mc_gen_ss_ks(State, [<<>>|OtherKeywords], MatcherConf, _IsNormalized) ->
	set_accept(State, MatcherConf),
	StartState = start_state(),
	mc_gen_ss_ks(StartState, OtherKeywords, MatcherConf, false);
mc_gen_ss_ks(State, [Keyword|OtherKeywords], MatcherConf, false = _IsNormalized) ->
	NormalizedKeyword = mydlp_nlp:normalize(Keyword),
	io:format("SS Keyword: ~ts~n", [NormalizedKeyword]),
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
	
mc_gen_ff() ->
	Q = queue:new(),
	Q1 = queue:in(root, Q),
	mc_gen_ff(Q1, undefined, []).

mc_gen_ff(Q, _ExState, []) -> 
	%case ExState of
	%	root -> io:format("FF State: <<root>>~n", []);
	%	S -> io:format("FF State: ~ts~n", [S]) end,
	case queue:out(Q) of
		{empty, _EmptyQ} -> ok;
		{{value, State}, Q1 } ->
			Leafs = get_leafs(State),
			mc_gen_ff(Q1, State, Leafs) end;
mc_gen_ff(Q, State, [[Char, NextState]|RestOfLeafs]) ->
	case get_failure(State) of
		not_found -> set_failure(NextState, start_state());
		FailureState -> case mc_longest_suffix(FailureState, Char) of
			not_found -> set_failure(NextState, start_state());
			LongestFailure -> set_failure(NextState, get_transition(LongestFailure, Char)) end end,
	case get_failure(NextState) of
		not_found -> ok;
		FS -> case is_accept(FS) of
			{ok, MC} -> set_accept(NextState, MC);
			false -> ok end end,
	Q1 = queue:in(NextState, Q),
	mc_gen_ff(Q1, State, RestOfLeafs).

mc_longest_suffix(FailureState, Char) ->
	case is_valid_transition(FailureState, Char) of
		false -> case get_failure(FailureState) of
			not_found -> not_found;
			NextFailureState -> mc_longest_suffix(NextFailureState, Char) end;
		true -> FailureState end.
		

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
"mc_fsm(_, _, <<_C/utf8, R/binary>>, I, A) -> mc_fsm(1, root, R, I+1, A).
mc_fsm_handle(P, {continue, S, D, I, A}) -> mc_fsm(P+1, S, D, I, A).
").

mc_gen_pages(Lines) -> 
	io:format("Total number of lines: ~w~n", [length(Lines)]),
	mc_gen_pages(Lines, length(Lines), []).

mc_gen_pages(Lines, NumberOfLines, Acc) when NumberOfLines > ?PAGE_CHUNK_NOL ->
	Page = lists:sublist(Lines, 1, ?PAGE_CHUNK_NOL),
	Rest = lists:sublist(Lines, ?PAGE_CHUNK_NOL + 1),
	PageBin = list_to_binary(Page),
	mc_gen_pages(Rest, NumberOfLines - ?PAGE_CHUNK_NOL, [PageBin|Acc]);
mc_gen_pages(LastPage, _NumberOfLines, Acc) ->
	PageBin = list_to_binary(LastPage),
	[PageBin|Acc].

mc_gen_dyn(Pages) ->
	NumberOfPages = length(Pages),
	DynSource = list_to_binary([?DYN_HEAD, [?DYN_PAGEDEF(N)||N <- lists:seq(1, NumberOfPages)], ?DYN_TAIL]),
	PageSources = [list_to_binary([?PAGE_HEADER(N), ?PAGE_HEAD, lists:nth(N, Pages), ?PAGE_TAIL])||N <- lists:seq(1, NumberOfPages)],
	{DynSource, PageSources}.

mc_gen_source() ->
	MCFSMLines = case ets:match(mc_states, '$1') of
		[] -> "";
		States -> mc_gen_source(States, []) end,
	Pages = mc_gen_pages(MCFSMLines),
	mc_gen_dyn(Pages).

mc_gen_source([], Acc) -> lists:flatten(Acc);
mc_gen_source([[{State, Acceptance}]|Rest], Acc) -> 
	IsAccept = case Acceptance of
		undefined -> false;
		MatcherConf -> {ok, MatcherConf} end,
	Acc1 = case get_failure(State) of
		not_found -> Acc;
		FailureState -> mc_gen_source_f(FailureState, State, IsAccept, Acc) end,
	Leafs = get_leafs(State),
	Acc2 = mc_gen_source_s(Leafs, State, IsAccept, Acc1),
	mc_gen_source(Rest, Acc2).

mc_gen_source_s([], _State, _IsAccept, Acc) -> Acc;
mc_gen_source_s([[Char, NextState]|Rest], State, false = _IsAccept, Acc) ->
	SD = "mc_fsm(" ++ p(State) ++ ", <<" ++ p(Char) ++ "/utf8, R/binary>>, I, A) ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, A);",
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, false = _IsAccept, [SLine|Acc]);
mc_gen_source_s([[Char, NextState]|Rest], State, {ok, MC} = _IsAccept, Acc) ->
	SD = "mc_fsm(" ++ p(State) ++ ", <<" ++ p(Char) ++ "/utf8, R/binary>>, I, A) ->",
	SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, [{I, " ++ p(State) ++ ", " ++ p(MC) ++ "}|A]);",
	%SB = "mc_fsm(" ++ p(NextState) ++ ", R, I+1, [{I, " ++ p(MC) ++ "}|A]);",
	SLine = list_to_binary(SD ++ SB ++ [10]),
	mc_gen_source_s(Rest, State, {ok, MC} = _IsAccept, [SLine|Acc]).

mc_gen_source_f(FailureState, State, false = _IsAccept, Acc) ->
	FD = "mc_fsm(" ++ p(State) ++ ", D, I, A) ->",
	FB = "mc_fsm(" ++ p(FailureState) ++ ", D, I, A);",
	FLine = list_to_binary(FD ++ FB ++ [10]),
	[FLine|Acc];
mc_gen_source_f(FailureState, State, {ok, MC} = _IsAccept, Acc) ->
	FD = "mc_fsm(" ++ p(State) ++ ", D, I, A) ->",
	FB = "mc_fsm(" ++ p(FailureState) ++ ", D, I, [{I, " ++ p(State) ++ ", " ++ p(MC) ++ "}|A]);",
	%FB = "mc_fsm(" ++ p(FailureState) ++ ", D, I, [{I, " ++ p(MC) ++ "}|A]);",
	FLine = list_to_binary(FD ++ FB ++ [10]),
	[FLine|Acc].






