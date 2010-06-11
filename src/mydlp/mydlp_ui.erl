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
%%% @doc Backend for mydlp ui functions.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_ui).
-author("kerem@medra.com.tr").

-include("mydlp_ui_thrift.hrl").
-include("mydlp_ui_types.hrl").

-export([start_link/0, stop/1,
         handle_function/2
         ]).

%%%%% EXTERNAL INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    thrift_server:start_link(9092, mydlp_ui_thrift, ?MODULE).

stop(Server) ->
    thrift_server:stop(Server),
    ok.

%%%%% THRIFT INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    case mydlp_mnesia:dyn_query(parse_func_name(Function), tuple_to_list(Args)) of
        ok -> ok;
        Reply -> {reply, Reply}
    end.

%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_func_name(Function) ->
	Words = get_words(Function),
	Atoms = words_to_atoms(Words),
	gen_funcall(Atoms).

get_words(Function) when is_atom(Function) -> get_words(atom_to_list(Function));
get_words(FuncName) when is_list(FuncName) -> get_words(FuncName, [], []).
get_words([$_|FuncName], Words, Tmp) -> get_words(FuncName, [lists:reverse(Tmp)|Words], []);
get_words([C|FuncName], Words, Tmp) -> get_words(FuncName, Words, [C|Tmp]);
get_words([], Words, Tmp) -> lists:reverse([lists:reverse(Tmp)|Words]).

words_to_atoms([A|Words]) -> words_to_atoms(Words, [list_to_atom(A)], []).
words_to_atoms(["by"|Words], Items, Tmp) -> words_to_atoms(Words, 
	[concat_words(lists:reverse(Tmp))|Items], []);
words_to_atoms(["and"|Words], Items, Tmp) -> words_to_atoms(Words, 
	[concat_words(lists:reverse(Tmp))|Items], []);
words_to_atoms([W|Words], Items, Tmp) -> words_to_atoms(Words, Items, [W|Tmp]);
words_to_atoms([], Items, Tmp) -> 
	lists:reverse([concat_words(lists:reverse(Tmp))|Items]).

gen_funcall([Action, Table|Ands]) -> {Action, Table, Ands}.

concat_words(Words) -> list_to_atom(string:join(Words,"_")).

