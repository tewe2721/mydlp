%%
%%%    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@mydlp.com>
%%%
%%%--------------------------------------------------------------------------
%%%    This file is part of bayeserl.
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

-ifdef(__MYDLP_NETWORK).

-module(mydlp_bayeserl_store).

-author("kerem@mydlp.com").

-include("mydlp_schema.hrl").

%% API
-export([
	start/0,
	stop/0,
	i_p_c/0,
	i_n_c/0,
	zero/0,
	i_p_wc/2,
	i_n_wc/2,
	g_p_c/0,
	g_n_c/0,
	g_p_wc/1,
	g_n_wc/1
	]).

start() -> ok.

stop() -> ok.

i_p_c() -> mnesia:dirty_update_counter(bayes_item_count, positive, 1).

i_n_c() -> mnesia:dirty_update_counter(bayes_item_count, negative, 1).

zero() -> mydlp_mnesia:truncate_bayes().

i_p_wc(Word, Count) -> mnesia:dirty_update_counter(bayes_positive, Word, Count).

i_n_wc(Word, Count) -> mnesia:dirty_update_counter(bayes_negative, Word, Count).

g_p_c() -> case mnesia:dirty_read(bayes_item_count, positive) of
		[#bayes_item_count{type=positive, count=C}] -> C;
		[] -> 0 end.

g_n_c() -> case mnesia:dirty_read(bayes_item_count, negative) of
		[#bayes_item_count{type=negative, count=C}] -> C;
		[] -> 0 end.

g_p_wc(Word) -> case mnesia:dirty_read(bayes_positive, Word) of
		[#bayes_positive{word_hash=Word, count=C}] -> C;
		[] -> 0 end.

g_n_wc(Word) -> case mnesia:dirty_read(bayes_negative, Word) of
		[#bayes_negative{word_hash=Word, count=C}] -> C;
		[] -> 0 end.

-endif.

