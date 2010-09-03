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

-module(mydlp_dynamic).

-author('kerem@medratech.com').

-export([
	load/0
]).

-include("mydlp.hrl").

load() ->
	load_mydlp_denied_page().

load_src(Src) ->
	try
		{Mod,Code} = dynamic_compile:from_string(Src),
		code:load_binary(Mod, "dynamic.erl", Code)
	catch
		Type:Error -> throw({dyn_compile, {Type, Error}})
	end.

denied_page_src() ->
	DP = case application:get_env(mydlp, denied_page) of
		{ok, Path} -> Path;
		undefined -> ?DENIED_PAGE end,
	{ok, Bin} = file:read_file(DP), binary_to_list(Bin).

mydlp_denied_page_src() ->
"-module(mydlp_denied_page).
-author('kerem@medratech.com').

-export([
	get/0,
	get_base64_str/0
]).

get() -> <<\"" ++ denied_page_src() ++ "\">>. 

get_base64_str() -> \"" ++ 
	binary_to_list(
		mydlp_api:insert_line_feed(
			base64:encode(denied_page_src())
		)
	)
 				++ "\". 

".

load_mydlp_denied_page() -> load_src(mydlp_denied_page_src()).

