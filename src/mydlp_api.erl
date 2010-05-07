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

-module(mydlp_api).

-author('kerem@medra.com.tr').

-compile(export_all).

%%--------------------------------------------------------------------
%% @doc Check if a byte is an HTTP character
%% @end
%%----------------------------------------------------------------------
is_char(X) when X >= 0 , X =< 127 -> true;
is_char(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is an HTTP control character
%% @end
%%----------------------------------------------------------------------
is_ctl(X) when X >= 0 , X =< 31 -> true;
is_ctl(X) when X == 127 -> true;
is_ctl(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is defined as an HTTP tspecial character.
%% @end
%%----------------------------------------------------------------------
is_tspecial(X) when
		X == $( ; X == $) ; X == $< ; X == $> ; X == $@ ;
		X == $, ; X == $; ; X == $: ; X == $\ ; X == $" ;
		X == $/ ; X == $[ ; X == $] ; X == $? ; X == $= ;
		X == ${ ; X == $} ; X == $\s ; X == $\t -> true;
is_tspecial(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is a digit.
%% @end
%%----------------------------------------------------------------------
is_digit(X) when X >= $0 , X =< $9 -> true;
is_digit(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is alphanumeric.
%% @end
%%----------------------------------------------------------------------
is_alpha(X) when X >= $a , X =< $z -> true;
is_alpha(X) when X >= $A , X =< $Z -> true;
is_alpha(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is defined as an HTTP URI unreserved character.
%% @end
%%----------------------------------------------------------------------
is_uuri_char(X) when X == $- ; X == $_ ; X == $. ; X == $~ -> true;
is_uuri_char(X) -> is_digit(X) or is_alpha(X).

%%--------------------------------------------------------------------
%% @doc Check if a byte is defined as an HTTP URI reserved character.
%% @end
%%----------------------------------------------------------------------
is_ruri_char(X) when
		X == $! ; X == $* ; X == $\ ; X == $( ; X == $) ;
		X == $; ; X == $: ; X == $@ ; X == $& ; X == $= ;
		X == $+ ; X == $$ ; X == $, ; X == $? ; X == $/ ;
		X == $% ; X == $# ; X == $[ ; X == $] -> true;
is_ruri_char(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is defined as an HTTP URI character.
%% @end
%%----------------------------------------------------------------------
is_uri_char(X) -> is_uuri_char(X) or is_ruri_char(X).

