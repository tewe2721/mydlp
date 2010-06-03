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
%%% @doc Matcher repository for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_matchers).
-author("kerem@medra.com.tr").

-include("mydlp.hrl").

%% API
-export([
	mime_match/2
]).

-include_lib("eunit/include/eunit.hrl").

mime_match(MimeTypes, {_Addr, Data, Files}) ->
	case length(Files) of
		0 -> mime_match1(MimeTypes, [Data]);
		_ -> mime_match1(MimeTypes, [F#file.data||F <- Files])
	end.
mime_match1(MimeTypes, [Data|Rest]) ->
	MT = mydlp_tc:get_mime(Data),
	case lists:member(MT, MimeTypes) of
		true -> pos;
		false -> mime_match1(MimeTypes, Rest)
	end;
mime_match1(_MimeTypes, []) -> neg.

