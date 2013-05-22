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

-ifndef(_MYDLP_MACRO_HRL).
-define(_MYDLP_MACRO_HRL, true).

-define(CFG(Key), mydlp_config:Key()).

% creates new Fun with encapsulates orginal fun to Log any Exception 
-define(FLE(Fun), fun() -> 
		try Fun()
		catch Class:Error ->
			?ERROR_LOG("Logged exception: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Class, Error, erlang:get_stacktrace()]) end
	 end).

-define(REPLYGUARD(Fun, ErrReply, ErrState), 
		try Fun()
		catch Class:Error ->
			?ERROR_LOG("Logged exception: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Class, Error, erlang:get_stacktrace()]),
			{reply, ErrReply, ErrState}
		end).

-define(NOREPLYGUARD(Fun, ErrState), 
		try Fun()
		catch Class:Error ->
			?ERROR_LOG("Logged exception: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Class, Error, erlang:get_stacktrace()]),
			{noreply, ErrState}
		end).

-define(SAFEREPLY(To, Reply), 
		try gen_server:reply(To, Reply)
		catch Class:Error ->
			?ERROR_LOG("Couldn't sent reply: Class: ["?S"]. Error: ["?S"]. To: ["?S"]. Reply: ["?S"]~nStack trace: "?S"~n",
				[Class, Error, To, Reply, erlang:get_stacktrace()]), ok
		end).

-define(EMF(Fun, ErrKey), 
		try Fun()
		catch Class:Error ->
			?ERROR_LOG("Logged exception: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Class, Error, erlang:get_stacktrace()]),
			{ierror, Class, {ErrKey,Error}} end).

-define(ASYNC0(Fun), mydlp_api:mspawn(?FLE(Fun))).

-define(ASYNC(Fun, Timeout), mydlp_api:mspawn(?FLE(Fun), Timeout)).

-endif.
