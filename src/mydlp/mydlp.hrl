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

-ifndef(_MYDLP_HRL).
-define(_MYDLP_HRL, true).

-include("mydlp_big_binary.hrl").

-include("mydlp_logger.hrl").

-define(CFG(Key), mydlp_config:Key()).

% creates new Fun with encapsulates orginal fun to Log any Exception 
-define(FLE(Fun), fun() -> 
		try Fun()
		catch Class:Error ->
			?ERROR_LOG("Logged exception: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Class, Error, erlang:get_stacktrace()]) end
	 end).

-define(EMF(Fun, ErrKey), 
		try Fun()
		catch Class:Error ->
			?ERROR_LOG("Logged exception: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Class, Error, erlang:get_stacktrace()]),
			{ierror, Class, {ErrKey,Error}} end).

-define(ASYNC0(Fun), mydlp_api:mspawn(?FLE(Fun))).

-define(ASYNC(Fun, Timeout), mydlp_api:mspawn(?FLE(Fun), Timeout)).

-define(MIME_TIKA_OOXML, <<"application/x-tika-ooxml">>).
-define(MIME_OOXML_WORD, <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>).
-define(MIME_OOXML_EXCEL, <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>).
-define(MIME_OOXML_POWERPOINT, <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>).
-define(MIME_ZIP, <<"application/zip">>).
-define(MIME_PDF, <<"application/pdf">>).
-define(MIME_XPS, <<"application/vnd.ms-xpsdocument">>).
-define(MIME_TEXT, <<"text/plain">>).
-define(MIME_OCTET_STREAM, <<"application/octet-stream">>).

%%%%%%%

%% end of import

-record(file, {
                name,
                filename,
                mime_type,
                given_type,
		md5_hash,
		size,
		meta=[],
		compressed_copy = false,
		is_encrypted = false,
                data,
		dataref,
		text,
		normal_text,
		mc_table
        }).

-record(matching_detail, {
		index,
		pattern,
		matcher_func
	}).

-record(aclq, {
		channel :: 'web' | 'mail' | 'removable' | 'printer' | 'discovery' | 'remote_discovery',
		src_domain=undefined,
		src_addr=unknown,
		src_user_h=unknown,
		destinations=[],
		has_hidden_destinations=false
        }).

-record(kgram, {
		hash,
		index
	}).


-endif.
