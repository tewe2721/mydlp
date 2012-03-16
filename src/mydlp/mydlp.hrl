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

-define(CFG(Key), mydlp_config:Key()).

% creates new Fun with encapsulates orginal fun to Log any Exception 
-define(FLE(Fun), fun() -> 
		try Fun()
		catch Class:Error ->
			?ERROR_LOG("Logged exception: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
                        [Class, Error, erlang:get_stacktrace()]) end
	 end).

-define(ASYNC0(Fun), mydlp_api:mspawn(?FLE(Fun))).

-define(ASYNC(Fun, Timeout), mydlp_api:mspawn(?FLE(Fun), Timeout)).


-define(ACL_LOG(Time, Channel, RuleId, Action, Ip, User, To, ITypeId, File, Misc), 
	?ACL_LOG_P(Time, Channel, RuleId, Action, Ip, User, To, ITypeId, File, Misc, none)).

-define(ACL_LOG_P(Time, Channel, RuleId, Action, Ip, User, To, ITypeId, File, Misc, Payload), 
	mydlp_api:acl_msg(Time, Channel, RuleId, Action, Ip, User, To, ITypeId, File, Misc, Payload)).

-define(ERROR_LOG(Format, Args),
	mydlp_logger:notify(error, "~P:~P " ++ Format, lists:append([ [I,32] || I <- ([?MODULE_STRING, ?LINE] ++ Args)]))
	).

-define(BINARY_LOG(ItemName, Binary),
	FN = mydlp_api:ref_to_fn(?CFG(log_dir), "binlog", erlang:now()),
	file:write_file(FN, Binary),
	?ERROR_LOG("Binary item ("?S") logged: "?S"~n", [ItemName, FN])
	).

-define(DEBUG(Format, Args),
	mydlp_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
	mydlp_logger:info_msg(?MODULE,?LINE,Format, Args)).

-define(S, "~P").

-define(MIME_OOXML_WORD, <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>).
-define(MIME_OOXML_EXCEL, <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>).
-define(MIME_OOXML_POWERPOINT, <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>).

%%%%%%%

%% end of import

-record(file, {
                name,
                filename,
                mime_type,
                given_type,
                data,
		dataref,
		text,
		compressed_copy = false,
		is_encrypted = false
        }).

-endif.
