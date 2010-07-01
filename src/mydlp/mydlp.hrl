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

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(KILL_TIMEOUT, 2000).

%% ---------------------------------
%% Logging mechanism

%% imported from http://github.com/processone/ejabberd/blob/master/src/ejabberd.hrl
-define(LOG_PATH, "mydlp.log").
-define(PID_FILE, "mydlp.pid").
-define(SSL_FILES, [
          {certfile, "/etc/mydlp/ssl/public.pem"},
          {keyfile, "/etc/mydlp/ssl/private.pem"}
         ]).

-define(ACL_LOG(From, To, Files, RuleId, Action), 
	mydlp_api:acl_msg(From, To, Files, RuleId, Action)).

-define(DEBUG(Format, Args),
	mydlp_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
	mydlp_logger:info_msg(?MODULE,?LINE,Format, Args)).

%% end of import

-record(file, {
                name,
                filename,
                mime_type,
                given_type,
                data,
		text,
		is_encrypted = false
        }).

-endif.
