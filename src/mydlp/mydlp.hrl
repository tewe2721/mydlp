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

-define(TIMEOUT, 120000).

%% ---------------------------------
%% Logging mechanism

%% imported from http://github.com/processone/ejabberd/blob/master/src/ejabberd.hrl
-define(LOG_PATH, "mydlp.log").
-define(PID_FILE, "mydlp.pid").
-define(SSL_FILES, [
		{certfile, "/etc/mydlp/ssl/public.pem"},
		{keyfile, "/etc/mydlp/ssl/private.pem"}
         ]).
-define(DENIED_PAGE, "denied_page.html").
-define(NLP_TR, [ 
          {activate, true},
          {pool_size, 2},
          {kokler, "mydlp_nlp_tr_kokler.txt"}
         ]).
-define(THRIFTCONF, [
          {client_pool_size, 4}
         ]).
-define(MYSQL, [
		{host, "localhost"},
		{port, 3306},
		{user, "root"},
		{password, ""},
		{database, "mydlp"},
		{pool_size, 2}
         ]).
-define(SMTP, [ 
          {helo_name, "mydlp.org"},
          {next_hop, {"localhost", 10026}}
         ]).

-define(ICAP, [
          {path, "/dlp"},
          {max_connections, 1000},
          {options_ttl, 3600}
         ]).

-define(ACL_LOG(Proto, RuleId, Action, Ip, User, To, Matcher, File, Misc), 
	mydlp_api:acl_msg(Proto, RuleId, Action, Ip, User, To, Matcher, File, Misc)).

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

-define(QUARANTINE_DIR, "/var/lib/mydlp/quarantine/").

-endif.
