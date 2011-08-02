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

-define(MAX_RESTART, 5).
-define(MAX_TIME, 20).
-define(KILL_TIMEOUT, 2000).

-define(TIMEOUT, 120000).

-define(MAX_MEM_OBJ, 204800). % 200 KB
-define(CHUNK_THRESHOLD, 1048576). % 1 MB

-define(MIN_SIZE_FOR_P, 131072). % 128 KB

-define(MIN_COUNT_FOR_P, 4). % 128 KB

%% ---------------------------------
%% Logging mechanism
%% imported from http://github.com/processone/ejabberd/blob/master/src/ejabberd.hrl

-define(LOG_DIR, ".").

-define(PID_FILE, "mydlp.pid").

-ifdef(__PLATFORM_LINUX).
-define(WORK_DIR, "/var/tmp/mydlp").
-endif.

-ifdef(__PLATFORM_WINDOWS).
-define(WORK_DIR, "C:/WINDOWS/Temp/mydlp").
-endif.

-define(ACL, [       
          {error_action, pass} % [pass/block]
         ]).

-ifdef(__MYDLP_NETWORK).
-define(SSL_FILES, [
		{certfile, "/etc/mydlp/ssl/public.pem"},
		{keyfile, "/etc/mydlp/ssl/private.pem"}
         ]).
-endif.

-ifdef(__MYDLP_NETWORK).
-define(NLP_TR, [ 
          {activate, false},
          {kokler, "mydlp_nlp_tr_kokler.txt"}
         ]).
-endif.

-ifdef(__MYDLP_NETWORK).
-define(MYSQL, [
		{host, "localhost"},
		{port, 3306},
		{user, "root"},
		{password, ""},
		{database, "mydlp"},
		{pool_size, 2}
         ]).
-endif.

-ifdef(__MYDLP_NETWORK).
-define(QUARANTINE, [
          {dir, "/var/lib/mydlp/quarantine/"},
          {uid, 33},
          {gid, 33}
         ]).
-endif.

-ifdef(__MYDLP_NETWORK).
-define(SMTP, [ 
          {helo_name, "mydlp.org"},
          {next_hop, {"localhost", 10027}},
          {bypass_on_fail, true},
          {enable_for_all, true}
         ]).
-endif.

-ifdef(__MYDLP_NETWORK).
-define(ICAP, [
          {path, "/dlp"},
          {path_respmod, "/dlp-respmod"},
          {max_connections, 0},
          {options_ttl, 0},
          {log_pass, false},
          {log_pass_lower_limit, 10240}
         ]).
-endif.

-ifdef(__MYDLP_NETWORK).
-define(SMB_DISCOVER, [ 
          {activate, false},
          {script_path, "/usr/sbin/mydlp-smb-discover"},
          {interval, 3600}
         ]).
-endif.

-ifdef(__MYDLP_NETWORK).
-define(ARCHIVE, [ 
          {minimum_size, 256}
         ]}).
-endif.

-ifdef(__MYDLP_NETWORK).
-define(MINIMUM_ARCHIVE_OBJ_SIZE, 512).
-endif.

-ifdef(__MYDLP_NETWORK).
-define(AUTO_DIST, [
          {activate, false}
         ]).
-endif.

-define(ACL_LOG(Proto, RuleId, Action, Ip, User, To, Matcher, File, Misc), 
	mydlp_api:acl_msg(Proto, RuleId, Action, Ip, User, To, Matcher, File, Misc)).

-define(ERROR_LOG(Format, Args),
	mydlp_logger:notify(error, Format, Args)).

-define(DEBUG(Format, Args),
	mydlp_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
	mydlp_logger:info_msg(?MODULE,?LINE,Format, Args)).

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
