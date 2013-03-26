%%%
%%%    Copyright (c) 2008-2010 Peter Lemenkov <lemenkov@gmail.com>
%%%    Copyright (C) 2012 Huseyin Kerem Cevahir <kerem@mydlp.com>
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

%% Specify levels.
-define(LOG_EMERGENCY, 0). % system is unusable
-define(LOG_ALERT,     1). % action must be taken immediately
-define(LOG_CRITICAL,  2). % critical conditions
-define(LOG_ERROR,     3). % error conditions
-define(LOG_WARNING,   4). % warning conditions
-define(LOG_NOTICE,    5). % normal but significant condition
-define(LOG_INFO,      6). % informational
-define(LOG_DEBUG,     7). % debug-level messages

% facility codes
-define(FAC_KERN,        (0 bsl 3)). % kernel messages
-define(FAC_USER,        (1 bsl 3)). % random user-level messages
-define(FAC_MAIL,        (2 bsl 3)). % mail system
-define(FAC_DAEMON,      (3 bsl 3)). % system daemons
-define(FAC_AUTH,        (4 bsl 3)). % security/authorization messages
-define(FAC_SYSLOG,      (5 bsl 3)). % messages generated internally by syslogd
-define(FAC_LPR,         (6 bsl 3)). % line printer subsystem
-define(FAC_NEWS,        (7 bsl 3)). % network news subsystem
-define(FAC_UUCP,        (8 bsl 3)). % UUCP subsystem
-define(FAC_CRON,        (9 bsl 3)). % clock daemon
-define(FAC_AUTHPRIV,   (10 bsl 3)). % security/authorization messages (private)
-define(FAC_FTP,        (11 bsl 3)). % ftp daemon

% these codes (from 12 through 15) are reserved for system use
%-define(FAC_NTP,	(12 bsl 3)).
%-define(FAC_LOG_ALERT,	(13 bsl 3)).
%-define(FAC_LOG_AUDIT,	(14 bsl 3)).
%-define(FAC_CLOCK,	(15 bsl 3)).

-define(FAC_LOCAL0,     (16 bsl 3)). % reserved for local use
-define(FAC_LOCAL1,     (17 bsl 3)). % reserved for local use
-define(FAC_LOCAL2,     (18 bsl 3)). % reserved for local use
-define(FAC_LOCAL3,     (19 bsl 3)). % reserved for local use
-define(FAC_LOCAL4,     (20 bsl 3)). % reserved for local use
-define(FAC_LOCAL5,     (21 bsl 3)). % reserved for local use
-define(FAC_LOCAL6,     (22 bsl 3)). % reserved for local use
-define(FAC_LOCAL7,     (23 bsl 3)). % reserved for local use

-define(SUCCESS_MOUNT_KEY, "mount.successful").
-define(UNSUCCESS_MOUNT_KEY, "mount.unsuccessful").
-define(SUCCESS_UMOUNT_KEY, "umount.successful").
-define(UNSUCCESS_UMOUNT_KEY, "umount.unsuccessful").


-record(report, {name, facility, format, data}).

-record(log, {
                time=undefined,
                channel=undefined,
                rule_id=undefined,
                action=undefined,
                ip=undefined,
                user=undefined,
                destination=undefined,
                itype_id=undefined,
                file=undefined,
                misc=undefined,
                payload=undefined,
		group_id=undefined
        }).

-record(opr_log, {
		time=undefined, 
		channel=undefined,
		rule_id=undefined,
		message_key=undefined,
		group_id=undefined,
		report_id=undefined
	}).

-define(ACL_LOG(Log),
        ?ACL_LOG_P(Log#log{payload=none})).

-define(ACL_LOG_P(Log),
        mydlp_api:acl_msg(Log)).

-define(LOGGER_NOTIFY(Tag,Format,Args),
        mydlp_logger:notify(Tag, "~P:~P " ++ Format, lists:append([ [I,32] || I <- ([?MODULE_STRING, ?LINE] ++ Args)]))
	).

-define(LOGGER_NOTIFY_0(Tag,Format,Args),
        mydlp_logger:notify(Tag, Format, lists:append([ [I,32] || I <- (Args)]))
	).

-define(ERROR_LOG(Format, Args),
	?LOGGER_NOTIFY(error, Format, Args)
        ).

-define(OPR_LOG(Format, Args),
	?LOGGER_NOTIFY_0({operational, general}, Format, Args)
        ).

-define(DISCOVERY_OPR_LOG(OprLog),
	?LOGGER_NOTIFY_0({operational, discovery}, OprLog, [])
	).

-define(BINARY_LOG(ItemName, Binary),
        FN = mydlp_api:ref_to_fn(?CFG(log_dir), "binlog", erlang:now()),
        file:write_file(FN, Binary),
        ?ERROR_LOG("Binary item ("?S") logged: "?S"~n", [ItemName, FN])
        ).

-define(DEBUG(Format, Args),
	?LOGGER_NOTIFY(info_msg, Format, Args)
	).

-define(S, "~P").

