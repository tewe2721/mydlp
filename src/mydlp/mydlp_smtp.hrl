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

-ifndef(_MYDLP_SMTP_HRL).
-define(_MYDLP_SMTP_HRL, true).

-include("mydlp.hrl").

-ifndef(D).
-define(D(X), ?DEBUG("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-endif.
-ifndef(CRLF).
-define(CRLF,[13,10]).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.


-define(SMTPD_PORT,25).
-define(SMTPD_MAX_CONN,25).
-define(SMTP_DATA_END, [13,10,46,13,10]). % End of data command "\r\n.\r\n"

-record(smtpc,{
	socket = [],
	features = [],
	type = smtp, % smtp server type: [smtp:esmtp]
	state = helo % State of command, [helo,mail,rcpt,data]
	}).

-record(smtpd_fsm,{
	socket      = [],
	addr        = [],
	relay       = false,
	enable_for_all = false,
	options     = [],
	buff        = <<>>,
	line        = [],
	cmd         = undefined,
	param       = undefined,
	host        = undefined,
	mail        = undefined,
	rcpt        = undefined,
	to          = undefined,
	messagename = undefined,
	message_record = undefined,
	message_mime = undefined,
	files       = [],
	data        = undefined
	}).

-record(message,{
        mail_from    = [], % mail from value
        from         = [], % single address for sender
        rcpt_to      = [], % rcpt to value
        to           = [], % address list for recepient
        cc           = [], % address list for carbon copy
        bcc          = [], % address list for blind carbon copy
        internaldate = [], % date message was received
        size         = 0,  % integer() size of message
        options      = [], % Key/Value list of options
        message      = []  % Whole Mail Message
        }).

-record(mime,{
        header      = [],
        header_text = [],
        body        = [],
        body_text   = [],
        content     = [],
        message     = []
        }).

-record(addr,{
        username    = [],
        domainname  = [],
        description = []
        }).

-endif.
