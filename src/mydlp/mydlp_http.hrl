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

-ifdef(__MYDLP_NETWORK).

-ifndef(_MYDLP_HTTP_HRL).
-define(_MYDLP_HTTP_HRL, true).

-include("mydlp.hrl").

-record(http_request, {
		method,
		path,
		version
	}).

-record(http_response, {
		code,
		phrase,
		version
	}).

-record(http_headers, {
		connection,
		host,
		cookie = [],
		keep_alive,
		content_length,
		content_type,
		content_disposition,
		content_encoding,
		transfer_encoding,
		other = []   %% misc other headers
	}).

-record(http_header, {
		num,
		key,
		reserved,
		value
	}).

%%% Keep-alive timeout (Apache web server default)
-define(KA_TIMEOUT, 15000).

-define(MPART_CNK, 1024).

-define(NEWLINE, <<"\r\n">>).

-endif.

-endif.

