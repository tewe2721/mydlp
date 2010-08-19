
-ifndef(_MYDLP_HTTP_HRL).
-define(_MYDLP_HTTP_HRL, true).

-include("mydlp.hrl").

-record(http_request, {
		method,
		path,
		version
	}).

-record(http_headers, {
		connection,
		host,
		cookie = [],
		keep_alive,
		content_length,
		content_type,
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
