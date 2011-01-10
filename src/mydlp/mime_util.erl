%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       Multipurpose Internet Mail Extention functions
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.6
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stuart Jackson, Simple Enigma, Inc. All Righs Reserved
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(mime_util).
-author('sjackson@simpleenigma.com').
-include("mydlp_smtp.hrl").

-export([decode/1, decode_content/2]).

-export([split/1,headers/1,split_multipart/2,get_header/2,get_header/3,dec_addr/1]).

%%-------------------------------------------------------------------------
%% @spec (Mime::mime()) -> mime() | {error,Reason::atom()}
%% @doc Recursively decodes a string into a #mime{} record.
%% @end
%%-------------------------------------------------------------------------
decode(Message) when is_list(Message) -> decode(list_to_binary(Message));
decode(Message) when is_binary(Message) -> 
	MIME = split(Message),
	Headers = headers(MIME#mime.header_text),
	case lists:keysearch('content-type',1,Headers) of
		{value,{'content-type',Value}} ->
			case lists:prefix("multipart",http_util:to_lower(Value)) of
				true -> 
					Pos = string:chr(Value,61),
					{_,B} = lists:split(Pos,Value),
					Boundary = string:strip(B,both,34),
					Content = case re:run(MIME#mime.body_text, 
							mydlp_api:escape_regex(Boundary), 
							[{capture,first}] ) of
						nomatch -> [];
						{match,[{0,_}]} -> [];
						{match,[{1,_}]} -> [];
						{match,[{2,_}]} -> [];
						{match,[{I,_}]} -> 
							CSize = I - 2,
							<<C:CSize/binary, _/binary>> = MIME#mime.body_text, C 
					end,
					Parts = split_multipart(Boundary,MIME#mime.body_text),
					MIMEParts = lists:map(fun(P) ->
							decode(P)
						end, Parts),
					MIME#mime{header = Headers, content = Content, body = MIMEParts};
				false -> MIME#mime{header = Headers, content = MIME#mime.body_text}
			end;
		_ -> MIME#mime{header = Headers, content = MIME#mime.body_text}
		%_ -> MIME#mime{header = Headers, body = MIME#mime.body_text, message = Message}
	end.

dec_addrs(AddrList) ->
	List = string:tokens(AddrList,[44]),
	lists:map(fun(Addr) -> 
		dec_addr(Addr)
		end,List).

dec_addr(Address) ->
	case mydlp_api:rsplit_at(Address) of
		{Email,[]} -> dec_addr2(Email,#addr{});
		{Desc,Email} -> dec_addr2(Email,#addr{description = mydlp_api:unquote(Desc)})
	end.

dec_addr2(Address,Addr) ->
	A = string:strip(string:strip(Address,left,60),right,62),
	{UserName,DomainName} = mydlp_api:split_email(A),
	Addr#addr{username = UserName,domainname = DomainName}.
	



%%-------------------------------------------------------------------------
%% @spec (HeaderText::string()) -> HeaderList::list()
%% @doc Parses HeaderText into a Key/Value list
%% @end
%%-------------------------------------------------------------------------
headers(HeaderText) when is_binary(HeaderText) -> headers(binary_to_list(HeaderText));
headers(HeaderText) when is_list(HeaderText) ->
	%{ok,H,_Lines} = regexp:gsub(HeaderText,"\r\n[\t ]"," "),
	H = re:replace(HeaderText,"\r\n[\t ]"," ", [global, {return, list}]),
	Tokens = string:tokens(H,[13,10]),
	headers(Tokens,[]).
%%-------------------------------------------------------------------------
%% @spec (list(),Acc::list()) -> list()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
headers([H|T],Acc) ->
	Pos = string:chr(H,58),
	{HeaderString,Val} = lists:split(Pos,H),
	Value = case Header = list_to_atom(http_util:to_lower(string:strip(HeaderString,right,58))) of
		from -> dec_addr(Val);
		to   -> dec_addrs(Val);
		cc   -> dec_addrs(Val);
		bcc  -> dec_addrs(Val);
		_ -> Val
	end,
	headers(T,[head_clean(Header,Value)|Acc]);
headers([],Acc) -> lists:reverse(Acc).

head_clean(Key, #addr{} = Value) -> {Key,Value};
head_clean(Key,Value) ->
	{Key,strip(Value)}.


strip(Value) -> strip(Value,[32,9,13,10]).
strip(Value,[]) -> Value;
strip(Value,[H|T]) ->
	strip(string:strip(Value,both,H),T).

	



%%-------------------------------------------------------------------------
%% @spec (Part::string()) -> mime() | {error,Reason::atom()}
%% @doc Splits the part at the lcoation of two CRLF (\r\n) in a row and 
%%      returns a #mime{} record. Performs some cleanup as well. Also checks 
%%      for two LF (\n) and splits on that as some bad messages for formed 
%%      this way.
%% @end
%%-------------------------------------------------------------------------
split(Part) when is_list(Part) -> split(list_to_binary(Part));
split(Part) when is_binary(Part) ->
	case re:run(Part, ?D_CRLF_BIN, [{capture,first}]) of
		nomatch ->
			case re:run(Part, <<"\n\n">>, [{capture,first}]) of
				nomatch -> {error,no_break_found};
				{match,[{Pos,2}]} -> 
					HeaderSize = Pos + 2,
					<<Header:HeaderSize/binary, Body/binary>> = Part,
					#mime{header_text=Header, body_text = Body} end;
		{match,[{Pos,4}]} -> 
			HeaderSize = Pos + 4,
			<<Header:HeaderSize/binary, Body/binary>> = Part,
			#mime{header_text=Header, body_text = Body} end.

%%-------------------------------------------------------------------------
%% @spec (Boundary::string(),Body::start()) -> Parts::list()
%% @doc Take the Body of a mutlipart MIME messages and split it into it's 
%%      parts on the boundary marks
%% @end
%%-------------------------------------------------------------------------
split_multipart(Boundary,Body) -> split_multipart(Boundary,Body,[]).
%%-------------------------------------------------------------------------
%% @spec (Boundary::string(),Body::start(),Acc::list()) -> Parts::list()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
split_multipart(_Boundary,<<>>,Acc) -> lists:reverse(Acc);
split_multipart(Boundary,Body,Acc) when is_binary(Body)-> 
	case re:run(Body, mydlp_api:escape_regex(Boundary), [{capture,first}]) of
		nomatch -> split_multipart(Boundary,<<>>,Acc);
		{match,[{Start,Length}]} when is_integer(Start) ->
			JSize = Start + Length + 1,
			<<_Pre:JSize/binary, New/binary>> = Body,
			%case regexp:match(New,Boundary) of
			case re:run(New, mydlp_api:escape_regex(Boundary), [{capture,first}]) of
				nomatch -> split_multipart(Boundary,<<>>,Acc);
				{match, [{Start2, _Length2}]} when is_integer(Start2) ->
					PSize = Start2 - 3,
					<<Part:PSize/binary, Next/binary>> = New,
					split_multipart(Boundary,Next,[Part|Acc])
			end
	end.

get_header(Key, #mime{} = MIME) -> get_header(Key,MIME#mime.header,[]);
get_header(Key,Header) -> get_header(Key,Header,[]).

get_header(Key, #mime{} = MIME,Default) -> get_header(Key,MIME#mime.header,Default);
get_header(Key,Header,Default) ->
	case lists:keysearch(Key,1,Header) of
		{value,{Key,Value}} -> Value;
		_ -> Default
	end.

quoted_to_raw(EncContent) when is_list(EncContent) -> quoted_to_raw(list_to_binary(EncContent));
quoted_to_raw(EncContent) when is_binary(EncContent) -> quoted_to_raw(EncContent, <<>>).

quoted_to_raw(<<$=, 13, 10, Rest/binary>>, Acc ) -> quoted_to_raw(Rest, Acc);
quoted_to_raw(<<$=, 10, Rest/binary>>, Acc ) -> quoted_to_raw(Rest, Acc);
quoted_to_raw(<<$=, H1, H2, Rest/binary>>, Acc ) -> 
	I = mydlp_api:hex2int([H1,H2]),
	quoted_to_raw(Rest, <<Acc/binary, I/integer>>);
quoted_to_raw(<<C/integer, Rest/binary>>, Acc ) -> quoted_to_raw(Rest, <<Acc/binary, C/integer>>);
quoted_to_raw(<<>>, Acc ) -> Acc.

decode_content("7bit", EncContent) -> decode_content('7bit', EncContent);
decode_content('7bit', EncContent) -> list_to_binary([EncContent]);
decode_content("8bit", EncContent) -> decode_content('8bit', EncContent);
decode_content('8bit', EncContent) -> list_to_binary([EncContent]);
decode_content("binary", EncContent) -> decode_content('binary', EncContent);
decode_content('binary', EncContent) -> list_to_binary([EncContent]);
decode_content("base64", EncContent) -> decode_content('base64', EncContent);
decode_content('base64', EncContent) -> base64:decode(EncContent);
decode_content("quoted-printable", EncContent) -> decode_content('quoted-printable', EncContent);
decode_content('quoted-printable', EncContent) -> quoted_to_raw(EncContent);
decode_content(_Other, EncContent) -> decode_content('7bit', EncContent).

-include_lib("eunit/include/eunit.hrl").

quoted_test() ->
	QuotedStr = <<"If you believe that truth=3Dbeauty, then surely=20=\nmathematics is the most beautiful branch of philosophy.">>,
	CleanStr = <<"If you believe that truth=beauty, then surely mathematics is the most beautiful branch of philosophy.">>,
	?assertEqual(CleanStr, decode_content('quoted-printable',QuotedStr) ).

multipart_test() ->
	RawMessage = <<"Subject: ugh\r\nMIME-Version: 1.0\r\nContent-Type: multipart/mixed; boundary=\"frontier\"\r\n\r\nThis is a message with multiple parts in MIME format.\r\n--frontier\r\nContent-Type: text/plain\r\n\r\nThis is the body of the message.\r\n--frontier\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: base64\r\n\r\nPGh0bWw+CiAgPGhlYWQ+CiAgPC9oZWFkPgogIDxib2R5PgogICAgPHA+VGhpcyBpcyB0aGUg\r\nYm9keSBvZiB0aGUgbWVzc2FnZS48L3A+CiAgPC9ib2R5Pgo8L2h0bWw+Cg==\r\n--frontier-x1 -\r\n">>,
	ParsedMessage = {mime,[{subject,"ugh"},
       {'mime-version',"1.0"},
       {'content-type',"multipart/mixed; boundary=\"frontier\""}],
      <<"Subject: ugh\r\nMIME-Version: 1.0\r\nContent-Type: multipart/mixed; boundary=\"frontier\"\r\n\r\n">>,
      [{mime,[{'content-type',"text/plain"}],
             <<"\nContent-Type: text/plain\r\n\r\n">>,[],
             <<"This is the body of the message.\r">>,
             <<"This is the body of the message.\r">>,[]},
       {mime,[{'content-type',"application/octet-stream"},
              {'content-transfer-encoding',"base64"}],
             <<"\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: base64\r\n\r\n">>,
             [],
             <<"PGh0bWw+CiAgPGhlYWQ+CiAgPC9oZWFkPgogIDxib2R5PgogICAgPHA+VGhpcyBpcyB0aGUg\r\nYm9keSBvZiB0aGUgbWVzc2FnZS48L3A+CiAgPC9ib2R5Pgo8L2h0bWw+Cg==\r">>,
             <<"PGh0bWw+CiAgPGhlYWQ+CiAgPC9oZWFkPgogIDxib2R5PgogICAgPHA+VGhpcyBpcyB0aGUg\r\nYm9keSBvZiB0aGUgbWVzc2FnZS48L3A+CiAgPC9ib2R5Pgo8L2h0bWw+Cg==\r">>,
             []}],
      <<"This is a message with multiple parts in MIME format.\r\n--frontier\r\nContent-Type: text/plain\r\n\r\nThis is the body of the message.\r\n--frontier\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: base64\r\n\r\nPGh0bWw+CiAgPGhlYWQ+CiAgPC9oZWFkPgogIDxib2R5PgogICAgPHA+VGhpcyBpcyB0aGUg\r\nYm9keSBvZiB0aGUgbWVzc2FnZS48L3A+CiAgPC9ib2R5Pgo8L2h0bWw+Cg==\r\n--frontier-x1 -\r\n">>,
      <<"This is a message with multiple parts in MIME format.\r\n">>,
      []},
	?assertEqual(ParsedMessage, decode(RawMessage)).
