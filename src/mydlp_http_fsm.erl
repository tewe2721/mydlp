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

-module(mydlp_http_fsm).

-author('kerem@medra.com.tr').

-compile(export_all).

-record(request, {
		method,
		uri,
		version
	}).

-record(headers, {
		connection,
		accept,
		host,
		keep_alive,
		content_length,
		content_type,
		content_encoding,		
		transfer_encoding,
		cookie = [],
		other =[]
	}).

-record(state, {
		request,
		headers,
		tmp=[],
		tmp2=[]
	}).

%% determining initial FSM state and creating state record.
init() ->
	{ok, 'M_START', #state{ request=#request{}, headers=#headers{} }}.

% HTTP Method
'M_START'({byte, Byte}, State) when Byte == $G ; Byte == $g ->
	{next_state, 'M_G', State, none};
'M_START'({byte, Byte}, State) when Byte == $P ; Byte == $p ->
	{next_state, 'M_P', State, none};
'M_START'({byte, Byte}, State) when Byte == $H ; Byte == $h ->
	{next_state, 'M_H', State, none};
'M_START'({byte, Byte}, State) when Byte == $T ; Byte == $t ->
	{next_state, 'M_T', State, none};
'M_START'({byte, Byte}, State) when Byte == $O ; Byte == $o ->
	{next_state, 'M_O', State, none};
'M_START'({byte, Byte}, State) when Byte == $D ; Byte == $d ->
	{next_state, 'M_D', State, none};
'M_START'({byte, Byte}, State) when Byte == $M ; Byte == $m ->
	{next_state, 'M_M', State, none};
'M_START'({byte, Byte}, State) when Byte == $C ; Byte == $c ->
	{next_state, 'M_C', State, none}.

'M_G'({byte, Byte}, State) when Byte == $E ; Byte == $e ->
	{next_state, 'M_GE', State, none}.

'M_GE'({byte, Byte}, State) when Byte == $T ; Byte == $t ->
	{next_state, 'M_GET', State, none}.

'M_GET'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=get}}, none}.

'M_P'({byte, Byte}, State) when Byte == $O ; Byte == $o ->
	{next_state, 'M_PO', State, none};
'M_P'({byte, Byte}, State) when Byte == $U ; Byte == $u ->
	{next_state, 'M_PU', State, none};
'M_P'({byte, Byte}, State) when Byte == $R ; Byte == $r ->
	{next_state, 'M_PR', State, none}.

'M_PO'({byte, Byte}, State) when Byte == $S ; Byte == $s ->
	{next_state, 'M_POS', State, none}.

'M_POS'({byte, Byte}, State) when Byte == $T ; Byte == $t ->
	{next_state, 'M_POST', State, none}.

'M_POST'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=post}}, none}.

'M_H'({byte, Byte}, State) when Byte == $E ; Byte == $e ->
	{next_state, 'M_HE', State, none}.

'M_HE'({byte, Byte}, State) when Byte == $A ; Byte == $a ->
	{next_state, 'M_HEA', State, none}.

'M_HEA'({byte, Byte}, State) when Byte == $D ; Byte == $d ->
	{next_state, 'M_HEAD', State, none}.

'M_HEAD'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=head}}, none}.

'M_T'({byte, Byte}, State) when Byte == $R ; Byte == $r ->
	{next_state, 'M_TR', State, none}.

'M_TR'({byte, Byte}, State) when Byte == $A ; Byte == $a ->
	{next_state, 'M_TRA', State, none}.

'M_TRA'({byte, Byte}, State) when Byte == $C ; Byte == $c ->
	{next_state, 'M_TRAC', State, none}.

'M_TRAC'({byte, Byte}, State) when Byte == $E ; Byte == $e ->
	{next_state, 'M_TRACE', State, none}.

'M_TRACE'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=trace}}, none}.

'M_O'({byte, Byte}, State) when Byte == $P ; Byte == $p ->
	{next_state, 'M_OP', State, none}.

'M_OP'({byte, Byte}, State) when Byte == $T ; Byte == $t ->
	{next_state, 'M_OPT', State, none}.

'M_OPT'({byte, Byte}, State) when Byte == $I ; Byte == $i ->
	{next_state, 'M_OPTI', State, none}.

'M_OPTI'({byte, Byte}, State) when Byte == $O ; Byte == $o ->
	{next_state, 'M_OPTIO', State, none}.

'M_OPTIO'({byte, Byte}, State) when Byte == $N ; Byte == $n ->
	{next_state, 'M_OPTION', State, none}.

'M_OPTION'({byte, Byte}, State) when Byte == $S ; Byte == $s ->
	{next_state, 'M_OPTIONS', State, none}.

'M_OPTIONS'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=options}}, none}.

'M_PU'({byte, Byte}, State) when Byte == $T ; Byte == $T ->
	{next_state, 'M_PUT', State, none}.

'M_PUT'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=put}}, none}.

'M_D'({byte, Byte}, State) when Byte == $E ; Byte == $e ->
	{next_state, 'M_DE', State, none}.

'M_DE'({byte, Byte}, State) when Byte == $L ; Byte == $l ->
	{next_state, 'M_DEL', State, none}.

'M_DEL'({byte, Byte}, State) when Byte == $E ; Byte == $e ->
	{next_state, 'M_DELE', State, none}.

'M_DELE'({byte, Byte}, State) when Byte == $T ; Byte == $t ->
	{next_state, 'M_DELET', State, none}.

'M_DELET'({byte, Byte}, State) when Byte == $E ; Byte == $e ->
	{next_state, 'M_DELETE', State, none}.

'M_DELETE'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=delete}}, none}.

'M_PR'({byte, Byte}, State) when Byte == $O ; Byte == $o ->
	{next_state, 'M_PRO', State, none}.

'M_PRO'({byte, Byte}, State) when Byte == $P ; Byte == $p ->
	{next_state, 'M_PROP', State, none}.

'M_PROP'({byte, Byte}, State) when Byte == $F ; Byte == $f ->
	{next_state, 'M_PROPF', State, none}.

'M_PROPF'({byte, Byte}, State) when Byte == $I ; Byte == $i ->
	{next_state, 'M_PROPFI', State, none}.

'M_PROPFI'({byte, Byte}, State) when Byte == $N ; Byte == $n ->
	{next_state, 'M_PROPFIN', State, none}.

'M_PROPFIN'({byte, Byte}, State) when Byte == $D ; Byte == $d ->
	{next_state, 'M_PROPFIND', State, none}.

'M_PROPFIND'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=propfind}}, none}.

'M_M'({byte, Byte}, State) when Byte == $O ; Byte == $o ->
	{next_state, 'M_MO', State, none};
'M_M'({byte, Byte}, State) when Byte == $K ; Byte == $k ->
	{next_state, 'M_MK', State, none}.

'M_MO'({byte, Byte}, State) when Byte == $V ; Byte == $v ->
	{next_state, 'M_MOV', State, none}.

'M_MOV'({byte, Byte}, State) when Byte == $E ; Byte == $e ->
	{next_state, 'M_MOVE', State, none}.

'M_MOVE'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=move}}, none}.

'M_C'({byte, Byte}, State) when Byte == $O ; Byte == $o ->
	{next_state, 'M_CO', State, none}.

'M_CO'({byte, Byte}, State) when Byte == $P ; Byte == $p ->
	{next_state, 'M_COP', State, none}.

'M_COP'({byte, Byte}, State) when Byte == $Y ; Byte == $y ->
	{next_state, 'M_COPY', State, none}.

'M_COPY'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=copy}}, none}.

'M_MK'({byte, Byte}, State) when Byte == $C ; Byte == $c ->
	{next_state, 'M_MKC', State, none}.

'M_MKC'({byte, Byte}, State) when Byte == $O ; Byte == $o ->
	{next_state, 'M_MKCO', State, none}.

'M_MKCO'({byte, Byte}, State) when Byte == $L ; Byte == $l ->
	{next_state, 'M_MKCOL', State, none}.

'M_MKCOL'({byte, Byte}, #state{request=R} = State) when Byte == $\s ->
	{next_state, 'URI', State#state{request=R#request{method=mkcol}}, none}.

% HTTP Uri
'URI'({byte, $\s}, #state{request=R, tmp=T} = State) ->
	{next_state, 'VER', State#state{request=R#request{uri=lists:reverse(T)}, tmp=[]}, none};
'URI'({byte, Byte}, #state{tmp=T} = State) -> 
	case mydlp_api:is_uri_char(Byte) of
		true -> {next_state, 'URI', State#state{tmp=[Byte|T]}, none};
		false -> {error, invalid_char}
	end.

% HTTP Protocol Version
'VER'({byte, Byte}, State) when Byte == $H ; Byte == $h ->
	{next_state, 'VER_H', State, none}.

'VER_H'({byte, Byte}, State) when Byte == $T ; Byte == $t ->
	{next_state, 'VER_HT', State, none}.

'VER_HT'({byte, Byte}, State) when Byte == $T ; Byte == $t ->
	{next_state, 'VER_HTT', State, none}.

'VER_HTT'({byte, Byte}, State) when Byte == $P ; Byte == $p ->
	{next_state, 'VER_HTTP', State, none}.

'VER_HTTP'({byte, $/}, State) ->
	{next_state, 'VER_HTTP/', State, none}.

'VER_HTTP/'({byte, $1}, State) ->
	{next_state, 'VER1', State, none}.

'VER1'({byte, $.}, State) ->
	{next_state, 'VER1.', State, none}.

'VER1.'({byte, $0}, #state{request=R} = State) ->
	{next_state, 'VER_CR', State#state{request=R#request{version=http10}}, none};
'VER1.'({byte, $1}, #state{request=R} = State) ->
	{next_state, 'VER_CR', State#state{request=R#request{version=http11}}, none}.

% Carriage return after version
'VER_CR'({byte, $\r}, State) ->
	{next_state, 'VER_NL', State, none}.

% New line after version
'VER_NL'({byte, $\n}, State) ->
	{next_state, 'H_KEY', State, none}.

% Headers
'H_KEY'({byte, $:}, State) ->
	{next_state, 'H_SEP', State, none};
'H_KEY'({byte, Byte}, #state{tmp=T} = State) ->
	case ((not mydlp_api:is_char(Byte)) 
			or mydlp_api:is_ctl(Byte) 
			or mydlp_api:is_tspecial(Byte)) of
		true -> {error, invalid_char};
		false -> {next_state, 'H_KEY', State#state{tmp=[Byte|T]}, none}
	end.

'H_SEP'({byte, Byte}, State) when Byte == $\s ; Byte == $\t ->
	{next_state, 'H_SEP', State, none};
'H_SEP'({byte, Byte}, State) ->
	'H_VAL'({byte, Byte}, State).

'H_VAL'({byte, $\r}, #state{headers=H, tmp=T, tmp2=T2} = State) ->
	Key = lists:reverse(T),
	Value = lists:reverse(T2),

	H1 = case string:to_lower(Key) of
		"host" -> H#headers{host=Value};
		"content-length" -> H#headers{content_length=list_to_integer(Value)};
		"content-type" -> H#headers{content_type=Value};
		"content-encoding" -> H#headers{content_encoding=Value};
		"cookie" -> Cookies = H#headers.cookie, H#headers{cookie=[Value|Cookies]};
		_ -> Others = H#headers.other, H#headers{other=[{Key,Value}|Others]}
	end,

	{next_state, 'H_CR', State#state{headers=H1, tmp=[], tmp2=[]}, none};
'H_VAL'({byte, Byte}, #state{tmp2=T2} = State) ->
	case mydlp_api:is_ctl(Byte) of
		true -> {error, invalid_char};
		false -> {next_state, 'H_VAL', State#state{tmp2=[Byte|T2]}, none}
	end.

'H_CR'({byte, $\n}, State) ->
	{next_state, 'H_NL', State, none}.

'H_NL'({byte, $\r}, State) ->
	{next_state, 'H_CR2', State, none};
'H_NL'({byte, Byte}, State) ->
	'H_KEY'({byte, Byte}, State).

'H_CR2'({byte, $\n}, #state{request=R, headers=H} = State) ->
	Others = H#headers.other,
	Cookies = H#headers.cookie,

	H1 = H#headers{
		other=lists:reverse(Others), 
		cookie=lists:reverse(Cookies)},
	
	NoBodyM = {next_state, '', State#state{headers=H1}, success},
	BodyM = {next_state, 'CONTENT', State#state{headers=H1}, none},

	case R#request.method of
		post -> BodyM;
		put -> BodyM;
		propfind -> BodyM;
		_ -> NoBodyM
	end.

'CONTENT'({byte, Byte}, State) ->
	ugh.



























