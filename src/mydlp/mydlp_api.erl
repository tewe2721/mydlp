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

-module(mydlp_api).

-author('kerem@medra.com.tr').

-compile(export_all).

-include("mydlp.hrl").
-include("mydlp_http.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%--------------------------------------------------------------------
%% @doc Check if a byte is an HTTP character
%% @end
%%----------------------------------------------------------------------
is_char(X) when X >= 0 , X =< 127 -> true;
is_char(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is an HTTP control character
%% @end
%----------------------------------------------------------------------
is_ctl(X) when X >= 0 , X =< 31 -> true;
is_ctl(X) when X == 127 -> true;
is_ctl(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is defined as an HTTP tspecial character.
%% @end
%%----------------------------------------------------------------------
is_tspecial(X) when
		X == $( ; X == $) ; X == $< ; X == $> ; X == $@ ;
		X == $, ; X == $; ; X == $: ; X == $\ ; X == $" ;
		X == $/ ; X == $[ ; X == $] ; X == $? ; X == $= ;
		X == ${ ; X == $} ; X == $\s ; X == $\t -> true;
is_tspecial(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is a digit.
%% @end
%%----------------------------------------------------------------------
is_digit(X) when X >= $0 , X =< $9 -> true;
is_digit(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is alphanumeric.
%% @end
%%----------------------------------------------------------------------
is_alpha(X) when X >= $a , X =< $z -> true;
is_alpha(X) when X >= $A , X =< $Z -> true;
is_alpha(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is defined as an HTTP URI unreserved character.
%% @end
%%----------------------------------------------------------------------
is_uuri_char(X) when X == $- ; X == $_ ; X == $. ; X == $~ -> true;
is_uuri_char(X) -> is_digit(X) or is_alpha(X).

%%--------------------------------------------------------------------
%% @doc Check if a byte is defined as an HTTP URI reserved character.
%% @end
%%----------------------------------------------------------------------
is_ruri_char(X) when
		X == $! ; X == $* ; X == $\ ; X == $( ; X == $) ;
		X == $; ; X == $: ; X == $@ ; X == $& ; X == $= ;
		X == $+ ; X == $$ ; X == $, ; X == $? ; X == $/ ;
		X == $% ; X == $# ; X == $[ ; X == $] -> true;
is_ruri_char(_) -> false.

%%--------------------------------------------------------------------
%% @doc Check if a byte is defined as an HTTP URI character.
%% @end
%%----------------------------------------------------------------------
is_uri_char(X) -> is_uuri_char(X) or is_ruri_char(X).

%%--------------------------------------------------------------------
%% @doc Truncates nl chars in a string.
%% @end
%%----------------------------------------------------------------------
nonl(B) when is_binary(B) ->
    nonl(binary_to_list(B));
nonl([10|T]) ->
    nonl(T);
nonl([13|T]) ->
    nonl(T);
nonl([32|T]) ->
    nonl(T);
nonl([H|T]) ->
    [H|nonl(T)];
nonl([]) ->
    [].

%%--------------------------------------------------------------------
%% @doc Converts hexadecimal to decimal integer
%% @end
%%----------------------------------------------------------------------
hex2int(Line) ->
    erlang:list_to_integer(nonl(Line),16).

%%--------------------------------------------------------------------
%% @doc Checks a string whether starts with given string
%% @end
%%----------------------------------------------------------------------
starts_with(_Str, []) ->
        false;

starts_with([Char|_Str], [Char|[]]) ->
        true;

starts_with([Char|Str], [Char|StrCnk]) ->
        starts_with(Str, StrCnk);

starts_with(_, _) ->
        false.

%%--------------------------------------------------------------------
%% @doc Computes md5 sum of given object.
%% @end
%%----------------------------------------------------------------------
md5_hex(S) ->
	Md5_bin =  erlang:md5(S),
	Md5_list = binary_to_list(Md5_bin),
	lists:flatten(list_to_hex(Md5_list)).
 
list_to_hex(L) ->
	lists:map(fun(X) -> int_to_hex(X) end, L).
 
%%--------------------------------------------------------------------
%% @doc Converts decimal integer ot hexadecimal
%% @end
%%----------------------------------------------------------------------
int_to_hex(N) when N < 256 ->
	[hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
	$0+N;
hex(N) when N >= 10, N < 16 ->
	$a + (N-10).

%%%% imported from yaws api
funreverse(List, Fun) ->
    funreverse(List, Fun, []).

funreverse([H|T], Fun, Ack) ->
    funreverse(T, Fun, [Fun(H)|Ack]);
funreverse([], _Fun, Ack) ->
    Ack.

to_lowerchar(C) when C >= $A, C =< $Z ->
    C+($a-$A);
to_lowerchar(C) ->
    C.

%%--------------------------------------------------------------------
%% @doc Extracts Texts from MS Office 97 - 2003 Files 
%% @end
%%----------------------------------------------------------------------
-define(DOC, {"/usr/bin/catdoc", ["-wx"]}).
-define(PPT, {"/usr/bin/catppt", []}).
-define(XLS, {"/usr/bin/xls2csv", ["-x"]}).

office_to_text(#file{filename = Filename, data = Data}) ->
	StrLen = case is_list(Filename) of
		true -> string:len(Filename),
		false -> 0 end,

	case StrLen >= 4 of
		true ->	Ext = string:sub_string(Filename, StrLen - 3, StrLen),
			case Ext of
% catppt always returns 0, should resolve this bug before uncommenting these.
%				".doc" -> office_to_text(Data, [?DOC, ?XLS, ?PPT]);
%				".xls" -> office_to_text(Data, [?XLS, ?DOC, ?PPT]);
%				".ppt" -> office_to_text(Data, [?PPT, ?DOC, ?XLS]);
				".doc" -> office_to_text(Data, [?DOC, ?XLS]);
				".xls" -> office_to_text(Data, [?XLS, ?DOC]);
				".ppt" -> office_to_text(Data, [?PPT, ?DOC, ?XLS]);
				_ -> office_to_text(Data, [?DOC, ?XLS, ?PPT]) end;
		false -> office_to_text(Data, [?DOC, ?XLS, ?PPT]) end.

office_to_text(Data, [Prog|Progs]) ->
	{Exec, Args} = Prog,
	{ok, FN} = mktempfile(),
	ok = file:write_file(FN, Data, [raw]),
	Port = open_port({spawn_executable, Exec}, 
			[{args, Args ++ [FN]},
%			[{args, Args},
			binary,
			use_stdio,
			exit_status,
			stderr_to_stdout]),

%%	port_command(Port, Data),
%%	port_command(Port, <<-1>>),

	Ret = case get_port_resp(Port, []) of
		{ok, Text} -> {ok, Text};
		{error, {retcode, _}} -> office_to_text(Data, Progs);
		{error, timeout} -> {error, timeout}
	end,
	ok = file:delete(FN), Ret;
office_to_text(_Data, []) -> {error, corrupted}.

%%--------------------------------------------------------------------
%% @doc Gets response from ports
%% @end
%%----------------------------------------------------------------------
get_port_resp(Port, Ret) ->
	receive
		{ Port, {data, Data}} -> get_port_resp(Port, [Data|Ret]);
		{ Port, {exit_status, 0}} -> {ok, list_to_binary(lists:reverse(Ret))};
		{ Port, {exit_status, RetCode}} -> { error, {retcode, RetCode} }
	after 15000 -> { error, timeout }
	end.

get_port_resp(Port) ->
	receive
		{ Port, {data, _}} -> get_port_resp(Port);
		{ Port, {exit_status, 0}} -> ok;
		{ Port, {exit_status, RetCode}} -> { error, {retcode, RetCode} }
	after 15000 -> { error, timeout }
	end.

%%--------------------------------------------------------------------
%% @doc Extracts Text from File records
%% @end
%%----------------------------------------------------------------------
get_text(#file{mime_type= <<"application/x-empty">>}) -> {ok, <<>>};
get_text(#file{mime_type= <<"text/plain">>, data=Data}) -> {ok, Data};
get_text(#file{mime_type= <<"application/xml">>, data=Data}) ->
	try
		Text = xml_to_txt(Data),
		{ok, Text}
	catch E -> {error, E}
	end;
get_text(#file{mime_type= <<"application/pdf">>, data=Data}) ->
	pdf_to_text(Data);
get_text(#file{mime_type= <<"text/rtf">>, data=Data}) ->
	office_to_text(Data, [?DOC]);
get_text(#file{mime_type= <<"application/vnd.ms-excel">>, data=Data}) ->
	office_to_text(Data, [?XLS, ?DOC, ?PPT]);
get_text(#file{mime_type= <<"CDF V2 Document", _/binary>>} = File) ->  %%% TODO: should be refined
	office_to_text(File);
get_text(#file{mime_type= <<"application/msword">>} = File) ->
	office_to_text(File);
get_text(#file{mime_type= <<"application/vnd.ms-office">>} = File) ->
	office_to_text(File);
get_text(#file{mime_type= <<"text/html">>, data=Data}) ->
	try
		Text = mydlp_tc:html_to_text(Data),
		{ok, Text}
	catch E -> {error, E}
	end;
get_text(#file{mime_type= <<"application/postscript">>, data=Data}) ->
	ps_to_text(Data);
get_text(#file{mime_type= <<"text/",_Rest/binary>>, data=Data}) -> {ok, Data};
get_text(#file{mime_type=undefined}) -> {error, unknown_type};
get_text(_File) -> {error, unsupported_type}.

%%--------------------------------------------------------------------
%% @doc Extracts Text from XML string
%% @end
%%----------------------------------------------------------------------
xml_to_txt(Data) when is_binary(Data)-> xml_to_txt(binary_to_list(Data));
xml_to_txt(Data) when is_list(Data) -> 
	RetList = xml_to_txt1(xmerl_scan:string(Data)),
	RetList1 = lists:filter(fun(I) -> (I >= 0) and (I < 256) end, RetList), 
	list_to_binary(RetList1).

xml_to_txt1(List) when is_list(List) -> xml_to_txt1(List, []);
%xml_to_txt1(#xmlElement{attributes=Attrs, content=Conts}) ->
	%string:join([xml_to_txt1(Attrs), xml_to_txt1(Conts)], " ");
xml_to_txt1(#xmlElement{content=Conts}) -> xml_to_txt1(Conts);
%xml_to_txt1(#xmlAttribute{value=Val}) -> Val;
xml_to_txt1(#xmlText{value=Val}) -> Val;
xml_to_txt1({XmlElement, _}) -> xml_to_txt1(XmlElement).

xml_to_txt1([Comp|Rest], Ret) -> 
	case string:strip(xml_to_txt1(Comp)) of
		[] -> xml_to_txt1(Rest, Ret);
		Else -> xml_to_txt1(Rest, [Else|Ret])
	end;
xml_to_txt1([], Ret) -> string:join(lists:reverse(Ret), " ").

%%--------------------------------------------------------------------
%% @doc Removes specified chars from string
%% @end
%%----------------------------------------------------------------------
remove_chars(Str, Chars) -> remove_chars(Str, Chars, []).

remove_chars([S|Str], Chars, Ret) ->
	case lists:member(S, Chars) of
		true -> remove_chars(Str, Chars, Ret);
		false -> remove_chars(Str, Chars, [S|Ret])
	end;
remove_chars([], _Chars, Ret) -> lists:reverse(Ret).

%%--------------------------------------------------------------------
%% @doc Check for Luhn algorithm.
%% @end
%%----------------------------------------------------------------------
check_luhn(IntegerStr) ->
	L = lists:map(fun(I) -> I - $0 end, IntegerStr),
	check_luhn(lists:reverse(L), false, 0).

check_luhn([I|IntList], false, Tot) -> check_luhn(IntList, true, Tot + I );
check_luhn([I|IntList], true, Tot) -> 
	I2 = I*2,
	case I2 > 9 of
		true -> check_luhn(IntList, false, Tot + I2 - 9 );
		false -> check_luhn(IntList, false, Tot + I2 )
	end;
check_luhn([], _, Tot) -> 0 == (Tot rem 10).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid credit card
%% @end
%%----------------------------------------------------------------------
is_valid_cc(CCStr) ->
	Clean = remove_chars(CCStr, " -"),
	case check_luhn(Clean) of
		false -> false;
		true -> is_valid_cc(Clean, length(Clean))
	end.

is_valid_cc([$4|_Rest], 13) -> true; % VISA
is_valid_cc([$3,$6|_Rest], 14) -> true; % Diners Club
is_valid_cc([$3,$0,$0|_Rest], 14) -> true; % Diners Club
is_valid_cc([$3,$0,$1|_Rest], 14) -> true; % Diners Club
is_valid_cc([$3,$0,$2|_Rest], 14) -> true; % Diners Club
is_valid_cc([$3,$0,$3|_Rest], 14) -> true; % Diners Club
is_valid_cc([$3,$0,$4|_Rest], 14) -> true; % Diners Club
is_valid_cc([$3,$0,$5|_Rest], 14) -> true; % Diners Club
is_valid_cc([$3,$4|_Rest], 15) -> true; % AMEX
is_valid_cc([$3,$7|_Rest], 15) -> true; % AMEX
is_valid_cc([$2,$1,$3,$1|_Rest], 15) -> true; % JCB
is_valid_cc([$1,$8,$0,$0|_Rest], 15) -> true; % JCB
is_valid_cc([$3|_Rest], 16) -> true; % JCB
is_valid_cc([$4|_Rest], 16) -> true; % VISA
is_valid_cc([$5,$1|_Rest], 16) -> true; % MASTERCARD
is_valid_cc([$5,$2|_Rest], 16) -> true; % MASTERCARD
is_valid_cc([$5,$3|_Rest], 16) -> true; % MASTERCARD
is_valid_cc([$5,$4|_Rest], 16) -> true; % MASTERCARD
is_valid_cc([$5,$5|_Rest], 16) -> true; % MASTERCARD
is_valid_cc([$6,$0,$1,$1|_Rest], 16) -> true; % Discover
is_valid_cc(_,_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid IBAN accoun number
%% @end
%%----------------------------------------------------------------------
is_valid_iban(IbanStr) ->
	Clean = remove_chars(IbanStr, " -"),
	mydlp_tc:is_valid_iban(Clean).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid TR ID number
%% @end
%%----------------------------------------------------------------------
is_valid_trid(TrIdStr) ->
	Clean = remove_chars(TrIdStr, " -"),
	[I0,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10] = 
		lists:map(fun(I) -> I - $0 end, Clean),
	S1 = (((I0 + I2 + I4 + I6 + I8)*7) - (I1 + I3 + I5 + I7)) rem 10,
	S2 = ((I0 + I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9) rem 10),
	(S1 == I9) and (S2 == I10).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid SSN number
%% @end
%%----------------------------------------------------------------------
is_valid_ssn(SSNStr) ->
	Clean = remove_chars(SSNStr, " -"),
	case string:len(Clean) of 
		9 ->
			AreaN = list_to_integer(string:substr(Clean,1,3)),
			GroupN = list_to_integer(string:substr(Clean,4,2)),
			SerialN = list_to_integer(string:substr(Clean,6,4)),

			case {AreaN, GroupN, SerialN} of
				{666,_,_} -> false;
				{0,_,_} -> false;
				{_,0,_} -> false;
				{_,_,0} -> false;
				{A1,_,_} -> (A1 < 773) and ( not ( (A1 < 750) and (A1 > 733 ) ) )
		%		{987,65,S1} -> (S1 < 4320) and (S1 > 4329); this line for advertisements
			end;
		_Else -> false
	end.

%%% imported from tsuraan tempfile module http://www.erlang.org/cgi-bin/ezmlm-cgi/4/41649

%%--------------------------------------------------------------------
%% @doc Creates safe temporary files.
%% @end
%%----------------------------------------------------------------------

mktempfile() -> mktemp([]).
mktempdir() -> mktemp([directory]).

mktemp(Args) ->
	Args1 = Args ++ [{tmpdir, "/var/tmp"}, {template, "mydlp.XXXXXXXXXX"}],
	CmdArgs = mt_process_args(Args1, []),
	Port = open_port({spawn_executable, "/bin/mktemp"}, [{args, CmdArgs},
					{line, 1000},
					use_stdio,
					exit_status,
					stderr_to_stdout]),
	mt_get_resp(Port, nil).

mt_process_args([], Cmd) -> lists:reverse(Cmd);
mt_process_args([directory| Rest], Cmd) -> mt_process_args(Rest, ["-d"|Cmd] );
mt_process_args([{Flag, Value} | Rest], Cmd) ->
	case Flag of
		tmpdir -> mt_process_args(Rest, ["--tmpdir=" ++ Value|Cmd]);
		template -> mt_process_args(Rest, [Value|Cmd])
	end.

mt_get_resp(Port, Resp) ->
	case Resp of
	nil ->
		receive
			{ Port, {data, {_, Line}}} -> mt_get_resp(Port, Line);
			{ Port, {exit_status, _ }} -> { error, "No response from mktemp" }
		after 1000 -> { error, timeout }
		end;
	Resp ->
		receive
			{ Port, {data, _}} -> mt_get_resp(Port, Resp);
			{ Port, {exit_status, 0}} -> { ok, Resp };
			{ Port, {exit_status, _}} -> { error, Resp }
		after 1000 -> { error, timeout }
		end
	end.

%%--------------------------------------------------------------------
%% @doc Unrars an Erlang binary 
%% @end
%%----------------------------------------------------------------------

unrar(Bin) when is_binary(Bin) -> 
	{ok, RarFN} = mktempfile(),
	ok = file:write_file(RarFN, Bin, [raw]),
	{ok, WorkDir} = mktempdir(),
	WorkDir1 = WorkDir ++ "/",
	Port = open_port({spawn_executable, "/usr/bin/unrar"}, 
			[{args, ["e","-y","-p-","-inul","--",RarFN]},
			{cd, WorkDir1},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	ok = file:delete(RarFN),

	case get_port_resp(Port) of
		ok -> {ok, rr_files(WorkDir1)};
		Else -> Else
	end.

%%--------------------------------------------------------------------
%% @doc Reads and removes files in WorkDir. Files will be returned as binaries.
%% @end
%%----------------------------------------------------------------------

rr_files(WorkDir) when is_list(WorkDir) ->
	{ok, FileNames} = file:list_dir(WorkDir),
	Return = rr_files(FileNames, WorkDir, []),
	ok = file:del_dir(WorkDir),
	Return.

rr_files([FN|FNs], WorkDir, Ret) -> 
	AbsPath = WorkDir ++ FN,
	{ok, Bin}  = file:read_file(AbsPath),
	ok = file:delete(AbsPath),
	rr_files(FNs, WorkDir, [{FN, Bin}|Ret]);
rr_files([], _WorkDir, Ret) -> lists:reverse(Ret).

%%--------------------------------------------------------------------
%% @doc Extracts Text from PostScript files
%% @end
%%----------------------------------------------------------------------
ps_to_text(Bin) when is_binary(Bin) -> 
	{ok, Ps} = mktempfile(),
	ok = file:write_file(Ps, Bin, [raw]),
	Port = open_port({spawn_executable, "/usr/bin/pstotext"}, 
			[{args, [Ps]},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	Ret = case get_port_resp(Port, []) of
		{ok, Text} -> {ok, Text};
		Else -> Else
	end,
	ok = file:delete(Ps), Ret.

%%--------------------------------------------------------------------
%% @doc Extracts Text from PDF files
%% @end
%%----------------------------------------------------------------------
pdf_to_text(Bin) when is_binary(Bin) -> 
	{ok, Pdf} = mktempfile(),
	ok = file:write_file(Pdf, Bin, [raw]),
	{ok, TextFN} = mktempfile(),
	Port = open_port({spawn_executable, "/usr/bin/pdftotext"}, 
			[{args, ["-q","-eol","unix",Pdf,TextFN]},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	Ret = case get_port_resp(Port) of
		ok -> {ok, Text} = file:read_file(TextFN), {ok, Text};
		Else -> Else
	end,
	ok = file:delete(Pdf), ok = file:delete(TextFN), Ret.

%%--------------------------------------------------------------------
%% @doc Logs acl messages
%% @end
%%----------------------------------------------------------------------
acl_msg(Proto, RuleId, Action, Ip, User, To, Matcher, File, Misc) ->
	FileS = file_to_str(File),
	acl_msg1(Proto, RuleId, Action, Ip, User, To, Matcher, FileS, Misc),

	case Action of
		log -> mydlp_mysql:push_log(Proto, RuleId, Action, Ip, User, To, Matcher, FileS, Misc);
		block -> mydlp_mysql:push_log(Proto, RuleId, Action, Ip, User, To, Matcher, FileS, Misc);
		_Else -> ok
	end.

acl_msg1(Proto, RuleId, Action, {Ip1,Ip2,Ip3,Ip4}, nil, To, Matcher, FileS, Misc) ->
	mydlp_logger:notify(acl_msg,
		"PROTOCOL: ~w , RULE: ~w , ACTION: ~w , FROM: ~w.~w.~w.~w , TO: \"~s\" , MATCHER: ~w , FILE: \"~s\" , MISC: ~s ~n",
		[Proto, RuleId, Action, Ip1,Ip2,Ip3,Ip4, To, Matcher, FileS, Misc]);
acl_msg1(Proto, RuleId, Action, nil, User, To, Matcher, FileS, Misc) ->
	mydlp_logger:notify(acl_msg,
		"PROTOCOL: ~w , RULE: ~w , ACTION: ~w , FROM: ~s , TO: \"~s\" , MATCHER: ~w , FILE: \"~s\" , MISC: ~s ~n",
		[Proto, RuleId, Action, User, To, Matcher, FileS, Misc]);
acl_msg1(Proto, RuleId, Action, {Ip1,Ip2,Ip3,Ip4}, User, To, Matcher, FileS, Misc) ->
	mydlp_logger:notify(acl_msg,
		"PROTOCOL: ~w , RULE: ~w , ACTION: ~w , FROM: ~w.~w.~w.~w (~s) , TO: \"~s\" , MATCHER: ~w , FILE: \"~s\" , MISC: ~s ~n",
		[Proto, RuleId, Action, Ip1,Ip2,Ip3,Ip4, User, To, Matcher, FileS, Misc]);
acl_msg1(_,_,_,_,_,_,_,_,_) -> ok.



files_to_str(Files) -> files_to_str(Files, []).

files_to_str([File|Files], Returns) -> 
	files_to_str(Files, [file_to_str(File)|Returns]);
files_to_str([], Returns) -> 
	lists:reverse(Returns).

file_to_str(#file{name=undefined, filename=undefined}) -> "data";
file_to_str(#file{name=Name, filename=undefined}) -> Name;
file_to_str(#file{name=undefined, filename=Filename}) -> Filename;
file_to_str(#file{name="extracted file", filename=Filename}) -> "extracted file: " ++ Filename;
file_to_str(#file{filename=Filename}) -> Filename;
file_to_str(_File) -> "data".

%%--------------------------------------------------------------------
%% @doc Returns whether given term has text
%% @end
%%----------------------------------------------------------------------
has_text(#file{is_encrypted=true}) -> false;
has_text(#file{text=undefined}) -> false;
has_text(#file{text=Text}) when is_binary(Text) -> 
	case size(Text) of
		0 -> false;
		_Else -> true
	end;
has_text(#file{text=Text}) when is_list(Text) -> 
	case length(Text) of
		0 -> false;
		_Else -> true
	end;
has_text(_) -> true.

%%--------------------------------------------------------------------
%% @doc Returns hashes of normalized senteces from Text
%% @end
%%----------------------------------------------------------------------
get_nsh(Text) -> 
	Res = mydlp_regex:split_bin(
		sentence,
		Text),
	Res1 = lists:filter(fun(I) -> string:len(I) > 10 end, Res), %%% 10 as string length threshold, shorter strings will be neglacted.
	lists:map(fun(I) -> 
			WL = lists:map(fun(W) -> mydlp_nlp_tr:safe_norm(W) end, mydlp_regex:match_bin(word, I)),
			erlang:phash2(WL) 
		end, Res1).

%%--------------------------------------------------------------------
%% @doc Defines files, detects mimetypes, extracts compressed ones.
%% @end
%%----------------------------------------------------------------------
df_to_files(Files) ->
	Files1 = df_to_files1(Files, []),
	Files2 = comp_to_files(Files1),
	lists:flatten(Files2).

df_to_files1([#file{mime_type=undefined} = File|Files], Returns) -> 
	MT = mydlp_tc:get_mime(File#file.data),
	df_to_files1(Files, [File#file{mime_type=MT}|Returns]);
df_to_files1([File|Files], Returns) -> 
	df_to_files1(Files, [File|Returns]);
df_to_files1([], Returns) -> lists:reverse(Returns).

comp_to_files(Files) -> comp_to_files(Files, []).
comp_to_files([#file{mime_type= <<"application/zip">>, is_encrypted=false} = File|Files], Returns) -> 
	case zip:extract(File#file.data, [memory]) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			comp_to_files(Files, [df_to_files(ExtFiles)|Returns]);
		{error, _ShouldBeLogged} -> 
			comp_to_files(Files, [File#file{is_encrypted=true}|Returns])
	end;
comp_to_files([#file{mime_type= <<"application/x-rar">>, is_encrypted=false} = File|Files], Returns) -> 
	case unrar(File#file.data) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			comp_to_files(Files, [df_to_files(ExtFiles)|Returns]);
		{error, _ShouldBeLogged} -> 
			comp_to_files(Files, [File#file{is_encrypted=true}|Returns])
	end;
comp_to_files([#file{mime_type= <<"application/vnd.oasis.opendocument.text">>}|_] = Files, Returns) -> % Needs refinement for better ODF handling
	try_unzip(Files, Returns);
comp_to_files([#file{mime_type= <<"application/octet-stream">>}|_] = Files, Returns) -> % Needs refinement for better ODF handling
	try_unzip(Files, Returns);
comp_to_files([File|Files], Returns) -> comp_to_files(Files, [File|Returns]);
comp_to_files([], Returns) -> lists:reverse(Returns).

try_unzip([File|Files], Returns) ->
	case zip:extract(File#file.data, [memory]) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			comp_to_files(Files, [df_to_files(ExtFiles)|Returns]);
		{error, _ShouldBeLogged} -> comp_to_files(Files, [File|Returns])
	end.

ext_to_file(Ext) ->
	[#file{name= "extracted file", 
		filename=Filename, 
		data=Data} 
		|| {Filename,Data} <- Ext].

%%--------------------------------------------------------------------
%% @doc Checks whether function returns true more than count for given function
%% @end
%%----------------------------------------------------------------------
more_than_count(Fun, Count, List) -> more_than_count(Fun, Count, List, 0).

more_than_count(_Fun, Count, _List, Count) -> true;
more_than_count(Fun, Count, [I|List], Curr) ->
	case Fun(I) of
		true -> more_than_count(Fun, Count, List, Curr + 1);
		_Else -> more_than_count(Fun, Count, List, Curr)
	end;
more_than_count(_, _, [], _) -> false.

%%-------------------------------------------------------------------------
%% @spec (String::string()) -> {string(),string()}
%% @doc Splits the given string into two strings at the last SPACE (chr(32))
%% @end
%%-------------------------------------------------------------------------
rsplit_at(String) -> rsplit_at(String,32).
%%-------------------------------------------------------------------------
%% @spec (String::string(),Chr::char()) -> {string(),string()}
%% @doc Splits the given string into two strings at the last instace of Chr
%% @end
%%-------------------------------------------------------------------------
rsplit_at(String,Chr) ->
        case string:rchr(String, Chr) of
                0 -> {String,[]};
                Pos ->
                        case lists:split(Pos,String) of
                                {One,Two} -> {string:strip(One),Two};
                                Other -> Other
                        end
        end.

%%-------------------------------------------------------------------------
%% @spec (String::string()) -> string()
%% @doc Removes Double Quotes and white space from both sides of a string
%% @end
%%-------------------------------------------------------------------------
unquote(String) ->
        S2 = string:strip(String,both,32),
        string:strip(S2,both,34).


split_email(Atom) when is_atom(Atom) -> split_email(atom_to_list(Atom));
split_email([]) -> {[],[]};
split_email(EmailAddress) ->
        case string:tokens(string:strip(EmailAddress),[64]) of
                [UserName,DomainName] -> {UserName,DomainName};
                _AnythingsElse -> {[],[]}
        end.


%%% imported from yaws (may be refactored for binary operation)
parse_multipart(HttpContent, H, Req) ->
	CT = H#http_headers.content_type,
	Res = case Req#http_request.method of
		'POST' ->
			case CT of
				undefined ->
					?DEBUG("Can't parse multipart if we "
						"have no Content-Type header",[]), [];
				"multipart/form-data"++Line ->
					LineArgs = parse_arg_line(Line),
					{value, {_, Boundary}} = lists:keysearch(boundary, 1, LineArgs),
					parse_multipart(binary_to_list(un_partial(list_to_binary(HttpContent))), Boundary);
				_Other ->
					?DEBUG("Can't parse multipart if we "
						"find no multipart/form-data",[]), []
			end;
		Other ->
			?DEBUG("Can't parse multipart if get a ~p", [Other]), []
	end,
	result_to_files(Res).

%%%%% multipart parsing
un_partial({partial, Bin}) ->
    Bin;
un_partial(Bin) ->
    Bin.

parse_arg_line(Line) ->
    parse_arg_line(Line, []).

parse_arg_line([],Acc) -> Acc;
parse_arg_line([$ |Line], Acc) ->
    parse_arg_line(Line, Acc);
parse_arg_line([$;|Line], Acc) ->
    {KV,Rest} = parse_arg_key(Line, [], []),
    parse_arg_line(Rest, [KV|Acc]).

%%

parse_arg_key([], Key, Value) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_key([$;|Line], Key, Value) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_key([$ |Line], Key, Value) ->
    parse_arg_key(Line, Key, Value);
parse_arg_key([$=|Line], Key, Value) ->
    parse_arg_value(Line, Key, Value, false, false);
parse_arg_key([C|Line], Key, Value) ->
    parse_arg_key(Line, [C|Key], Value).

%%
%% We need to deal with quotes and initial spaces here.
%% parse_arg_value(String, Key, ValueAcc, InQuoteBool, InValueBool)
%%

parse_arg_value([], Key, Value, _, _) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_value([$\\,$"|Line], Key, Value, Quote, Begun) ->
    parse_arg_value(Line, Key, [$"|Value], Quote, Begun);
parse_arg_value([$"|Line], Key, Value, false, _) ->
    parse_arg_value(Line, Key, Value, true, true);
parse_arg_value([$"], Key, Value, true, _) ->
    make_parse_line_reply(Key, Value, []);
parse_arg_value([$",$;|Line], Key, Value, true, _) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_value([$;|Line], Key, Value, false, _) ->
    make_parse_line_reply(Key, Value, [$;|Line]);
parse_arg_value([$ |Line], Key, Value, false, true) ->
    make_parse_line_reply(Key, Value, Line);
parse_arg_value([$ |Line], Key, Value, false, false) ->
    parse_arg_value(Line, Key, Value, false, false);
parse_arg_value([C|Line], Key, Value, Quote, _) ->
    parse_arg_value(Line, Key, [C|Value], Quote, true).


%%

make_parse_line_reply(Key, Value, Rest) ->
    X = {{list_to_atom(mydlp_api:funreverse(Key, {mydlp_api, to_lowerchar})),
          lists:reverse(Value)}, Rest},
    X.
%%

isolate_arg(Str) -> isolate_arg(Str, []).

isolate_arg([$:,$ |T], L) -> {mydlp_api:funreverse(L, {mydlp_api, to_lowerchar}), T};
isolate_arg([H|T], L)     -> isolate_arg(T, [H|L]).

%%
%%% Stateful parser of multipart data - allows easy re-entry
%% States are header|body|boundary|is_end

parse_multipart(Data, St) ->
    case parse_multi(Data, St) of
        {cont, St2, Res} ->
            {cont, {cont, St2}, lists:reverse(Res)};
        {result, Res} ->
            {result, lists:reverse(Res)}
    end.

%% Re-entry
parse_multi(Data, {cont, {boundary, Start_data, PartBoundary,
                          Acc, {Possible,Boundary}}}) ->
    parse_multi(boundary, Start_data++Data, PartBoundary, Acc, [],
                {Possible++Data,Boundary});
parse_multi(Data, {cont, {State, Start_data, Boundary, Acc, Tmp}}) ->
    parse_multi(State, Start_data++Data, Boundary, Acc, [], Tmp);

%% Initial entry point
parse_multi(Data, Boundary) ->
    B1 = "\r\n--"++Boundary,
    D1 = "\r\n"++Data,
    parse_multi(boundary, D1, B1, start, [], {D1, B1}).

parse_multi(header, "\r\n\r\n"++Body, Boundary, Acc, Res, Tmp) ->
    Header = do_header(lists:reverse(Acc)),
    parse_multi(body, Body, Boundary, [], [{head, Header}|Res], Tmp);
parse_multi(header, "\r\n"++Body, Boundary, [], Res, Tmp) ->
    Header = do_header([]),
    parse_multi(body, Body, Boundary, [], [{head, Header}|Res], Tmp);
parse_multi(header, "\r\n\r", Boundary, Acc, Res, Tmp) ->
    {cont, {header, "\r\n\r", Boundary, Acc, Tmp}, Res};
parse_multi(header, "\r\n", Boundary, Acc, Res, Tmp) ->
    {cont, {header, "\r\n", Boundary, Acc, Tmp}, Res};
parse_multi(header, "\r", Boundary, Acc, Res, Tmp) ->
    {cont, {header, "\r", Boundary, Acc, Tmp}, Res};
parse_multi(header, [H|T], Boundary, Acc, Res, Tmp) ->
    parse_multi(header, T, Boundary, [H|Acc], Res, Tmp);
parse_multi(header, [], Boundary, Acc, Res, Tmp) ->
    {cont, {header, [], Boundary, Acc, Tmp}, Res};

parse_multi(body, [B|T], [B|T1], Acc, Res, _Tmp) ->
    parse_multi(boundary, T, T1, Acc, Res, {[B|T], [B|T1]}); %% store in case no match
parse_multi(body, [H|T], Boundary, Acc, Res, Tmp) ->
    parse_multi(body, T, Boundary, [H|Acc], Res, Tmp);
parse_multi(body, [], Boundary, [], Res, Tmp) ->  %% would be empty partial body result
    {cont, {body, [], Boundary, [], Tmp}, Res};
parse_multi(body, [], Boundary, Acc, Res, Tmp) ->        %% make a partial body result
    {cont, {body, [], Boundary, [], Tmp}, [{part_body, lists:reverse(Acc)}|Res]};

parse_multi(boundary, [B|T], [B|T1], Acc, Res, Tmp) ->
    parse_multi(boundary, T, T1, Acc, Res, Tmp);
parse_multi(boundary, [_H|_T], [_B|_T1], start, Res, {[D|T2], Bound}) -> %% false alarm
    parse_multi(body, T2, Bound, [D], Res, []);
parse_multi(boundary, [_H|_T], [_B|_T1], Acc, Res, {[D|T2], Bound}) -> %% false alarm
    parse_multi(body, T2, Bound, [D|Acc], Res, []);
parse_multi(boundary, [], [B|T1], Acc, Res, Tmp) -> %% run out of body
    {cont, {boundary, [], [B|T1], Acc, Tmp}, Res};
parse_multi(boundary, [], [], start, Res, {_, Bound}) ->
    {cont, {is_end, [], Bound, [], []}, Res};
parse_multi(boundary, [], [], Acc, Res, {_, Bound}) ->
    {cont, {is_end, [], Bound, [], []}, [{body, lists:reverse(Acc)}|Res]};
parse_multi(boundary, [H|T], [], start, Res, {_, Bound}) -> %% matched whole boundary!
    parse_multi(is_end, [H|T], Bound, [], Res, []);
parse_multi(boundary, [H|T], [], Acc, Res, {_, Bound}) -> %% matched whole boundary!
    parse_multi(is_end, [H|T], Bound, [], [{body, lists:reverse(Acc)}|Res], []);

parse_multi(is_end, "--"++_, _Boundary, _Acc, Res, _Tmp) ->
    {result, Res};
parse_multi(is_end, "-", Boundary, Acc, Res, Tmp) ->
    {cont, {is_end, "-", Boundary, Acc, Tmp}, Res};
parse_multi(is_end, "\r\n"++Next, Boundary, _Acc, Res, _Tmp) ->
    parse_multi(header, Next, Boundary, [], Res, []);
parse_multi(is_end, "\r", Boundary, Acc, Res, Tmp) ->
    {cont, {is_end, "\r", Boundary, Acc, Tmp}, Res}.

do_header([]) -> {[]};
do_header(Head) ->
    Fields = string:tokens(Head, "\r\n"),
    MFields = merge_lines_822(Fields),
    Header = lists:map(fun isolate_arg/1, MFields),
    case lists:keysearch("content-disposition", 1, Header) of
        {value, {_,"form-data"++Line}} ->
            Parameters = parse_arg_line(Line),
            {value, {_,Name}} = lists:keysearch(name, 1, Parameters),
            {Name, Parameters};
        _ ->
            {Header}
    end.

merge_lines_822(Lines) ->
    merge_lines_822(Lines, []).

merge_lines_822([], Acc) ->
    lists:reverse(Acc);
merge_lines_822([Line=" "++_|Lines], []) ->
    merge_lines_822(Lines, [Line]);
merge_lines_822([Line=" "++_|Lines], [Prev|Acc]) ->
    merge_lines_822(Lines, [Prev++Line|Acc]);
merge_lines_822(["\t"++Line|Lines], [Prev|Acc]) ->
    merge_lines_822(Lines, [Prev++[$ |Line]|Acc]);
merge_lines_822([Line|Lines], Acc) ->
    merge_lines_822(Lines, [Line|Acc]).

%%%% convert yaws multipart result to file records
result_to_files({result, Rest}) ->
	result_to_files(Rest, [], undefined).
result_to_files([], Files, File) ->
	lists:reverse([File|Files]);
result_to_files([{head,Head}|Rest], Files, File) ->
	Files1 = case File of
		undefined -> Files;
		Else -> [Else|Files]
	end,
	{_, Heads} = Head,
	result_to_files(Rest, Files1, heads_to_file(Heads));
result_to_files([{body,Data}|Rest], Files, File) ->
	result_to_files(Rest, Files, File#file{data=list_to_binary(Data)}).

heads_to_file(Heads) ->
	heads_to_file(Heads, #file{}).
heads_to_file([{filename,FileName}|Heads], File) ->
	heads_to_file(Heads, File#file{filename=FileName});
heads_to_file([{name,PostName}|Heads], File) ->
	heads_to_file(Heads, File#file{name=PostName});
heads_to_file([_|Heads], File) ->
	ignore,
	heads_to_file(Heads, File);
heads_to_file([], File) ->
	File.

%%-------------------------------------------------------------------------
%% @doc Writes files to quarantine directory.
%% @end
%%-------------------------------------------------------------------------
quarantine(#file{} = File) ->
	FN = case File#file.filename of
		"" -> integer_to_list(erlang:phash2(File#file.data));
		undefined -> integer_to_list(erlang:phash2(File#file.data));
		Else -> Else end,
	Path = ?QUARANTINE_DIR ++ FN,
	file:write_file(Path, File#file.data, [raw]), ok;
quarantine([File|Files]) ->
	quarantine(File),
	quarantine(Files);
quarantine([]) -> ok.

%%-------------------------------------------------------------------------
%% @doc Return denied page for different formats
%% @end
%%-------------------------------------------------------------------------

get_denied_page(html) -> mydlp_denied_page:get();
get_denied_page(html_base64_str) -> mydlp_denied_page:get_base64_str().

%%-------------------------------------------------------------------------
%% @doc Inserts line feed for long lines
%% @end
%%-------------------------------------------------------------------------

insert_line_feed(List) when is_list(List) -> insert_line_feed(list_to_binary(List));
insert_line_feed(Bin) when is_binary(Bin) -> insert_line_feed_76(Bin).

insert_line_feed_76(Bin) when is_binary(Bin) -> insert_line_feed_76(Bin, <<>>).

insert_line_feed_76(<<Line:76/binary, Rest/binary>>, Acc) -> 
	insert_line_feed_76(Rest, <<Acc/binary, Line/binary, "\r\n">>);
insert_line_feed_76(ShortLine, Acc) -> <<Acc/binary, ShortLine/binary>>.

