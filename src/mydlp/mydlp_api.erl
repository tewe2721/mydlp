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
-include("mydlp_smtp.hrl").
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

is_all_digit(Str) when is_list(Str) -> lists:all(fun(C) -> is_digit(C) end, Str).

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

hex2bytelist(Str) when is_binary(Str) -> hex2bytelist(binary_to_list(Str));
hex2bytelist(Str) when is_list(Str) -> hex2bytelist(Str, <<>>).

hex2bytelist([C1,C0|Rest], Bin) -> I = hex2int([C1,C0]), hex2bytelist(Rest, <<Bin/binary, I/integer>>);
hex2bytelist([C0|Rest], Bin) -> I = hex2int([C0]), hex2bytelist(Rest, <<Bin/binary, I/integer >> );
hex2bytelist([], Bin) -> Bin.

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
-define(DOC, {"/usr/bin/catdoc", ["-wx", "-dutf-8"]}).
-define(PPT, {"/usr/bin/catppt", []}).
-define(XLS, {"/usr/bin/xls2csv", ["-x"]}).

office_to_text(#file{filename = Filename, data = Data}) ->
	StrLen = case is_list(Filename) of
		true -> string:len(Filename);
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
get_text(#file{is_encrypted=true}) -> {error, encrypted};
get_text(#file{compressed_copy=true}) -> {error, compression};
get_text(#file{mime_type= <<"application/x-empty">>}) -> {ok, <<>>};
get_text(#file{mime_type= <<"text/plain">>, data=Data}) -> {ok, Data};
get_text(#file{mime_type= <<"application/xml">>, data=Data}) ->
	try	Text = xml_to_txt(Data),
		{ok, Text}
	catch _:E -> {error, E} end;
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
	try	Text = mydlp_tc:html_to_text(Data),
		{ok, Text}
	catch _:E -> {error, E} end;
get_text(#file{mime_type= <<"application/postscript">>, data=Data}) ->
	ps_to_text(Data);
get_text(#file{mime_type= <<"text/",_Rest/binary>>, data=Data}) -> {ok, Data};
get_text(#file{mime_type=undefined}) -> {error, unknown_type};
get_text(#file{mime_type=MimeType}) -> 
	MiscMimeCate = mime_category(MimeType),
	{error, MiscMimeCate}.

%%--------------------------------------------------------------------
%% @doc Extracts Text from XML string
%% @end
%%----------------------------------------------------------------------
xml_to_txt(Data) when is_binary(Data) -> 
	X = case size(Data) > (?MAX_MEM_OBJ/4) of
		true ->	{ok, XmlF} = mktempfile(),
			ok = file:write_file(XmlF, Data, [raw]),
			X1 = xmerl_scan:file(XmlF),
			ok = file:delete(XmlF), X1;
		false -> XmlS = binary_to_list(Data),
			xmerl_scan:string(XmlS) end,

	RetList = xml_to_txt1(X),
	unicode:characters_to_binary(RetList).

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
	is_valid_trid1(Clean).

is_valid_trid1("00000000000") -> false;
is_valid_trid1(TrIdStr) ->
	[I0,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10] = 
		lists:map(fun(I) -> I - $0 end, TrIdStr),
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

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid Canada SIN Number
%% @end
%%----------------------------------------------------------------------
is_valid_sin(SINStr) ->
	Clean = remove_chars(SINStr, " -"),
	is_valid_sin1(Clean).

is_valid_sin1("000000000") -> false;
is_valid_sin1(SINStr) -> check_luhn(SINStr).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid France INSEE code
%% @end
%%----------------------------------------------------------------------
is_valid_insee(INSEEStr) ->
	Clean = remove_chars(INSEEStr, " -"),
	case string:len(Clean) of 
		15 ->
			SexN = list_to_integer(string:substr(Clean,1,1)),
			YearN = list_to_integer(string:substr(Clean,2,2)),
			MonthN = list_to_integer(string:substr(Clean,4,2)),
			BirthPlaceN = list_to_integer(string:substr(Clean,6,2)),
			RestN = list_to_integer(string:substr(Clean,8,6)),
			ControlN = list_to_integer(string:substr(Clean,14,2)),
			is_valid_insee1(SexN, YearN, MonthN, BirthPlaceN, RestN, ControlN);
		13 ->
			SexN = list_to_integer(string:substr(Clean,1,1)),
			YearN = list_to_integer(string:substr(Clean,2,2)),
			MonthN = list_to_integer(string:substr(Clean,4,2)),
			BirthPlaceN = list_to_integer(string:substr(Clean,6,2)),
			RestN = list_to_integer(string:substr(Clean,8,6)),
			is_valid_insee1(SexN, YearN, MonthN, BirthPlaceN, RestN);
		_Else -> false
	end.

is_valid_insee1(SexN, YearN, MonthN, BirthPlaceN, RestN) ->
	is_valid_insee1(SexN, YearN, MonthN, BirthPlaceN, RestN, none).

is_valid_insee1(SexN, YearN, MonthN, BirthPlaceN, RestN, ControlN) 
		when SexN == 1; SexN == 2 -> 
	is_valid_insee2(SexN, YearN, MonthN, BirthPlaceN, RestN, ControlN);
is_valid_insee1(_,_,_,_,_,_) -> false.

is_valid_insee2(SexN, YearN, MonthN, BirthPlaceN, RestN, ControlN) 
		when 0 < MonthN, MonthN < 13  -> 
	is_valid_insee3(SexN, YearN, MonthN, BirthPlaceN, RestN, ControlN);
is_valid_insee2(_,_,_,_,_,_) -> false.

is_valid_insee3(SexN, YearN, MonthN, BirthPlaceN, RestN, ControlN) 
		when 0 < BirthPlaceN, BirthPlaceN < 100  -> 
	is_valid_inseeF(SexN, YearN, MonthN, BirthPlaceN, RestN, ControlN);
is_valid_insee3(_,_,_,_,_,_) -> false.

is_valid_inseeF(_,_,_,_,_, none) -> true;
is_valid_inseeF(SexN, YearN, MonthN, BirthPlaceN, RestN, ControlN) ->
	NumToControl =  
		1000000000000*SexN +
		10000000000*YearN +
		100000000*MonthN + 
		1000000*BirthPlaceN + 
		RestN,
	ControlN == 97 - (NumToControl rem 97).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid UK NINO number
%% @end
%%----------------------------------------------------------------------
is_valid_nino(SINStr) ->
	Clean = remove_chars(SINStr, " -"),
	Clean1 = string:to_lower(Clean),
	case string:len(Clean1) of
		9 -> is_valid_nino1(Clean1);
		8 -> is_valid_nino1(Clean1);
		_Else -> false end.

is_valid_nino1([$d |_]) -> false;
is_valid_nino1([$f |_]) -> false;
is_valid_nino1([$i |_]) -> false;
is_valid_nino1([$q |_]) -> false;
is_valid_nino1([$u |_]) -> false;
is_valid_nino1([$v |_]) -> false;
is_valid_nino1([_, $d |_]) -> false;
is_valid_nino1([_, $f |_]) -> false;
is_valid_nino1([_, $i |_]) -> false;
is_valid_nino1([_, $o |_]) -> false;
is_valid_nino1([_, $q |_]) -> false;
is_valid_nino1([_, $u |_]) -> false;
is_valid_nino1([_, $v |_]) -> false;
is_valid_nino1([$g, $b |_]) -> false;
is_valid_nino1([$n, $k |_]) -> false;
is_valid_nino1([$t, $n |_]) -> false;
is_valid_nino1([$z, $z |_]) -> false;

is_valid_nino1([_, _ |Rest]) ->
	case string:len(Rest) of
		6 -> is_all_digit(Rest);
		7 -> 	DigitPart = string:substr(Rest,1,6),
			LastChar = lists:last(Rest),
			is_all_digit(DigitPart) and 
				( LastChar >= $a ) and
				( $d >= LastChar );
		_Else -> false end.

%%% imported from tsuraan tempfile module http://www.erlang.org/cgi-bin/ezmlm-cgi/4/41649

%%--------------------------------------------------------------------
%% @doc Creates safe temporary files.
%% @end
%%----------------------------------------------------------------------

tempfile() -> mydlp_workdir:tempfile().

tempdir() -> tempfile().
	
mktempfile() -> 
	{ok, FN} = tempfile(),
	case file:write_file(FN, <<>>, [raw]) of
		ok -> {ok, FN};
		Err -> throw(Err) end.

mktempdir() ->
	{ok, DN} = tempdir(),
	case file:make_dir(DN) of
		ok -> {ok, DN};
		Err -> throw(Err) end.

%%--------------------------------------------------------------------
%% @doc Un7zs an Erlang binary, this can be used for ZIP, CAB, ARJ, GZIP, BZIP2, TAR, CPIO, RPM and DEB formats.
%% @end
%%----------------------------------------------------------------------

un7z(Arg) -> un7z(Arg, "nofilename").

un7z0(Filename) ->
	{ok, ZFNDir} = mktempdir(),
	ZFN = ZFNDir ++ "/" ++ normalize_fn(Filename),
	{ZFNDir, ZFN}.

un7z({memory, Bin}, Filename) -> un7z(Bin, Filename);
un7z({Type, _Value} = Ref, Filename) when
		Type == cacheref; Type == unixfile -> 
	{ZFNDir, ZFN} = un7z0(Filename),
	ok = file:make_symlink(?BB_P(Ref), ZFN),
	un7z1(ZFNDir, ZFN);

un7z(Bin, Filename) when is_binary(Bin) -> 
	{ZFNDir, ZFN} = un7z0(Filename),
	ok = file:write_file(ZFN, Bin, [raw]),
	un7z1(ZFNDir, ZFN).

un7z1(ZFNDir, ZFN) -> 
	{ok, WorkDir} = mktempdir(),
	WorkDir1 = WorkDir ++ "/",
	Port = open_port({spawn_executable, "/usr/bin/7z"}, 
			[{args, ["-p","-y","-bd","-o" ++ WorkDir1,"x",ZFN]},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	Ret = case get_port_resp(Port) of
		ok -> {ok, rr_files(WorkDir1)};
		Else -> rmrf_dir(WorkDir1), Else end,
	ok = rmrf_dir(ZFNDir),
	Ret.

%%--------------------------------------------------------------------
%% @doc Removes files in Dir. 
%% @end
%%----------------------------------------------------------------------
rmrf_dir(Dir) when is_list(Dir) ->
	Dir1 = case lists:last(Dir) of
		$/ -> Dir;
		_Else -> Dir ++ "/" end,
	{ok, FileNames} = file:list_dir(Dir1),
	rmrf_dir(FileNames, Dir1),
	file:del_dir(Dir1), ok.

rmrf_dir([FN|FNs], Dir) -> 
	AbsPath = Dir ++ FN,
	case filelib:is_dir(AbsPath) of
		true -> rmrf_dir(AbsPath);
		false -> catch file:delete(AbsPath),
			rmrf_dir(FNs, Dir) end;
rmrf_dir([], _Dir) -> ok.

%%--------------------------------------------------------------------
%% @doc Reads and removes files in WorkDir. Files will be returned as binaries.
%% @end
%%----------------------------------------------------------------------

rr_files(WorkDir) when is_list(WorkDir) ->
	WorkDir1 = case lists:last(WorkDir) of
		$/ -> WorkDir;
		_Else -> WorkDir ++ "/" end,
	{ok, FileNames} = file:list_dir(WorkDir1),
	Return = rr_files(FileNames, WorkDir1, []),
	file:del_dir(WorkDir1),
	Return.

rr_files([FN|FNs], WorkDir, Ret) -> 
	AbsPath = WorkDir ++ FN,
	case file:read_link(AbsPath) of
		{ok, _Filename} ->	ok = file:delete(AbsPath),
					rr_files(FNs, WorkDir, Ret);
		{error, _Reason} -> case filelib:is_regular(AbsPath) of
			true -> CacheRef = ?BB_C({unixfile, AbsPath}),
				file:delete(AbsPath), %ensure file deletion
				rr_files(FNs, WorkDir, [{FN, CacheRef}|Ret]);
			false -> case filelib:is_dir(AbsPath) of
				true -> Ret1 = rr_files(AbsPath),
					rr_files(FNs, WorkDir, [Ret1|Ret]);
				false -> catch file:delete(AbsPath),
					rr_files(FNs, WorkDir, Ret) end end end;
rr_files([], _WorkDir, Ret) -> lists:flatten(lists:reverse(Ret)).

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
		{log, {cc,false} } -> mydlp_mysql:push_log(Proto, RuleId, Action, Ip, User, To, Matcher, FileS, Misc);
		{block, {cc,false} } -> mydlp_mysql:push_log(Proto, RuleId, Action, Ip, User, To, Matcher, FileS, Misc);
		{log, {cc,true} } -> mydlp_mysql:push_log(Proto, RuleId, Action, Ip, User, To, Matcher, FileS, File, Misc);
		{block, {cc,true} } -> mydlp_mysql:push_log(Proto, RuleId, Action, Ip, User, To, Matcher, FileS, File, Misc);
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

file_to_str(File) -> normalize_fn(file_to_str1(File)).

file_to_str1(#file{name=undefined, filename=undefined}) -> "data";
file_to_str1(#file{name=Name, filename=undefined}) -> Name;
file_to_str1(#file{name=undefined, filename=Filename}) -> Filename;
file_to_str1(#file{name="extracted file", filename=Filename}) -> "extracted file: " ++ Filename;
file_to_str1(#file{filename=Filename}) -> Filename;
file_to_str1(_File) -> "data".

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
%% @doc Analyzes structure of files. Creates new files if necessary.
%% @end
%%----------------------------------------------------------------------
analyze(#file{} = File) -> analyze([File]);
analyze(Files) when is_list(Files) -> 
	Files1 = get_mimes(Files),
	comp_to_files(Files1).

%%--------------------------------------------------------------------
%% @doc Loads data properties of files from references.
%% @end
%%----------------------------------------------------------------------
load_files(#file{} = File) -> [Ret] = load_files([File]), Ret;
load_files(Files) when is_list(Files) ->
	lists:map(fun(F) -> load_file(F) end, Files).

load_file(#file{dataref=undefined} = File) -> File;
load_file(#file{data=undefined} = File) ->
	Data = ?BB_R(File#file.dataref),
	File#file{data=Data};
load_file(#file{} = File) -> File.

%%--------------------------------------------------------------------
%% @doc Cleans cache references.
%% @end
%%----------------------------------------------------------------------
clean_files(#file{} = File) -> [Ret] = clean_files([File]), Ret;
clean_files(Files) when is_list(Files) ->
	lists:map(fun(F) -> clean_file(F) end, Files).

clean_file(#file{dataref=undefined} = File) -> File;
clean_file(#file{} = File) -> 
	?BB_D(File#file.dataref), % no need for reference to exist
	File#file{dataref=undefined}.

%%--------------------------------------------------------------------
%% @doc Detects mimetypes of all files given, 
%% @end
%%----------------------------------------------------------------------
get_mimes(Files) -> get_mimes(Files, []).

get_mimes([#file{mime_type=undefined} = File|Files], Returns) -> 
	MT = mydlp_tc:get_mime(File#file.data),
	get_mimes(Files, [File#file{mime_type=MT}|Returns]);
get_mimes([File|Files], Returns) -> 
	get_mimes(Files, [File|Returns]);
get_mimes([], Returns) -> lists:reverse(Returns).

%%--------------------------------------------------------------------
%% @doc Extract all compressed files given
%% @end
%%----------------------------------------------------------------------
comp_to_files(Files) -> comp_to_files(Files, [], []).

ctf_ok(Files, File, ExtFiles, Processed, New) -> 
	comp_to_files(Files, [File#file{compressed_copy=true}|Processed], [ExtFiles|New]).
	%comp_to_files(Files, [File#file{compressed_copy=true}, df_to_files(ExtFiles)|Returns]).

ctf_err_enc(Files, File, Processed, New) -> 
	comp_to_files(Files, [File#file{is_encrypted=true}|Processed], New).

comp_to_files([#file{mime_type= <<"application/zip">>, compressed_copy=false, is_encrypted=false} = File|Files], Processed, New) -> 
	case zip:extract(File#file.data, [memory]) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			ctf_ok(Files, File, ExtFiles, Processed, New);
		{error, _ShouldBeLogged} -> 
			ctf_err_enc(Files, File, Processed, New) end;
comp_to_files([#file{mime_type= <<"application/vnd.oasis.opendocument.text">>}|_] = Files, Processed, New) -> % Needs refinement for better ODF handling
	try_unzip(Files, Processed, New);
comp_to_files([#file{mime_type= <<"application/octet-stream">>}|_] = Files, Processed, New) -> % Needs refinement for better ODF handling
	%try_unzip(Files, Returns);
	try_un7z(Files, Processed, New);
comp_to_files([#file{mime_type= MimeType, compressed_copy=false, is_encrypted=false} = File | Rest ] = Files, Processed, New) -> 
		case is_compression_mime(MimeType) of
			true -> use_un7z(Files, Processed, New);
			false -> comp_to_files(Rest, [File|Processed], New) end;
comp_to_files([File|Files], Processed, New) -> comp_to_files(Files, [File|Processed], New);
comp_to_files([], Processed, New) -> 
	{ lists:reverse(Processed), 
	  lists:flatten(lists:reverse(New)) }.

use_un7z([File|Files], Processed, New) ->
	case un7z(File#file.dataref, File#file.filename) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			ctf_ok(Files, File, ExtFiles, Processed, New);
		{error, _ShouldBeLogged} -> 
			ctf_err_enc(Files, File, Processed, New) end.

try_unzip([File|Files], Processed, New) ->
	case zip:extract(File#file.data, [memory]) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			ctf_ok(Files, File, ExtFiles, Processed, New);
		{error, _ShouldBeLogged} -> comp_to_files(Files, [File|Processed], New) end.

try_un7z([File|Files], Processed, New) ->
	case un7z(File#file.dataref, File#file.filename) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			ctf_ok(Files, File, ExtFiles, Processed, New);
		{error, _ShouldBeLogged} -> comp_to_files(Files, [File|Processed], New) end.

ext_to_file(Ext) ->
	[#file{name= "extracted file", 
		filename=Filename, 
		dataref=?BB_C(Data)} 
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
					MIME = #mime{body_text=HttpContent},
					mime_util:decode_multipart(MIME, Boundary);
				_Other ->
					?DEBUG("Can't parse multipart if we "
						"find no multipart/form-data",[]), []
			end;
		Other ->
			?DEBUG("Can't parse multipart if get a ~p", [Other]), []
	end,
	mime_to_files(Res).

%%%%% multipart parsing
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

make_parse_line_reply(Key, Value, Rest) ->
    X = {{list_to_atom(mydlp_api:funreverse(Key, {mydlp_api, to_lowerchar})),
          lists:reverse(Value)}, Rest},
    X.

%%-------------------------------------------------------------------------
%% @doc Writes files to quarantine directory.
%% @end
%%-------------------------------------------------------------------------
quarantine(#file{data=Data}) -> mydlp_quarantine:s(Data).

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

%%-------------------------------------------------------------------------
%% @doc Converts url encoded data to file
%% @end
%%-------------------------------------------------------------------------

uenc_to_file(Bin) ->
	case parse_uenc_data(Bin) of
		none -> [];
		{ok, RData} when is_list(RData) -> 
			[ #file{name="urlencoded-data",
			dataref=?BB_C(RData)} ] end.

parse_uenc_data(Bin) when is_list(Bin) -> parse_uenc_data(list_to_binary(Bin));
parse_uenc_data(Bin) when is_binary(Bin) ->
	do_parse_spec(Bin, []).

do_parse_spec(<<$%, $%, Tail/binary>>, Cur) -> do_parse_spec(<<$% , Tail/binary>>, Cur);

do_parse_spec(<<$%, Hi:8, Lo:8, Tail/binary>>, Cur) when Hi /= $u ->
	Hex = try hex2int([Hi, Lo]) catch _:_ -> $\s end,
	do_parse_spec(Tail, [ Hex | Cur]);
               
do_parse_spec(<<$&, Tail/binary>>, Cur) -> do_parse_spec(Tail, [ $\n | Cur]);
do_parse_spec(<<$+, Tail/binary>>, Cur) -> do_parse_spec(Tail, [ $\s | Cur]);
do_parse_spec(<<$=, Tail/binary>>, Cur) -> do_parse_spec(Tail, [ <<": ">> | Cur]);

do_parse_spec(<<$%, $u, A:8, B:8,C:8,D:8, Tail/binary>>, Cur) ->
	%% non-standard encoding for Unicode characters: %uxxxx,		     
	Hex = try hex2int([A,B,C,D]) catch _:_ -> $\s end,
	BinRep = case unicode:characters_to_binary([Hex]) of
		<<Bin/binary>> -> Bin;
		_Else -> $_ end,
	do_parse_spec(Tail, [ BinRep | Cur]);

do_parse_spec(<<H:8, Tail/binary>>, Cur) -> do_parse_spec(Tail, [H|Cur]);
do_parse_spec(<<>>, Cur) -> {ok, lists:reverse(Cur)};
do_parse_spec(undefined,_) -> none.

parse_uenc_data1(D) ->
	case parse_uenc_data(D) of 
		none -> []; 
		{ok, R} -> R end.

uri_to_hr_str(Uri) when is_binary(Uri) -> uri_to_hr_str(binary_to_list(Uri));
uri_to_hr_str(Uri) when is_list(Uri) ->
	{_Path, QueryStr} = httpd_util:split_path(Uri),
	case QueryStr of
		[] -> [];
		_Else -> Tokens = string:tokens(QueryStr, "?=;&"),
			Cleans = lists:map(fun(I) -> parse_uenc_data1(I) end, Tokens),
			string:join(Cleans, " ") end.

uri_to_hr_file(Uri) ->
	RData = uri_to_hr_str(Uri),
	case RData of
		[] -> none;
		_Else -> #file{name="uri-data", dataref=?BB_C(RData)} end.
	

%%-------------------------------------------------------------------------
%% @doc Escapes regex special chars
%% @end
%%-------------------------------------------------------------------------
escape_regex(Str) -> escape_regex(Str, []).

escape_regex([$\\ |Str], Acc) -> escape_regex(Str, [$\\ ,$\\ |Acc]);
escape_regex([$^ |Str], Acc) -> escape_regex(Str, [$^, $\\ |Acc]);
escape_regex([$$ |Str], Acc) -> escape_regex(Str, [$$ ,$\\ |Acc]);
escape_regex([$. |Str], Acc) -> escape_regex(Str, [$., $\\ |Acc]);
escape_regex([$[ |Str], Acc) -> escape_regex(Str, [$[, $\\ |Acc]);
escape_regex([$| |Str], Acc) -> escape_regex(Str, [$|, $\\ |Acc]);
escape_regex([$( |Str], Acc) -> escape_regex(Str, [$(, $\\ |Acc]);
escape_regex([$) |Str], Acc) -> escape_regex(Str, [$), $\\ |Acc]);
escape_regex([$? |Str], Acc) -> escape_regex(Str, [$?, $\\ |Acc]);
escape_regex([$* |Str], Acc) -> escape_regex(Str, [$*, $\\ |Acc]);
escape_regex([$+ |Str], Acc) -> escape_regex(Str, [$+, $\\ |Acc]);
escape_regex([${ |Str], Acc) -> escape_regex(Str, [${, $\\ |Acc]);
escape_regex([C|Str], Acc) -> escape_regex(Str, [C|Acc]);
escape_regex([], Acc) -> lists:reverse(Acc).

%%-------------------------------------------------------------------------
%% @doc Normalizes filenames.
%% @end
%%-------------------------------------------------------------------------
normalize_fn([_|_] = FN) -> normalize_fn(FN, []);
normalize_fn(FN) when is_binary(FN) -> normalize_fn(binary_to_list(FN));
normalize_fn(FN) when is_integer(FN) -> normalize_fn(integer_to_list(FN));
normalize_fn(FN) when is_atom(FN) -> normalize_fn(atom_to_list(FN));
normalize_fn(_FN) -> "nofilename".

normalize_fn([C|FN], Acc) ->
	case is_uuri_char(C) of
		true -> normalize_fn(FN, [C|Acc]);
		false -> normalize_fn(FN, [$_|Acc]) end;
normalize_fn([], Acc) -> lists:reverse(Acc).

%%-------------------------------------------------------------------------
%% @doc Returns category of misc given mime type.
%% @end
%%-------------------------------------------------------------------------
mime_category(<<"application/zip">>) -> compression;
mime_category(<<"application/x-rar">>) -> compression;
mime_category(<<"application/x-tar">>) -> compression;
mime_category(<<"application/x-gzip">>) -> compression;
mime_category(<<"application/x-bzip2">>) -> compression;
mime_category(<<"application/x-gtar">>) -> compression;
mime_category(<<"application/x-archive">>) -> compression;
mime_category(<<"application/x-rpm">>) -> compression;
mime_category(<<"application/x-arj">>) -> compression;
mime_category(<<"application/arj">>) -> compression;
mime_category(<<"application/x-7z-compressed">>) -> compression;
mime_category(<<"application/x-7z">>) -> compression;
mime_category(<<"application/x-compress">>) -> compression;
mime_category(<<"application/x-compressed">>) -> compression;
mime_category(<<"application/x-iso9660-image">>) -> compression;
mime_category(<<"application/x-lzop">>) -> compression;
mime_category(<<"application/x-lzip">>) -> compression;
mime_category(<<"application/x-lzma">>) -> compression;
mime_category(<<"application/x-xz">>) -> compression;
mime_category(<<"application/x-winzip">>) -> compression;
mime_category(<<"application/vnd.ms-cab-compressed">>) -> compression;
mime_category(<<"application/x-executable">>) -> cobject;
mime_category(<<"application/x-sharedlib">>) -> cobject;
mime_category(<<"application/x-object">>) -> cobject;
mime_category(<<"application/java-vm">>) -> binary_format;
mime_category(<<"image/cgm">>) -> image;
mime_category(<<"image/g3fax">>) -> image;
mime_category(<<"image/gif">>) -> image;
mime_category(<<"image/ief">>) -> image;
mime_category(<<"image/jpeg">>) -> image;
mime_category(<<"image/naplps">>) -> image;
mime_category(<<"image/pcx">>) -> image;
mime_category(<<"image/png">>) -> image;
mime_category(<<"image/prs.btif">>) -> image;
mime_category(<<"image/prs.pti">>) -> image;
mime_category(<<"image/svg+xml">>) -> image;
mime_category(<<"image/tiff">>) -> image;
mime_category(<<"image/vnd.cns.inf2">>) -> image;
mime_category(<<"image/vnd.djvu">>) -> image;
mime_category(<<"image/vnd.dwg">>) -> image;
mime_category(<<"image/vnd.dxf">>) -> image;
mime_category(<<"image/vnd.fastbidsheet">>) -> image;
mime_category(<<"image/vnd.fpx">>) -> image;
mime_category(<<"image/vnd.fst">>) -> image;
mime_category(<<"image/vnd.fujixerox.edmics-mmr">>) -> image;
mime_category(<<"image/vnd.fujixerox.edmics-rlc">>) -> image;
mime_category(<<"image/vnd.mix">>) -> image;
mime_category(<<"image/vnd.net-fpx">>) -> image;
mime_category(<<"image/vnd.svf">>) -> image;
mime_category(<<"image/vnd.wap.wbmp">>) -> image;
mime_category(<<"image/vnd.xiff">>) -> image;
mime_category(<<"image/x-canon-cr2">>) -> image;
mime_category(<<"image/x-canon-crw">>) -> image;
mime_category(<<"image/x-cmu-raster">>) -> image;
mime_category(<<"image/x-coreldraw">>) -> image;
mime_category(<<"image/x-coreldrawpattern">>) -> image;
mime_category(<<"image/x-coreldrawtemplate">>) -> image;
mime_category(<<"image/x-corelphotopaint	">>) -> image;
mime_category(<<"image/x-epson-erf">>) -> image;
mime_category(<<"image/x-icon">>) -> image;
mime_category(<<"image/x-jg">>) -> image;
mime_category(<<"image/x-jng">>) -> image;
mime_category(<<"image/x-ms-bmp">>) -> image;
mime_category(<<"image/x-nikon-nef">>) -> image;
mime_category(<<"image/x-olympus-orf">>) -> image;
mime_category(<<"image/x-photoshop">>) -> image;
mime_category(<<"image/x-portable-anymap">>) -> image;
mime_category(<<"image/x-portable-bitmap">>) -> image;
mime_category(<<"image/x-portable-graymap">>) -> image;
mime_category(<<"image/x-portable-pixmap">>) -> image;
mime_category(<<"image/x-rgb">>) -> image;
mime_category(<<"image/x-xbitmap">>) -> image;
mime_category(<<"image/x-xpixmap">>) -> image;
mime_category(<<"image/x-xwindowdump">>) -> image;
mime_category(_Else) -> unsupported_type.

%%-------------------------------------------------------------------------
%% @doc Returns whether given mime type belongs to a compression format or not.
%% @end
%%-------------------------------------------------------------------------
is_compression_mime(MimeType) -> 
	case mime_category(MimeType) of
		compression -> true;
		_Else -> false end.

%%-------------------------------------------------------------------------
%% @doc Returns whether given mime type belongs to a C/C++ object or not.
%% @end
%%-------------------------------------------------------------------------
is_cobject_mime(MimeType) -> 
	case mime_category(MimeType) of
		cobject -> true;
		_Else -> false end.

%%-------------------------------------------------------------------------
%% @doc Calculates binary size
%% @end
%%-------------------------------------------------------------------------

binary_size(Obj) when is_binary(Obj) -> size(Obj);
binary_size(Obj) when is_list(Obj) ->
	L1 = lists:map(fun(I) -> binary_size(I) end, Obj),
	lists:sum(L1);
binary_size(Obj) when is_integer(Obj), 0 =< Obj, Obj =< 255 -> 1;
binary_size(Obj) when is_integer(Obj) -> throw({error, bad_integer, Obj});
binary_size(Obj) when is_atom(Obj) -> throw({error, bad_element});
binary_size(Obj) when is_tuple(Obj) -> throw({error, bad_element});
binary_size(_Obj) -> throw({error, bad_element}).

%%-------------------------------------------------------------------------
%% @doc Converts mime objects to files
%% @end
%%-------------------------------------------------------------------------

heads_to_file(Headers) -> heads_to_file(Headers, #file{}).

heads_to_file([{'content-disposition', "inline"}|Rest], #file{filename=undefined, name=undefined} = File) ->
	case lists:keysearch('content-type',1,Rest) of
		{value,{'content-type',"text/plain"}} -> heads_to_file(Rest, File#file{name="Inline text message"});
		{value,{'content-type',"text/html"}} -> heads_to_file(Rest, File#file{name="Inline HTML message"});
		_Else -> heads_to_file(Rest, File)
	end;
heads_to_file([{'content-disposition', CD}|Rest], #file{filename=undefined} = File) ->
	case string:str(CD, "filename=") of
		0 -> heads_to_file(Rest, File);
		I when is_integer(I) -> 
			FN = case string:strip(string:substr(CD, I + 9)) of
				"\\\"" ++ Str -> 
					Len = string:len(Str),
					"\"\\" = string:substr(Str, Len - 1),
					string:substr(Str, 1, Len - 2);
				"\"" ++ Str -> 
					Len = string:len(Str),
					"\"" = string:substr(Str, Len),
					string:substr(Str, 1, Len - 1);
				Str -> Str
			end,
			heads_to_file(Rest, File#file{filename=FN})
	end;
heads_to_file([{'content-type', "text/html"}|Rest], #file{filename=undefined, name=undefined} = File) ->
	case lists:keysearch('content-disposition',1,Rest) of
		{value,{'content-disposition',"inline"}} -> heads_to_file(Rest, File#file{name="Inline HTML message"});
		_Else -> heads_to_file(Rest, File)
	end;
heads_to_file([{'content-type', "text/plain"}|Rest], #file{filename=undefined, name=undefined} = File) ->
	case lists:keysearch('content-disposition',1,Rest) of
		{value,{'content-disposition',"inline"}} -> heads_to_file(Rest, File#file{name="Inline text message"});
		_Else -> heads_to_file(Rest, File)
	end;
heads_to_file([{'content-type', CT}|Rest], #file{filename=undefined} = F) ->
	GT = case string:chr(CT, $;) of
		0 -> CT;
		I when is_integer(I) -> string:substr(CT, 1, I - 1)
	end,
	File = F#file{given_type=GT},
	case string:str(CT, "name=") of
		0 -> heads_to_file(Rest, File);
		I2 when is_integer(I2) -> 
			FN = case string:strip(string:substr(CT, I2 + 5)) of
				"\\\"" ++ Str -> 
					Len = string:len(Str),
					"\"\\" = string:substr(Str, Len - 1),
					string:substr(Str, 1, Len - 2);
				"\"" ++ Str -> 
					Len = string:len(Str),
					"\"" = string:substr(Str, Len),
					string:substr(Str, 1, Len - 1);
				Str -> Str
			end,
			heads_to_file(Rest, File#file{filename=FN})
	end;
heads_to_file([{'content-id', CI}|Rest], #file{filename=undefined, name=undefined} = File) ->
	heads_to_file(Rest, File#file{name=CI});
heads_to_file([{subject, Subject}|Rest], #file{filename=undefined, name=undefined} = File) ->
	heads_to_file(Rest, File#file{name="Mail: " ++ Subject});
heads_to_file([_|Rest], File) ->
	ignore,	heads_to_file(Rest, File);
heads_to_file([], File) -> File.

mime_to_files(Mime) -> mime_to_files([Mime], []).

mime_to_files([#mime{content=Content, header=Headers, body=Body}|Rest], Acc) ->
	File = heads_to_file(Headers),
	CTE = case lists:keysearch('content-transfer-encoding',1,Headers) of
		{value,{'content-transfer-encoding',Value}} -> Value;
		_ -> '7bit'
	end,
	Data = mime_util:decode_content(CTE, Content),
	mime_to_files(lists:append(Body, Rest), [File#file{dataref=?BB_C(Data)}|Acc]);
mime_to_files([], Acc) -> lists:reverse(Acc).

%%-------------------------------------------------------------------------
%% @doc Select chuck from files
%% @end
%%-------------------------------------------------------------------------
get_chunk(Files) -> get_chunk(0, Files, []).

get_chunk(_TotalSize, [], InChunk) -> {InChunk, []};
get_chunk(TotalSize, [#file{dataref=Ref} = File|Files], InChunk) 
		when TotalSize < ?CHUNK_THRESHOLD ->
	FS = ?BB_S(Ref),
	get_chunk(TotalSize + FS, Files, [File|InChunk]);
get_chunk(_TotalSize, Rest, InChunk) -> {InChunk, Rest}.
	


-include_lib("eunit/include/eunit.hrl").

escape_regex_test_() -> [
	?_assertEqual("\\^testov\\(ic\\)\\?\\$", escape_regex("^testov(ic)?$")),
	?_assertEqual("\\\\n \\\\r \\\\n", escape_regex("\\n \\r \\n")),
	?_assertEqual("\\[a-Z]\\{0-9}", escape_regex("[a-Z]{0-9}"))
].

normalize_fn_test_() -> [
	?_assertEqual("hello_world_", normalize_fn("hello world!")),
	?_assertEqual("hello_world_", normalize_fn("hello:world/")),
	?_assertEqual("h-w_d", normalize_fn(<<"h-w_d">>)),
	?_assertEqual("helloworld", normalize_fn(helloworld)),
	?_assertEqual("helloworld62", normalize_fn(helloworld62)),
	?_assertEqual("62", normalize_fn(62)),
	?_assertEqual("nofilename", normalize_fn("")),
	?_assertEqual("hello_world_", normalize_fn("hello|world>"))
].

binary_size_test_() -> [
	?_assertEqual(5, binary_size("hello")),
	?_assertEqual(6, binary_size("hello" ++ [212])),
	?_assertEqual(12, binary_size("hello" ++ [212] ++ [<<" world">>])),
	?_assertEqual(5, binary_size(<<"hello">>)),
	?_assertEqual("\\\\n \\\\r \\\\n", escape_regex("\\n \\r \\n")),
	?_assertEqual("\\[a-Z]\\{0-9}", escape_regex("[a-Z]{0-9}"))
].

