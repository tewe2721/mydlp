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
%%----------------------------------------------------------------------
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
	StrLen = string:len(Filename),
	case StrLen >= 4 of
		true ->
			Ext = string:sub_string(Filename, StrLen - 3, StrLen),
			case Ext of
				".doc" -> office_to_text(Data, [?DOC, ?XLS, ?PPT]);
				".xls" -> office_to_text(Data, [?XLS, ?DOC, ?PPT]);
				".ppt" -> office_to_text(Data, [?PPT, ?DOC, ?XLS]);
				_ -> office_to_text(Data, [?DOC, ?XLS, ?PPT])
			end;
		false -> office_to_text(Data, [?DOC, ?XLS, ?PPT])
	end.

office_to_text(Data, [Prog|Progs]) ->
	{Exec, Args} = Prog,
	{ok, FN} = mktempfile(),
	ok = file:write_file(FN, Data),
	Port = open_port({spawn_executable, Exec}, 
			[{args, Args ++ [FN]},
%			[{args, Args},
			binary,
			use_stdio,
			exit_status,
			stderr_to_stdout]),

%%	port_command(Port, Data),
%%	port_command(Port, <<-1>>),

	Ret = case ott_get_resp(Port, []) of
		{ok, Text} -> Text;
		{error, {retcode, _}} -> office_to_text(Data, Progs);
		{error, timeout} -> timeout
	end,

	ok = file:delete(FN),
	
	case Ret of
		timeout -> throw({otterr, timeout});
		Else -> Else
	end;
office_to_text(_Data, []) -> throw({otterr, corrupted}).

ott_get_resp(Port, Ret) ->
	receive
		{ Port, {data, Data}} -> ott_get_resp(Port, [Data|Ret]);
		{ Port, {exit_status, 0}} -> {ok, list_to_binary(lists:reverse(Ret))};
		{ Port, {exit_status, RetCode}} -> { error, {retcode, RetCode} }
	after 15000 -> { error, timeout }
	end.

%%--------------------------------------------------------------------
%% @doc Extracts Text from File records
%% @end
%%----------------------------------------------------------------------
get_text(#file{mime_type=undefined, data=Data}) -> Data;
get_text(#file{mime_type= <<"application/x-empty">>, data=Data}) -> Data;
get_text(#file{mime_type= <<"text/plain">>, data=Data}) -> Data;
get_text(#file{mime_type= <<"application/xml">>, data=Data}) ->
	xml_to_txt(Data);
get_text(#file{mime_type= <<"application/pdf">>, data=Data}) ->
	mydlp_tc:get_pdf_text(Data);
get_text(#file{mime_type= <<"application/postscript">>, data=Data}) ->
	mydlp_tc:get_pdf_text(Data);
get_text(#file{mime_type= <<"application/vnd.ms-excel">>, data=Data}) ->
	office_to_text(Data, [?XLS, ?DOC, ?PPT]);
get_text(#file{mime_type= <<"CDF V2 Document", _/binary>>} = File) ->  %%% TODO: should be refined
	office_to_text(File);
get_text(#file{mime_type= <<"application/msword">>} = File) ->
	office_to_text(File);
get_text(#file{mime_type= <<"application/vnd.ms-office">>} = File) ->
	office_to_text(File);
get_text(#file{data=Data}) -> Data.

%%--------------------------------------------------------------------
%% @doc Extracts Text from XML string
%% @end
%%----------------------------------------------------------------------
xml_to_txt(Data) when is_binary(Data)-> xml_to_txt(binary_to_list(Data));
xml_to_txt(Data) when is_list(Data) -> list_to_binary(xml_to_txt1(xmerl_scan:string(Data))).

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
	ok = file:write_file(RarFN, Bin),
	{ok, WorkDir} = mktempdir(),
	WorkDir1 = WorkDir ++ "/",
	Port = open_port({spawn_executable, "/usr/bin/unrar"}, 
			[{args, ["e","-y","-p-","-inul","--",RarFN]},
			{cd, WorkDir1},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	case ur_get_resp(Port) of
		ok -> {ok, rr_files(WorkDir1)};
		Else -> Else
	end.
	
ur_get_resp(Port) ->
	receive
		{ Port, {data, _}} -> ur_get_resp(Port);
		{ Port, {exit_status, 0}} -> ok;
		{ Port, {exit_status, RetCode}} -> { error, {retcode, RetCode} }
	after 15000 -> { error, timeout }
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

	
