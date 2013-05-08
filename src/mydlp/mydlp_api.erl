%%%
%%%    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@mydlp.com>
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

-author('kerem@mydlp.com').

-compile(export_all).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

-ifdef(__MYDLP_NETWORK).

-include("mydlp_http.hrl").

-endif.

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
%% @doc Check if a byte is defined as an Filename character.
%% @end
%%----------------------------------------------------------------------
is_fn_char($/) -> false;
is_fn_char($\\) -> false;
is_fn_char($?) -> false;
is_fn_char($%) -> false;
is_fn_char($:) -> false;
is_fn_char($") -> false;
is_fn_char($<) -> false;
is_fn_char($>) -> false;
is_fn_char(I) when I =< 31 -> false;
is_fn_char(_Else) -> true.

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
%% @doc Extracts Text from File records
%% @end
%%----------------------------------------------------------------------
get_text(#file{filename=Filename} = File) ->
	case get_text1(File) of
		{ok, ContentText} -> 
			FNBin = case Filename of
				undefined -> <<>>;
				_ -> 	FNL = filename_to_list(Filename), 
					Extension = filename:extension(FNL),
					RootName = filename:rootname(FNL),
					RNBin = filename_to_bin(RootName),
					EBin = case Extension of
						"" -> <<>>;
						[$. | RoE] -> filename_to_bin(RoE);
						_ -> filename_to_bin(Extension) end,
					<<RNBin/binary, " " , EBin/binary>> end,
			Text = case FNBin of
				<<>> -> ContentText;
				_Else -> <<" ", FNBin/binary, " ", "\n", ContentText/binary>> end,
			{ok, Text};
		Else -> Else end.

get_text1(#file{is_encrypted=true}) -> {error, encrypted};
get_text1(#file{compressed_copy=true}) -> {error, compression};
get_text1(#file{mime_type= <<"application/x-empty">>}) -> {ok, <<>>};
get_text1(#file{mime_type=undefined}) -> {error, unknown_type};
get_text1(#file{mime_type=MimeType, filename=Filename, data=Data}) -> 
	case mime_category(MimeType) of
		compression -> {error, compression};
		audio-> {error, audio};
		video-> {error, video};
		image-> {error, image};
		encrypted-> {error, encrypted};
		_ -> 	try 	case mydlp_tc:get_text(Filename, MimeType, Data) of
					{error, E} -> {error, E};
					Text -> {ok, Text} end
			catch Class:Error -> {error, {Class,Error}} end
	end.

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
-define(WS, [9, 10, 12, 13, 32]).
-define(WS_WITH_DASH, [$-|?WS]).
-define(WS_WITH_DASH_AND_DOT, [$.,$-|?WS]).

is_valid_cc(CCStr) ->
	Clean = remove_chars(CCStr, ?WS_WITH_DASH),
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
%% @doc Checks whether string is a valid credit card track 1
%% @end
%%----------------------------------------------------------------------

is_valid_cc_track([I1|Rest]) -> 
	CcStr = case I1 of
			$% -> [_|Rest2] = Rest,
				string:sub_word(Rest2, 1, $^);
			$; -> string:sub_word(Rest, 1, $=)
		end,
	case is_valid_cc(CcStr) of 
		true -> true;
		false -> [_, _|SCcStr] = CcStr, 
			is_valid_cc(SCcStr)
	end.		 

%%--------------------------------------------------------------------
%% @doc Finds valid and applicable DNA patterns in given binary data 
%% @end
%%----------------------------------------------------------------------
valid_dna_patterns(Data) ->
	dna_fsm(root, Data, 0, 0,[]).%State, Data, Index, Length, Acc

dna_fsm(_, <<>>, I, L, Acc) when L > 50 -> lists:reverse([(I-L)|Acc]);
dna_fsm(_, <<>>, _I, _L, Acc) -> lists:reverse(Acc);
dna_fsm(root, <<C/utf8, Rest/binary>>, I, L, Acc) when C==97;C==99;C==103;C==116 -> dna_fsm(dna_accept, Rest, I+1, L+1, Acc); % 0 represents the length of the DNA sequence. 
dna_fsm(root, <<_C/utf8, Rest/binary>>, I, L, Acc) -> dna_fsm(root, Rest, I+1, L, Acc);
dna_fsm(dna_accept, <<C/utf8, Rest/binary>>, I, L, Acc) when C==97;C==99;C==103;C==116;C==32 -> dna_fsm(dna_accept, Rest, I+1, L+1, Acc);
dna_fsm(dna_accept, <<_C/utf8, Rest/binary>>, I, L, Acc) when L >= 50 -> dna_fsm(root, Rest, I+1, 0, [(I-L)|Acc]);
dna_fsm(dna_accept, <<_C/utf8, Rest/binary>>, I, _L, Acc)  -> dna_fsm(root, Rest, I+1, 0, Acc).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid IP
%% @end
%%----------------------------------------------------------------------
is_valid_ip(IpStr) ->
	Clean = remove_chars(IpStr, ?WS),
	SplitList = string:tokens(Clean, "."),
	IntegerList = lists:map(fun(I) -> list_to_integer(I) end, SplitList),
	lists:all(fun(C) -> C =< 255 end, IntegerList).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid MAC
%% @end
%%----------------------------------------------------------------------
is_valid_mac(_MacStr) -> true. % For a applicable validation, Organizationally Unique Identifier List must be used.

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid ICD10-Code
%% @end
%%----------------------------------------------------------------------
is_valid_icd10(IcdStr) ->
	Clean = remove_chars(IcdStr, ?WS),
	Numeric = list_to_integer(string:substr(Clean, 2, length(Clean) -  1)),
	case lists:nth(1, Clean) of
		$d -> Numeric =< 89;
		$e -> Numeric =< 90;
		$k -> Numeric =< 93;
		$p -> Numeric =< 96;
		$t -> Numeric =< 98;
		$y -> Numeric =< 98;
		_  -> Numeric =< 99
	end.

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid IBAN account number
%% @end
%%----------------------------------------------------------------------
is_valid_iban(IbanStr) ->
	Clean = remove_chars(IbanStr, ?WS_WITH_DASH),
	is_valid_iban1(Clean).

is_valid_iban1([CC1, CC2, CD1, CD2|Rest]) ->
	M = calculate_modulus_97(Rest ++ [CC1, CC2] ++ "00" ),
	M == list_to_integer([CD1,CD2]).
	
calculate_modulus_97(Str) ->
	R = calculate_modulus_97(lists:reverse(Str), 1, 0) rem 97,
	98 - R.

calculate_modulus_97([C|Rest], M, Acc) ->
	{M1, A} = case to_iban_value(C) of
		I when I > 9 -> {M*100, M*I};
		I -> {M*10, M*I} end,
	
	calculate_modulus_97(Rest, M1, Acc + A);
calculate_modulus_97([], _M, Acc) -> Acc.
	

to_iban_value(C) when C >= $0 , C =< $9 -> C - $0;
to_iban_value(C) when C >= $a , C =< $z -> C - $a + 10;
to_iban_value(C) when C >= $A , C =< $Z -> C - $A + 10;
to_iban_value(_C) -> throw({error, unexcepected_character}).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid ABA Routing Number
%% @end
%%----------------------------------------------------------------------
is_valid_aba(AbaStr) ->
	Clean = remove_chars(AbaStr, ?WS_WITH_DASH),
	is_valid_aba1(Clean).

is_valid_aba1([A1,A2|Rest]) ->
	case  list_to_integer([A1,A2]) of
		P when P =< 12 ->  check_aba_modulus([A1,A2|Rest]);
		P when 21 =< P,  P =< 32 ->  check_aba_modulus([A1,A2|Rest]);
		P when 21 =< P,  P =< 32 ->  check_aba_modulus([A1,A2|Rest])
	end.
		
check_aba_modulus(Str) ->
	check_aba_modulus1([ C - $0 || C <- Str]).

check_aba_modulus1([A1, A2, A3, A4, A5, A6, A7, A8, A9]) ->
	((3 * (A1 + A4 + A7)) + (7 * (A2 + A5 + A8)) + (A3 + A6 + A9)) rem 10 == 0. 

%%------------------------------------------------------------------------
%% @doc Checks whether string is a valid permanent identification number
%% @end
%%------------------------------------------------------------------------

is_valid_pan(PanStr) ->
	Clean = remove_chars(PanStr, ?WS),
	[_I1, _I2, _I3, I4|_Tail] = Clean,
	case I4 of
		$c -> true;
		$p -> true;
		$h -> true;
		$f -> true;
		$a -> true;
		$t -> true;
		$b -> true;
		$l -> true;
		$j -> true;
		$g -> true;
		_Else -> false
	end.

%%------------------------------------------------------------------------
%% @doc Checks whether string is a valid India TAN 
%% @end
%%------------------------------------------------------------------------

is_valid_tan(TanStr) ->
	Clean = remove_chars(TanStr, ?WS),
	[I1,I2,I3|_Tail]= Clean,
	is_valid_tan1([I1, I2, I3]).

is_valid_tan1(_Code="agr") -> true;	
is_valid_tan1(_Code="ahm") -> true;	
is_valid_tan1(_Code="ald") -> true;	
is_valid_tan1(_Code="amr") -> true;	
is_valid_tan1(_Code="bbn") -> true;	
is_valid_tan1(_Code="blr") -> true;	
is_valid_tan1(_Code="bpl") -> true;	
is_valid_tan1(_Code="brd") -> true;	
is_valid_tan1(_Code="cal") -> true;	
is_valid_tan1(_Code="che") -> true;	
is_valid_tan1(_Code="chn") -> true;	
is_valid_tan1(_Code="cmb") -> true;	
is_valid_tan1(_Code="del") -> true;	
is_valid_tan1(_Code="hyd") -> true;	
is_valid_tan1(_Code="jbp") -> true;	
is_valid_tan1(_Code="jdh") -> true;	
is_valid_tan1(_Code="jld") -> true;	
is_valid_tan1(_Code="jpr") -> true;	
is_valid_tan1(_Code="klp") -> true;	
is_valid_tan1(_Code="knp") -> true;	
is_valid_tan1(_Code="lkn") -> true;	
is_valid_tan1(_Code="mri") -> true;	
is_valid_tan1(_Code="mrt") -> true;	
is_valid_tan1(_Code="mum") -> true;	
is_valid_tan1(_Code="ngp") -> true;	
is_valid_tan1(_Code="nsk") -> true;	
is_valid_tan1(_Code="pne") -> true;	
is_valid_tan1(_Code="ptl") -> true;	
is_valid_tan1(_Code="ptn") -> true;	
is_valid_tan1(_Code="rch") -> true;	
is_valid_tan1(_Code="rkt") -> true;	
is_valid_tan1(_Code="shl") -> true;	
is_valid_tan1(_Code="srt") -> true;	
is_valid_tan1(_Code="tvd") -> true;	
is_valid_tan1(_Code="vpn") -> true;	
is_valid_tan1(_Code="guj") -> true;	
is_valid_tan1(_Code="nwr") -> true;	
is_valid_tan1(_Code="kar") -> true;	
is_valid_tan1(_Code="dlc") -> true;	
is_valid_tan1(_Code="wbg") -> true;	
is_valid_tan1(_Code="krl") -> true;	
is_valid_tan1(_Code="apr") -> true;	
is_valid_tan1(_Code="rjn") -> true;	
is_valid_tan1(_Code)-> false.

%%---------------------------------------------------------------------
%% @doc Checks whether string is a valid China Identity Card Number
%% @end 
%%---------------------------------------------------------------------

is_valid_china_icn(IcnStr) ->
	Clean = remove_chars(IcnStr, ?WS),
	is_valid_china_icn1(Clean).

is_valid_china_icn1(IcnStr) ->
	CheckSumList = "10x98765432",
	[I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,I15,I16,I17,_I18] =
		lists:map(fun(I) -> I - $0 end, IcnStr),
	S1 = (I1*7 + I2*9 + I3*10 + I4*5 + I5*8 + I6*4 + I7*2 + I8*1 +
		I9*6 + I10*3 + I11*7 + I12*9 + I13*10 + I14*5 + I15*8 +
		I16*4 + I17*2) rem 11,
	Result = lists:nth(S1+1, CheckSumList),
	LastElement = lists:last(IcnStr),
	Result == LastElement.

%%----------------------------------------------------------------------
%% @doc Checks whether string is a valid Brasil CPF Number
%% @end
%%----------------------------------------------------------------------

is_valid_cpf(CpfStr)->
	Clean = remove_chars(CpfStr, ?WS_WITH_DASH_AND_DOT),
	is_valid_cpf1(Clean).

is_valid_cpf1("00000000000") -> false;
is_valid_cpf1(CpfStr) ->
	[I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11] =
		lists:map(fun(I) -> I - $0 end, CpfStr),
	S1 = 11- ((10*I1 + 9*I2 + 8*I3 + 7*I4 + 6*I5+
		5*I6 + 4*I7 + 3*I8 + 2*I9) rem 11), 
	case S1 >= 10 of
		true -> T1 = 0;
		false -> T1 = S1
	end,
	S2 = 11- ((11*I1 + 10*I2 + 9*I3 + 8*I4 + 7*I5+
		6*I6 + 5*I7 + 4*I8 + 3*I9 + 2*T1) rem 11),
	case S2 >= 10 of
		true -> T2 = 0;
		false -> T2 = S2
	end,
	(T1 == I10) and (T2 == I11).

%%-------------------------------------------------------------------
%% @doc Checks whether string is a valid credit card expiration date 
%% @end
%%-------------------------------------------------------------------

is_valid_cc_edate(EdateStr) ->
	Clean = remove_chars(EdateStr, ?WS),
	[I1,I2,_I3,I4,I5] = 
		lists:map(fun(I) -> I - $0 end, Clean),
	Mm = I1*10 + I2,
	case (Mm =< 12) and (Mm > 0) of
		true -> ValidMonth = true;
		false -> ValidMonth = false
	end,
	Yy = I4*10 + I5,
	{Y, _M, _D} = date(),
	YearNow = Y rem 100,
	case (Yy >= YearNow) and (Yy =< (YearNow + 10)) of
		true -> ValidYear = true;
		false -> ValidYear = false
	end,
	ValidMonth and ValidYear.	

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid date
%% @end
%%--------------------------------------------------------------------

is_valid_date(DateStr) ->
	{Year, Month, Day} = split_date_string(DateStr),
	(is_valid_year(Year) and is_valid_month(Month)) and is_valid_day(Day).

is_valid_year(Year) ->
	[I1,I2,I3,I4] = lists:map(fun(I) -> I - $0 end, Year),
	Y = I1*1000 + I2*100 + I3*10 + I4,
	((Y >= 1000) and (Y =< 3000)).
%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid birthdate
%% @end
%%--------------------------------------------------------------------

is_valid_birthdate(BirthdateStr) ->
	{Year, Month, Day} = split_date_string(BirthdateStr),
	(is_valid_birthdate_year(Year) and is_valid_month(Month)) and is_valid_day(Day).

split_date_string(BirthdateStr) ->
	Clean = remove_chars(BirthdateStr, ","),
	SplittedDate = string:tokens(Clean, "/-. "),
	[Head|Tail] = SplittedDate,
	{LeftList, Year} = case is_year_string(Head) of 
				true -> {Tail, Head};
				false -> {lists:sublist(SplittedDate, 2), lists:last(Tail)}
			end,
	{Day, Month} = find_month_string(LeftList),
	{Year, Month, Day}.

is_year_string(Str) ->
	case length(Str) of
		4 -> is_all_digit(Str);
		_ -> false
	end.

find_month_string([H,T]) when length(H) == 3 -> {T, H};
find_month_string([H,T]) when length(T) == 3 -> {H, T};
find_month_string([H,T]) when length(H) == 2 ->
	case (list_to_integer(H) < 13) and (list_to_integer(H) > 0) of 
		true -> {T,H};
		false -> {H, T}
	end;
find_month_string([H,T]) -> {H, T}.

is_valid_birthdate_year(Year) ->
	[I1,I2,I3,I4] = lists:map(fun(I) -> I - $0 end, Year),
	Y = I1*1000 + I2*100 + I3*10 + I4,
	{Yy, _M, _D} = date(),
	((Y > 1899) and (Y =< Yy)).

is_valid_month(MonthStr) ->
	case is_all_digit(MonthStr) of
		true -> ((list_to_integer(MonthStr) > 0) and (list_to_integer(MonthStr) < 13));
		false ->
			case MonthStr of
				"jan" -> true;
				"feb" -> true;
				"mar" -> true;
				"apr" -> true;
				"may" -> true;
				"jun" -> true;
				"jul" -> true;
				"aug" -> true;
				"sep" -> true;
				"oct" -> true;
				"nov" -> true;
				"dec" -> true;
				_ -> false
			end
	end.

is_valid_day(Day) when length(Day) == 2 ->
	[I1,I2] = lists:map(fun(I) -> I - $0 end, Day),
	D = I1*10 + I2,
	((D > 0) and (D < 32));

is_valid_day(Day) when length(Day) == 1 ->
	[I1] = lists:map(fun(I) -> I - $0 end, Day),
	((I1 > 0) and (I1 < 10)).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid TR ID number
%% @end
%%----------------------------------------------------------------------
is_valid_trid(TrIdStr) ->
	Clean = remove_chars(TrIdStr, ?WS_WITH_DASH),
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
%is_valid_ssn([_WS|SSNStr]) ->
is_valid_ssn(SSNStr) ->
	%SSNStr1 = string:substr(SSNStr, 1, string:len(SSNStr) - 1),
	Clean = remove_chars(SSNStr, ?WS_WITH_DASH),
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
	Clean = remove_chars(SINStr, ?WS_WITH_DASH),
	is_valid_sin1(Clean).

is_valid_sin1("000000000") -> false;
is_valid_sin1(SINStr) -> check_luhn(SINStr).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid France INSEE code
%% @end
%%----------------------------------------------------------------------
is_valid_insee(INSEEStr) ->
	Clean = remove_chars(INSEEStr, ?WS_WITH_DASH),
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
	Clean = remove_chars(SINStr, ?WS_WITH_DASH),
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

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid National ID Number of Taiwan. 
%% @end
%%----------------------------------------------------------------------
is_valid_taiwan_nid(DNIStr0) ->
	DNIStr = string:to_lower(DNIStr0),
	Clean = remove_chars(DNIStr, ?WS),
	case length(Clean) of
		10 -> is_valid_taiwan_nid1(Clean);
		_ -> false
	end.

is_valid_taiwan_nid1([_,$1|_] = Str) -> is_valid_taiwan_nid2(Str);
is_valid_taiwan_nid1([_,$2|_] = Str) -> is_valid_taiwan_nid2(Str);
is_valid_taiwan_nid1(_Else) -> false.

is_valid_taiwan_nid2([$a|Rest]) -> is_valid_taiwan_nid3("10" ++ Rest);
is_valid_taiwan_nid2([$b|Rest]) -> is_valid_taiwan_nid3("11" ++ Rest);
is_valid_taiwan_nid2([$c|Rest]) -> is_valid_taiwan_nid3("12" ++ Rest);
is_valid_taiwan_nid2([$d|Rest]) -> is_valid_taiwan_nid3("13" ++ Rest);
is_valid_taiwan_nid2([$e|Rest]) -> is_valid_taiwan_nid3("14" ++ Rest);
is_valid_taiwan_nid2([$f|Rest]) -> is_valid_taiwan_nid3("15" ++ Rest);
is_valid_taiwan_nid2([$g|Rest]) -> is_valid_taiwan_nid3("16" ++ Rest);
is_valid_taiwan_nid2([$h|Rest]) -> is_valid_taiwan_nid3("17" ++ Rest);
is_valid_taiwan_nid2([$i|Rest]) -> is_valid_taiwan_nid3("34" ++ Rest);
is_valid_taiwan_nid2([$j|Rest]) -> is_valid_taiwan_nid3("18" ++ Rest);
is_valid_taiwan_nid2([$k|Rest]) -> is_valid_taiwan_nid3("19" ++ Rest);
is_valid_taiwan_nid2([$l|Rest]) -> is_valid_taiwan_nid3("20" ++ Rest);
is_valid_taiwan_nid2([$m|Rest]) -> is_valid_taiwan_nid3("21" ++ Rest);
is_valid_taiwan_nid2([$n|Rest]) -> is_valid_taiwan_nid3("22" ++ Rest);
is_valid_taiwan_nid2([$o|Rest]) -> is_valid_taiwan_nid3("35" ++ Rest);
is_valid_taiwan_nid2([$p|Rest]) -> is_valid_taiwan_nid3("23" ++ Rest);
is_valid_taiwan_nid2([$q|Rest]) -> is_valid_taiwan_nid3("24" ++ Rest);
is_valid_taiwan_nid2([$r|Rest]) -> is_valid_taiwan_nid3("25" ++ Rest);
is_valid_taiwan_nid2([$s|Rest]) -> is_valid_taiwan_nid3("26" ++ Rest);
is_valid_taiwan_nid2([$t|Rest]) -> is_valid_taiwan_nid3("27" ++ Rest);
is_valid_taiwan_nid2([$u|Rest]) -> is_valid_taiwan_nid3("28" ++ Rest);
is_valid_taiwan_nid2([$v|Rest]) -> is_valid_taiwan_nid3("29" ++ Rest);
is_valid_taiwan_nid2([$w|Rest]) -> is_valid_taiwan_nid3("32" ++ Rest);
is_valid_taiwan_nid2([$x|Rest]) -> is_valid_taiwan_nid3("30" ++ Rest);
is_valid_taiwan_nid2([$y|Rest]) -> is_valid_taiwan_nid3("31" ++ Rest);
is_valid_taiwan_nid2([$z|Rest]) -> is_valid_taiwan_nid3("33" ++ Rest);
is_valid_taiwan_nid2(_Else) -> false.

is_valid_taiwan_nid3(Str)->
	[I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11] = lists:map(fun(I) -> I - $0 end, Str),
	Sum = I1*1 + I2*9 + I3*8 + I4*7 + I5*6 + I6*5 + I7*4 + I8*3 + I9*2 + I10*1 + I11*1,
	0 == (Sum rem 10).

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid Spain DNI. 
%% @end
%%----------------------------------------------------------------------
is_valid_dni(DNIStr) ->
	Clean = remove_chars(DNIStr, ?WS),
	case length(Clean) of
		9 -> is_valid_dni1(Clean);
		10 -> is_valid_dni1(remove_chars(Clean, ?WS_WITH_DASH));
		_ -> false
	end.

is_valid_dni1(Str)->
	ChecksumList = "trwagmyfpdxbnjzsqvhlcke",
	Checksum = lists:last(Str),
	[I1,I2,I3,I4,I5,I6,I7,I8,_I9] = lists:map(fun(I) -> I - $0 end, Str),
	Sum = I1*10000000+I2*1000000+I3*100000+I4*10000+I5*1000+I6*100+I7*10+I8,
	Rem = (Sum rem 23) + 1,
	RealChecksum = lists:nth(Rem, ChecksumList),
	RealChecksum == Checksum.

%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid Italian Fiscal Code. 
%% @end
%%----------------------------------------------------------------------
is_valid_fc(FcStr) ->
	Clean = remove_chars(FcStr, ?WS),
	SumEven = fc_partial_sum(Clean, 2, 0, fc_even_value),
	SumOdd = fc_partial_sum(Clean, 1, 0, fc_odd_value),
	RealChecksum = (SumEven + SumOdd) rem 26,
	Checksum = lists:last(Clean),
	Checksum == RealChecksum + 97.

fc_partial_sum(List, Index, Sum, Func) ->
	case Index >= length(List) of
		true -> Sum;
		false -> Value = ?MODULE:Func(lists:nth(Index, List)),
			 fc_partial_sum(List, Index+2, Sum+Value, Func)
	end.

fc_even_value(C) when C >= 48, C =< 57 -> C-48;
fc_even_value(C) -> C - 97.

fc_odd_value(C) when C >= 48, C =< 57 ->
	IntValue = C - 48,
	TempValue = case IntValue < 5 of
			true -> IntValue*2 + 1;
			false -> IntValue*2 + 3
		end,
	case TempValue of
		3 -> 0;
		_ -> TempValue
	end;
fc_odd_value(C) when C >= 97, C =< 101 ->
	TempValue = (C - 97)*2 + 1,
	case TempValue of
		3 -> 0;
		_ -> TempValue
	end;
fc_odd_value(C) when C >= 102, C =< 106 -> (C-97)*2 + 3;
fc_odd_value(_C=107) -> 2;
fc_odd_value(_C=108) -> 4;
fc_odd_value(_C=109) -> 18;
fc_odd_value(_C=110) -> 20;
fc_odd_value(_C=111) -> 11;
fc_odd_value(_C=112) -> 3;
fc_odd_value(_C=113) -> 6;
fc_odd_value(_C=114) -> 8;
fc_odd_value(_C=115) -> 12;
fc_odd_value(_C=116) -> 14;
fc_odd_value(_C=117) -> 16;
fc_odd_value(_C=118) -> 10;
fc_odd_value(_C=119) -> 22;
fc_odd_value(_C=120) -> 25;
fc_odd_value(_C=121) -> 24;
fc_odd_value(_C=122) -> 23;
fc_odd_value(_C) -> -1.
	
%%--------------------------------------------------------------------
%% @doc Checks whether string is a valid South Africa Identity Document number. 
%% @end
%%----------------------------------------------------------------------
is_valid_said(SAIDStr) ->
	Clean = remove_chars(SAIDStr, ?WS_WITH_DASH),
	Clean1 = string:to_lower(Clean),
	case string:len(Clean1) of
		13 -> is_valid_said1(Clean1);
		_Else -> false end.

is_valid_said1([_O1,_E1, O2, E2, O3, E3,_O4,_E4,_O5,_E5, O6,_E6,_CS] = SAIDStr) ->
	CSChk = list_to_integer([O6]) < 2, %% should be 0 or 1
	MMChk = list_to_integer([O2, E2]) < 13, %% should be a number from 1 to 12
	DDChk = list_to_integer([O3, E3]) < 32, %% should be a number from 1 to 31
	CSChk and MMChk and DDChk and is_valid_said2(SAIDStr).

is_valid_said2([O1,E1,O2,E2,O3,E3,O4,E4,O5,E5,O6,E6,CS]) ->
	OddSum = lists:sum(lists:map(fun(NC) -> list_to_integer([NC]) end, [O1,O2,O3,O4,O5,O6])),
	EvenCCI = list_to_integer([E1,E2,E3,E4,E5,E6]),
	EvenCCDI = EvenCCI * 2,
	EvenCCDSum = lists:sum(lists:map(fun(NC) -> list_to_integer([NC]) end, integer_to_list(EvenCCDI))),
	Sum = OddSum + EvenCCDSum,
	CSI = list_to_integer([CS]),
	CSI == 10 - (Sum rem 10).

%%--------------------------------------------------------------------
%% @doc Generates matcher patterns
%% @end
%%----------------------------------------------------------------------

generate_patterns({PatternList, Opts}) ->
	PurePattern= generate_pattern1(PatternList, [[]]),
	PatternWithOpts = lists:map(fun(I) -> apply_opts(I, Opts) end, PurePattern),
	lists:map(fun(H) -> lists:flatten(H) end, PatternWithOpts).

generate_pattern1([], Acc) -> lists:map(fun(H) -> lists:reverse(H) end, Acc);
generate_pattern1([Head|Tail], Acc) ->
	Acc1 = generate_pattern2(Head, Acc),
	generate_pattern1(Tail, Acc1).

generate_pattern2({special, Specials}, Acc) -> generate_patterns_with_specials(Specials, Acc, []);
	
generate_pattern2({Group, {Min, Max}}, Acc) ->
	Acc1 = generate_pattern2({Group, Min}, Acc),
	Acc2 = generate_cartesian_patterns({Group, {Min+1, Max}}, Acc, []),
	lists:append(Acc2, Acc1);
generate_pattern2({alpha, L}, Acc) -> add_character_all_patterns("A", L, Acc);
generate_pattern2({numeric, L}, Acc) -> add_character_all_patterns("N", L, Acc);
generate_pattern2({chinese, L}, Acc) -> add_character_all_patterns("C", L, Acc);
generate_pattern2(ws, Acc) ->  add_character_all_patterns(" ", 1, Acc).

generate_cartesian_patterns({Group, {Min, Max = Min}}, OldPatterns, Acc)->
	Acc1 = generate_pattern2({Group, Max}, OldPatterns),
	lists:append(Acc, Acc1);
generate_cartesian_patterns({Group, {Min, Max}}, OldPatterns, Acc) ->
	Acc1 = generate_pattern2({Group, Min}, OldPatterns),
	generate_cartesian_patterns({Group, {Min+1, Max}}, OldPatterns, lists:append(Acc, Acc1)).

generate_patterns_with_specials([], _OldPatterns, Acc) -> Acc;
generate_patterns_with_specials([Head|Tail], OldPatterns, Acc) ->
	Acc1 = add_character_all_patterns(Head, 1, OldPatterns),
	Acc2 = lists:append(Acc, Acc1),
	generate_patterns_with_specials(Tail, OldPatterns, Acc2).

add_character(C, Acc) -> add_character(C, 1, Acc).
add_character(_C, _L = 0, Acc) -> Acc;
add_character(C, L, Acc) -> add_character(C, L-1, [C|Acc]).

add_character_all_patterns(C, L, Patterns) -> add_character_all_patterns(C, L, Patterns, []).
add_character_all_patterns(_C, _L, [], Acc ) -> Acc;
add_character_all_patterns(C, L, [Head|Tail], Acc) ->
	Acc1 = add_character(C, L, Head),
	Acc2 = lists:flatten(Acc1),
	add_character_all_patterns(C, L, Tail, [Acc2|Acc]).

apply_opts(PurePattern, none) -> PurePattern;
apply_opts(PurePattern, encap_ws) -> " " ++ PurePattern ++ " ";
apply_opts(PurePattern, join_ws) ->
	PurePattern1 = remove_chars(lists:flatten(PurePattern), " "),
	join_ws(PurePattern1, []).

join_ws([], Acc) -> [" "|lists:reverse(Acc)];
join_ws([Head|Tail], Acc) -> join_ws(Tail, [" ",Head|Acc]).
	

%%--------------------------------------------------------------------
%% @doc Gets response from ports
%% @end
%%----------------------------------------------------------------------
get_port_resp(Port, Ret) -> get_port_resp(Port, Ret, 180000).

get_port_resp(Port, Ret, Timeout) ->
	receive
		{ Port, {data, Data}} -> get_port_resp(Port, [Data|Ret]);
		{ Port, {exit_status, 0}} -> {ok, list_to_binary(lists:reverse(Ret))};
		{ Port, {exit_status, RetCode}} -> { error, {retcode, RetCode, list_to_binary(lists:reverse(Ret))} }
	after Timeout -> { error, { port_timeout , list_to_binary(lists:reverse(Ret)) } }
	end.

get_port_resp(Port) ->
	receive
		{ Port, {data, _Data}} -> get_port_resp(Port);
		{ Port, {exit_status, 0}} -> ok;
		{ Port, {exit_status, RetCode}} -> { error, {retcode, RetCode} }
	after 180000 -> { error, port_timeout }
	end.

%%% imported from tsuraan tempfile module http://www.erlang.org/cgi-bin/ezmlm-cgi/4/41649

-ifdef(__MYDLP_ENDPOINT).

-ifdef(__PLATFORM_LINUX).

-define(AGENTVERSIONBIN, "/usr/sbin/mydlp-agent-version").

get_agent_version() ->
	Port = open_port({spawn_executable, ?AGENTVERSIONBIN},
		[{args, []},
		use_stdio,
		exit_status,
		stderr_to_stdout]),

	case get_port_resp(Port, [], 50000) of
		{ok, Data} -> Data;
		_Else -> "" end.

-define(LOGGEDONUSERBIN, "/usr/sbin/mydlp-logged-on-user").

get_logged_on_user() ->
	Port = open_port({spawn_executable, ?LOGGEDONUSERBIN},
		[{args, []},
		use_stdio,
		exit_status,
		stderr_to_stdout]),

	case get_port_resp(Port, [], 25000) of
		{ok, Data} -> Data;
		_Else -> "" end.

-define(LOGGEDONDOMAINBIN, "/usr/sbin/mydlp-logged-on-domain").

get_logged_on_domain() ->
	Port = open_port({spawn_executable, ?LOGGEDONDOMAINBIN},
		[{args, []},
		use_stdio,
		exit_status,
		stderr_to_stdout]),

	case get_port_resp(Port, [], 25000) of
		{ok, Data} -> Data;
		_Else -> "" end.

-endif.

-endif.

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
%% @doc Helper command for uncompression operations
%% @end
%%----------------------------------------------------------------------

-ifdef(__PLATFORM_LINUX).

uncompress_sl(Src,Dest) ->
	ok = file:make_symlink(Src, Dest).

-endif.

-ifdef(__PLATFORM_WINDOWS).

uncompress_sl(Src,Dest) ->
	{ok, _ByteCount} = file:copy(Src, Dest).

-endif.

uncompress(Method, {memory, Bin}, Filename) -> uncompress(Method, Bin, Filename);

uncompress(Method, {Type, _Value} = Ref, Filename) when
		Type == cacheref; Type == tmpfile; Type == regularfile -> 
	{FNDir, FN} = uncompress0(Method, Filename),

	uncompress_sl(?BB_P(Ref), FN),

	mydlp_api:Method(FNDir, FN);

uncompress(Method, Bin, Filename) when is_binary(Bin) -> 
	{FNDir, FN} = uncompress0(Method, Filename),
	ok = file:write_file(FN, Bin, [raw]),
	mydlp_api:Method(FNDir, FN).

uncompress0(ungzipc, Filename) ->
	{ok, FNDir} = mktempdir(),
	NFN = normalize_fn(Filename),
	NFN1 = case string:len(NFN) of
		L when L > 3 -> case string:to_lower(string:substr(NFN, L - 2)) of
			".gz" -> NFN;
			_Else -> NFN ++ ".gz" end;
		_Else2 -> NFN ++ ".gz" end,
	
	FN = FNDir ++ "/" ++ NFN1,
	{FNDir, FN};
uncompress0(_Method, Filename) ->
	{ok, FNDir} = mktempdir(),
	FN = FNDir ++ "/" ++ normalize_fn(Filename),
	{FNDir, FN}.

%%--------------------------------------------------------------------
%% @doc Un7zs an Erlang binary, this can be used for ZIP, CAB, ARJ, GZIP, BZIP2, TAR, CPIO, RPM and DEB formats.
%% @end
%%----------------------------------------------------------------------
-ifdef(__PLATFORM_LINUX).

-define(SEVENZBIN, "/usr/bin/7z").

-endif.

-ifdef(__PLATFORM_WINDOWS).

-define(SEVENZBIN, ?CFG(app_dir) ++ "/cygwin/bin/7z.exe").

-endif.

-define(SEVENZARGS(WorkDir, ZFN), ["-p","-y","-bd","-o" ++ WorkDir,"x",ZFN] ).

un7z(Arg) -> un7z(Arg, "nofilename").

un7z(Arg, FN) -> uncompress(un7zc, Arg, FN).

un7zc(ZFNDir, ZFN) -> 
	{ok, WorkDir} = mktempdir(),
	WorkDir1 = WorkDir ++ "/",
	Port = open_port({spawn_executable, ?SEVENZBIN}, 
			[{args, ?SEVENZARGS(WorkDir1, ZFN)},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	Ret = case get_port_resp(Port) of
		ok -> {ok, rr_files(WorkDir1)};
		Else -> rmrf_dir(WorkDir1), Else end,
	ok = rmrf_dir(ZFNDir),
	Ret.

%%--------------------------------------------------------------------
%% @doc Untnefs an Erlang binary, this can be used for winmail.dat files.
%% @end
%%----------------------------------------------------------------------
-ifdef(__PLATFORM_LINUX).

-define(TNEFBIN, "/usr/bin/tnef").

-endif.

-ifdef(__PLATFORM_WINDOWS).

-define(TNEFBIN, ?CFG(app_dir) ++ "/cygwin/bin/tnef.exe").

-endif.

-define(TNEFARGS(WorkDir, TNEFFN), ["--overwrite", "--use-paths", "-f", TNEFFN, "-C", WorkDir] ).

untnef(Arg) -> untnef(Arg, "nofilename").

untnef(Arg, FN) -> uncompress(untnefc, Arg, FN).

untnefc(TNEFDir, TNEFFN) -> 
	{ok, WorkDir} = mktempdir(),
	WorkDir1 = WorkDir ++ "/",
	Port = open_port({spawn_executable, ?TNEFBIN}, 
			[{args, ?TNEFARGS(WorkDir1, TNEFFN)},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	Ret = case get_port_resp(Port) of
		ok -> {ok, rr_files(WorkDir1)};
		Else -> rmrf_dir(WorkDir1), Else end,
	ok = rmrf_dir(TNEFDir),
	Ret.

%%--------------------------------------------------------------------
%% @doc Ungzips an Erlang binary, this can be used for GZIP format.
%% @end
%%----------------------------------------------------------------------
-ifdef(__PLATFORM_LINUX).

-define(GZIPBIN, "/bin/gzip").

-endif.

-ifdef(__PLATFORM_WINDOWS).

-define(GZIPBIN, ?CFG(app_dir) ++ "/cygwin/bin/gzip.exe").

-endif.

-define(GZIPARGS(FN), ["-d","-f","-q", FN] ).

ungzip(Arg) -> ungzip(Arg, "nofilename").

ungzip(Arg, FN) -> uncompress(ungzipc, Arg, FN).

ungzipc(FNDir, FN) -> 
	Port = open_port({spawn_executable, ?GZIPBIN}, 
			[{args, ?GZIPARGS(FN)},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	case get_port_resp(Port) of
		ok -> {ok, rr_files(FNDir)};
		Else -> rmrf_dir(FNDir), Else end.

%%--------------------------------------------------------------------
%% @doc Unars an Erlang binary, this can be used for AR format.
%% @end
%%----------------------------------------------------------------------
-ifdef(__PLATFORM_LINUX).

-define(ARBIN, "/usr/bin/ar").

-endif.

-ifdef(__PLATFORM_WINDOWS).

-define(ARBIN, ?CFG(app_dir) ++ "/cygwin/bin/ar.exe").

-endif.

-define(ARARGS(ArFN), ["x",ArFN] ).

unar(Arg) -> unar(Arg, "nofilename").

unar(Arg, FN) -> uncompress(unarc, Arg, FN).

unarc(ArFNDir, ArFN) -> 
	{ok, WorkDir} = mktempdir(),
	WorkDir1 = WorkDir ++ "/",
	Port = open_port({spawn_executable, ?ARBIN}, 
			[{args, ?ARARGS(ArFN)},
			{cd, WorkDir1},
			use_stdio,
			exit_status,
			stderr_to_stdout]),

	Ret = case get_port_resp(Port) of
		ok -> {ok, rr_files(WorkDir1)};
		Else -> rmrf_dir(WorkDir1), Else end,
	ok = rmrf_dir(ArFNDir),
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

rmrf(FN) -> case filelib:is_dir(FN) of
		true -> rmrf_dir(FN);
		false -> catch file:delete(FN) end.

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
			true -> CacheRef = ?BB_C({tmpfile, AbsPath}),
				file:delete(AbsPath), %ensure file deletion
				rr_files(FNs, WorkDir, [{FN, CacheRef}|Ret]);
			false -> case filelib:is_dir(AbsPath) of
				true -> Ret1 = rr_files(AbsPath),
					rr_files(FNs, WorkDir, [Ret1|Ret]);
				false -> catch file:delete(AbsPath),
					rr_files(FNs, WorkDir, Ret) end end end;
rr_files([], _WorkDir, Ret) -> lists:flatten(lists:reverse(Ret)).

is_store_action({custom, _}) -> false;
is_store_action(pass) -> false;
is_store_action(block) -> false;
is_store_action(log) -> false;
is_store_action(quarantine) -> true;
is_store_action(archive) -> true.

%%--------------------------------------------------------------------
%% @doc Logs acl messages
%% @end
%%----------------------------------------------------------------------

-ifdef(__MYDLP_NETWORK).

acl_msg(#log{file=[]}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="post-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="urlencoded-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="uri-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="seap-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="resp-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name=undefined, filename=undefined}}) -> ok;
acl_msg(#log{file=#file{} = File} = Log) ->
	acl_msg(Log#log{file=[File]});
acl_msg(#log{action=Action, file=PreFiles}=Log) ->
	PreFiles1 = metafy_files(PreFiles),
	Files = case is_store_action(Action) of
		false -> remove_all_data(PreFiles1);
		true -> remove_mem_data(PreFiles1) end,
	Log1 = Log#log{file=Files},
	case Log1 of
		#log{channel=inbound} -> ok;
		#log{rule_id=-1} -> ok;
		_Else -> acl_msg_logger(Log1) end,
	mydlp_incident:l(Log1),
	ok.

smtp_msg(Format, Args) ->
	Format1 = "PID=~w " ++ Format ++ "~n",
        Args1 = [self() | Args],
        mydlp_logger:notify(smtp_msg, Format1, Args1).

-endif.

-ifdef(__MYDLP_ENDPOINT).

acl_msg(#log{file=[]}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="post-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="urlencoded-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="uri-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="seap-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="resp-data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name="data"}}) -> ok;
acl_msg(#log{rule_id=-1, action=log, file=#file{name=undefined, filename=undefined}}) -> ok;
acl_msg(#log{file=#file{}=File}=Log) ->
	acl_msg(Log#log{file=[File]});
acl_msg(#log{action=Action, file=PreFiles}=Log) ->
	PreFiles1 = metafy_files(PreFiles),
	Files = case is_store_action(Action) of
		false -> remove_all_data(PreFiles1);
		true -> remove_crs(PreFiles1) end,
	Log1 = Log#log{file=Files},
	case Log1 of
		#log{channel=inbound} -> ok;
		#log{rule_id=-1} -> ok;
		_Else -> acl_msg_logger(Log1) end,
	mydlp_item_push:p({endpoint_log, Log1}),
	ok.

-endif.

get_month_str(1) -> "Jan";
get_month_str(2) -> "Feb";
get_month_str(3) -> "Mar";
get_month_str(4) -> "Apr";
get_month_str(5) -> "May";
get_month_str(6) -> "Jun";
get_month_str(7) -> "Jul";
get_month_str(8) -> "Aug";
get_month_str(9) -> "Sep";
get_month_str(10) -> "Oct";
get_month_str(11) -> "Nov";
get_month_str(12) -> "Dec".

formatted_cur_date() -> formatted_cur_date(calendar:universal_time()).

formatted_cur_date({{Year, Month, Day}, {Hours, Minutes, Seconds}}) ->
	MonthS = get_month_str(Month),
	io_lib:format("~s ~2..0B ~4..0B ~2..0B:~2..0B:~2..0B",[MonthS, Day, Year, Hours, Minutes, Seconds]).

escape_es(Bin) when is_binary(Bin) -> escape_es(binary_to_list(Bin));
escape_es(Atom) when is_atom(Atom) -> escape_es(atom_to_list(Atom));
escape_es(Str) when is_list(Str) -> escape_es(Str, []).

escape_es([$=|Str], Rest) -> escape_es(Str, [$=, $\\|Rest]);
escape_es([C|Str], Rest) -> escape_es(Str, [C|Rest]);
escape_es([], Rest) -> lists:reverse(Rest).

acl_src(nil) -> {[], []};
acl_src(undefined) -> {[], []};
acl_src(unknown) -> {[], []};
acl_src({Ip1,Ip2,Ip3,Ip4}) -> {[" src=~B.~B.~B.~B"], [Ip1,Ip2,Ip3,Ip4]}.

acl_suser(nil) -> {[], []};
acl_suser(unknown) -> {[], []};
acl_suser(undefined) -> {[], []};
acl_suser(User) -> {[" suser=~s"], [escape_es(User)]}.

str_channel(web) -> "Web";
str_channel(mail) -> "Mail";
str_channel(removable) -> "Removable Storage";
str_channel(printer) -> "Printer";
str_channel(remote_discovery) -> "Remote Discovery";
str_channel(discovery) -> "Discovery";
str_channel(api) -> "API";
str_channel(_) -> "Unknown".

acl_act(_Channel, pass) -> {[], []};
acl_act(discovery, block) -> {[" act=~s"], ["Deleted"]};
acl_act(discovery, {custom, {seclore, pass, Name, {HotFolderId, ActivityComment}}}) -> 
	{[" act=~s cn3Label=~ts cn3=~B cs2Label=~ts cs2=~ts cs3Label=~ts cs3=~ts cs4Label=~ts cs4=~ts"], 
	["Encrypted", "HotFolder Id", HotFolderId, "Name of Custom Action", escape_es(Name), "IRM Model", "Seclore FileSecure", "Activity Comment", escape_es(ActivityComment)]};
acl_act(mail, {custom, {seclore, pass, Name, {HotFolderId, ActivityComment}}}) -> 
	{[" act=~s cn3Label=~ts cn3=~B cs2Label=~ts cs2=~ts cs3Label=~ts cs3=~ts cs4Label=~ts cs4=~ts"], 
	["Encrypted", "HotFolder Id", HotFolderId, "Name of Custom Action", escape_es(Name), "IRM Model", "Seclore FileSecure", "Activity Comment", escape_es(ActivityComment)]};
acl_act(_Channel, block) -> {[" act=~s"], ["Blocked"]};
acl_act(_Channel, log) -> {[" act=~s"], ["Logged"]};
acl_act(_Channel, quarantine) -> {[" act=~s"], ["Quarantined"]};
acl_act(_Channel, archive) -> {[" act=~s"], ["Archived"]};
acl_act(_Channel, _Else) -> {[], []}.

acl_destination(web, Destination) ->
	{[" dhost=~ts"], [escape_es(Destination)]};
acl_destination(mail, {RcptTo, RcptList}) ->
	{[" duser=~ts msg=~ts"], [escape_es(RcptTo), ("Full list of receipients: " ++ escape_es(RcptList))]};
acl_destination(mail, RcptList) ->
	{[" msg=~ts"], [("Full list of receipients: " ++ escape_es(RcptList))]};
acl_destination(removable, Dest) ->
	{[" cs1Label=~ts cs1=~ts"], ["Destination File", escape_es(Dest)]};
acl_destination(discovery, Dest) ->
	{[" cs1Label=~ts cs1=~ts"], ["Destination File", escape_es(Dest)]};
acl_destination(printer, PrinterName) ->
	{[" cs1Label=~ts cs1=~ts"], ["Printer Name", escape_es(PrinterName)]};
acl_destination(_Channel, _Else) -> {[], []}.

acl_file_hash(undefined) -> {[], []};
acl_file_hash(Hash) -> {[" fileHash=~s"], [Hash]}.

acl_file_mt(undefined) -> {[], []};
acl_file_mt(MT) when is_binary(MT) -> acl_file_mt(binary_to_list(MT));
acl_file_mt(MT) when is_list(MT) -> {[" fileType=~s"], [MT]}.

acl_file_size(undefined) -> {[], []};
acl_file_size(Size) -> {[" fsize=~B"], [Size]}.

acl_files([]) -> {[], []};
acl_files([#file{size=Size, md5_hash=Hash, mime_type=MT} = File]) ->
	FormatHead =[" fname=~ts"], 
	ArgsHead= [escape_es(file_to_str(File))],

	{SizeF, SizeA} = acl_file_size(Size),
	{HashF, HashA} = acl_file_hash(Hash),
	{TypeF, TypeA} = acl_file_mt(MT),

	{lists:flatten([FormatHead, SizeF, HashF, TypeF]), 
		lists:append([ArgsHead, SizeA, HashA, TypeA])};
acl_files(Files) when is_list(Files) -> 
	FileS = string:join([file_to_str(F) || F <- Files] , ", "),
	{[" cs5Label=~ts cs5=~ts"], ["Filenames", escape_es(FileS)]}.

acl_misc("") -> {[], []};
acl_misc(<<>>) -> {[], []};
acl_misc(undefined) -> {[], []};
acl_misc(Misc) -> {[" cs6Label=~ts cs6=~ts"], ["Misc", escape_es(Misc)]}.

-ifdef(__MYDLP_NETWORK).

get_message(_Channel, pass) -> "No action taken.";
get_message(web, block) -> "Transfer of sensitive information to web has been blocked.";
get_message(mail, block) -> "Transfer of e-mail has been blocked because of containing sensitive information.";
get_message(removable, block) -> "Transfer of file to a removable storage device on endpoint has been blocked because of containing sensitive information.";
get_message(discovery, block) -> "A file containing sensitive information on endpoint has been discovered and deleted from endpoint file system.";
get_message(printer, block) -> "Prevented printing of document containing sensitive information on removable.";
get_message(api, block) -> "Specified file should be blocked in response to API query.";
get_message(web, log) -> "Transfer of sensitive information to web has been logged.";
get_message(mail, log) -> "Transfer of e-mail containing sensitive information has been logged.";
get_message(removable, log) -> "Transfer of file containing sensitive information to a removable storage device on endpoint has been logged.";
get_message(discovery, log) -> "A file containing sensitive information on endpoint has been discovered and logged.";
get_message(remote_discovery, log) -> "A file containing sensitive information on remote has been discovered and logged.";
get_message(printer, log) -> "Printing of document containing sensitive information on endpoint has been logged.";
get_message(api, log) -> "Specified file should not be blocked in response to API query and logged query.";
get_message(web, quarantined) -> "Transfer of sensitive information to web has been blocked and a copy of file has been quarantined at central data store.";
get_message(mail, quarantine) -> "Because of containing sensitive information, transfer of e-mail has been blocked and a copy of file has been quarantined at central data store.";
get_message(removable, quarantine) -> "Because of containing sensitive information, transfer of file to a removable storage device on endpoint has been blocked and a copy has been quarantined at central data store.";
get_message(discovery, quarantine) -> "A file containing sensitive information on endpoint has been discovered, deleted from endpoint file system and a copy has been quarantined at central data store.";
get_message(printer, quarantine) -> "Prevented printing of document containing sensitive information on endpoint and a copy has been quarantined at central data store.";
get_message(api, quarantine) -> "Specified file should be blocked in response to API query and a copy of file has been quarantined in central data store.";
get_message(web, archive) -> "Transfer of sensitive information to web has been logged and a copy of file has been archived in central data store.";
get_message(mail, archive) -> "Transfer of e-mail containing sensitive information has been logged and a copy of file has been archived in central data store.";
get_message(removable, archive) -> "Transfer of file containing sensitive information to a removable storage devicea on endpoint has been logged and a copy has been archived in central data store.";
get_message(discovery, archive) -> "A file containing sensitive information on endpoint has been discovered, logged and a copy has been archived in central data store.";
get_message(remote_discovery, archive) -> "A file containing sensitive information on remote has been discovered, logged and a copy has been archived in central data store.";
get_message(printer, archive) -> "Printing of document containing sensitive information on endpoint has been logged and a copy has been archived in data store.";
get_message(api, archive) -> "Specified file should not be blocked in response to API query, logged query and a copy of file has been archived in central data store.";
get_message(discovery, {custom, {seclore, pass, _, _}}) -> "A file containing sensitive information has been discovered, logged and protected with Seclore FileSecure IRM.";
get_message(mail, {custom, {seclore, pass, _, _}}) -> "A file containing sensitive information has been detected, logged and protected with Seclore FileSecure IRM. Protected file leaved mail gateway.";
get_message(_, _) -> "Check MyDLP Logs using management console for details.".

-endif.

-ifdef(__MYDLP_ENDPOINT).

get_message(_Channel, pass) -> "No action taken.";
get_message(removable, block) -> "Transfer of file to a removable storage device has been blocked because of containing sensitive information.";
get_message(discovery, block) -> "A file containing sensitive information has been discovered and deleted from file system.";
get_message(printer, block) -> "Prevented printing of document containing sensitive information.";
get_message(removable, log) -> "Transfer of file containing sensitive information to a removable storage device has been logged.";
get_message(discovery, log) -> "A file containing sensitive information has been discovered and logged.";
get_message(printer, log) -> "Printing of document containing sensitive information has been logged.";
get_message(removable, quarantine) -> "Because of containing sensitive information, transfer of file to a removable storage device has been blocked and a copy has been sent to central data store to be quaratined.";
get_message(discovery, quarantine) -> "A file containing sensitive information has been discovered, deleted from file system and a copy has been sent to central data store to be quaratined.";
get_message(printer, quarantine) -> "Prevented printing of document containing sensitive information and a copy has been sent to central data store to be quaratined.";
get_message(removable, archive) -> "Transfer of file containing sensitive information to a removable storage device has been logged and a copy has been sent to central data store to be archived.";
get_message(discovery, archive) -> "A file containing sensitive information has been discovered, logged and a copy has been sent to central data store to be archived.";
get_message(printer, archive) -> "Printing of document containing sensitive information has been logged and a copy has been sent to central data store to be archived.";
get_message(discovery, {custom, {seclore, pass, _, _}}) -> "A file containing sensitive information has been discovered, logged and protected with Seclore FileSecure IRM.";
get_message(_, _) -> "Check MyDLP Logs using management console for details.".

-endif.

acl_msg_logger(#log{time=Time, channel=Channel, rule_id=RuleId, action=Action, ip=SrcIp, user=SrcUser, destination=To, itype_id=ITypeId, file=Files, misc=Misc}) ->
	FormatHead=["CEF:0|Medra Inc.|MyDLP|1.0|~B|~ts|~B|rt=~s cn1Label=~ts cn1=~B cn2Label=~ts cn2=~B proto=~s"],
	GeneratedMessage = get_message(Channel, Action),
	Severity = 10,
	RTime = formatted_cur_date(Time),
	ChannelS = str_channel(Channel),
	ArgsHead = [RuleId, GeneratedMessage, Severity, RTime, "Rule Id", RuleId, "Infromation Type Id", ITypeId, ChannelS],

	{SrcF, SrcA} = acl_src(SrcIp),
	{SUserF, SUserA} = acl_suser(SrcUser),
	{ActF, ActA} = acl_act(Channel, Action),
	{DestF, DestA} = acl_destination(Channel, To),
	{FilesF, FilesA} = acl_files(Files),
	{MiscF, MiscA} = acl_misc(Misc),

	Format = lists:flatten([FormatHead, SrcF, SUserF, DestF, ActF, FilesF, MiscF]),
	Args = lists:append([ArgsHead, SrcA, SUserA, DestA, ActA, FilesA, MiscA]),

	NotifyType = case Channel of
		discovery -> discovery_msg;
		remote_discovery -> discovery_msg;
		_Else -> acl_msg end,
	mydlp_logger:notify(NotifyType, Format, Args).
	
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

load_file(#file{} = File) ->
	File1 = load_file_data(File),
	File2 = load_file_hash(File1),
	File2.

load_file_data(#file{dataref=undefined} = File) -> File;
load_file_data(#file{data=undefined} = File) ->
	Data = ?BB_R(File#file.dataref),
	File#file{data=Data};
load_file_data(#file{} = File) -> File.

load_file_hash(#file{data=undefined} = File) -> File;
load_file_hash(#file{md5_hash=undefined, data=Data} = File) ->
	Hash = mydlp_api:md5_hex(Data),
	File#file{md5_hash=Hash};
load_file_hash(File) -> File.

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

clean_files_excluding(#file{} = File, DRef) -> [Ret] = clean_files_excluding([File], DRef), Ret;
clean_files_excluding(Files, DRef) when is_list(Files) -> clean_files_excluding(Files, DRef, []).

clean_files_excluding([#file{dataref=DRef} = File|Rest], DRef, Acc) -> clean_files_excluding(Rest, DRef, [File|Acc]);
clean_files_excluding([File|Rest], DRef, Acc) -> 
	File1 = clean_file(File),
	clean_files_excluding(Rest, DRef, [File1|Acc]);
clean_files_excluding([], _DRef, Acc) -> lists:reverse(Acc).


%%--------------------------------------------------------------------
%% @doc Remove cache references.
%% @end
%%----------------------------------------------------------------------
remove_crs(#file{} = File) -> [Ret] = remove_crs([File]), Ret;
remove_crs(Files) when is_list(Files) ->
	lists:map(fun(F) -> remove_cr(F) end, Files).

remove_cr(#file{} = File) -> 
	File1 = load_file(File),
	case File#file.dataref of
		undefined -> ok;
		DataRef -> ?BB_D(DataRef) end,
	remove_text(File1#file{dataref=undefined}).

remove_all_data([]) -> [];
remove_all_data([_|_] = ListOfFiles) ->
	[remove_all_data(F) || F <- ListOfFiles];

remove_all_data(#file{dataref=undefined} = File) ->
	File#file{data=undefined, dataref=undefined, text=undefined, normal_text=undefined, mc_table=undefined};
remove_all_data(#file{dataref=_DataRef} = File) ->
	File1 = clean_file(File),
	remove_all_data(File1).

remove_mem_data([]) -> [];
remove_mem_data([_|_] = ListOfFiles) ->
	[remove_mem_data(F) || F <- ListOfFiles];

remove_mem_data(#file{data=undefined, dataref=undefined} = File) -> remove_text(File);
remove_mem_data(#file{data=undefined} = File) -> remove_text(File);
remove_mem_data(#file{dataref=undefined, data=Data} = File) ->
	DataRef = ?BB_C(Data),
	remove_text(File#file{data=undefined, dataref=DataRef});
remove_mem_data(#file{} = File) -> remove_text(File#file{data=undefined}).

remove_text(#file{} = File) -> File#file{text=undefined, normal_text=undefined, mc_table=undefined}.


%%--------------------------------------------------------------------
%% @doc Reconstructs cache references.
%% @end
%%----------------------------------------------------------------------
reconstruct_crs(#file{} = File) -> [Ret] = reconstruct_crs([File]), Ret;
reconstruct_crs(Files) when is_list(Files) ->
	lists:map(fun(F) -> reconstruct_cr(F) end, Files).

reconstruct_cr(#file{} = File) -> 
	File1 = load_file(File),
	case File1#file.data of
		undefined -> File1;
		Data ->	?BF_C(File1, Data) end.

%%--------------------------------------------------------------------
%% @doc Detects mimetypes of all files given, 
%% @end
%%----------------------------------------------------------------------
get_mimes(Files) -> get_mimes(Files, []).

get_mimes([File|Files], Returns) -> 
	get_mimes(Files, [mimefy(File)|Returns]);
get_mimes([], Returns) -> lists:reverse(Returns).

mimefy(#file{mime_type=undefined, data=undefined} = File) -> File;
mimefy(#file{mime_type=undefined} = File) ->
	MT = mydlp_tc:get_mime(File#file.filename, File#file.data),
	File#file{mime_type=MT};
mimefy(#file{} = File) -> File.

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
%comp_to_files([#file{mime_type= <<"application/x-gzip">>, compressed_copy=false, is_encrypted=false} = File|Files], Processed, New) ->
%	case ungzip(File#file.dataref, File#file.filename) of
%		{ok, Ext} -> 
%			ExtFiles = ext_to_file(Ext),
%			ctf_ok(Files, File, ExtFiles, Processed, New);
%		{error, _ShouldBeLogged} -> 
%			ctf_err_enc(Files, File, Processed, New) end;
%comp_to_files([#file{mime_type= <<"application/x-archive">>, compressed_copy=false, is_encrypted=false} = File|Files], Processed, New) ->
%	case unar(File#file.dataref, File#file.filename) of
%		{ok, Ext} -> 
%			ExtFiles = ext_to_file(Ext),
%			ctf_ok(Files, File, ExtFiles, Processed, New);
%		{error, _ShouldBeLogged} -> 
%			ctf_err_enc(Files, File, Processed, New) end;
comp_to_files([#file{mime_type= <<"application/vnd.ms-tnef">>, compressed_copy=false, is_encrypted=false} = File|Files], Processed, New) ->
	case untnef(File#file.dataref, File#file.filename) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			ctf_ok(Files, File, ExtFiles, Processed, New);
		{error, _ShouldBeLogged} -> 
			ctf_err_enc(Files, File, Processed, New) end;
comp_to_files([#file{mime_type= <<"message/rfc822">>, compressed_copy=false, is_encrypted=false} = File|Files], Processed, New) ->
	try	ExtFiles = rfc822_to_files(File#file.data),
		ctf_ok(Files, File, ExtFiles, Processed, New)
	catch Class:Error ->
		?ERROR_LOG("Error occurred when parsing rfc822 to files. Class: "?S". Error: "?S". Stacktrace:"?S"~nFile: "?S, 
				[Class, Error, erlang:get_stacktrace(), File]),
		ctf_err_enc(Files, File, Processed, New) end; % may be returned as plain text but this will introduce false positives
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
	[?BF_C(#file{name= "extracted file", 
		filename=Filename}, Data)
		|| {Filename,Data} <- Ext].

%%-----------------------------------------------------------------------
%% @doc Checks whether funciton returns true. In addition returns indexes of occurrences.
%%
%%------------------------------------------------------------------------

regex_filter_map(Fun, Data, IndexList) -> regex_filter_map(Fun, Data, IndexList, []).

regex_filter_map(_Fun, _Data, [], Acc) -> lists:reverse(Acc);

regex_filter_map(Fun, Data, [I|List], Acc) ->
	{Index, Length} = I,
	MatchString = binary:bin_to_list(Data, {Index, Length}),
	case Fun(MatchString) of
		true -> regex_filter_map(Fun, Data, List, [{Index, MatchString}|Acc]);
		false -> regex_filter_map(Fun, Data, List, Acc) end.

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

filter_count(Fun, List) -> filter_count(Fun, List, 0).

filter_count(Fun, [I|List], Curr) ->
	case Fun(I) of
		true -> filter_count(Fun, List, Curr + 1);
		_Else -> filter_count(Fun, List, Curr)
	end;
filter_count(_, [], Curr) -> Curr.

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

rfc822_to_files(Bin) when is_binary(Bin)-> 
	MIME = mime_util:decode(Bin),
	mime_to_files(MIME).

-ifdef(__MYDLP_NETWORK).

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
			?DEBUG("Can't parse multipart if get a "?S, [Other]), []
	end,
	mime_to_files(Res).

-endif.

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

metafy_files(Files) -> metafy_files(Files, []).

metafy_files([F|Rest], Acc) -> metafy_files(Rest, [metafy(F)|Acc]);
metafy_files([], Acc) -> lists:reverse(Acc).

metafy(#file{} = File) ->
	File1 = load_file(File),
	File2 = hashify(File1),
	File3 = sizefy(File2),
	File4 = mimefy(File3),
	File4.

%%-------------------------------------------------------------------------
%% @doc If present hashes data returns file.
%% @end
%%-------------------------------------------------------------------------
hashify_files(Files) -> hashify_files(Files, []).

hashify_files([F|Rest], Acc) -> hashify_files(Rest, [hashify(F)|Acc]);
hashify_files([], Acc) -> lists:reverse(Acc).

hashify(#file{md5_hash=undefined, data=undefined} = File) -> File;
hashify(#file{md5_hash=undefined, data=Data} = File) ->
	Hash = mydlp_api:md5_hex(Data),
	File#file{md5_hash=Hash};
hashify(#file{} = File) -> File.

%%-------------------------------------------------------------------------
%% @doc If present set file size of data and returns file.
%% @end
%%-------------------------------------------------------------------------
sizefy(#file{size=undefined, dataref=undefined, data=undefined} = File) -> File;
sizefy(#file{size=undefined, dataref=undefined, data=Data} = File) -> 
	Size = size(Data),
	File#file{size=Size};
sizefy(#file{size=undefined, dataref=Ref} = File) -> 
	Size = ?BB_S(Ref),
	File#file{size=Size};
sizefy(#file{} = File) -> File.

%%-------------------------------------------------------------------------
%% @doc Writes files to quarantine directory.
%% @end
%%-------------------------------------------------------------------------
	
quarantine(#file{data=Data, md5_hash=undefined}) -> mydlp_quarantine:q(Data);
quarantine(#file{data=Data, md5_hash=Hash}) -> mydlp_quarantine:q(Hash, Data).

%%-------------------------------------------------------------------------
%% @doc Return denied page for different formats
%% @end
%%-------------------------------------------------------------------------

get_denied_page(OrigRuleId, Format) -> mydlp_mnesia:get_user_message(OrigRuleId, Format).

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
		{ok, Files} -> Files end.

parse_uenc_data(Bin) when is_list(Bin) -> parse_uenc_data(list_to_binary(Bin));
parse_uenc_data(Bin) when is_binary(Bin) ->
	do_parse_uenc(Bin, [], #file{}, []).

do_parse_uenc(<<$%, $u, A:8, B:8,C:8,D:8, Tail/binary>>, CurData, CurFile, Files) when 
		A >= 48, A =< 57,
		B >= 48, B =< 57,
		C >= 48, C =< 57,
		D >= 48, D =< 57 ->
	%% non-standard encoding for Unicode characters: %uxxxx,		     
	Hex = try hex2int([A,B,C,D]) catch _:_ -> $\s end,
	BinRep = case unicode:characters_to_binary([Hex]) of
		<<Bin/binary>> -> Bin;
		_Else -> $_ end,
	do_parse_uenc(Tail, [ BinRep | CurData], CurFile, Files);

do_parse_uenc(<<$%, A:8, B:8, Tail/binary>>, CurData, CurFile, Files) when
		A >= 48, A =< 57,
		B >= 48, B =< 57 ->
	Hex = try hex2int([A, B]) catch _:_ -> $\s end,
	do_parse_uenc(Tail, [ Hex | CurData], CurFile, Files);

do_parse_uenc(<<$%, A:8, B:8, Tail/binary>>, CurData, CurFile, Files) when
		A >= 48, A =< 57,
		B >= 65, B =< 70 ->
	Hex = try hex2int([A, B]) catch _:_ -> $\s end,
	do_parse_uenc(Tail, [ Hex | CurData], CurFile, Files);

do_parse_uenc(<<$%, A:8, B:8, Tail/binary>>, CurData, CurFile, Files) when
		A >= 65, A =< 70,
		B >= 48, B =< 57 ->
	Hex = try hex2int([A, B]) catch _:_ -> $\s end,
	do_parse_uenc(Tail, [ Hex | CurData], CurFile, Files);

do_parse_uenc(<<$%, A:8, B:8, Tail/binary>>, CurData, CurFile, Files) when
		A >= 65, A =< 70,
		B >= 65, B =< 70 ->
	Hex = try hex2int([A, B]) catch _:_ -> $\s end,
	do_parse_uenc(Tail, [ Hex | CurData], CurFile, Files);

do_parse_uenc(<<$+, Tail/binary>>, CurData, CurFile, Files) ->
	do_parse_uenc(Tail, [ $\s | CurData], CurFile, Files);

do_parse_uenc(<<$=, Tail/binary>>, CurData, CurFile, Files) -> 
	NewData = lists:reverse(CurData),
	Name = "uenc key: " ++ filename_to_list(list_to_binary(NewData)),
	do_parse_uenc(Tail, [], CurFile#file{name=Name}, Files);

do_parse_uenc(<<$&, Tail/binary>>, CurData, CurFile, Files) ->
	NewData = lists:reverse(CurData),
	NewFile = ?BF_C(CurFile,NewData),
	do_parse_uenc(Tail, [], #file{}, [NewFile|Files]);

do_parse_uenc(<<H:8, Tail/binary>>, CurData, CurFile, Files) ->
	do_parse_uenc(Tail, [H|CurData], CurFile, Files);
do_parse_uenc(<<>>, CurData, CurFile, Files) -> 
	NewData = lists:reverse(CurData),
	NewFile = ?BF_C(CurFile,NewData),
	NewFiles = [NewFile| Files],
	{ok, lists:reverse(NewFiles)};
do_parse_uenc(undefined, _, _, _) -> none.

prettify_uenc_data(Bin) when is_list(Bin) -> prettify_uenc_data(list_to_binary(Bin));
prettify_uenc_data(Bin) when is_binary(Bin) ->
	do_prettify_uenc(Bin, []).

do_prettify_uenc(<<$%, $%, Tail/binary>>, Cur) -> do_prettify_uenc(<<$% , Tail/binary>>, Cur);

do_prettify_uenc(<<$%, $u, A:8, B:8,C:8,D:8, Tail/binary>>, Cur) when 
		A >= 48, A =< 57,
		B >= 48, B =< 57,
		C >= 48, C =< 57,
		D >= 48, D =< 57 ->
	%% non-standard encoding for Unicode characters: %uxxxx,		     
	Hex = try hex2int([A,B,C,D]) catch _:_ -> $\s end,
	BinRep = case unicode:characters_to_binary([Hex]) of
		<<Bin/binary>> -> Bin;
		_Else -> $_ end,
	do_prettify_uenc(Tail, [ BinRep | Cur]);

do_prettify_uenc(<<$%, A:8, B:8, Tail/binary>>, Cur) when
		A >= 48, A =< 57,
		B >= 48, B =< 57 ->
	Hex = try hex2int([A, B]) catch _:_ -> $\s end,
	do_prettify_uenc(Tail, [ Hex | Cur]);
               
do_prettify_uenc(<<$&, Tail/binary>>, Cur) -> do_prettify_uenc(Tail, [ $\n | Cur]);
do_prettify_uenc(<<$+, Tail/binary>>, Cur) -> do_prettify_uenc(Tail, [ $\s | Cur]);
do_prettify_uenc(<<$=, Tail/binary>>, Cur) -> do_prettify_uenc(Tail, [ <<": ">> | Cur]);

do_prettify_uenc(<<H:8, Tail/binary>>, Cur) -> do_prettify_uenc(Tail, [H|Cur]);
do_prettify_uenc(<<>>, Cur) -> {ok, lists:reverse(Cur)};
do_prettify_uenc(undefined,_) -> none.


uri_to_hr_str(Uri) when is_binary(Uri) -> uri_to_hr_str(binary_to_list(Uri));
uri_to_hr_str(("/" ++ _Rest) = Uri) -> prettify_uri(Uri);
uri_to_hr_str(Uri) ->
	Str = case string:chr(Uri, $:) of
		0 -> Uri;
		I when I < 10 -> case string:substr(Uri, I + 1) of
				"//" ++ Rest -> case string:chr(Rest, $/) of
						0 -> "";
						I2 -> string:substr(Rest, I2) end;
				_Else2 -> Uri end;
		_Else -> Uri end,

	prettify_uri(Str).

get_host(Uri) -> 
	Str = case string:chr(Uri, $:) of
		0 -> throw({error, not_a_uri_with_fqdn});
		I when I < 10 -> case string:substr(Uri, I + 1) of 
				"//" ++ Rest -> case string:chr(Rest, $/) of
						0 -> Rest;
						I2 -> string:substr(Rest, 1, I2 - 1) end;
				_ -> throw({error, not_a_uri_with_fqdn}) end;
		_ -> throw({error, not_a_uri_with_fqdn}) end,

	Str2 = case string:chr(Str, $@) of
		0 -> Str;
		I3 -> string:substr(Str, I3 + 1) end,

	drop_host_port(Str2).

drop_host_port(Host) ->
	case string:chr(Host, $:) of
		0 -> Host;
		I -> string:substr(Host, 1, I - 1) end.

prettify_uenc_data1(D) ->
	case prettify_uenc_data(D) of 
		none -> []; 
		{ok, R} -> R end.

prettify_uri("") -> "";
prettify_uri(UriStr) -> 
	Tokens = string:tokens(UriStr, "?=;&/"),
	Cleans = lists:map(fun(I) -> prettify_uenc_data1(I) end, Tokens),
	string:join(Cleans, " ").

uri_to_hr_file(Uri) ->
	RData = uri_to_hr_str(Uri),
	case RData of
		[] -> none;
		_Else -> ?BF_C(#file{name="uri-data"}, RData) end.
	

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
	case is_fn_char(C) of
		true -> normalize_fn(FN, [C|Acc]);
		false -> normalize_fn(FN, [$_|Acc]) end;
normalize_fn([], Acc) -> lists:reverse(Acc).

%%-------------------------------------------------------------------------
%% @doc Normalizes usernames.
%% @end
%%-------------------------------------------------------------------------

hash_un(UN) when is_list(UN) ->
	UN1 = mydlp_nlp:to_lower_str(UN),
	UN2 = normalize_fn(UN1),
	erlang:phash2(UN2);
hash_un(UN) when is_binary(UN) ->
	hash_un(filename_to_list(UN)); %% To user error tolerant unicode conversion
hash_un(UN) when is_atom(UN) ->
	hash_un(atom_to_list(UN)).

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
mime_category(<<"application/vnd.ms-tnef">>) -> compression;
mime_category(<<"message/rfc822">>) -> compression;
mime_category(<<"application/vnd.android.package-archive">>) -> compression;
mime_category(<<"application/x-archive">>) -> cobject;
mime_category(<<"application/x-executable">>) -> cobject;
mime_category(<<"application/x-sharedlib">>) -> cobject;
mime_category(<<"application/x-object">>) -> cobject;
mime_category(<<"application/java-vm">>) -> binary_format;
mime_category(<<"image/cgm">>) -> image;
mime_category(<<"image/g3fax">>) -> image;
mime_category(<<"image/gif">>) -> image;
mime_category(<<"image/ief">>) -> image;
mime_category(<<"image/jpeg">>) -> image;
mime_category(<<"image/pjpeg">>) -> image;
mime_category(<<"image/x-jpeg">>) -> image;
mime_category(<<"image/jpg">>) -> image;
mime_category(<<"image/pjpg">>) -> image;
mime_category(<<"image/x-jpg">>) -> image;
mime_category(<<"image/jpe">>) -> image;
mime_category(<<"image/naplps">>) -> image;
mime_category(<<"image/pcx">>) -> image;
mime_category(<<"image/png">>) -> image;
mime_category(<<"image/x-png">>) -> image;
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
mime_category(<<"image/x-ico">>) -> image;
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
mime_category(<<"image/",_/binary>>) -> image;
mime_category(<<"audio/",_/binary>>) -> audio;
mime_category(<<"video/",_/binary>>) -> video;
mime_category(<<"text/",_/binary>>) -> text;
mime_category(<<"application/pgp-encrypted">>) -> encrypted;
mime_category(<<"application/pgp-keys">>) -> encrypted;
mime_category(<<"application/pgp-signature">>) -> encrypted;
mime_category(<<"application/pkcs10">>) -> encrypted;
mime_category(<<"application/pkcs7-mime">>) -> encrypted;
mime_category(<<"application/pkcs7-signature">>) -> encrypted;
mime_category(<<"application/pkix-cert">>) -> encrypted;
mime_category(<<"application/pkix-crl">>) -> encrypted;
mime_category(<<"application/pkix-pkipath">>) -> encrypted;
mime_category(<<"application/pkixcmp">>) -> encrypted;
mime_category(<<"application/x-x509-ca-cert">>) -> encrypted;
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

binary_size([]) -> 0;
binary_size([_|_] = Obj) -> binary_size(lists:flatten(Obj), 0);
binary_size(Obj) -> binary_size(Obj, 0).

binary_size(<<Obj/binary>>, Acc) -> Acc + size(Obj);
binary_size([] , Acc) -> Acc;
binary_size([O|Rest], Acc) ->
	binary_size(Rest, Acc + binary_size(O));
binary_size(Obj, _Acc) when is_integer(Obj), 0 =< Obj, Obj =< 255 -> 1;
binary_size(Obj, _Acc) when is_integer(Obj) -> throw({error, bad_integer, Obj});
binary_size(Obj, _Acc) when is_atom(Obj) -> throw({error, bad_element_atom, Obj});
binary_size(Obj, _Acc) when is_tuple(Obj) -> throw({error, bad_element_tuple, Obj});
binary_size(Obj, _Acc) -> throw({error, bad_element, Obj}).

%%-------------------------------------------------------------------------
%% @doc Converts mime objects to files
%% @end
%%-------------------------------------------------------------------------

filename_to_list(Filename) ->
        case Filename of
                F when is_binary(F) ->
                        case unicode:characters_to_list(Filename) of
                                R when is_list(R) -> R;
                                _ ->    try binary_to_list(Filename)
                                        catch _:_ ->    ?ERROR_LOG("FTL: Encountered with a filename in an unexpected encoding. Filename: ["?S"]", [Filename]),
                                                        "noname"
                                        end
                        end;
                F when is_list(F) -> F;
                Else -> ?ERROR_LOG("Encountered with a filename in an unexpected type. Filename: ["?S"]", [Else]),
                        "noname" end.

filename_to_bin(Filename) ->
        case Filename of
                F when is_list(F) ->
                        case unicode:characters_to_binary(Filename) of
                                R when is_binary(R) -> R;
                                _ ->    try list_to_binary(Filename)
                                        catch _:_ ->    ?ERROR_LOG("FTB: Encountered with a filename in an unexpected encoding. Filename: ["?S"]", [Filename]),
                                                        <<"noname">>
                                        end
                        end;
                F when is_binary(F) -> F;
                Else -> ?ERROR_LOG("Encountered with a filename in an unexpected type. Filename: ["?S"]", [Else]),
                        <<"noname">> end.

heads_to_file_int1(Str, QS, CT) ->
	case string:str(Str, QS) of
		0 -> ?ERROR_LOG("Improper composition of Content-Type line "
				"Content-Type: "?S"~n", [CT]), "noname";
		1 -> "noname";
		L -> string:substr(Str, 1, L - 1) end.

heads_to_file(Headers) -> heads_to_file(Headers, #file{meta=[{mime_headers, Headers}]}).

heads_to_file([{'content-disposition', "inline"}|Rest], #file{filename=undefined, name=undefined} = File) ->
	case lists:keysearch('content-type',1,Rest) of
		{value,{'content-type',"text/plain"}} -> heads_to_file(Rest, File#file{name="Inline text message", given_type="text/plain"});
		{value,{'content-type',"text/html"}} -> heads_to_file(Rest, File#file{name="Inline HTML message", given_type="text/html"});
		_Else -> heads_to_file(Rest, File)
	end;
heads_to_file([{'content-disposition', CD}|Rest], #file{filename=undefined} = File) ->
	case cd_to_fn(CD) of
		none -> heads_to_file(Rest, File);
		FN -> 	heads_to_file(Rest, File#file{filename=FN})
	end;
heads_to_file([{'content-type', "text/html"}|Rest], #file{filename=undefined, name=undefined} = File) ->
	case lists:keysearch('content-disposition',1,Rest) of
		{value,{'content-disposition',"inline"}} -> heads_to_file(Rest, File#file{name="Inline HTML message", given_type="text/html"});
		_Else -> heads_to_file(Rest, File)
	end;
heads_to_file([{'content-type', "text/plain"}|Rest], #file{filename=undefined, name=undefined} = File) ->
	case lists:keysearch('content-disposition',1,Rest) of
		{value,{'content-disposition',"inline"}} -> heads_to_file(Rest, File#file{name="Inline text message", given_type="text/plain"});
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
				"\\\"" ++ Str -> heads_to_file_int1(Str, "\\\"", CT);
				"\"" ++ Str -> heads_to_file_int1(Str, "\"", CT);
				Str -> Str
			end,
			FN1 = multipart_decode_fn(FN),
			heads_to_file(Rest, File#file{filename=FN1})
	end;
heads_to_file([{'content-id', CI}|Rest], #file{filename=undefined, name=undefined} = File) ->
	heads_to_file(Rest, File#file{name=CI});
heads_to_file([{subject, Subject}|Rest], #file{filename=undefined, name=undefined} = File) ->
	heads_to_file(Rest, File#file{name=multipart_decode_fn(Subject)});
heads_to_file([_|Rest], File) ->
	ignore,	heads_to_file(Rest, File);
heads_to_file([], File) -> File.

find_char_in_range(Str, Range, Char) -> find_char_in_range(Str, Range, Char, 0).

find_char_in_range(_Str, Range, _Char, Range = _Acc) -> not_found;
find_char_in_range([Char|_Rest], _Range, Char, Acc) -> Acc + 1;
find_char_in_range([_C|Rest], Range, Char, Acc) -> find_char_in_range(Rest, Range, Char, Acc + 1);
find_char_in_range([], _Range, _Char, _Acc) -> not_found.

multipart_decode_fn(Filename0) -> 
	Filename = filename_to_list(list_to_binary(Filename0)),
	multipart_decode_fn_1(Filename).

multipart_decode_fn_1(Filename) -> multipart_decode_fn_1(Filename, [], []).

multipart_decode_fn_1([C|Filename], Current0, Acc) 
		when C == $\s; C == $\r; C == $\n; C == $\t ->
	Current = lists:reverse(Current0),
	Current1 = multipart_decode_fn_part(Current, C),
	multipart_decode_fn_1(Filename, [], [ Current1 | Acc]);
multipart_decode_fn_1([C|Filename], Current, Acc) ->
	multipart_decode_fn_1(Filename, [C|Current], Acc);
multipart_decode_fn_1([], [], Acc) -> lists:append(lists:reverse(Acc)); % return
multipart_decode_fn_1([], Current0, Acc) -> 
	Current = lists:reverse(Current0),
	Current1 = multipart_decode_fn_part(Current, none),
	multipart_decode_fn_1([], [], [ Current1 | Acc]).

multipart_decode_fn_part(("=?" ++ _Rest) = FilenamePart, _C) -> multipart_decode_fn_rfc2047(FilenamePart);
multipart_decode_fn_part(FilenamePart, none) -> multipart_decode_fn_xml(FilenamePart);
multipart_decode_fn_part(FilenamePart, C) -> multipart_decode_fn_xml(FilenamePart) ++ [C].

multipart_decode_fn_rfc2047("=?" ++ Rest) -> 
	case string:chr(Rest, $?) of
		0 -> multipart_decode_fn(Rest);
		I -> 	Charset = string:substr(Rest, 1, I - 1),
			case string:substr(Rest, I) of
				[$?, Encoding, $? | Rest2 ] -> 
					case string:chr(Rest2, $?) of
						0 -> multipart_decode_fn(Rest);
						I2 -> 	Data = string:substr(Rest2, 1, I2 - 1),
							case string:substr(Rest2, I2) of
								[$?, $=] -> multipart_decode_fn_rfc2047(Charset, Encoding, Data);
								_Else -> multipart_decode_fn(Rest) end end;
				_Else -> multipart_decode_fn(Rest) end end.

multipart_decode_fn_rfc2047(Charset, Encoding, Data) ->
	multipart_decode_fn_rfc2047_1(string:to_lower(Charset), string:to_lower(Encoding), Data).
	
multipart_decode_fn_rfc2047_1("unicode", Encoding, Data) -> multipart_decode_fn_rfc2047_2({api, unicode}, Encoding, Data);
multipart_decode_fn_rfc2047_1("utf-8", Encoding, Data) -> multipart_decode_fn_rfc2047_2({api, unicode}, Encoding, Data);
multipart_decode_fn_rfc2047_1("utf-16", Encoding, Data) -> multipart_decode_fn_rfc2047_2({api, utf16}, Encoding, Data);
multipart_decode_fn_rfc2047_1("utf-32", Encoding, Data) -> multipart_decode_fn_rfc2047_2({api, utf32}, Encoding, Data);
multipart_decode_fn_rfc2047_1("latin1", Encoding, Data) -> multipart_decode_fn_rfc2047_2({api, latin1}, Encoding, Data);
multipart_decode_fn_rfc2047_1(Charset, Encoding, Data) -> multipart_decode_fn_rfc2047_2({backend, string:to_upper(Charset)}, Encoding, Data).

multipart_decode_fn_rfc2047_2(Charset, $Q, Data) -> multipart_decode_fn_rfc2047_3(Charset, quoted_printable, Data);
multipart_decode_fn_rfc2047_2(Charset, $q, Data) -> multipart_decode_fn_rfc2047_3(Charset, quoted_printable, Data);
multipart_decode_fn_rfc2047_2(Charset, $B, Data) -> multipart_decode_fn_rfc2047_3(Charset, base64, Data);
multipart_decode_fn_rfc2047_2(Charset, $b, Data) -> multipart_decode_fn_rfc2047_3(Charset, base64, Data);
multipart_decode_fn_rfc2047_2(Charset, _Else , Data) -> multipart_decode_fn_rfc2047_3(Charset, quoted_printable, Data).

multipart_decode_fn_rfc2047_3(Charset, base64, Base64Str) ->
	DataBin = try base64:decode(Base64Str)
	catch 	Class:Error ->
		?ERROR_LOG("Error occured when base64 decoding: "
			"Class: ["?S"]. Error: ["?S"].~n"
			"Stacktrace: "?S"~n"
			"Base64Str: "?S"~n",
			[Class, Error, erlang:get_stacktrace(), Base64Str]),
		list_to_binary(Base64Str) end,
	multipart_decode_fn_rfc2047_4(Charset, DataBin);
multipart_decode_fn_rfc2047_3(Charset, quoted_printable, QPStr) ->
	QPStr1 = multipart_decode_fn_rfc2047_3_1(QPStr),
	DataBin = try quoted_to_raw(QPStr1)
	catch 	Class:Error ->
		?ERROR_LOG("Error occured when base64 decoding: "
			"Class: ["?S"]. Error: ["?S"].~n"
			"Stacktrace: "?S"~n"
			"QPStr: "?S"~n",
			[Class, Error, erlang:get_stacktrace(), QPStr]),
		list_to_binary(QPStr) end,
	multipart_decode_fn_rfc2047_4(Charset, DataBin).

multipart_decode_fn_rfc2047_3_1(QPStr) -> multipart_decode_fn_rfc2047_3_1(QPStr, []).

multipart_decode_fn_rfc2047_3_1([$_|Rest], Acc) -> multipart_decode_fn_rfc2047_3_1(Rest, [$0,$2,$=|Acc]);
multipart_decode_fn_rfc2047_3_1([C|Rest], Acc) -> multipart_decode_fn_rfc2047_3_1(Rest, [C|Acc]);
multipart_decode_fn_rfc2047_3_1([], Acc) -> lists:reverse(Acc).
	
multipart_decode_fn_rfc2047_4({api, Charset}, DataBin) ->
	B = list_to_binary([DataBin]),
	case unicode:characters_to_list(B, Charset) of
		R when is_list(R) -> R;
		_ -> 	try binary_to_list(DataBin)
			catch _:_ -> ?ERROR_LOG("Error occured when unicode decoding: "
					"DataBin: ["?S"]~n", [DataBin]), "noname" 
			end
	end;
multipart_decode_fn_rfc2047_4({backend, Charset}, DataBin) ->
	B = list_to_binary([DataBin]),
	UnicodeText = case mydlp_tc:get_unicode_text(Charset, B) of
		{error, _Error} -> ?ERROR_LOG("Error occurred when getting unicode text.", []), B;
		T when is_binary(T) -> T end,
	multipart_decode_fn_rfc2047_4({api, unicode}, UnicodeText).


multipart_decode_fn_xml(Filename) -> multipart_decode_fn_xml(Filename, []).

multipart_decode_fn_xml([$&, $#, $X|Filename], Acc) -> multipart_decode_fn_xml([$&, $#, $x|Filename], Acc);
multipart_decode_fn_xml([$&, $#, $x|Filename], Acc) -> 
	case find_char_in_range(Filename, 9, $;) of
		not_found -> multipart_decode_fn_xml(Filename, [$x, $#, $&|Acc]);
		1 -> multipart_decode_fn_xml(Filename, [$x, $#, $&|Acc]);
		I when is_integer(I) -> 
			HexStr = string:substr(Filename, 1, I - 1),
			Char = hex2int(HexStr),
			Rest = string:substr(Filename, I + 1),
			multipart_decode_fn_xml(Rest, [Char|Acc]) end;

multipart_decode_fn_xml([$&, $#|Filename], Acc) -> 
	case find_char_in_range(Filename, 9, $;) of
		not_found -> multipart_decode_fn_xml(Filename, [$#, $&|Acc]);
		1 -> multipart_decode_fn_xml(Filename, [$#, $&|Acc]);
		I when is_integer(I) -> 
			IntStr = string:substr(Filename, 1, I - 1),
			case lists:all(fun(C) -> (($0 =< C) and (C =< $9)) end, IntStr) of
				false -> multipart_decode_fn_xml(Filename, [$#, $&|Acc]);
				true -> Char = list_to_integer(IntStr),
					Rest = string:substr(Filename, I + 1),
					multipart_decode_fn_xml(Rest, [Char|Acc]) end end;

multipart_decode_fn_xml([$&|Filename], Acc) -> 
	case find_char_in_range(Filename, 9, $;) of
		not_found -> multipart_decode_fn_xml(Filename, [$&|Acc]);
		1 -> multipart_decode_fn_xml(Filename, [$&|Acc]);
		I when is_integer(I) -> 
			XmlStr = string:substr(Filename, 1, I - 1),
			Rest = string:substr(Filename, I + 1),
			case mydlp_nlp:xml_char(XmlStr) of
				Char when is_integer(Char) -> multipart_decode_fn_xml(Rest, [Char|Acc]);
				not_found -> case mydlp_nlp:xml_char(string:to_lower(XmlStr)) of
					Char when is_integer(Char) -> multipart_decode_fn_xml(Rest, [Char|Acc]);
					not_found -> multipart_decode_fn_xml(Filename, [$&|Acc]) end end end;

multipart_decode_fn_xml([C|Filename], Acc) -> multipart_decode_fn_xml(Filename, [C|Acc]);
multipart_decode_fn_xml([], Acc) -> lists:reverse(Acc).

mime_to_files(Mime) -> mime_to_files([Mime], []).

mime_to_files([#mime{content=Content, header=Headers, body=Body}|Rest], Acc) ->
	File = heads_to_file(Headers),
	CTE = case lists:keysearch('content-transfer-encoding',1,Headers) of
		{value,{'content-transfer-encoding',Value}} -> Value;
		_ -> '7bit'
	end,
	Data = 	try	mime_util:decode_content(CTE, Content)
		catch 	Class:Error ->
			?ERROR_LOG("Error occured when decoding: "
				"Class: ["?S"]. Error: ["?S"].~n"
				"Stacktrace: "?S"~n"
				"Content-Transfer-Encoding: "?S"~n.Content: "?S"~n",
				[Class, Error, CTE, erlang:get_stacktrace(), Content]),
			Content end,
	mime_to_files(lists:append(Body, Rest), [?BF_C(File,Data)|Acc]);
mime_to_files([], Acc) -> lists:reverse(Acc).

files_to_mime_encoded([#file{meta=[{mime_headers, Headers}]} = File]) ->
	MimeEncap = #file{meta=[{mime_headers, Headers}]},
	File1 = File#file{meta=[{mime_headers, []}]},
	Files = [MimeEncap, File1],
	files_to_mime_encoded(Files);
files_to_mime_encoded(Files) -> 
	Boundary = list_to_binary(get_random_string()),
	files_to_mime_encoded(Files, Boundary, <<>>).

files_to_mime_encoded([File|RestOfFiles], Boundary, Acc) -> 
	IsFirst = case Acc of
		<<>> -> true;
		_ -> false end,
	HeadersBin = case IsFirst of
		true -> headers_to_mime_encoded(File, Boundary);
		false -> headers_to_mime_encoded(File) end,
	ContentBin = content_to_mime_encoded(File),
	Tail = case ContentBin of
		<<>> -> <<"\r\n">>;
		_ -> <<ContentBin/binary, "\r\n\r\n">> end,
	NewAcc = case IsFirst of
		true -> <<HeadersBin/binary, "\r\n", Tail/binary>>;
		false -> <<Acc/binary, "--", Boundary/binary, "\r\n", HeadersBin/binary, "\r\n", Tail/binary>> end,
	files_to_mime_encoded(RestOfFiles, Boundary, NewAcc);
files_to_mime_encoded([], Boundary, Acc) -> <<Acc/binary, "--", Boundary/binary, "--\r\n">>.

content_to_mime_encoded(File0) ->
	File = load_file(File0),
	Base64 = case File#file.data of
		<<>> -> <<>>;
		undefined -> <<>>;
		Data -> base64:encode(Data) end,
	prettify_base64(Base64).

prettify_base64(Base64) -> prettify_base64(Base64, <<>>).

prettify_base64(Base64, Acc) when size(Base64) > 80 ->
	<<C:80/binary, Rest/binary>> = Base64,
	prettify_base64(Rest, <<Acc/binary, C/binary, "\r\n">>);
prettify_base64(Base64, Acc) ->
	<<Acc/binary, Base64/binary>>.

	
headers_to_mime_encoded(File) -> headers_to_mime_encoded(File, undefined).

headers_to_mime_encoded(#file{meta=[{mime_headers, Headers}], filename=FN, given_type=GT}, Boundary) -> 
	Headers1 = override_mime_headers(Headers, FN, GT, Boundary),
	headers_to_mime_encoded1(Headers1, <<>>).

headers_to_mime_encoded1([{KeyAtom, Value}|RestOfHeaders], Acc) ->
	KeyS = atom_to_list(KeyAtom),
	Key = to_mime_key_upper(KeyS),
	KeyB = list_to_binary([Key]),
	ValueS = case KeyAtom of
		from -> encode_smtp_addr(Value);
		to -> encode_smtp_addrs(Value);
		cc -> encode_smtp_addrs(Value);
		bcc -> encode_smtp_addrs(Value);
		_ -> Value end,
	ValueB = list_to_binary([ValueS]),
	NewAcc = <<Acc/binary, KeyB/binary, ": ", ValueB/binary, "\r\n">>,
	headers_to_mime_encoded1(RestOfHeaders, NewAcc);
headers_to_mime_encoded1([], Acc) -> Acc.

to_mime_key_upper([F|Str]) -> to_mime_key_upper(Str, string:to_upper([F])).

to_mime_key_upper([$-,C|Str], Acc) -> to_mime_key_upper(Str, string:to_upper([C]) ++ "-" ++ Acc);
to_mime_key_upper([C|Str], Acc) -> to_mime_key_upper(Str, [C|Acc]);
to_mime_key_upper([], Acc) -> lists:reverse(Acc).


override_mime_headers(Headers, FN, GT0, Boundary) ->
	GT = case Boundary of
		undefined -> GT0;
		_ -> "multipart/mixed; boundary=\"" ++ binary_to_list(Boundary) ++ "\"" end,

	Headers1 = case lists:keyfind('content-type', 1, Headers) of
		false -> 		Headers ++ [{'content-type', GT}];
		{'content-type', _} -> 	lists:keyreplace('content-type', 1, Headers, {'content-type', GT}) end,

	Headers2 = case lists:keyfind('content-transfer-encoding', 1, Headers1) of
		false -> 				Headers1 ++ [{'content-transfer-encoding', "base64"}];
		{'content-transfer-encoding', _} -> 	lists:keyreplace('content-transfer-encoding', 1, Headers1, {'content-transfer-encoding', "base64"}) end,

	Headers3 = case lists:keyfind('content-disposition', 1, Headers2) of
		false -> case FN of
			"" -> 		Headers2 ++ [{'content-disposition', "inline"}];
			undefined -> 	Headers2 ++ [{'content-disposition', "inline"}];
			_ -> 		Headers2 ++ [{'content-disposition', "attachment; filename=\"" ++ encode_rfc2047(FN) ++ "\""}] end;
		{'content-disposition', _} -> case FN of
			"" -> 		lists:keyreplace('content-disposition', 1, Headers2, {'content-disposition', "inline"});
			undefined -> 	lists:keyreplace('content-disposition', 1, Headers2, {'content-disposition', "inline"});
			_ ->	 	lists:keyreplace('content-disposition', 1, Headers2, {'content-disposition', "attachment; filename=\"" ++ encode_rfc2047(FN) ++ "\""}) end end,

	Headers4 = lists:keydelete('content-length', 1, Headers3),
	Headers4.

encode_rfc2047(S) -> 
	"=?UTF-8?B?" ++ [base64:encode(unicode:characters_to_binary(S))] ++ "?=".
	
encode_smtp_addrs(Addrs) -> 
	Strings = lists:map(fun(A) -> encode_smtp_addr(A) end, Addrs),
	string:join(Strings, ", ").

encode_smtp_addr({addr, User, Domain, Name}) ->	
	B = list_to_binary([$", encode_rfc2047(Name), $", $\s, User, $@, Domain]),
	binary_to_list(B). %% TODO: workaround

random_uniform_256() ->
	RandByte = get_random_bytes(1),
	<<Int:8/integer>> = RandByte,
	Int.

random_uniform(Length) when is_integer(Length), Length > 1, Length < 255 ->
	Int = random_uniform_256(),
	( Int rem Length ) + 1.

get_random_string() ->
	AllowedChars = "qwertyuioplkjhgfdsazxcvbnm0987654321QWERTYUIOPLKJHGFDSAZXCVBNM",
	Length = 32,
	lists:foldl(fun(_, Acc) ->
		[lists:nth(random_uniform(Length) , AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).


get_random_bytes() -> get_random_bytes(16).

get_random_bytes(Size) when is_integer(Size) -> crypto:rand_bytes(Size).



%%-------------------------------------------------------------------------
%% @doc Extracts filename from value of content disposition header
%% @end
%%-------------------------------------------------------------------------

cd_to_fn(ContentDisposition) ->
	ExtFN = case string:str(ContentDisposition, "filename=") of
		0 -> none; 
		I ->	FNVal = string:strip(string:substr(ContentDisposition, I + 9)),
			FNVal1 = string:strip(FNVal, right, $;),
			case FNVal1 of
				"\\\"" ++ Str -> 
					Len = string:len(Str),
					case string:substr(Str, Len - 1) of
						"\"\\" -> ok;
						"\\\"" -> ok end,
					string:substr(Str, 1, Len - 2);
				"\"" ++ Str -> 
					Len = string:len(Str),
					"\"" = string:substr(Str, Len),
					string:substr(Str, 1, Len - 1);
				Str -> Str end end,
	case ExtFN of
		none -> none;
		_Else -> multipart_decode_fn(ExtFN) end.

%%-------------------------------------------------------------------------
%% @doc Select chuck from files
%% @end
%%-------------------------------------------------------------------------
get_chunk(Files) -> get_chunk(0, Files, []).

get_chunk(_TotalSize, [], InChunk) -> {InChunk, []};
get_chunk(TotalSize, [#file{dataref=Ref} = File|Files], InChunk) ->
	case TotalSize < ?CFG(maximum_chunk_size) of
		true -> FS = ?BB_S(Ref),
			get_chunk(TotalSize + FS, Files, [File|InChunk]);
		false -> {InChunk, [File|Files]} end.
	
%%-------------------------------------------------------------------------
%% @doc Prints report browser output to specified file.
%% @end
%%-------------------------------------------------------------------------

rb_to_fie(Filename) when is_list(Filename) ->
	try	rb:start(),
		rb:start_log(Filename),
		rb:show(),
		rb:stop_log(),
		rb:stop()
	catch Class:Error -> {"Error occurred.", Class, Error} end.

%%-------------------------------------------------------------------------
%% @doc Extracts texts from file and inner files and concats them.
%% @end
%%-------------------------------------------------------------------------
concat_texts(#file{} = File) -> concat_texts([File]);
concat_texts(Files) when is_list(Files) -> 
	Files1 = extract_all(Files),
	concat_texts(Files1, []).

concat_texts([File|Files], Returns) ->
	case get_text(File) of
		{ok, Txt} -> concat_texts(Files, [<<"\n">>, Txt| Returns]);
		_Else -> concat_texts(Files, Returns)
	end;
concat_texts([], Returns) -> list_to_binary(lists:reverse(Returns)).

extract_all(Files) -> extract_all(Files, []).

extract_all([], Return) -> Return;
extract_all(Files, Return) ->
	Files1 = load_files(Files),
	{PFiles, NewFiles} = analyze(Files1),
	PFiles1 = clean_files(PFiles),
	extract_all(NewFiles, lists:append(Return, PFiles1)).

%%-------------------------------------------------------------------------
%% @doc Converts a binary, list or file record term to file record.
%% @end
%%-------------------------------------------------------------------------

term2file(Term) when is_binary(Term) -> ?BF_C(#file{}, Term);
term2file(#file{} = Term) -> Term;
term2file(Term) when is_list(Term) ->
	{ok, Bin} = file:read_file(Term),
	?BF_C(#file{}, Bin).

%%-------------------------------------------------------------------------
%% @doc Creates an empty acl result tuple with given files
%% @end
%%-------------------------------------------------------------------------

empty_aclr(Files) -> empty_aclr(Files, "").

%%-------------------------------------------------------------------------
%% @doc Creates an empty acl result tuple with given files and itype id
%% @end
%%-------------------------------------------------------------------------

empty_aclr(Files, Misc) -> empty_aclr(-1, Files, Misc).

%%-------------------------------------------------------------------------
%% @doc Creates an empty acl result tuple with given rule id,  files and itype id
%% @end
%%-------------------------------------------------------------------------

empty_aclr(RuleId, Files, Misc) -> {{rule, RuleId}, {file, Files}, {itype, -1}, {misc, Misc}, {matching_details, []}}.


%%-------------------------------------------------------------------------
%% @doc Spawns a process, monitors it and kills after timeout
%% @end
%%-------------------------------------------------------------------------

mspawn(Fun) -> mspawn(Fun, ?CFG(spawn_timeout)).

mspawn(Fun, Timeout) ->
	Pid = spawn(Fun),
	mspawntimer(Timeout, Pid),
	Pid.

mspawn_link(Fun) -> mspawn_link(Fun, ?CFG(spawn_timeout)).

mspawn_link(Fun, Timeout) ->
	Pid = spawn_link(Fun),
	mspawntimer(Timeout, Pid),
	Pid.

mspawntimer(Timeout, Pid) ->
	{ok, _} = timer:exit_after(Timeout, Pid, timeout).

%%-------------------------------------------------------------------------
%% @doc Paralel mapping function with managed spawns
%% @end
%%-------------------------------------------------------------------------

pmap(Fun, ListOfArgs) -> pmap(Fun, ListOfArgs, ?CFG(spawn_timeout)).

pmap(Fun, ListOfArgs, Timeout) ->
	Self = self(),
	Pids = lists:map(fun(I) -> mspawn_link(fun() -> pmap_f(Self, Fun, I) end, Timeout) end, ListOfArgs),
	pmap_gather(Pids, Timeout).

pmap_gather(Pids, Timeout) -> pmap_gather(Pids, Timeout, []).

pmap_gather([Pid|Rest], Timeout, Returns) ->
	Return = receive
		{Pid, {ierror, {Class, Error}}} -> exception(Class, Error);
		{Pid, Ret} -> Ret
	after Timeout + 1000 ->
		exit({timeout, {pmap, Pid}})
	end,
	pmap_gather(Rest, Timeout, [Return|Returns]);
pmap_gather([], _Timeout, Returns) -> lists:reverse(Returns).

pmap_f(Parent, Fun, I) ->
	Message = try
		Ret = Fun(I),
		{self(), Ret}
	catch Class:Error ->
		{self(), {ierror, {Class, {Error, erlang:get_stacktrace()}}}} end,
	Parent ! Message.

%%-------------------------------------------------------------------------
%% @doc Paralel lists:all function with managed spawns, stacks and return anything other than false or neg
%% @end
%%-------------------------------------------------------------------------
pall(Fun, ListOfArgs) -> pall(Fun, ListOfArgs, ?CFG(spawn_timeout)).

pall(Fun, ListOfArgs, Timeout) -> pany(Fun, ListOfArgs, Timeout, true).

%%-------------------------------------------------------------------------
%% @doc Paralel lists:any function with managed spawns, returns for anything other than false or neg
%% @end
%%-------------------------------------------------------------------------

pany(Fun, ListOfArgs) -> pany(Fun, ListOfArgs, ?CFG(spawn_timeout)).

pany(Fun, ListOfArgs, Timeout) -> pany(Fun, ListOfArgs, Timeout, false).

pany(Fun, ListOfArgs, Timeout, IsPAll) ->
	Self = self(),
	Pid = mspawn(fun() -> pany_sup_f(Self, Fun, ListOfArgs, Timeout, IsPAll) end, Timeout + 1500),
	pany_gather(Pid, Timeout).

pany_gather(Pid, Timeout) ->
	receive
		{Pid, {ierror, {Class, Error}}} -> exception(Class, Error);
		{Pid, Ret} -> Ret
	after Timeout + 2000 ->
		exit({timeout, {pany, Pid}})
	end.

pany_sup_f(Parent, Fun, ListOfArgs, Timeout, IsPAll) -> 
	Self = self(),
	Pids = lists:map(fun(I) -> mspawn_link(fun() -> pany_child_f(Self, Fun, I, Timeout) end, Timeout + 500) end, ListOfArgs),

	NumberOfPids = length(Pids),
	pany_sup_gather(NumberOfPids, Parent, Timeout, IsPAll, []).

pany_sup_gather(0, Parent, _Timeout, false = _IsPAll, _Results) -> 
	pany_sup_return(Parent, false);
pany_sup_gather(NumberOfPids, Parent, Timeout, false = IsPAll, Results) ->
	receive
		{_Pid, _I, false} -> 		pany_sup_gather(NumberOfPids - 1, Parent, Timeout, IsPAll, Results);
		{_Pid, _I, neg} -> 		pany_sup_gather(NumberOfPids - 1, Parent, Timeout, IsPAll, Results);
		{_Pid, _I, negative} -> 	pany_sup_gather(NumberOfPids - 1, Parent, Timeout, IsPAll, Results);
		{_Pid, {ierror, Reason}} -> 	pany_sup_return(Parent, {ierror, Reason} ),
						exit({pany_error});
		{_Pid, {timeout, I, T}} -> 	pany_sup_return(Parent, {ierror, {exit, {timeout, I, T}}} ),
						exit({pany_timeout});
		{_Pid, I, Else} -> 		pany_sup_return(Parent, {ok, I, Else}),
						exit({pany_returned})
	after Timeout + 1000 -> % delay for receiving other timeout messages
		exit({timeout, pany_sup_gather}) % not needed to emit to parent
	end;
pany_sup_gather(0, Parent, _Timeout, true = _IsPAll, Results) -> 
	pany_sup_return(Parent, {ok, lists:reverse(Results)});
pany_sup_gather(NumberOfPids, Parent, Timeout, true = IsPAll, Results) ->
	receive
		{_Pid, _I, false} -> 		pany_sup_return(Parent, false),
						exit({pany_returned});
		{_Pid, _I, neg} -> 		pany_sup_return(Parent, false),
						exit({pany_returned});
		{_Pid, _I, negative} -> 	pany_sup_return(Parent, false),
						exit({pany_returned});
		{_Pid, {ierror, Reason}} -> 	pany_sup_return(Parent, {ierror, Reason} ),
						exit({pany_error});
		{_Pid, {timeout, I, T}} -> 	pany_sup_return(Parent, {ierror, {exit, {timeout, I, T}}} ),
						exit({pany_timeout});
		{_Pid, _I, Else} -> 		pany_sup_gather(NumberOfPids - 1, Parent, Timeout, IsPAll, [Else|Results])
	after Timeout + 1000 -> % delay for receiving other timeout messages
		exit({timeout, pany_sup_gather}) % not needed to emit to parent
	end.

pany_sup_return(Parent, Return) -> Parent ! {self(), Return}.

pany_child_f(Parent, Fun, I, Timeout) -> 
	{ok, TRef} = timer:send_after(Timeout, Parent, {self(), {timeout, I, Timeout}}),
	Message = try
		Ret = Fun(I),
		{self(), I, Ret}
	catch Class:Error ->
		{self(), {ierror, {Class, {Error, erlang:get_stacktrace()}}}} end,
	timer:cancel(TRef),
	Parent ! Message.

%%-------------------------------------------------------------------------
%% @doc Reproduces exception for given type
%% @end
%%-------------------------------------------------------------------------

exception(error, Reason) -> erlang:error(Reason);
exception(throw, Reason) -> erlang:throw(Reason);
exception(exit, Reason) -> erlang:exit(Reason).

%%-------------------------------------------------------------------------
%% @doc Converts given string to ip address tuple.
%% @end
%%-------------------------------------------------------------------------

str_to_ip(IpStr) -> 
	Tokens = string:tokens(IpStr,"."),
	[_,_,_,_] = Tokens,
	Ints = lists:map(fun(S) -> list_to_integer(S) end, Tokens),
	case lists:any(fun(I) -> ( I > 255 ) or ( I < 0 ) end, Ints) of
		true -> throw({error, {bad_ip, Ints}});
		false -> ok end,
	[I1,I2,I3,I4] = Ints,
	{I1,I2,I3,I4}.

-ifdef(__MYDLP_NETWORK).

%%-------------------------------------------------------------------------
%% @doc Returns ruletable for given ip address if necessary
%% @end
%%-------------------------------------------------------------------------
%%%%%%%%%%%%% TODO: beware of race condifitons when compile_customer had been called.
generate_client_policy(EndpointId, IpAddr, UserH, RevisionId) -> 
	RuleTables = mydlp_acl:get_remote_rule_tables(IpAddr, UserH),
	ItemDump = mydlp_mnesia:dump_client_tables(),
	MCModule = mydlp_mnesia:get_remote_mc_module(mydlp_mnesia:get_dfid(), IpAddr, UserH),
	CDBObj = {{rule_tables, RuleTables}, {mc, MCModule}, {items, ItemDump}},
	CDBHash = erlang:phash2(CDBObj),
	Commands = mydlp_mnesia:get_endpoint_commands(EndpointId),
	case {CDBHash, Commands} of
		{RevisionId, []} -> <<"up-to-date">>;
		{RevisionId, Commands} -> erlang:term_to_binary(Commands, [compressed]);
		{_Else, []} -> erlang:term_to_binary(CDBObj, [compressed]);
		{_Else, Commands} -> erlang:term_to_binary([CDBObj|Commands], [compressed]) end.

-endif.

-ifdef(__MYDLP_ENDPOINT).

use_client_policy(<<>>) -> ?ERROR_LOG("USE_CLIENT_POLICY: Management server returned empty response.", []), ok;
use_client_policy(<<"up-to-date">>) -> ok;
use_client_policy(CDBBin) ->
	try	CDBObj = erlang:binary_to_term(CDBBin), % TODO: binary_to_term/2 with safe option
		apply_cdbobj(CDBObj)
	catch Class:Error ->
		?ERROR_LOG("USE_CLIENT_POLICY: Error occured: Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~nCDBBin: ["?S"].~n",
			[Class, Error, erlang:get_stacktrace(), CDBBin]),
		RevisionId = get_client_policy_revision_id(),
		mydlp_sync:set_policy_id(RevisionId)
	end,
	ok.

apply_cdbobj(L) when is_list(L) -> lists:foreach(fun(C) -> apply_cdbobj(C) end, L); % TODO: Updating mnesia rule table before commands should be guarented.
apply_cdbobj({{rule_tables, RuleTables}, {mc, MCModule}, {items, ItemDump}}=PolicyObj) ->
	mydlp_mnesia:truncate_nondata(),
	( catch mydlp_mnesia:write(ItemDump) ),
	( catch mydlp_mnesia:write([ MCModule ]) ),
	( catch mydlp_mnesia:write([ #rule_table{channel=C, destination=D,table = RT} || {C, D, RT} <- RuleTables ]) ),

	NewRevisionId = erlang:phash2(PolicyObj),
	mydlp_sync:set_policy_id(NewRevisionId),

	mydlp_mnesia:post_start(),
	populate_win32reg(),
	mydlp_tc:load(),
	mydlp_container:schedule_confupdate(),
	ok;
apply_cdbobj({command, L}) when is_list(L) -> lists:foreach(fun(C) -> apply_cdbobj({command, C}) end, L);
apply_cdbobj({command, {set_enc_key, EncKey}}) when is_binary(EncKey), size(EncKey) == 64 ->
	?ASYNC0(fun() -> mydlp_sync:set_enc_key(EncKey), mydlp_container:schedule_confupdate() end), ok;
apply_cdbobj({command, stop_discovery, [{ruleId, RuleId}, {groupId, GroupId}]}) ->
	?ASYNC0(fun() -> mydlp_discover_fs:stop_discovery(RuleId, GroupId) end), ok;
apply_cdbobj({command, start_discovery, [{ruleId, RuleId}, {groupId, GroupId}]}) ->
	?ASYNC0(fun() -> mydlp_discover_fs:start_discovery(RuleId, GroupId) end), ok;
apply_cdbobj({command, pause_discovery, [{ruleId, RuleId}, {groupId, GroupId}]}) ->
	?ASYNC0(fun() -> mydlp_discover_fs:pause_discovery(RuleId, GroupId) end), ok;
apply_cdbobj({command, continue_discovery, [{ruleId, RuleId}, {groupId, GroupId}]}) ->
	?ASYNC0(fun() -> mydlp_discover_fs:continue_discovery(RuleId, GroupId) end), ok;
apply_cdbobj({command, Else, _}) ->
	?ERROR_LOG("Unknown remote command: "?S, [Else]);
apply_cdbobj(Else) ->
	?ERROR_LOG("Unkown cdbobj: "?S, [Else]).

-ifdef(__PLATFORM_WINDOWS).

populate_win32reg() -> mydlp_dynamic:populate_win32reg().

-endif.

-ifdef(__PLATFORM_LINUX).

populate_win32reg() -> ok.

-endif.

get_client_policy_revision_id() ->
	% ======================================== BEWARE ============================
	% sequence should be same with mydlp_mnesia:get_remote_rule_tables
	% ============================================================================
	RemovableStorageRuleTable = mydlp_mnesia:get_rule_table(removable),
	PrinterRuleTable = mydlp_mnesia:get_rule_table(printer),
	DiscoveryRuleTable = mydlp_mnesia:get_rule_table(discovery),
	ScreenshotRuleTable = mydlp_mnesia:get_rule_table(screenshot),
	InboundRuleTable = mydlp_mnesia:get_rule_table(inbound),
	EncryptionRuleTable = mydlp_mnesia:get_rule_table(encryption),
	Directories = mydlp_mnesia:get_rule_table_destination(discovery),
	ApplicationNames = mydlp_mnesia:get_rule_table_destination(screenshot),
	RuleTables = [
		{removable, none, RemovableStorageRuleTable},
		{printer, none, PrinterRuleTable},
		{discovery, Directories, DiscoveryRuleTable},
		{screenshot, ApplicationNames, ScreenshotRuleTable},
		{inbound, none, InboundRuleTable},
		{encryption, none, EncryptionRuleTable}
	],
	ItemDump = mydlp_mnesia:dump_client_tables(),
	MCMods = mydlp_mnesia:get_mc_module(),
	MCModule = #mc_module{target=local, modules=MCMods},
	CDBObj = {{rule_tables, RuleTables}, {mc, MCModule}, {items, ItemDump}},
	erlang:phash2(CDBObj).
-endif.

%%-------------------------------------------------------------------------
%% @doc Converts given binary to an integer.
%% @end
%%-------------------------------------------------------------------------
binary_to_integer(Bin) -> list_to_integer(binary_to_list(Bin)).

%%-------------------------------------------------------------------------
%% @doc Converts given reference to an absolute filepath in given directory with given prefix.
%% @end
%%-------------------------------------------------------------------------
ref_to_fn(Dir, Prefix, Ref) ->
	{A,B,C} = Ref,
	RN = lists:flatten(io_lib:format("~s-~p.~p.~p",[Prefix,A,B,C])),
	filename:absname(RN, Dir).

%%-------------------------------------------------------------------------
%% @doc Removes trailing CRLF from given string
%% @end
%%-------------------------------------------------------------------------
rm_trailing_crlf("") -> "";
rm_trailing_crlf("\r") -> "";
rm_trailing_crlf("\n") -> "";
rm_trailing_crlf(<<>>) -> <<>>;
rm_trailing_crlf(<<"\r">>) -> <<>>;
rm_trailing_crlf(<<"\n">>) -> <<>>;
rm_trailing_crlf([_] = Str) -> Str;
rm_trailing_crlf(<<_:1/binary>> = Bin) -> Bin;
rm_trailing_crlf(Str) when is_list(Str) ->
	StrL = string:len(Str),
	"\r\n" = string:substr(Str, StrL - 1, 2),
	string:substr(Str, 1, StrL - 2);
rm_trailing_crlf(Bin) when is_binary(Bin) -> 
	BuffSize = size(Bin) - 2,
	<<Buff:BuffSize/binary, "\r\n">> = Bin,
	Buff.

%%-------------------------------------------------------------------------
%% @doc Decodes given quoted-printable string.
%% @end
%%-------------------------------------------------------------------------
quoted_to_raw(EncContent) when is_list(EncContent) -> quoted_to_raw(list_to_binary(EncContent));
quoted_to_raw(EncContent) when is_binary(EncContent) -> quoted_to_raw(EncContent, <<>>).

quoted_to_raw(<<$=, 13, 10, Rest/binary>>, Acc ) -> quoted_to_raw(Rest, Acc);
quoted_to_raw(<<$=, 10, Rest/binary>>, Acc ) -> quoted_to_raw(Rest, Acc);
quoted_to_raw(<<$=, H1, H2, Rest/binary>>, Acc ) -> 
	I = try mydlp_api:hex2int([H1,H2]) catch _:_ -> $\s end,
	quoted_to_raw(Rest, <<Acc/binary, I/integer>>);
quoted_to_raw(<<C/integer, Rest/binary>>, Acc ) -> quoted_to_raw(Rest, <<Acc/binary, C/integer>>);
quoted_to_raw(<<>>, Acc ) -> Acc.

%%-------------------------------------------------------------------------
%% @doc Removes html tags from given text
%% @end
%%-------------------------------------------------------------------------
remove_html_tags(Text) -> remove_html_tags(Text, none, [],<<>>).

remove_html_tags(Rest, textualize, [<<"br">>|TagStack], Acc) -> remove_html_tags(Rest, none, TagStack, <<Acc/binary, "\n">>);
remove_html_tags(Rest, textualize, TagStack, Acc) -> remove_html_tags(Rest, none, TagStack, Acc);
remove_html_tags(<<>>, _, [], Acc) -> Acc;
remove_html_tags(<<>>, _, TagStack, _Acc) -> throw({error, {there_are_still_tags_in_stack, TagStack}});
remove_html_tags(<<$\s, Rest/binary>>, wait_direct_end, TagStack, Acc) -> remove_html_tags(Rest, wait_direct_end, TagStack, Acc);
remove_html_tags(<<$>, Rest/binary>>, wait_direct_end, [Tag|TagStack], Acc) -> 
	Acc1 = remove_html_tags(<<>>, textualize, [Tag], Acc),
	remove_html_tags(Rest, none, TagStack, Acc1);
remove_html_tags(<<_C/integer, _Rest/binary>>, wait_direct_end, _TagStack, _Acc) -> throw({error, inproper_xml_tag_1});
remove_html_tags(<<$/, Rest/binary>>, wait_end, TagStack, Acc) -> remove_html_tags(Rest, wait_direct_end, TagStack, Acc);
remove_html_tags(<<$>, Rest/binary>>, wait_end, TagStack, Acc) -> remove_html_tags(Rest, textualize, TagStack, Acc);
remove_html_tags(<<_C/integer, Rest/binary>>, wait_end, TagStack, Acc) -> remove_html_tags(Rest, wait_end, TagStack, Acc);
remove_html_tags(<<$<, Rest/binary>>, none, TagStack, Acc) -> remove_html_tags(Rest, <<>>, TagStack, Acc);
remove_html_tags(<<C/utf8, Rest/binary>>, none, TagStack, Acc) -> remove_html_tags(Rest, none, TagStack, <<Acc/binary, C/utf8>>);
remove_html_tags(<<C/integer, Rest/binary>>, none, TagStack, Acc) -> remove_html_tags(Rest, none, TagStack, <<Acc/binary, C/integer>>);
remove_html_tags(<<$\s, Rest/binary>>, close_tag, TagStack, Acc) -> remove_html_tags(Rest, close_tag, TagStack, Acc);
remove_html_tags(<<$>, Rest/binary>>, close_tag, [<<>>|TagStack], Acc) -> remove_html_tags(Rest, none, TagStack, Acc);
remove_html_tags(<<C/integer, Rest/binary>>, close_tag, [Tag|TagStack], Acc) -> 
	C1 = string:to_lower(C),
	case Tag of
		<<C1/integer, TagRest/binary>> -> remove_html_tags(Rest, close_tag, [TagRest|TagStack], Acc);
		_Else -> throw({error, inproper_xml_tag_2}) end;
remove_html_tags(<<$\s, Rest/binary>>, <<>>, TagStack, Acc) -> remove_html_tags(Rest, <<>>, TagStack, Acc);
remove_html_tags(<<$/, Rest/binary>>, <<>>, TagStack, Acc) -> remove_html_tags(Rest, close_tag, TagStack, Acc);
remove_html_tags(<<$\s, Rest/binary>>, LatestTag, TagStack, Acc) -> remove_html_tags(Rest, wait_end, [LatestTag|TagStack], Acc);
remove_html_tags(<<$/, Rest/binary>>, LatestTag, TagStack, Acc) -> remove_html_tags(Rest, wait_direct_end, [LatestTag|TagStack], Acc);
remove_html_tags(<<$>, Rest/binary>>, LatestTag, TagStack, Acc) -> remove_html_tags(Rest, textualize, [LatestTag|TagStack], Acc);
remove_html_tags(<<C/integer, Rest/binary>>, LatestTag, TagStack, Acc) -> 
	C1 = string:to_lower(C),
	remove_html_tags(Rest, <<LatestTag/binary, C1/integer>>, TagStack, Acc).


ensure_unicode(Bin) -> ensure_unicode(Bin, byte_size(Bin), 0, 0, 2).

ensure_unicode(_Bin, _BinSize, _LeftI, _RightI, Limit) when Limit > 16 ->
	<<"can_not_convert_to_unicode">>;
ensure_unicode(Bin, BinSize, LeftI, _RightI, Limit) when LeftI >= Limit ->
	ensure_unicode(Bin, BinSize, Limit, Limit, Limit*2);
ensure_unicode(Bin, BinSize, LeftI, RightI, Limit) when RightI >= Limit ->
	ensure_unicode(Bin, BinSize, LeftI + 1, 0, Limit);
ensure_unicode(Bin, BinSize, LeftI, RightI, Limit) when BinSize =< RightI + LeftI ->
	ensure_unicode(Bin, BinSize, LeftI + 1, 0, Limit);
ensure_unicode(Bin, BinSize, LeftI, RightI, Limit) ->
	PSize = case BinSize - LeftI - RightI of
		S when S > 0 -> S;
		_ -> 0 end,
	<<_:LeftI/binary, Phrase:PSize/binary, _/binary>> = Bin,
	case unicode:characters_to_list(Phrase) of
		R when is_list(R) -> Phrase;
		_ -> ensure_unicode(Bin, BinSize, LeftI, RightI + 1, Limit) end.
	

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

pmap_test_() -> [
	?_assertEqual([2,4,6,8,10,12,14,16], 
			pmap(fun(I) -> I*2 end, 
				[1,2,3,4,5,6,7,8]))
].

pany_test_() -> [
	?_assertEqual(false,
			pany(fun(_I) -> false end,
				[1,2,3,4,5,6,7,8])),
	?_assertEqual({ok, 5, true},
			pany(fun(I) -> 
				case I of
					2 -> neg;
					3 -> negative;
					5 -> true;
					_ -> false end end,
				[1,2,3,4,5,6,7,8]))
].

%%-------------------------------------------------------------------------
%% @doc Reverses given binary.
%% @end
%%-------------------------------------------------------------------------

reverse_binary(Bin) ->
	S = size(Bin)*8, 
	<<X:S/integer-little>> = Bin,
	<<X:S/integer-big>>.

bf_encrypt(Key, Data) when is_binary(Data) >= 8 ->
	bf_encrypt(Key, Data, <<>>, size(Data)).

bf_encrypt(Key, <<Data:8/binary, Rest/binary>>, Acc, Size) ->
	Cipher = crypto:blowfish_ecb_encrypt(Key, Data),
	bf_encrypt(Key, Rest, <<Acc/binary, Cipher/binary>>, Size);
bf_encrypt(_Key, <<>>, Acc, Size) -> {cipher, Size, Acc};
bf_encrypt(Key, <<Data/binary>>, Acc, Size)->
	BitSize = ( 8 - (Size rem 8) ) * 8,
	Data1 = <<Data/binary, 0:BitSize/integer>>,
	Cipher = crypto:blowfish_ecb_encrypt(Key, Data1),
	bf_encrypt(Key, <<>>, <<Acc/binary, Cipher/binary>>, Size).


bf_decrypt(Key, {cipher, Size, Data}) when Size >= 0, size(Data) >= Size  ->
	bf_decrypt(Key, Data, <<>>, Size).

bf_decrypt(Key, <<Data:8/binary, Rest/binary>>, Acc, Size) ->
	Cipher = crypto:blowfish_ecb_decrypt(Key, Data),
	bf_decrypt(Key, Rest, <<Acc/binary, Cipher/binary>>, Size);
bf_decrypt(_Key, <<>>, Acc, Size) -> <<Data:Size/binary, _/binary>> = Acc, Data.


aes_encrypt(Key, Data) when is_binary(Key), size(Key) == 16, is_binary(Data) ->
	OrigSize = size(Data),
	PaddingByte = case (OrigSize rem 16) of
		0 -> 0;
		16 -> 0;
		I -> 16 - I end,
	PaddingBit = PaddingByte * 8,
	Data1 = <<Data/binary, 0:PaddingBit/integer>>,
	IV = get_random_bytes(),
	Cipher = crypto:aes_cbc_128_encrypt(Key, IV, Data1),
	{cipher, OrigSize, IV, Cipher}.

aes_decrypt(Key, {cipher, Size, IV, Data}) when 
		Size >= 0, is_binary(Data), size(Data) >= Size, 
		is_binary(Key), size(Key) == 16, 
		is_binary(IV), size(IV) == 16 ->
	Clear = crypto:aes_cbc_128_decrypt(Key, IV, Data),
	<<Orig:Size/binary, _/binary>> = Clear, Orig.

aes_cipher_to_binary({cipher, Size, IV, Data}) when 
		Size >= 0, is_binary(Data), size(Data) >= Size, 
		is_binary(IV), size(IV) == 16 ->
	<<"MyDLP_MEC", Size:64/integer, IV/binary, Data/binary>>.


is_aes_cipher_binary(<<"MyDLP_MEC", Size:64/integer, IV:16/binary, Data/binary>>) when
		Size >= 0, is_binary(Data), size(Data) >= Size, 
		is_binary(IV), size(IV) == 16 -> true;
is_aes_cipher_binary(_) -> false.

binary_to_aes_cipher(<<"MyDLP_MEC", Size:64/integer, IV:16/binary, Data/binary>>) when
		Size >= 0, is_binary(Data), size(Data) >= Size, 
		is_binary(IV), size(IV) == 16 ->
	{cipher, Size, IV, Data}.

aes_encrypt_binary(Key, Data) ->
	Cipher = mydlp_api:aes_encrypt(Key, Data),
	mydlp_api:aes_cipher_to_binary(Cipher).
	
aes_decrypt_binary(Key, Data) -> 
	Cipher = mydlp_api:binary_to_aes_cipher(Data),
	mydlp_api:aes_decrypt(Key, Cipher).

qp_decode(Str) when is_list(Str) -> qp_decode(list_to_binary(Str));
qp_decode(Str) when is_binary(Str) ->
        DBin = mydlp_api:quoted_to_raw(Str),
        unicode:characters_to_list(DBin).
        %DList = unicode:characters_to_list(DBin, {utf16, little}),
        %DList = unicode:characters_to_list(DBin).
        %filename:nativename(DList).

-ifdef(__MYDLP_ENDPOINT).

encrypt_payload(Data0) when is_binary(Data0) ->
	Data = <<"MyDLPEPPayload_", Data0/binary>>,
        case { (catch get_endpoint_id()), (catch get_endpoint_secret()) } of
                {retry, _} -> retry;
                {_, retry} -> retry;
                {EpId, EpSecret} when is_binary(EpId), is_binary(EpSecret) ->
			{cipher, Size, Cipher} = mydlp_api:bf_encrypt(EpSecret, Data),
        		SizeB = list_to_binary(lists:flatten(io_lib:format("~16..0B", [Size]))),
                        <<"MyDLPEPSync_",  EpId/binary, "_", SizeB/binary, "_", Cipher/binary>>;
		Err -> ?ERROR_LOG("Error occurred when encrypting. Err: "?S , [Err]), retry end;
encrypt_payload(Data) when is_list(Data) -> encrypt_payload(list_to_binary(Data)).

decrypt_payload(<<"MyDLPEPSync_", SizeB:16/binary, "_", Cipher/binary>> = Chunk) when size(Chunk) >= 45 -> 
	try	Size = binary_to_integer(SizeB),
		case (catch get_endpoint_secret()) of
			retry -> retry;
			EpSecret when is_binary(EpSecret) ->
				PayloadData = bf_decrypt(EpSecret, {cipher, Size, Cipher}),
				<<"MyDLPEPPayload_", Data/binary>> = PayloadData, Data;
			Err -> ?ERROR_LOG("Error occurred when decrypting. Err: "?S , [Err]), retry end
	catch Class:Error ->
		?ERROR_LOG("Error occurred when decrypting. Class: "?S" Error: "?S , [Class, Error]), retry end;
decrypt_payload(Data) when is_list(Data) -> decrypt_payload(list_to_binary(Data));
decrypt_payload(Else) -> 
	?ERROR_LOG("Improper paylaod to decrypt. Payload: "?S , [Else]),
	retry.

get_endpoint_id() -> 
	EpKey = get_endpoint_key(),
	[<<"EPKEY">>, Rest] = binary:split(EpKey, <<"_">>),
	[Id, _Secret] = binary:split(Rest, <<"_">>),
	Id.

get_endpoint_secret() ->
	EpKey = get_endpoint_key(),
	[<<"EPKEY">>, Rest] = binary:split(EpKey, <<"_">>),
	[_Id, Secret] = binary:split(Rest, <<"_">>),
	Secret.

generate_endpoint_key() -> 
        Url = "https://" ++ ?CFG(management_server_address) ++ "/register",
        case catch httpc:request(Url) of
                {ok, {{_HttpVer, Code, _Msg}, _Headers, Body}} ->
                        case {Code, Body} of
                                {200, Resp} -> case list_to_binary(Resp) of
					<<>> ->	?ERROR_LOG("REGISTER: Empty response: Url="?S"~n", [Url]), retry;
                                	<<"retry", _/binary>> -> retry;
                                	<<"EPKEY_", _/binary>> = EpKey -> ?ERROR_LOG("REGISTER: Successfully registered.", []), EpKey end;
                                Else2 -> ?ERROR_LOG("REGISTER: An error occured during HTTP req:Ret="?S, [Else2]), retry end;
                Else -> ?ERROR_LOG("REGISTER: An error occured during HTTP req: Obj="?S"~n", [Else]), retry end.

is_valid_token(Token) when is_list(Token) ->
        Url = "https://" ++ ?CFG(management_server_address) ++ "/token?q=is_valid",
	RawPayload = list_to_binary(Token),
	Payload = mydlp_api:encrypt_payload(RawPayload),
        case catch httpc:request(post, {Url, [], "application/octet-stream", Payload}, [], []) of
                {ok, {{_HttpVer, Code, _Msg}, _Headers, Body}} ->
                        case {Code, Body} of
                                {200, Resp} -> case list_to_binary(Resp) of
					<<>> ->	?ERROR_LOG("TOKEN: Empty response: Url="?S"~n", [Url]), error;
                                	<<"error", _/binary>> -> error;
                                	<<"invalid", _/binary>> -> delete_endpoint_key(), error;
					RespData when is_binary(RespData) ->
						case mydlp_api:decrypt_payload(RespData) of
							retry -> error;
							<<"invalid", _/binary>> -> delete_endpoint_key(), error;
							<<"true", _/binary>> -> true;
							<<"false", _/binary>> -> false end end;
                                Else2 -> ?ERROR_LOG("TOKEN: An error occured during HTTP req:Ret="?S, [Else2]), retry end;
                Else -> ?ERROR_LOG("TOKEN: An error occured during HTTP req: Obj="?S"~n", [Else]), retry end.

new_token() ->
        Url = "https://" ++ ?CFG(management_server_address) ++ "/token?q=new",
	Payload = mydlp_api:encrypt_payload(<<>>),
        case catch httpc:request(post, {Url, [], "application/octet-stream", Payload}, [], []) of
                {ok, {{_HttpVer, Code, _Msg}, _Headers, Body}} ->
                        case {Code, Body} of
                                {200, Resp} -> case list_to_binary(Resp) of
					<<>> ->	?ERROR_LOG("TOKEN: Empty response: Url="?S"~n", [Url]), error;
                                	<<"TOKEN: ", Token/binary>> -> Token;
                                	<<"error", _/binary>> -> error;
                                	<<"invalid", _/binary>> -> delete_endpoint_key(), error;
					RespData when is_binary(RespData) ->
						case mydlp_api:decrypt_payload(RespData) of
							retry -> error;
							<<"invalid", _/binary>> -> delete_endpoint_key(), error;
							<<"TOKEN: ", Token/binary>> -> Token end end;
                                Else2 -> ?ERROR_LOG("TOKEN: An error occured during HTTP req:Ret="?S, [Else2]), retry end;
                Else -> ?ERROR_LOG("TOKEN: An error occured during HTTP req: Obj="?S"~n", [Else]), retry end.

-ifdef(__PLATFORM_LINUX).

-define(ENDPOINTKEYFILE, "/var/lib/mydlp/endpoint_key").

get_endpoint_key() ->
	case filelib:is_regular(?ENDPOINTKEYFILE) of
		true -> ok;
		false -> create_endpoint_key() end,
	read_endpoint_key().

read_endpoint_key() ->
	case file:read_file(?ENDPOINTKEYFILE) of
                {ok, Bin} -> Bin;
                Else -> throw({error, Else}) end.

create_endpoint_key() ->
	EndpointKey = case generate_endpoint_key() of
		retry -> throw({error, generate_retry});
		B when is_binary(B) -> B end,
	filelib:ensure_dir(?ENDPOINTKEYFILE),
	file:write_file(?ENDPOINTKEYFILE, EndpointKey),
	ok.

delete_endpoint_key() ->
	case file:delete(?ENDPOINTKEYFILE) of
		ok -> ok;
		{error, enoent} -> ok;
		{error, Else} -> throw({error, Else}) end.

-endif.

-ifdef(__PLATFORM_WINDOWS).

-define(WIN32REGENDPOINTKEY, "endpoint_key").

get_endpoint_key() ->
	{ok, RegHandle} = win32reg:open([read,write]),
        win32reg:change_key_create(RegHandle, "\\hklm\\software\\MyDLP"),

	try case win32reg:value(RegHandle, ?WIN32REGENDPOINTKEY) of
		{ok, EndpointKey} -> EndpointKey;
		_ ->	create_endpoint_key(RegHandle),
			read_endpoint_key(RegHandle) end
	after
		win32reg:close(RegHandle)
	end.

read_endpoint_key(RegHandle) ->
	case win32reg:value(RegHandle, ?WIN32REGENDPOINTKEY) of
		{ok, EndpointKey} -> EndpointKey;
                Err -> throw({error, Err}) end.

create_endpoint_key(RegHandle) ->
	EndpointKey = case generate_endpoint_key() of
		retry -> throw({error, generate_retry});
		B when is_binary(B) -> B end,
	case win32reg:set_value(RegHandle, ?WIN32REGENDPOINTKEY, EndpointKey) of
		ok -> ok;
		Err -> throw({error, Err}) end,
	ok.

delete_endpoint_key() ->
	{ok, RegHandle} = win32reg:open([read,write]),
        win32reg:change_key_create(RegHandle, "\\hklm\\software\\MyDLP"),
	try	win32reg:delete_value(RegHandle, ?WIN32REGENDPOINTKEY)
	after	win32reg:close(RegHandle) end.
	

-endif.

-endif.

-ifdef(__MYDLP_NETWORK).

-ifdef(__PLATFORM_LINUX).

-define(ENCKEYFILE, "/var/lib/mydlp/encryption_key").

-endif.

get_encryption_key() ->
	case filelib:is_regular(?ENCKEYFILE) of
		true -> ok;
		false -> create_encryption_key() end,
	read_encryption_key().

read_encryption_key() ->
	case file:read_file(?ENCKEYFILE) of
                {ok, Bin} -> Bin;
                Else -> throw({error, Else}) end.

create_encryption_key() ->
	EncKey = case generate_encryption_key() of
		retry -> throw({error, generate_retry});
		B when is_binary(B) -> B end,
	filelib:ensure_dir(?ENCKEYFILE),
	file:write_file(?ENCKEYFILE, EncKey),
	ok.

generate_encryption_key() -> get_random_bytes(64).

-endif.

write_to_tmpfile(Bin) when is_binary(Bin) ->
	Now = now(),
	FN = ref_to_fn(?CFG(work_dir), "keyfile", Now),
	ok = file:write_file(FN, <<>>, [raw]),
        ok = file:change_mode(FN, 8#00600),
	ok = file:write_file(FN, Bin, [raw]),
	{ok, FN}.

-ifdef(__MYDLP_NETWORK).

-ifdef(__PLATFORM_LINUX).

cmd(Command) -> cmd(Command, []).

cmd(Command, Args) -> cmd(Command, Args, []). 

cmd(Command, Args, Envs) -> cmd(Command, Args, Envs, none). % Last variable for Stdin

% envs should be like [{"key","value"}] and Stdin shold be "Stdin\n" format
cmd(Command, Args, Envs, Stdin) when is_list(Args), is_list(Envs) ->
       Port = open_port({spawn_executable, Command},
                       [{args, Args},
                       {env, Envs},
                       use_stdio,
                       exit_status,
                       stderr_to_stdout]),

	case Stdin of 
		none -> ok;
		S -> port_command(Port, S) 
	end,

	case get_port_resp(Port, []) of
		{ok, _Data} -> ok;
		{error, _} = Error -> ?ERROR_LOG("Error calling "?S", Args: "?S"~nOutput: "?S, [Command, Args, Error]),
			Error end.

cmd_retcode(Command) -> cmd_retcode(Command, []).

cmd_retcode(Command, Args) -> cmd_retcode(Command, Args, []). 

cmd_retcode(Command, Args, Envs) -> cmd_retcode(Command, Args, Envs, none). % Last variable for Stdin

% envs should be like [{"key","value"}] and Stdin shold be "Stdin\n" format
cmd_retcode(Command, Args, Envs, Stdin) when is_list(Args), is_list(Envs) ->
       Port = open_port({spawn_executable, Command},
                       [{args, Args},
                       {env, Envs},
                       use_stdio,
                       exit_status,
                       stderr_to_stdout]),

	case Stdin of 
		none -> ok;
		S -> port_command(Port, S) 
	end,

	case get_port_resp(Port, []) of
		{ok, _Data} -> ok;
		{error, {retcode, I, _}} when is_integer(I) ->  {retcode, I};
		{error, _} = Error -> ?ERROR_LOG("Error calling "?S", Args: "?S"~nOutput: "?S, [Command, Args, Error]),
			Error end.


cmd_bool(Command) -> cmd_bool(Command, []).

cmd_bool(Command, Args) -> cmd_bool(Command, Args, []). 

cmd_bool(Command, Args, Envs) -> cmd_bool(Command, Args, Envs, none). % Last variable for Stdin

% envs should be like [{"key","value"}] and Stdin shold be "Stdin\n" format
cmd_bool(Command, Args, Envs, Stdin) when is_list(Args), is_list(Envs) ->
	case cmd_retcode(Command, Args, Envs, Stdin) of
		ok -> true;
		{retcode, _Else} -> false;
		Else -> Else end.


-endif.

-endif.

-ifdef(__MYDLP_ENDPOINT).

-define(IECP_SOCKET_OPTS, [binary, {packet, 0}, {reuseaddr, true}, {nodelay, true}, {keepalive, true}, {active, false}]).

iecp_command(IpAddr, FilePath, PropDict) ->
	?ASYNC(fun() -> 
		Token = case new_token() of
			T when is_binary(T) -> T;
			Err -> throw({error, {cannot_get_token, Err}}) end,
		Socket = case gen_tcp:connect(IpAddr, 9100, ?IECP_SOCKET_OPTS) of
			{ok, S} -> S;
			Err1 -> throw({error, {cannot_connect_to_addr, Err1, IpAddr}}) end,
		try 	iecp_command_init(Socket, Token),
			iecp_command_send_propdict(Socket, PropDict),
			iecp_command_send_payload(Socket, FilePath),
			iecp_command_end(Socket)
		after	gen_tcp:close(Socket)
		end
	end, 300000), ok.

iecp_command_init(Socket, Token) ->
	gen_tcp:send(Socket, ["TOKEN ", Token, "\r\n"]),
	case gen_tcp:recv(Socket, 0) of
		{ok, <<"OK\r\n">>} -> ok;
		Err -> throw({error, {not_ok_resp_for_token, Err, Token}}) end.

iecp_command_send_propdict(Socket, PropDict) ->
	iecp_command_send_propdict1(Socket, dict:to_list(PropDict)).
iecp_command_send_propdict1(Socket, [{Key, Value}|Rest]) ->
	gen_tcp:send(Socket, ["SETPROP ", Key, "=", Value, "\r\n"]),
	case gen_tcp:recv(Socket, 0) of
		{ok, <<"OK\r\n">>} -> iecp_command_send_propdict1(Socket, Rest);
		Err -> throw({error, {not_ok_resp_for_setprop, Err}}) end;
iecp_command_send_propdict1(_Socket, []) -> ok.

iecp_command_send_payload(Socket, FilePath) ->
	FileBin = case file:read_file(FilePath) of
		{ok, B} -> B;
		Err -> throw({error, {cannot_open_file, Err, FilePath}}) end,
	Size = size(FileBin),
	gen_tcp:send(Socket, ["PUSH ", integer_to_list(Size), "\r\n"]),
	gen_tcp:send(Socket, FileBin),
	case gen_tcp:recv(Socket, 0) of
		{ok, <<"OK\r\n">>} -> ok;
		Err2 -> throw({error, {not_ok_resp_for_push, Err2}}) end.

iecp_command_end(Socket) ->
	gen_tcp:send(Socket, ["END\r\n"]),
	case gen_tcp:recv(Socket, 0) of
		{ok, <<"OK\r\n">>} -> ok;
		Err -> throw({error, {not_ok_resp_for_end, Err}}) end,
	ok.

-endif.


