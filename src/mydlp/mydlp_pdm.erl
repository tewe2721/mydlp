%
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

%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@mydlp.com>
%%% @copyright 2012, H. Kerem Cevahir
%%% @doc MyDLP Partial Document Matchinf module
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_pdm).
-author("kerem@mydlp.com").

-include("mydlp.hrl").

%% API
-export([fingerprint/1]).

-define(KGRAMSIZE, 50).
-define(WINDOWSIZE, 100).

-define(MINBYTES, ?KGRAMSIZE + ?WINDOWSIZE).

fingerprint(<<Bin/binary>>) when size(Bin) < ?MINBYTES -> [];
fingerprint(<<Bin/binary>>) -> fingerprint(Bin, 0, <<>>, [], none, ?WINDOWSIZE, 0, 0, []);
fingerprint(L) when is_list(L) -> fingerprint(list_to_binary(L)).

fingerprint(Bin, CharCount, KG, Window, _WM, _WMLife = 0, _WMCount, PrevousWMCount, Fingerprints) ->
	NewWM = lists:min(Window),
	{LastIndex, Count} = get_li_and_count(NewWM, Window),
	fingerprint(Bin, CharCount, KG, Window, NewWM, LastIndex, Count, PrevousWMCount, Fingerprints);
fingerprint(Bin, CharCount, KG, Window, WM, WMLife, WMCount, PrevousWMCount, Fingerprints) when length(Window) == ?WINDOWSIZE ->
	PreviousWM = case Fingerprints of
		[] -> none;
		[PWM|_] -> PWM end,
	
	Fingerprints1 = case PreviousWM == WM of
		true -> case WMCount > PrevousWMCount of
			true -> [WM|Fingerprints];
			false -> Fingerprints end;
		false -> [WM| Fingerprints] end,

	[P|NewWindow] = Window,
	WMCount1 = case P of
		WM -> WMCount - 1;
		_Else -> WMCount end,
	fingerprint(Bin, CharCount, KG, NewWindow, WM, WMLife-1, WMCount1, WMCount, Fingerprints1);

fingerprint(Bin, CharCount = ?KGRAMSIZE, KG, Window, WM, WMLife, WMCount, PrevousWMCount, Fingerprints) ->
	<<_/utf8, NextKG/binary>> = KG,
	Hash = hash(KG),
	{WM1, WMLife1, WMCount1} = case Hash of
		H when WM == none -> {H, length(Window) + 1, 1}; % TODO: refine this
		H when H > WM -> {WM, WMLife, WMCount};
		WM -> {WM, length(Window), WMCount + 1};
		H when H < WM -> {H, length(Window), 1} end,
	fingerprint(Bin, CharCount - 1, NextKG, Window ++ [Hash], WM1, WMLife1, WMCount1, PrevousWMCount, Fingerprints);

fingerprint(<<>>, _CharCount, _KG, [], _WM, _WMLife, _WMCount, _PrevousWMCount, []) -> [];
fingerprint(<<>>, _CharCount, _KG, Window, _WM, _WMLife, _WMCount, _PrevousWMCount, []) -> [lists:min(Window)];
fingerprint(<<>>, _CharCount, _KG, _Window, _WM, _WMLife, _WMCount, _PrevousWMCount, Fingerprints) -> Fingerprints; %% Caution: fingerprints are in reverse order.

fingerprint(Bin, CharCount, KG, Window, WM, WMLife, WMCount, PrevousWMCount, Fingerprints) ->
	case get_uchar(Bin) of
		none -> fingerprint(<<>>, CharCount, KG, Window, WM, WMLife, WMCount, PrevousWMCount, Fingerprints);
		{C,Rest} ->
			{CharCount1, KG1} = case normal_bin(C) of
				<<>> -> {CharCount, KG};
				Else -> {CharCount + 1, <<KG/binary, Else/binary>>} end,
			fingerprint(Rest, CharCount1, KG1, Window, WM, WMLife, WMCount, PrevousWMCount, Fingerprints) end.

get_uchar(<<>>) -> none;
get_uchar(Bin) ->
	try 	<<C/utf8, Rest/binary>> = Bin,
		{C, Rest}
	catch _:_ -> 
		<<_:1/binary, Rest2/binary>> = Bin,
		get_uchar(Rest2) end.

hash(Item) -> erlang:phash2(Item).

get_li_and_count(Item, List) -> get_li_and_count(Item, lists:reverse(List), length(List), 0).

get_li_and_count(_, [], LastIndex, Count) -> {LastIndex, Count};
get_li_and_count(Item, [Item|Rest], Index, 0) -> get_li_and_count(Item, Rest, Index, 1);
get_li_and_count(Item, [Item|Rest], LastIndex, Count) -> get_li_and_count(Item, Rest, LastIndex, Count+1);
get_li_and_count(Item, [_|Rest], Index, 0) -> get_li_and_count(Item, Rest, Index-1, 0);
get_li_and_count(Item, [_|Rest], Index, Count) -> get_li_and_count(Item, Rest, Index, Count).

normal_bin(C) when C =< 47 -> <<>>;
normal_bin(C) when C >= 58, C =< 64 -> <<>>;
normal_bin(C) when C >= 91, C =< 96 -> <<>>;
normal_bin(C) when C >= 91, C =< 96 -> <<>>;
normal_bin(C) when C >= 123, C =< 127 -> <<>>;
normal_bin(C) when C >= 8192, C =< 8303 -> <<>>; %% Unicode General Punctuation Block	U+2000	U+206F
normal_bin(C) when C >= 11776, C =< 11903 -> <<>>; %% Unicode Supplemental Punctuation	U+2E00	U+2E7F
normal_bin(C) when C >= 12288, C =< 12351 -> <<>>; %% Unicode CJK Symbols and Punctuation	U+3000	U+303F
normal_bin(C) -> C1 = mydlp_nlp:to_lower(C), <<C1/utf8>>.

