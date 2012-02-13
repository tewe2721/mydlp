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
-export([normalize/1]).

-define(KGRAMSIZE, 32).
-define(WINDOWSIZE, 64).

normalize(Bin) -> fingerprint(Bin).

fingerprint(<<Bin/binary>>) -> fingerprint(Bin, 0, <<>>, [], none, ?WINDOWSIZE, 0, 0, []).

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
	<<C/utf8, Rest/binary>> = Bin,
	{CharCount1, KG1} = case normal_bin(C) of
		<<>> -> {CharCount, KG};
		Else -> {CharCount + 1, <<KG/binary, Else/binary>>} end,
	fingerprint(Rest, CharCount1, KG1, Window, WM, WMLife, WMCount, PrevousWMCount, Fingerprints).

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
normal_bin(C) -> C1 = to_lower(C), <<C1/utf8>>.

to_lower(382) -> $z; % custom z
to_lower(381) -> $z; % custom Z
to_lower(380) -> $z; % custom z
to_lower(379) -> $z; % custom Z
to_lower(378) -> $z; % custom z
to_lower(377) -> $z; % custom Z
to_lower(376) -> $y; % custom Y
to_lower(375) -> $y; % custom y
to_lower(374) -> $y; % custom Y
to_lower(373) -> $w; % custom w
to_lower(372) -> $w; % custom W
to_lower(371) -> $u; % custom u
to_lower(370) -> $u; % custom U
to_lower(369) -> 252; % custom u-
to_lower(368) -> 252; % custom U-
to_lower(367) -> 252; % custom u-
to_lower(366) -> 252; % custom U-
to_lower(365) -> 252; % custom u-
to_lower(364) -> 252; % custom U-
to_lower(363) -> 252; % custom u-
to_lower(362) -> 252; % custom U-
to_lower(361) -> 252; % custom u-
to_lower(360) -> 252; % custom U-
to_lower(359) -> $t; % custom t
to_lower(358) -> $t; % custom T
to_lower(357) -> $t; % custom t
to_lower(356) -> $t; % custom T
to_lower(355) -> $t; % custom t
to_lower(354) -> $t; % custom T
to_lower(353) -> $s; % custom s
to_lower(352) -> $s; % custom S
to_lower(351) -> 351; % s-
to_lower(350) -> 351; % S-
to_lower(349) -> $s; % custom s
to_lower(348) -> $s; % custom S
to_lower(347) -> $s; % custom s
to_lower(346) -> $s; % custom S
to_lower(345) -> $r; % custom r
to_lower(344) -> $r; % custom R
to_lower(343) -> $r; % custom r
to_lower(342) -> $r; % custom R
to_lower(341) -> $r; % custom r
to_lower(340) -> $r; % custom R
to_lower(337) -> 246; % custom o-
to_lower(336) -> 246; % custom O-
to_lower(335) -> 246; % custom o-
to_lower(334) -> 246; % custom O-
to_lower(333) -> 246; % custom o-
to_lower(332) -> 246; % custom O-
to_lower(331) -> $n; % custom n
to_lower(330) -> $n; % custom N
to_lower(329) -> $n; % custom n
to_lower(328) -> $n; % custom n
to_lower(327) -> $n; % custom N
to_lower(326) -> $n; % custom n
to_lower(325) -> $n; % custom N
to_lower(324) -> $n; % custom n
to_lower(323) -> $n; % custom N
to_lower(322) -> $l; % custom l
to_lower(321) -> $l; % custom L
to_lower(320) -> $l; % custom l
to_lower(319) -> $l; % custom L
to_lower(318) -> $l; % custom l
to_lower(317) -> $l; % custom L
to_lower(316) -> $l; % custom l
to_lower(315) -> $l; % custom L
to_lower(314) -> $l; % custom l
to_lower(313) -> $l; % custom L
to_lower(312) -> $k; % custom k
to_lower(311) -> $k; % custom k
to_lower(310) -> $k; % custom K
to_lower(309) -> $j; % custom j
to_lower(308) -> $j; % custom J
to_lower(307) -> $j; % custom j
to_lower(306) -> $j; % custom J
to_lower(305) -> 305; % i-
to_lower(304) -> $i; % I-
to_lower(303) -> $i; % custom i
to_lower(302) -> $i; % custom I-
to_lower(301) -> $i; % custom i
to_lower(300) -> $i; % custom I-
to_lower(299) -> $i; % custom i
to_lower(298) -> $i; % custom I-
to_lower(297) -> $i; % custom i
to_lower(296) -> $i; % custom I-
to_lower(295) -> $h; % custom h
to_lower(294) -> $h; % custom H
to_lower(293) -> $h; % custom h
to_lower(292) -> $h; % custom H
to_lower(291) -> 287; % custom g-
to_lower(290) -> 287; % custom G-
to_lower(289) -> 287; % custom g-
to_lower(288) -> 287; % custom G-
to_lower(287) -> 287; % g-
to_lower(286) -> 287; % G-
to_lower(285) -> 287; % custom g-
to_lower(284) -> 287; % custom G-
to_lower(283) -> $e; % custom e
to_lower(282) -> $e; % custom E
to_lower(281) -> $e; % custom e
to_lower(280) -> $e; % custom E
to_lower(279) -> $e; % custom e
to_lower(278) -> $e; % custom E
to_lower(277) -> $e; % custom e
to_lower(276) -> $e; % custom E
to_lower(275) -> $e; % custom e
to_lower(274) -> $e; % custom E
to_lower(273) -> $d; % custom d
to_lower(272) -> $d; % custom D
to_lower(271) -> $d; % custom d
to_lower(270) -> $d; % custom D
to_lower(269) -> $c; % custom c
to_lower(268) -> $c; % custom C
to_lower(267) -> $c; % custom c
to_lower(266) -> $c; % custom C
to_lower(265) -> $c; % custom c
to_lower(264) -> $c; % custom C
to_lower(263) -> $c; % custom c
to_lower(262) -> $c; % custom C
to_lower(261) -> $a; % custom A
to_lower(260) -> $a; % custom A
to_lower(259) -> $a; % custom A
to_lower(258) -> $a; % custom A
to_lower(257) -> $a; % custom A
to_lower(256) -> $a; % custom A
to_lower(255) -> $y; % custom y
to_lower(253) -> $y; % custom y
to_lower(252) -> 252; % u-
to_lower(251) -> 252; % custom u
to_lower(250) -> 252; % custom u-
to_lower(249) -> 252; % custom u-
to_lower(248) -> $0; % custom 0
to_lower(246) -> 246; % o-
to_lower(245) -> 246; % custom o
to_lower(244) -> 246; % custom o
to_lower(243) -> 246; % custom o
to_lower(242) -> 246; % custom o
to_lower(241) -> $n; % custom n
to_lower(240) -> $d; % custom d
to_lower(239) -> $i; % custom i
to_lower(238) -> $i; % custom i
to_lower(237) -> $i; % custom i
to_lower(236) -> $i; % custom i
to_lower(235) -> $e; % custom e
to_lower(234) -> $e; % custom e
to_lower(233) -> $e; % custom e
to_lower(232) -> $e; % custom e
to_lower(231) -> 231; % c-
to_lower(230) -> $a; % custom a
to_lower(229) -> $a; % custom a
to_lower(228) -> $a; % custom a
to_lower(227) -> $a; % custom a
to_lower(226) -> $a; % custom a
to_lower(225) -> $a; % custom a
to_lower(224) -> $a; % custom a
to_lower(223) -> $b; % custom B
to_lower(221) -> $y; % custom Y
to_lower(220) -> 252; % U-
to_lower(219) -> 252; % custom U-
to_lower(218) -> 252; % custom U-
to_lower(217) -> 252; % custom U-
to_lower(216) -> $0; % custom 0
to_lower(215) -> $x; % custom x
to_lower(214) -> 246; % O-
to_lower(213) -> 246; % custom O
to_lower(212) -> 246; % custom O
to_lower(211) -> 246; % custom O
to_lower(210) -> 246; % custom O
to_lower(209) -> $n; % custom N
to_lower(208) -> $d; % custom D
to_lower(207) -> $i; % custom I-
to_lower(206) -> $i; % custom I-
to_lower(205) -> $i; % custom I-
to_lower(204) -> $i; % custom I-
to_lower(203) -> $e; % custom E
to_lower(202) -> $e; % custom E
to_lower(201) -> $e; % custom E
to_lower(200) -> $e; % custom E
to_lower(199) -> 231; % C-
to_lower(198) -> $a; % custom A
to_lower(197) -> $a; % custom A
to_lower(196) -> $a; % custom A
to_lower(195) -> $a; % custom A
to_lower(194) -> $a; % custom A
to_lower(193) -> $a; % custom A
to_lower(192) -> $a; % custom A
to_lower(C) when C >= 97 , C =< 122; C >= 48 , C =< 57 -> C; % a-z 0-9
to_lower(C) when C >= 65 , C =< 90 -> C+32; % A-Z -> a-z
to_lower(C) -> C.
