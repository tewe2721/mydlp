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
-module(mydlp_nlp).
-author("kerem@mydlp.com").

-include("mydlp.hrl").

%% API
-export([to_lower_str/1]).
-export([to_lower/1]).
-export([xml_char/1]).
-export([normalize/1]).
-export([normalize/2]).
-export([reverse/1]).
-export([reverse/2]).


normalize(Bin) -> normalize(Bin, true).

normalize(Bin, IgnoreCase) -> normalize(Bin, IgnoreCase, <<>>, false).

normalize(<<>>, _IgnoreCase, Acc, _HitSpace) -> <<Acc/binary>>;
normalize(Bin, IgnoreCase, Acc, HitSpace) -> 
	case { get_uchar(Bin), IgnoreCase } of
		{{C,Rest}, false} 
				when C >= 65, C =< 90 -> 
			normalize(Rest, IgnoreCase, <<Acc/binary, C>> , false);
		{{C,Rest}, _} -> case {HitSpace, normal_bin(C)} of
				{true, <<32>>} -> normalize(Rest, IgnoreCase, Acc, true);
				{true, <<_J:2/binary, 32>>} -> normalize(Rest, IgnoreCase, Acc, true);
				{false, <<32>>} -> normalize(Rest, IgnoreCase, <<Acc/binary, 32>>, true);
				{false, <<_J:2/binary, 32>>} -> normalize(Rest, IgnoreCase, <<Acc/binary, 32>>, true);
				{_Else, CBin} -> normalize(Rest, IgnoreCase, <<Acc/binary, CBin/binary>> , false) end;
		{none, _} -> normalize(<<>>, IgnoreCase, Acc, HitSpace) end.

reverse(NormalBin) -> reverse(NormalBin, <<>>).

reverse(<<>>, Acc) -> Acc;
reverse(Bin, Acc) ->
	case get_uchar(Bin) of
		{C,Rest} when C >= 65, C =< 90 -> 
			reverse(Rest, <<C, Acc/binary>>);
		{C,Rest} ->
			CBin = normal_bin(C),
			reverse(Rest, <<CBin/binary, Acc/binary>>);
		none -> reverse(<<>>, Acc) end.

get_uchar(<<>>) -> none;
get_uchar(Bin) ->
        try     <<C/utf8, Rest/binary>> = Bin,
                {C, Rest}
        catch _:_ ->
                <<_:1/binary, Rest2/binary>> = Bin,
                get_uchar(Rest2) end.

normal_bin(C) when C =< 31 -> <<32>>;
normal_bin(32) -> <<32>>;
normal_bin(C) when C >= 33, C =< 47 -> <<32, C, 32>>; % punctuation
normal_bin(C) when C >= 58, C =< 64 -> <<32, C, 32>>; % punctuation
normal_bin(C) when C >= 91, C =< 96 -> <<32, C, 32>>; % punctuation
normal_bin(C) when C >= 123, C =< 127 -> <<32>>;
normal_bin(C) when C >= 8192, C =< 8303 -> <<32>>; %% Unicode General Punctuation Block	U+2000	U+206F
normal_bin(C) when C >= 11776, C =< 11903 -> <<32>>; %% Unicode Supplemental Punctuation	U+2E00	U+2E7F
normal_bin(C) when C >= 12288, C =< 12351 -> <<32>>; %% Unicode CJK Symbols and Punctuation	U+3000	U+303F
normal_bin(C) -> C1 = to_lower(C), <<C1/utf8>>.

to_lower_str(Str) -> to_lower_str(Str, []).

to_lower_str([C|Rest], Ret) -> to_lower_str(Rest, [to_lower(C)|Ret]);
to_lower_str([], Ret) -> lists:reverse(Ret).

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


xml_char("quot") -> 34;
xml_char("amp") -> 38;
xml_char("apos") -> 39;
xml_char("lt") -> 60;
xml_char("gt") -> 62;
xml_char("nbsp") -> 160;
xml_char("iexcl") -> 161;
xml_char("cent") -> 162;
xml_char("pound") -> 163;
xml_char("curren") -> 164;
xml_char("yen") -> 165;
xml_char("brvbar") -> 166;
xml_char("sect") -> 167;
xml_char("uml") -> 168;
xml_char("copy") -> 169;
xml_char("ordf") -> 170;
xml_char("laquo") -> 171;
xml_char("not") -> 172;
xml_char("shy") -> 173;
xml_char("reg") -> 174;
xml_char("macr") -> 175;
xml_char("deg") -> 176;
xml_char("plusmn") -> 177;
xml_char("sup2") -> 178;
xml_char("sup3") -> 179;
xml_char("acute") -> 180;
xml_char("micro") -> 181;
xml_char("para") -> 182;
xml_char("middot") -> 183;
xml_char("cedil") -> 184;
xml_char("sup1") -> 185;
xml_char("ordm") -> 186;
xml_char("raquo") -> 187;
xml_char("frac14") -> 188;
xml_char("frac12") -> 189;
xml_char("frac34") -> 190;
xml_char("iquest") -> 191;
xml_char("Agrave") -> 192;
xml_char("Aacute") -> 193;
xml_char("Acirc") -> 194;
xml_char("Atilde") -> 195;
xml_char("Auml") -> 196;
xml_char("Aring") -> 197;
xml_char("AElig") -> 198;
xml_char("Ccedil") -> 199;
xml_char("Egrave") -> 200;
xml_char("Eacute") -> 201;
xml_char("Ecirc") -> 202;
xml_char("Euml") -> 203;
xml_char("Igrave") -> 204;
xml_char("Iacute") -> 205;
xml_char("Icirc") -> 206;
xml_char("Iuml") -> 207;
xml_char("ETH") -> 208;
xml_char("Ntilde") -> 209;
xml_char("Ograve") -> 210;
xml_char("Oacute") -> 211;
xml_char("Ocirc") -> 212;
xml_char("Otilde") -> 213;
xml_char("Ouml") -> 214;
xml_char("times") -> 215;
xml_char("Oslash") -> 216;
xml_char("Ugrave") -> 217;
xml_char("Uacute") -> 218;
xml_char("Ucirc") -> 219;
xml_char("Uuml") -> 220;
xml_char("Yacute") -> 221;
xml_char("THORN") -> 222;
xml_char("szlig") -> 223;
xml_char("agrave") -> 224;
xml_char("aacute") -> 225;
xml_char("acirc") -> 226;
xml_char("atilde") -> 227;
xml_char("auml") -> 228;
xml_char("aring") -> 229;
xml_char("aelig") -> 230;
xml_char("ccedil") -> 231;
xml_char("egrave") -> 232;
xml_char("eacute") -> 233;
xml_char("ecirc") -> 234;
xml_char("euml") -> 235;
xml_char("igrave") -> 236;
xml_char("iacute") -> 237;
xml_char("icirc") -> 238;
xml_char("iuml") -> 239;
xml_char("eth") -> 240;
xml_char("ntilde") -> 241;
xml_char("ograve") -> 242;
xml_char("oacute") -> 243;
xml_char("ocirc") -> 244;
xml_char("otilde") -> 245;
xml_char("ouml") -> 246;
xml_char("divide") -> 247;
xml_char("oslash") -> 248;
xml_char("ugrave") -> 249;
xml_char("uacute") -> 250;
xml_char("ucirc") -> 251;
xml_char("uuml") -> 252;
xml_char("yacute") -> 253;
xml_char("thorn") -> 254;
xml_char("yuml") -> 255;
xml_char("OElig") -> 338;
xml_char("oelig") -> 339;
xml_char("Scaron") -> 352;
xml_char("scaron") -> 353;
xml_char("Yuml") -> 376;
xml_char("fnof") -> 402;
xml_char("circ") -> 710;
xml_char("tilde") -> 732;
xml_char("Alpha") -> 913;
xml_char("Beta") -> 914;
xml_char("Gamma") -> 915;
xml_char("Delta") -> 916;
xml_char("Epsilon") -> 917;
xml_char("Zeta") -> 918;
xml_char("Eta") -> 919;
xml_char("Theta") -> 920;
xml_char("Iota") -> 921;
xml_char("Kappa") -> 922;
xml_char("Lambda") -> 923;
xml_char("Mu") -> 924;
xml_char("Nu") -> 925;
xml_char("Xi") -> 926;
xml_char("Omicron") -> 927;
xml_char("Pi") -> 928;
xml_char("Rho") -> 929;
xml_char("Sigma") -> 931;
xml_char("Tau") -> 932;
xml_char("Upsilon") -> 933;
xml_char("Phi") -> 934;
xml_char("Chi") -> 935;
xml_char("Psi") -> 936;
xml_char("Omega") -> 937;
xml_char("alpha") -> 945;
xml_char("beta") -> 946;
xml_char("gamma") -> 947;
xml_char("delta") -> 948;
xml_char("epsilon") -> 949;
xml_char("zeta") -> 950;
xml_char("eta") -> 951;
xml_char("theta") -> 952;
xml_char("iota") -> 953;
xml_char("kappa") -> 954;
xml_char("lambda") -> 955;
xml_char("mu") -> 956;
xml_char("nu") -> 957;
xml_char("xi") -> 958;
xml_char("omicron") -> 959;
xml_char("pi") -> 960;
xml_char("rho") -> 961;
xml_char("sigmaf") -> 962;
xml_char("sigma") -> 963;
xml_char("tau") -> 964;
xml_char("upsilon") -> 965;
xml_char("phi") -> 966;
xml_char("chi") -> 967;
xml_char("psi") -> 968;
xml_char("omega") -> 969;
xml_char("thetasym") -> 977;
xml_char("upsih") -> 978;
xml_char("piv") -> 982;
xml_char("ensp") -> 8194;
xml_char("emsp") -> 8195;
xml_char("thinsp") -> 8201;
xml_char("zwnj") -> 8204;
xml_char("zwj") -> 8205;
xml_char("lrm") -> 8206;
xml_char("rlm") -> 8207;
xml_char("ndash") -> 8211;
xml_char("mdash") -> 8212;
xml_char("lsquo") -> 8216;
xml_char("rsquo") -> 8217;
xml_char("sbquo") -> 8218;
xml_char("ldquo") -> 8220;
xml_char("rdquo") -> 8221;
xml_char("bdquo") -> 8222;
xml_char("dagger") -> 8224;
xml_char("Dagger") -> 8225;
xml_char("bull") -> 8226;
xml_char("hellip") -> 8230;
xml_char("permil") -> 8240;
xml_char("prime") -> 8242;
xml_char("Prime") -> 8243;
xml_char("lsaquo") -> 8249;
xml_char("rsaquo") -> 8250;
xml_char("oline") -> 8254;
xml_char("frasl") -> 8260;
xml_char("euro") -> 8364;
xml_char("image") -> 8465;
xml_char("weierp") -> 8472;
xml_char("real") -> 8476;
xml_char("trade") -> 8482;
xml_char("alefsym") -> 8501;
xml_char("larr") -> 8592;
xml_char("uarr") -> 8593;
xml_char("rarr") -> 8594;
xml_char("darr") -> 8595;
xml_char("harr") -> 8596;
xml_char("crarr") -> 8629;
xml_char("lArr") -> 8656;
xml_char("uArr") -> 8657;
xml_char("rArr") -> 8658;
xml_char("dArr") -> 8659;
xml_char("hArr") -> 8660;
xml_char("forall") -> 8704;
xml_char("part") -> 8706;
xml_char("exist") -> 8707;
xml_char("empty") -> 8709;
xml_char("nabla") -> 8711;
xml_char("isin") -> 8712;
xml_char("notin") -> 8713;
xml_char("ni") -> 8715;
xml_char("prod") -> 8719;
xml_char("sum") -> 8721;
xml_char("minus") -> 8722;
xml_char("lowast") -> 8727;
xml_char("radic") -> 8730;
xml_char("prop") -> 8733;
xml_char("infin") -> 8734;
xml_char("ang") -> 8736;
xml_char("and") -> 8743;
xml_char("or") -> 8744;
xml_char("cap") -> 8745;
xml_char("cup") -> 8746;
xml_char("int") -> 8747;
xml_char("there4") -> 8756;
xml_char("sim") -> 8764;
xml_char("cong") -> 8773;
xml_char("asymp") -> 8776;
xml_char("ne") -> 8800;
xml_char("equiv") -> 8801;
xml_char("le") -> 8804;
xml_char("ge") -> 8805;
xml_char("sub") -> 8834;
xml_char("sup") -> 8835;
xml_char("nsub") -> 8836;
xml_char("sube") -> 8838;
xml_char("supe") -> 8839;
xml_char("oplus") -> 8853;
xml_char("otimes") -> 8855;
xml_char("perp") -> 8869;
xml_char("sdot") -> 8901;
xml_char("lceil") -> 8968;
xml_char("rceil") -> 8969;
xml_char("lfloor") -> 8970;
xml_char("rfloor") -> 8971;
xml_char("lang") -> 9001;
xml_char("rang") -> 9002;
xml_char("loz") -> 9674;
xml_char("spades") -> 9824;
xml_char("clubs") -> 9827;
xml_char("hearts") -> 9829;
xml_char("diams") -> 9830;
xml_char(_Else) -> not_found.

