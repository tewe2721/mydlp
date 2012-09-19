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

%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@mydlp.com>
%%% @copyright 2010, H. Kerem Cevahir
%%% @doc Matcher repository for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_matchers).
-author("kerem@mydlp.com").

-include("mydlp.hrl").

%% API
-export([
	mc_match/4,
	md5_match/0,
	md5_match/1,
	md5_match/2,
	pdm_match/0,
	pdm_match/1,
	pdm_match/2,
	regex_match/0,
	regex_match/1,
	regex_match/2,
	keyword_match/0,
	keyword_match/1,
	iban_match/0,
	iban_match/1,
	iban_match/2,
	cc_track1_match/0,
	cc_track1_match/1,
	cc_track1_match/2,
	cc_track2_match/0,
	cc_track2_match/1,
	cc_track2_match/2,
	cc_track3_match/0,
	cc_track3_match/1,
	cc_track3_match/2,
	aba_match/0,
	aba_match/1,
	aba_match/2,
	trid_match/0,
	trid_match/1,
	trid_match/2,
	ssn_match/0,
	ssn_match/1,
	ssn_match/2,
	canada_sin_match/0,
	canada_sin_match/1,
	canada_sin_match/2,
	france_insee_match/0,
	france_insee_match/1,
	france_insee_match/2,
	uk_nino_match/0,
	uk_nino_match/1,
	uk_nino_match/2,
	spain_dni_match/0,
	spain_dni_match/1,
	spain_dni_match/2,
	italy_fc_match/0,
	italy_fc_match/1,
	italy_fc_match/2,
	said_match/0,
	said_match/1,
	said_match/2,
	e_file_match/0,
	e_file_match/1,
	e_file_match/2,
	e_archive_match/0,
	e_archive_match/1,
	e_archive_match/2,
	p_text_match/0,
	p_text_match/1,
	p_text_match/2,
	scode_match/0,
	scode_match/1,
	scode_match/2,
	scode_ada_match/0,
	scode_ada_match/1,
	scode_ada_match/2,
	cc_match/0,
	cc_match/1,
	cc_match/2,
	pan_match/0,
	pan_match/1,
	pan_match/2,
	cpf_match/0,
	cpf_match/1,
	cpf_match/2,
	china_icn_match/0,
	china_icn_match/1,
	china_icn_match/2,
	cc_edate_match/0,
	cc_edate_match/1,
	cc_edate_match/2,
	birthdate_match/0,
	birthdate_match/1,
	birthdate_match/2,
	gdate_match/0,
	gdate_match/1,
	gdate_match/2
]).

-include_lib("eunit/include/eunit.hrl").

-define(P(Pattern), mydlp_api:generate_patterns(Pattern)).

mc_is_apply(Func) ->
	{_, {distance, _}, {pd, IsApply}, {kw, _}} = apply(mydlp_matchers, Func, []), IsApply.

mc_match(MatcherId, Func, FuncOpts, #file{mc_table=MCTable, normal_text=NT}) ->
	Matched = lists:filter(fun({_I, _CI, {_L, ML}}) -> lists:member(MatcherId, ML) end, MCTable),
	MatchedIndex = case mc_is_apply(Func) of
		true -> lists:map(fun({I, CI, {L, _ML}}) ->
				Head = size(NT) + CI - L,
				<<_:Head/binary, Phrase:L/binary, _/binary>> = NT,
				PhraseS = unicode:characters_to_list(Phrase),
				case apply(mydlp_matchers, Func, [FuncOpts, PhraseS]) of
					true -> [I];
					false -> [] end
			end, Matched);
		false -> lists:map(fun({I, _CI, {_L, _ML}}) -> I end, Matched) end,
	MI = lists:flatten(MatchedIndex),
	{length(MI), MI}.

regex_match() -> {normalized, {distance, true}, {pd, false}, {kw, false}}.

regex_match({conf, RGIs}) -> RGIs.

regex_match(RGIs, File) ->
	mydlp_regex:match_count(RGIs, File#file.text).

keyword_match({conf, _Conf}) -> none.

keyword_match() -> {normalized, {distance, true}, {pd, false}, {kw, true}}.

cc_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

cc_match({conf, _Conf}) -> none;

cc_match({pd_patterns, "narrow"}) ->
	?P({[{numeric, 4}, ws, {numeric, 4}, ws, {numeric, 4}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 4}, ws, {numeric, 6}, ws, {numeric, 5}], encap_ws}) ++
	?P({[{numeric, 4}, ws, {numeric, 6}, ws, {numeric, 4}], encap_ws});

cc_match({pd_patterns, "normal"}) -> 
	?P({[{numeric, 4}, ws, {numeric, 4}, ws, {numeric, 4}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 4}, ws, {numeric, 6}, ws, {numeric, 5}], encap_ws}) ++
	?P({[{numeric, 4}, ws, {numeric, 6}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, {13,16}}], encap_ws});

cc_match({pd_patterns, "wide"}) -> 
	?P({[{numeric, 4}, ws, {numeric, 4}, ws, {numeric, 4}, ws, {numeric, 4}], join_ws}) ++
	?P({[{numeric, 4}, ws, {numeric, 6}, ws, {numeric, 5}], join_ws}) ++
	?P({[{numeric, 4}, ws, {numeric, 6}, ws, {numeric, 4}], join_ws}) ++
	?P({[{numeric, {13,16}}], join_ws}) ++
	?P({[{numeric, 4}, ws, {numeric, 4}, ws, {numeric, 4}, ws, {numeric, 4}], none}) ++
	?P({[{numeric, 4}, ws, {numeric, 6}, ws, {numeric, 5}], none}) ++
	?P({[{numeric, 4}, ws, {numeric, 6}, ws, {numeric, 4}], none}) ++
	?P({[{numeric, {13,16}}], none}).

cc_match(_Conf, Phrase) -> mydlp_api:is_valid_cc(Phrase).
%cc_match(_Conf, File) ->
%	{Data, IndexList} = mydlp_regex:match_bin(
%	 	credit_card, 
%		File#file.text),
%	WIList = mydlp_api:regex_filter_map(fun(I) -> mydlp_api:is_valid_cc(I) end, Data, IndexList),
%	{length(WIList), WIList}.

cc_track1_match() -> {normalized, {distance, true}, {pd, false}, {kw, false}}.	

cc_track1_match({conf, _Conf}) -> none.

cc_track1_match(_Conf, File) ->
	{Data, IndexList} = mydlp_regex:match_bin(
		cc_track1,
		File#file.text),
	io:format("~ts~n",[File#file.text]),
	WIList = mydlp_api:regex_filter_map(fun(I) -> mydlp_api:is_valid_cc_track(I) end, Data, IndexList),
	{length(WIList), WIList}.

cc_track2_match() -> {normalized, {distance, true}, {pd, false}, {kw, false}}.	

cc_track2_match({conf, _Conf}) -> none.

cc_track2_match(_Conf, File) ->
	{Data, IndexList} = mydlp_regex:match_bin(
		cc_track2,
		File#file.text),
	io:format("~ts~n",[File#file.text]),
	WIList = mydlp_api:regex_filter_map(fun(I) -> mydlp_api:is_valid_cc_track(I) end, Data, IndexList),
	{length(WIList), WIList}.


cc_track3_match() -> {normalized, {distance, true}, {pd, false}, {kw, false}}.	

cc_track3_match({conf, _Conf}) -> none.

cc_track3_match(_Conf, File) ->
	{Data, IndexList} = mydlp_regex:match_bin(
		cc_track3,
		File#file.text),
	WIList = mydlp_api:regex_filter_map(fun(I) -> mydlp_api:is_valid_cc_track(I) end, Data, IndexList),
	{length(WIList), WIList}.

iban_match() -> {normalized, {distance, true}, {pd, false}, {kw, false}}. 

iban_match({conf, _Conf}) -> none.

iban_match(_Conf, File) ->
	{Data, IndexList} = mydlp_regex:match_bin(
	 	iban, 
		File#file.normal_text),
	WIList = mydlp_api:regex_filter_map(fun(I) -> mydlp_api:is_valid_iban(I) end, Data, IndexList),
	{length(WIList), WIList}.

aba_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

aba_match({conf, _Conf}) -> none;

aba_match({pd_patterns, "narrow"}) ->
	?P({[{numeric, 4}, {special, "-"}, {numeric, 4}, {special, "-"}, {numeric, 1}], encap_ws});
aba_match({pd_patterns, "normal"}) ->
	?P({[{numeric, 4}, {special, "-"}, {numeric, 4}, {special, "-"}, {numeric, 1}], encap_ws}) ++
	?P({[{numeric, 9}], encap_ws});
aba_match({pd_patterns, "wide"}) ->
	?P({[{numeric, 4}, {special, "-"}, {numeric, 4}, {special, "-"}, {numeric, 1}], none}) ++
	?P({[{numeric, 9}], none}) ++
	?P({[{numeric, 4}, {special, "-"}, {numeric, 4}, {special, "-"}, {numeric, 1}], join_ws}) ++
	?P({[{numeric, 9}], join_ws}).

aba_match(_Conf, Phrase) -> mydlp_api:is_valid_aba(Phrase).

trid_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

trid_match({conf, _Conf}) -> none;

trid_match({pd_patterns, "narrow"}) -> ?P({[{numeric, 11}], encap_ws});
trid_match({pd_patterns, "normal"}) -> ?P({[{numeric, 11}], encap_ws});
trid_match({pd_patterns, "wide"}) ->
	?P({[{numeric, 11}], none}),
	?P({[{numeric, 11}], join_ws}).

trid_match(_Conf, Phrase) -> mydlp_api:is_valid_trid(Phrase).

pan_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

pan_match({conf, _Conf}) -> none;

pan_match({pd_patterns, "narrow"}) -> ?P({[{alpha, 5}, {numeric, 4}, {alpha, 1}], encap_ws});
pan_match({pd_patterns, "normal"}) -> ?P({[{alpha, 5}, {numeric, 4}, {alpha, 1}], encap_ws});
pan_match({pd_patterns, "wide"}) -> 
	?P({[{alpha, 5}, {numeric, 4}, {alpha, 1}], none}) ++ 
	?P({[{alpha, 5}, {numeric, 4}, {alpha, 1}], join_ws}).

pan_match(_Conf, Phrase) -> mydlp_api:is_valid_pan(Phrase).

cpf_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

cpf_match({conf, _Conf}) -> none;

cpf_match({pd_patterns, "narrow"}) -> ?P({[{numeric, 3}, {special, "."}, {numeric, 3}, {special, "."}, {numeric, 3}, {special, "-"}, {numeric, 2}], encap_ws});
cpf_match({pd_patterns, "normal"}) -> 
	?P({[{numeric, 3}, {special, "."}, {numeric, 3}, {special, "."}, {numeric, 3}, {special, "-"}, {numeric, 2}], encap_ws}) ++
	?P({[{numeric, 3}, ws, {special, "."}, ws, {numeric, 3}, ws, {special, "."}, ws, {numeric, 3}, ws, {special, "-"}, ws, {numeric, 2}], encap_ws});
cpf_match({pd_patterns, "wide"}) -> 
	?P({[{numeric, 3}, {special, "."}, {numeric, 3}, {special, "."}, {numeric, 3}, {special, "-"}, {numeric, 2}], none}) ++
	?P({[{numeric, 3}, {special, "."}, {numeric, 3}, {special, "."}, {numeric, 3}, {special, "-"}, {numeric, 2}], join_ws}).

cpf_match(_Conf, Phrase) -> mydlp_api:is_valid_cpf(Phrase).

china_icn_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

china_icn_match({conf, _Conf}) -> none;

china_icn_match({pd_patterns, "narrow"}) -> 
	?P({[{numeric, 18}], encap_ws}) ++
	?P({[{numeric, 17}, {alpha, 1}], encap_ws});
china_icn_match({pd_patterns, "normal"}) ->
	?P({[{numeric, 18}], encap_ws}) ++
	?P({[{numeric, 17}, {alpha, 1}], encap_ws});
china_icn_match({pd_patterns, "wide"}) ->
	?P({[{numeric, 18}], none}) ++
	?P({[{numeric, 17}, {alpha, 1}], join_ws}).

china_icn_match(_Conf, Phrase) -> mydlp_api:is_valid_china_icn(Phrase).

cc_edate_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

cc_edate_match({conf, _Conf}) -> none;

cc_edate_match({pd_patterns, "narrow"}) -> ?P({[{numeric, 2}, {special, "-/"}, {numeric, 2}], encap_ws});
cc_edate_match({pd_patterns, "normal"}) -> ?P({[{numeric, 2}, {special, "-/"}, {numeric, 2}], encap_ws});
cc_edate_match({pd_patterns, "wide"}) -> 
	 ?P({[{numeric, 2}, {special, "-/"}, {numeric, 2}], none}) ++
	 ?P({[{numeric, 2}, {special, "-/"}, {numeric, 2}], join_ws}).

cc_edate_match(_Conf, Phrase) -> mydlp_api:is_valid_cc_edate(Phrase).

gdate_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

gdate_match({conf, _Conf}) -> none;
	
gdate_match({pd_patterns, "narrow"}) -> 
	?P({[{numeric, 2}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 2}, {special, "/-"}, {alpha, 3}, {special, "/-"}, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 2}, ws, {alpha, 3}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{alpha, 3}, ws, {numeric, {1, 2}}, {special, ","}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 4}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 2}], encap_ws});
gdate_match({pd_patterns, "normal"}) ->
	?P({[{numeric, 2}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 2}, {special, "/-"}, {alpha, 3}, {special, "/-"}, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 2}, ws, {alpha, 3}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{alpha, 3}, ws, {numeric, {1, 2}}, {special, ","}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 4}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 2}], encap_ws});
gdate_match({pd_patterns, "wide"}) ->
	?P({[{numeric, 2}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 4}], none}) ++
	?P({[{numeric, 2}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 4}], none}) ++
	?P({[{numeric, 2}, {special, "/-"}, {alpha, 3}, {special, "/-"}, {numeric, 4}], none}) ++
	?P({[{numeric, 2}, ws, {alpha, 3}, ws, {numeric, 4}], none}) ++
	?P({[{alpha, 3}, ws, {numeric, {1, 2}}, {special, ","}, ws, {numeric, 4}], none}).

gdate_match(_Conf, Phrase) -> mydlp_api:is_valid_date(Phrase).

birthdate_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

birthdate_match({conf, _Conf}) -> none;

birthdate_match({pd_patterns, "narrow"}) -> 
	?P({[{numeric, 2}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 2}, {special, "/-"}, {alpha, 3}, {special, "/-"}, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 2}, ws, {alpha, 3}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{alpha, 3}, ws, {numeric, {1, 2}}, {special, ","}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 4}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 2}], encap_ws});
birthdate_match({pd_patterns, "normal"}) ->
	?P({[{numeric, 2}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 2}, {special, "/-"}, {alpha, 3}, {special, "/-"}, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 2}, ws, {alpha, 3}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{alpha, 3}, ws, {numeric, {1, 2}}, {special, ","}, ws, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 4}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 2}], encap_ws});
birthdate_match({pd_patterns, "wide"}) ->
	?P({[{numeric, 2}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 4}], none}) ++
	?P({[{numeric, 2}, {special, "/-"}, {numeric, 2}, {special, "/-"}, {numeric, 4}], none}) ++
	?P({[{numeric, 2}, {special, "/-"}, {alpha, 3}, {special, "/-"}, {numeric, 4}], none}) ++
	?P({[{numeric, 2}, ws, {alpha, 3}, ws, {numeric, 4}], none}) ++
	?P({[{alpha, 3}, ws, {numeric, {1, 2}}, {special, ","}, ws, {numeric, 4}], none}).

birthdate_match(_Conf, Phrase) -> mydlp_api:is_valid_birthdate(Phrase).

ssn_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

ssn_match({conf, _Conf}) -> none;

ssn_match({pd_patterns, "narrow"}) -> ?P({[{numeric, 3}, {special, "-"}, {numeric, 2}, {special, "-"}, {numeric, 4}], encap_ws});
ssn_match({pd_patterns, "normal"}) -> ?P({[{numeric, 3}, {special, "-"}, {numeric, 2}, {special, "-"}, {numeric, 4}], encap_ws});
ssn_match({pd_patterns, "wide"}) ->
	?P({[{numeric, 3}, {special, "-"}, {numeric, 2}, {special, "-"}, {numeric, 4}], none}) ++
	?P({[{numeric, 3}, {special, "-"}, {numeric, 2}, {special, "-"}, {numeric, 4}], join_ws}).

ssn_match(_Conf, Phrase) -> mydlp_api:is_valid_ssn(Phrase).

canada_sin_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

canada_sin_match({conf, _Conf}) -> none;

canada_sin_match({pd_patterns, "narrow"}) -> ?P({[{numeric, 3}, {special, "-"}, {numeric, 2}, {special, "-"}, {numeric, 4}], encap_ws});	
canada_sin_match({pd_patterns, "normal"}) -> 
	?P({[{numeric, 3}, {special, "-"}, {numeric, 2}, {special, "-"}, {numeric, 4}], encap_ws}) ++
	?P({[{numeric, 3}, ws, {numeric, 2}, ws, {numeric, 4}], encap_ws});	
canada_sin_match({pd_patterns, "wide"}) ->
	?P({[{numeric, 3}, {special, "-"}, {numeric, 2}, {special, "-"}, {numeric, 4}], none}) ++
	?P({[{numeric, 3}, ws, {numeric, 2}, ws, {numeric, 4}], none}) ++	
	?P({[{numeric, 3}, {special, "-"}, {numeric, 2}, {special, "-"}, {numeric, 4}], join_ws}) ++
	?P({[{numeric, 3}, ws, {numeric, 2}, ws, {numeric, 4}], join_ws}).	

canada_sin_match(_Conf, Phrase) -> mydlp_api:is_valid_sin(Phrase).

france_insee_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

france_insee_match({conf, _Conf}) -> none;

france_insee_match({pd_patterns, "narrow"}) -> 
	?P({[{numeric, 13}], encap_ws}) ++
	?P({[{numeric, 15}], encap_ws});
france_insee_match({pd_patterns, "normal"}) ->
	?P({[{numeric, 1}, ws, {numeric, 2}, ws, {numeric, 2}, ws, {numeric, 5}, ws, {numeric, 3}], encap_ws}) ++
	?P({[{numeric, 1}, ws, {numeric, 2}, ws, {numeric, 2}, ws, {numeric, 5}, ws, {numeric, 3}, ws, {numeric, 2}], encap_ws}) ++
	?P({[{numeric, 13}], encap_ws}) ++
	?P({[{numeric, 15}], encap_ws}); 
france_insee_match({pd_patterns, "wide"}) ->
	?P({[{numeric, 1}, ws, {numeric, 2}, ws, {numeric, 2}, ws, {numeric, 5}, ws, {numeric, 3}], none}) ++
	?P({[{numeric, 1}, ws, {numeric, 2}, ws, {numeric, 2}, ws, {numeric, 5}, ws, {numeric, 3}, ws, {numeric, 2}], none}) ++
	?P({[{numeric, 13}], none}) ++
	?P({[{numeric, 15}], none}) ++ 
	?P({[{numeric, 1}, ws, {numeric, 2}, ws, {numeric, 2}, ws, {numeric, 5}, ws, {numeric, 3}], join_ws}) ++
	?P({[{numeric, 1}, ws, {numeric, 2}, ws, {numeric, 2}, ws, {numeric, 5}, ws, {numeric, 3}, ws, {numeric, 2}], join_ws}) ++
	?P({[{numeric, 13}], join_ws}) ++
	?P({[{numeric, 15}], join_ws}). 

france_insee_match(_Conf, Phrase) -> mydlp_api:is_valid_insee(Phrase).

uk_nino_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

uk_nino_match({conf, _Conf}) -> none;

uk_nino_match({pd_patterns, "narrow"}) -> ?P({[{alpha, 2}, {numeric, 6}, {alpha, {0, 1}}], encap_ws});
uk_nino_match({pd_patterns, "normal"}) -> ?P({[{alpha, 2}, {numeric, 6}, {alpha, {0, 1}}], encap_ws});
uk_nino_match({pd_patterns, "wide"}) -> 
	?P({[{alpha, 2}, {numeric, 6}, {alpha, {0, 1}}], none}) ++
	?P({[{alpha, 2}, {numeric, 6}, {alpha, {0, 1}}], join_ws}).

uk_nino_match(_Conf, Phrase) -> mydlp_api:is_valid_nino(Phrase).

spain_dni_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

spain_dni_match({conf, _Conf}) -> none;

spain_dni_match({pd_patterns, "narrow"}) -> ?P({[{numeric, 8}, {special, "-"}, {alpha, 1}], encap_ws});
spain_dni_match({pd_patterns, "normal"}) -> ?P({[{numeric, 8}, {special, "-"}, {alpha, 1}], encap_ws});
spain_dni_match({pd_patterns, "wide"}) -> 
	?P({[{numeric, 8}, {alpha, 1}], none}) ++
	?P({[{numeric, 8}, {alpha, 1}], join_ws}) ++
	?P({[{numeric, 8}, {special, "-"}, {alpha, 1}], none}) ++
	?P({[{numeric, 8}, {special, "-"}, {alpha, 1}], join_ws}).

spain_dni_match(_Conf, Phrase) -> mydlp_api:is_valid_dni(Phrase).

italy_fc_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

italy_fc_match({conf, _Conf}) -> none;

italy_fc_match({pd_patterns, "narrow"}) -> ?P({[{alpha, 3}, ws, {alpha, 3}, ws, {numeric, 2}, {alpha, 1}, {numeric, 2}, ws, {alpha, 1}, {numeric, 3}, {alpha, 1}], encap_ws});
italy_fc_match({pd_patterns, "normal"}) -> 
	?P({[{alpha, 3}, ws, {alpha, 3}, ws, {numeric, 2}, {alpha, 1}, {numeric, 2}, ws, {alpha, 1}, {numeric, 3}, {alpha, 1}], encap_ws}) ++
	?P({[{alpha, 3}, {alpha, 3}, {numeric, 2}, {alpha, 1}, {numeric, 2}, {alpha, 1}, {numeric, 3}, {alpha, 1}], encap_ws});
italy_fc_match({pd_patterns, "wide"}) -> 
	?P({[{alpha, 3}, ws, {alpha, 3}, ws, {numeric, 2}, {alpha, 1}, {numeric, 2}, ws, {alpha, 1}, {numeric, 3}, {alpha, 1}], nonne}) ++
	?P({[{alpha, 3}, {alpha, 3}, {numeric, 2}, {alpha, 1}, {numeric, 2}, {alpha, 1}, {numeric, 3}, {alpha, 1}], none}) ++
	?P({[{alpha, 3}, ws, {alpha, 3}, ws, {numeric, 2}, {alpha, 1}, {numeric, 2}, ws, {alpha, 1}, {numeric, 3}, {alpha, 1}], join_ws}) ++
	?P({[{alpha, 3}, {alpha, 3}, {numeric, 2}, {alpha, 1}, {numeric, 2}, {alpha, 1}, {numeric, 3}, {alpha, 1}], join_ws}).

italy_fc_match(_Conf, Phrase) -> mydlp_api:is_valid_fc(Phrase).

said_match() -> {normalized, {distance, true}, {pd, true}, {kw, false}}.

said_match({conf, _Conf}) -> none;

said_match({pd_patterns, "narrow"}) -> ?P({[{numeric, 13}], encap_ws});
said_match({pd_patterns, "normal"}) -> ?P({[{numeric, 13}], encap_ws});
said_match({pd_patterns, "wide"}) ->
	?P({[{numeric, 13}], none}) ++
	?P({[{numeric, 13}], join_ws}).

said_match(_Conf, Phrase) -> mydlp_api:is_valid_said(Phrase).

-define(CFILE_MINSIZE, 128).

e_archive_match() -> {analyzed, {distance, false}, {pd, false}, {kw, false}}.

e_archive_match({conf, _Conf}) -> none.

e_archive_match(_Opts, #file{mime_type=MimeType, data=Data, is_encrypted=true}) -> 
	case size(Data) > ?CFILE_MINSIZE of
		true -> case mydlp_api:is_compression_mime(MimeType) of
			true -> 1;
			false -> 0 end;
		false -> 0 end;
e_archive_match(_Opts, _File) -> 0.

-define(EFILE_MINSIZE, 256).

e_file_match() -> {analyzed, {distance, false}, {pd, false}, {kw, false}}.

e_file_match({conf, _Conf}) -> none.

e_file_match(_Opts, #file{data=Data, mime_type=MimeType, is_encrypted=true}) -> 
	case size(Data) > ?EFILE_MINSIZE of
		% compressed files which are marked as encrypted, should not be handled here. (fun e_archive_match)
		true -> case mydlp_api:is_compression_mime(MimeType) of 
			true -> 0;
			false -> 1 end;
		false -> 0 end;
e_file_match(_Opts, _File) -> 0.

p_text_match() -> {analyzed, {distance, false}, {pd, false}, {kw, false}}.

p_text_match({conf, _Conf}) -> none.

p_text_match(_Conf, #file{mime_type=MimeType} = File) ->
	case mydlp_api:is_compression_mime(MimeType) of
		true -> 0;
		false -> p_text_match1(File) end.

p_text_match1(#file{data=Data}) ->
	%% sequential operation seems more efficient than parallel, if needed uncomment below.
	%[S1, S2] = mydlp_api:pmap(fun(I) -> 
	%		mydlp_regex:longest_bin(I, Data) end,
	%		[hexencoded, base64encoded]),
	%Score = lists:max([S1/2,S2]),

	S1 = ( mydlp_regex:longest_bin(hexencoded, Data) ) / 2,
	S2 = mydlp_regex:longest_bin(base64encoded, Data),
	lists:max([S1,S2]).

% TODO: refine this
md5_match() -> {raw, {distance, false}, {pd, false}, {kw, false}}.

md5_match({conf, [HGI]}) -> HGI.

md5_match(HGI, File) ->
	Hash = erlang:md5(File#file.data),
	case mydlp_mnesia:is_hash_of_gid(Hash, HGI) of
		true -> 1;
		false -> 0 end.

pdm_match() -> {text, {distance, false}, {pd, false}, {kw, false}}.

pdm_match({conf, [FGI]}) -> FGI.

pdm_match(FGI, File) ->
	FList = mydlp_pdm:fingerprint(File#file.text),
	mydlp_mnesia:pdm_of_gid(FList, FGI).

scode_match() -> {text, {distance, false}, {pd, false}, {kw, false}}.

scode_match({conf, _Conf}) -> none.

scode_match(_Conf, File) ->
	mydlp_regex:score_suite(scode, File#file.text).

scode_ada_match() -> {text, {distance, false}, {pd, false}, {kw, false}}.

scode_ada_match({conf, _Conf}) -> none.

scode_ada_match(_Conf, File) ->
	mydlp_regex:score_suite(scode_ada, File#file.text).

