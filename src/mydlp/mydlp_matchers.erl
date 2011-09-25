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
	file/1,
	mime_match/0,
	mime_match/1,
	mime_match/3,
	md5_match/0,
	md5_match/1,
	md5_match/3,
	md5_dr_match/0,
	md5_dr_match/1,
	md5_dr_match/3,
	regex_match/0,
	regex_match/1,
	regex_match/3,
	iban_match/0,
	iban_match/1,
	iban_match/3,
	trid_match/0,
	trid_match/1,
	trid_match/3,
	ssn_match/0,
	ssn_match/1,
	ssn_match/3,
	canada_sin_match/0,
	canada_sin_match/1,
	canada_sin_match/3,
	france_insee_match/0,
	france_insee_match/1,
	france_insee_match/3,
	uk_nino_match/0,
	uk_nino_match/1,
	uk_nino_match/3,
	e_file_match/0,
	e_file_match/1,
	e_file_match/3,
	e_archive_match/0,
	e_archive_match/1,
	e_archive_match/3,
	i_binary_match/0,
	i_binary_match/1,
	i_binary_match/3,
	i_archive_match/0,
	i_archive_match/1,
	i_archive_match/3,
	p_text_match/0,
	p_text_match/1,
	p_text_match/3,
	shash_match/0,
	shash_match/1,
	shash_match/3,
	bayes_match/0,
	bayes_match/1,
	bayes_match/3,
	scode_match/0,
	scode_match/1,
	scode_match/3,
	scode_ada_match/0,
	scode_ada_match/1,
	scode_ada_match/3,
	cc_match/0,
	cc_match/1,
	cc_match/3
]).

-include_lib("eunit/include/eunit.hrl").

mime_match() -> raw.

mime_match(MGIs) -> MGIs.

mime_match(MGIs, _Addr, File) ->
	MT = case File#file.mime_type of 
		undefined -> mydlp_tc:get_mime(File#file.data);
		Else -> Else
	end,

	case mydlp_mnesia:is_mime_of_gid(MT, MGIs) of
		true -> {pos, {file, File}};
		false -> neg end.

regex_match() -> text.

regex_match(RGIs) -> RGIs.

regex_match(RGIs, _Addr, File) ->
	case mydlp_regex:match(RGIs, File#file.text) of
		{match, {id, RId}, {group_id, GId}} -> 
			{pos, {file, File},
				{misc, "regex_id=" ++ integer_to_list(RId) ++
				" group_id=" ++ integer_to_list(GId)}};
		nomatch -> neg end.

cc_match() -> text.

cc_match(Conf) when is_list(Conf) -> 
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 1
	end, Count.

cc_match(Count, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	credit_card, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_cc(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> neg end.

iban_match() -> text.

iban_match(Conf) when is_list(Conf) -> 
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 1
	end, Count.

iban_match(Count, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	iban, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_iban(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> neg end.

trid_match() -> text.

trid_match(Conf) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 1
	end, Count.

trid_match(Count, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	trid, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_trid(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> neg end.

ssn_match() -> text.

ssn_match(Conf) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 5
	end, Count.

ssn_match(Count, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	ssn, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_ssn(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> neg end.

canada_sin_match() -> text.

canada_sin_match(Conf) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 5
	end, Count.

canada_sin_match(Count, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	sin, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_sin(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> neg end.

france_insee_match() -> text.

france_insee_match(Conf) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 5
	end, Count.

france_insee_match(Count, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	insee, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_insee(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> neg end.

uk_nino_match() -> text.

uk_nino_match(Conf) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 5
	end, Count.

uk_nino_match(Count, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	nino, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_nino(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> neg end.

-define(CFILE_MINSIZE, 128).

e_archive_match() -> analyzed.

e_archive_match(_Conf) -> none.

e_archive_match(_Opts, _Addr, #file{mime_type=MimeType, data=Data, is_encrypted=true} = File) -> 
	case size(Data) > ?CFILE_MINSIZE of
		true -> case mydlp_api:is_compression_mime(MimeType) of
			true -> {pos, {file, File}};
			false -> neg end;
		false -> neg end;
e_archive_match(_Opts, _Addr, _File) -> neg.

-define(EFILE_MINSIZE, 256).

e_file_match() -> analyzed.

e_file_match(_Conf) -> none.

e_file_match(_Opts, _Addr, #file{data=Data, mime_type=MimeType, is_encrypted=true} = File) -> 
	case size(Data) > ?EFILE_MINSIZE of
		% compressed files which are marked as encrypted, should not be handled here. (fun e_archive_match)
		true -> case mydlp_api:is_compression_mime(MimeType) of 
			true -> neg;
			false -> {pos, {file, File}} end;
		false -> neg end;
e_file_match(_Opts, _Addr, _File) -> neg.

i_binary_match() -> raw.

i_binary_match(_Conf) -> none.

i_binary_match(_Opts, _Addr, #file{mime_type=MimeType, data=Data} = File) ->
	case mydlp_api:is_cobject_mime(MimeType) of
		true -> case mydlp_tc:check_binary_integrity(Data) of
			false -> {pos, {file, File}};
			true -> neg end;
		false -> neg end.

i_archive_match() -> raw.

i_archive_match(_Conf) -> none.

i_archive_match(_Opts, _Addr, #file{mime_type=MimeType, data=Data} = File) ->
	case mydlp_api:is_compression_mime(MimeType) of
		true -> case mydlp_tc:check_archive_integrity(Data) of
			false -> {pos, {file, File}};
			true -> neg end;
		false -> neg end.

p_text_match() -> analyzed.

p_text_match(Conf) ->
	Score = case lists:keyfind(score, 1, Conf) of
		{score, S} -> S;
		false -> 512
	end, Score.

p_text_match(Limit, _Addr, #file{mime_type=MimeType} = File) ->
	case mydlp_api:is_compression_mime(MimeType) of
		true -> neg;
		false -> p_text_match1(Limit, File) end.

p_text_match1(Limit, #file{data=Data} = File) ->
	%% sequential operation seems more efficient than parallel, if needed uncomment below.
	%[S1, S2] = mydlp_api:pmap(fun(I) -> 
	%		mydlp_regex:longest_bin(I, Data) end,
	%		[hexencoded, base64encoded]),
	%Score = lists:max([S1/2,S2]),

	S1 = ( mydlp_regex:longest_bin(hexencoded, Data) ) / 2,
	S2 = mydlp_regex:longest_bin(base64encoded, Data),
	Score = lists:max([S1,S2]),

	case Score >= Limit of
		true -> {pos, {file, File}, 
			{misc, "score=" ++ integer_to_list(Score)}};
		false -> neg end.

md5_match() -> raw.

md5_match(HGIs) -> HGIs.

md5_match(HGIs, _Addr, File) ->
	Hash = erlang:md5(File#file.data),
	case mydlp_mnesia:is_fhash_of_gid(Hash, HGIs) of
		true -> {pos, {file, File}};
		false -> neg end.

md5_dr_match() -> raw.

md5_dr_match(_Conf) -> 
%	CustomerId = case lists:keyfind(cid, 1, Source) of
%		{cid, C} -> C;
%		false -> mydlp_mnesia:get_dcid()
%	end,
%	CustomerKey = {bl, CustomerId},
	_CustomerKey = {bl, mydlp_mnesia:get_dcid()}.   % should be refined for multisite usage

md5_dr_match(CustomerKey, _Addr, File) ->
	Hash = erlang:md5(File#file.data),
	case mydlp_mnesia:is_dr_fh_of_fid(Hash, CustomerKey) of
		true -> {pos, {file, File}};
		false -> neg end.

shash_match() -> text.

shash_match(Conf) when is_list(Conf) -> 
	Perc = case lists:keyfind(percentage, 1, Conf) of
		{percentage, P} -> P;
		false -> 0.5
	end,
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 50
	end,
	HGIs = case lists:keyfind(group_ids, 1, Conf) of
		{group_ids, G} -> G;
		false -> []
	end,
	{HGIs, Perc, Count}.

shash_match({HGIs, Perc, Count}, _Addr, File) ->
	Res2 = mydlp_api:get_nsh(File#file.text),
	Res3 = lists:filter(fun(I) -> mydlp_mnesia:is_shash_of_gid(I, HGIs) end, Res2),
	TotalLen = length(Res2), MatchLen = length(Res3),
	case TotalLen of 
		0 -> neg;
		_Else ->
			FilePerc = MatchLen/TotalLen,
			case ((Perc /= undefined) and (Perc < (MatchLen/TotalLen))) or
				((Count /= undefined) and ( Count < MatchLen)) of
				true -> {pos, {file, File}, 
					{misc, "count=" ++ integer_to_list(MatchLen) ++
					" percentage=" ++ float_to_list(float(FilePerc))}};
				false -> neg
			end
	end.

bayes_match() -> text.

bayes_match(Conf) when is_list(Conf) ->
	Threshold = case lists:keyfind(threshold, 1, Conf) of
		{threshold, T} -> T;
		false -> 0.5
	end, Threshold.

bayes_match(Threshold, _Addr, File) ->
	BayesScore = bayeserl:score(File#file.text),
	case (BayesScore > Threshold) of
		true -> {pos, {file, File}, 
			{misc, "score=" ++ float_to_list(float(BayesScore))}};
		false -> neg end.

file(Conf) ->
	WF = case lists:keyfind(whitefile, 1, Conf) of
                {whitefile, true} -> {whitefile, []};
                _Else -> []
        end,
	Groups = case lists:keyfind(group_ids, 1, Conf) of
                {group_ids, G} -> G;
                _Else2 -> []
        end,
	FH = {md5_match, Groups},
	SH = case lists:keyfind(shash, 1, Conf) of
                {shash, true} ->
			SFP = [{group_ids, Groups}],
			SFP1 = SFP ++ case lists:keyfind(shash_count, 1, Conf) of
					{shash_count, C} -> [{count, C}];
					_Else3 -> []
			end,
			SFP2 = SFP1 ++ case lists:keyfind(shash_percentage, 1, Conf) of
					{shash_percentage, P} -> [{percentage, P}];
					_Else4 -> []
			end,
			{shash_match, lists:flatten(SFP2)};
                _Else5 -> []
        end,
	BYS = case lists:keyfind(bayes, 1, Conf) of
                {bayes, true} -> 
			BFP = case lists:keyfind(bayes_threshold, 1, Conf) of
					{bayes_threshold, T} -> [{threshold, T}];
					_Else6 -> []
			end,
			{bayes_match, BFP};
                _Else7 -> []
        end,
	lists:flatten([WF, FH, SH, BYS]).

scode_match() -> text.

scode_match(Conf) when is_list(Conf) -> 
	Score = case lists:keyfind(score, 1, Conf) of
		{score, S} -> S;
		false -> 100
	end, Score.

scode_match(Limit, _Addr, File) ->
	Score = mydlp_regex:score_suite(
	 	scode, 
		File#file.text),

	case Score >= Limit of
		true -> {pos, {file, File}, 
			{misc, "score=" ++ integer_to_list(Score)}};
		false -> neg end.

scode_ada_match() -> text.

scode_ada_match(Conf) when is_list(Conf) -> 
	Score = case lists:keyfind(score, 1, Conf) of
		{score, S} -> S;
		false -> 100
	end, Score.

scode_ada_match(Limit, _Addr, File) ->
	Score = mydlp_regex:score_suite(
	 	scode_ada, 
		File#file.text),

	case Score >= Limit of
		true -> {pos, {file, File}, 
			{misc, "score=" ++ integer_to_list(Score)}};
		false -> neg end.

