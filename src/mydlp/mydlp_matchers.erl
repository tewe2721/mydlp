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

%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@medratech.com>
%%% @copyright 2010, H. Kerem Cevahir
%%% @doc Matcher repository for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_matchers).
-author("kerem@medra.com.tr").

-include("mydlp.hrl").

%% API
-export([
	file/1,
	mime_match/0,
	mime_match/2,
	md5_match/0,
	md5_match/2,
	md5_dr_match/0,
	md5_dr_match/2,
	regex_match/0,
	regex_match/2,
	iban_match/0,
	iban_match/2,
	trid_match/0,
	trid_match/2,
	ssn_match/0,
	ssn_match/2,
	canada_sin_match/0,
	canada_sin_match/2,
	france_insee_match/0,
	france_insee_match/2,
	uk_nino_match/0,
	uk_nino_match/2,
	e_file_match/0,
	e_file_match/2,
	e_archive_match/0,
	e_archive_match/2,
	i_binary_match/0,
	i_binary_match/2,
	i_archive_match/0,
	i_archive_match/2,
	p_text_match/0,
	p_text_match/2,
	shash_match/0,
	shash_match/2,
	bayes_match/0,
	bayes_match/2,
	scode_match/0,
	scode_match/2,
	scode_ada_match/0,
	scode_ada_match/2,
	cc_match/0,
	cc_match/2
]).

-include_lib("eunit/include/eunit.hrl").

mime_match() -> raw.

mime_match(MGIs, {_Addr, Files}) -> mime_match(MGIs, Files);
mime_match(MGIs, [File|Files]) ->
	MT = case File#file.mime_type of 
		undefined -> mydlp_tc:get_mime(File#file.data);
		Else -> Else
	end,

	case mydlp_mnesia:is_mime_of_gid(MT, MGIs) of
		true -> {pos, {file, File}};
		false -> mime_match(MGIs, Files)
	end;
mime_match(_MimeTypes, []) -> neg.

regex_match() -> text.

regex_match(RGIs, {_Addr, Files}) -> regex_match(RGIs, Files);
regex_match(RGIs, [File|Files]) ->
	case mydlp_regex:match(RGIs, File#file.text) of
		{match, {id, RId}, {group_id, GId}} -> 
			{pos, {file, File},
				{misc, "regex_id=" ++ integer_to_list(RId) ++
				" group_id=" ++ integer_to_list(GId)}};
		nomatch -> regex_match(RGIs, Files)
	end;
regex_match(_RGIs, []) -> neg.

cc_match() -> text.

cc_match(Conf, {_Addr, Files}) when is_list(Conf) -> 
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 1
	end,
	cc_match1(Count, Files).

cc_match1(Count, [File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	credit_card, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_cc(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> cc_match1(Count, Files)
	end;
cc_match1(_Count, []) -> neg.

iban_match() -> text.

iban_match(Conf, {_Addr, Files}) when is_list(Conf) -> 
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 1
	end,
	iban_match1(Count, Files).

iban_match1(Count, [File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	iban, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_iban(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> iban_match1(Count, Files)
	end;
iban_match1(_Count, []) -> neg.

trid_match() -> text.

trid_match(Conf, {_Addr, Files}) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 1
	end,
	trid_match1(Count, Files).

trid_match1(Count, [File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	trid, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_trid(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> trid_match1(Count, Files)
	end;
trid_match1(_Count, []) -> neg.

ssn_match() -> text.

ssn_match(Conf, {_Addr, Files}) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 5
	end,
	ssn_match1(Count, Files).

ssn_match1(Count, [File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	ssn, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_ssn(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> ssn_match1(Count, Files)
	end;
ssn_match1(_Count, []) -> neg.

canada_sin_match() -> text.

canada_sin_match(Conf, {_Addr, Files}) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 5
	end,
	canada_sin_match1(Count, Files).

canada_sin_match1(Count, [File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	sin, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_sin(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> canada_sin_match1(Count, Files)
	end;
canada_sin_match1(_Count, []) -> neg.

france_insee_match() -> text.

france_insee_match(Conf, {_Addr, Files}) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 5
	end,
	france_insee_match1(Count, Files).

france_insee_match1(Count, [File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	insee, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_insee(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> france_insee_match1(Count, Files)
	end;
france_insee_match1(_Count, []) -> neg.

uk_nino_match() -> text.

uk_nino_match(Conf, {_Addr, Files}) when is_list(Conf) ->
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> 5
	end,
	uk_nino_match1(Count, Files).

uk_nino_match1(Count, [File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	nino, 
		File#file.text),
	
	case mydlp_api:more_than_count(fun(I) -> mydlp_api:is_valid_nino(I) end, Count, Res) of
		true -> {pos, {file, File}};
		false -> uk_nino_match1(Count, Files)
	end;
uk_nino_match1(_Count, []) -> neg.

-define(CFILE_MINSIZE, 128).

e_archive_match() -> analyzed.

e_archive_match(_, {_Addr, Files}) -> e_archive_match(Files).

e_archive_match([#file{mime_type=MimeType, data=Data, is_encrypted=true} = File|Files]) -> 
	case size(Data) > ?CFILE_MINSIZE of
		true -> case mydlp_api:is_compression_mime(MimeType) of
			true -> {pos, {file, File}};
			false -> e_archive_match(Files) end;
		false -> e_archive_match(Files) end;
e_archive_match([_File|Files]) -> e_archive_match(Files);
e_archive_match([]) -> neg.

-define(EFILE_MINSIZE, 256).

e_file_match() -> analyzed.

e_file_match(_, {_Addr, Files}) -> e_file_match(Files).

e_file_match([#file{data=Data, mime_type=MimeType, is_encrypted=true} = File|Files]) -> 
	case size(Data) > ?EFILE_MINSIZE of
		% compressed files which are marked as encrypted, should not be handled here. (fun e_archive_match)
		true -> case mydlp_api:is_compression_mime(MimeType) of 
			true -> e_file_match(Files);
			false -> {pos, {file, File}} end;
		false -> e_file_match(Files) end;
e_file_match([_File|Files]) -> e_file_match(Files);
e_file_match([]) -> neg.

i_binary_match() -> raw.

i_binary_match(_Conf, {_Addr, Files}) -> i_binary_match(Files).
i_binary_match([#file{mime_type=MimeType, data=Data} = File|Files]) ->
	case mydlp_api:is_cobject_mime(MimeType) of
		true -> case mydlp_tc:check_binary_integrity(Data) of
			false -> {pos, {file, File}};
			true -> i_binary_match(Files) end;
		false -> i_binary_match(Files) end;
i_binary_match([]) -> neg.

i_archive_match() -> raw.

i_archive_match(_Conf, {_Addr, Files}) -> i_archive_match(Files).
i_archive_match([#file{mime_type=MimeType, data=Data} = File|Files]) ->
	case mydlp_api:is_compression_mime(MimeType) of
		true -> case mydlp_tc:check_archive_integrity(Data) of
			false -> {pos, {file, File}};
			true -> i_archive_match(Files) end;
		false -> i_archive_match(Files) end;
i_archive_match([]) -> neg.

p_text_match() -> analyzed.

p_text_match(Conf, {_Addr, Files}) ->
	Score = case lists:keyfind(score, 1, Conf) of
		{score, S} -> S;
		false -> 512
	end,
	p_text_match0(Score, Files).

p_text_match0(Limit, [#file{mime_type= <<"application/x-tar">>}|Files]) -> p_text_match0(Limit, Files);
p_text_match0(Limit, Files) -> p_text_match1(Limit, Files).

p_text_match1(Limit, [#file{data=Data} = File|Files]) ->
	S1 = ( mydlp_regex:longest_bin(hexencoded, Data) ) / 2,
	S2 = mydlp_regex:longest_bin(base64encoded, Data),
	Score = lists:max([S1,S2]),

	case Score >= Limit of
		true -> {pos, {file, File}, 
			{misc, "score=" ++ integer_to_list(Score)}};
		false -> p_text_match1(Limit, Files)
	end;
p_text_match1(_Score, []) -> neg.

md5_match() -> raw.

md5_match(HGIs, {_Addr, Files}) -> md5_match(HGIs, Files);
md5_match(HGIs, [File|Files]) ->
	Hash = erlang:md5(File#file.data),
	case mydlp_mnesia:is_fhash_of_gid(Hash, HGIs) of
		true -> {pos, {file, File}};
		false -> md5_match(HGIs, Files)
	end;
md5_match(_HGIs, []) -> neg.

md5_dr_match() -> raw.

md5_dr_match(_Conf, {Source, Files}) -> 
	CustomerId = case lists:keyfind(cid, 1, Source) of
		{cid, C} -> C;
		false -> mydlp_mnesia:get_dcid()
	end,
	CustomerKey = {bl, CustomerId},
	md5_dr_match(CustomerKey, Files);
md5_dr_match(CustomerKey, [File|Files]) ->
	Hash = erlang:md5(File#file.data),
	case mydlp_mnesia:is_dr_fh_of_fid(Hash, CustomerKey) of
		true -> {pos, {file, File}};
		false -> md5_dr_match(CustomerKey, Files)
	end;
md5_dr_match(_CustomerKey, []) -> neg.

shash_match() -> text.

shash_match(Conf, {_Addr, Files}) when is_list(Conf) -> 
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
	shash_match(HGIs, Perc, Count, Files).

shash_match(HGIs, Perc, Count, [File|Files]) ->
	Res2 = mydlp_api:get_nsh(File#file.text),
	Res3 = lists:filter(fun(I) -> mydlp_mnesia:is_shash_of_gid(I, HGIs) end, Res2),
	TotalLen = length(Res2), MatchLen = length(Res3),
	case TotalLen of 
		0 -> shash_match(HGIs, Perc, Count, Files);
		_Else ->
			FilePerc = MatchLen/TotalLen,
			case ((Perc /= undefined) and (Perc < (MatchLen/TotalLen))) or
				((Count /= undefined) and ( Count < MatchLen)) of
				true -> {pos, {file, File}, 
					{misc, "count=" ++ integer_to_list(MatchLen) ++
					" percentage=" ++ float_to_list(float(FilePerc))}};
				false -> shash_match(HGIs, Perc, Count, Files)
			end
	end;
shash_match(_HGIs, _Perc, _Count, []) -> neg.

bayes_match() -> text.

bayes_match(Conf, {_Addr, Files}) when is_list(Conf) ->
	Threshold = case lists:keyfind(threshold, 1, Conf) of
		{threshold, T} -> T;
		false -> 0.5
	end,
	bayes_match1(Threshold, Files).

bayes_match1(Threshold, [File|Files]) ->
	BayesScore = bayeserl:score(File#file.text),
	case (BayesScore > Threshold) of
		true -> {pos, {file, File}, 
			{misc, "score=" ++ float_to_list(float(BayesScore))}};
		false -> bayes_match1(Threshold, Files)
	end;
bayes_match1(_Count, []) -> neg.

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

scode_match(Conf, {_Addr, Files}) when is_list(Conf) -> 
	Score = case lists:keyfind(score, 1, Conf) of
		{score, S} -> S;
		false -> 100
	end,
	scode_match1(Score, Files).

scode_match1(Limit, [File|Files]) ->
	Score = mydlp_regex:score_suite(
	 	scode, 
		File#file.text),

	case Score >= Limit of
		true -> {pos, {file, File}, 
			{misc, "score=" ++ integer_to_list(Score)}};
		false -> scode_match1(Limit, Files)
	end;
scode_match1(_Limit, []) -> neg.

scode_ada_match() -> text.

scode_ada_match(Conf, {_Addr, Files}) when is_list(Conf) -> 
	Score = case lists:keyfind(score, 1, Conf) of
		{score, S} -> S;
		false -> 100
	end,
	scode_ada_match1(Score, Files).

scode_ada_match1(Limit, [File|Files]) ->
	Score = mydlp_regex:score_suite(
	 	scode_ada, 
		File#file.text),

	case Score >= Limit of
		true -> {pos, {file, File}, 
			{misc, "score=" ++ integer_to_list(Score)}};
		false -> scode_ada_match1(Limit, Files)
	end;
scode_ada_match1(_Limit, []) -> neg.

