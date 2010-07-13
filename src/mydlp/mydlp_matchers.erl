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
	mime_match/0,
	mime_match/2,
	md5_match/0,
	md5_match/2,
	regex_match/0,
	regex_match/2,
	iban_match/0,
	iban_match/2,
	trid_match/0,
	trid_match/2,
	e_file_match/0,
	e_file_match/2,
	e_archive_match/0,
	e_archive_match/2,
	shash_match/0,
	shash_match/2,
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
		true -> pos;
		false -> mime_match(MGIs, Files)
	end;
mime_match(_MimeTypes, []) -> neg.

regex_match() -> text.

regex_match(RGIs, {_Addr, Files}) -> regex_match(RGIs, Files);
regex_match(RGIs, [File|Files]) ->
	case mydlp_regex:match(RGIs, File#file.text) of
		true -> pos;
		false -> regex_match(RGIs, Files)
	end;
regex_match(_RGIs, []) -> neg.

md5_match() -> raw.

md5_match(HGIs, {_Addr, Files}) -> md5_match(HGIs, Files);
md5_match(HGIs, [File|Files]) ->
	Hash = erlang:md5(File#file.data),
	case mydlp_mnesia:is_fhash_of_gid(Hash, HGIs) of
		true -> pos;
		false -> md5_match(HGIs, Files)
	end;
md5_match(_HGIs, []) -> neg.

cc_match() -> text.

cc_match(_, {_Addr, Files}) -> cc_match(Files).

cc_match([File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	credit_card, 
		File#file.text),
	
	case lists:any(fun(I) -> mydlp_api:is_valid_cc(I) end, Res) of
		true -> pos;
		false -> cc_match(Files)
	end;
cc_match([]) -> neg.

iban_match() -> text.

iban_match(_, {_Addr, Files}) -> iban_match(Files).

iban_match([File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	iban, 
		File#file.text),
	
	case lists:any(fun(I) -> mydlp_api:is_valid_iban(I) end, Res) of
		true -> pos;
		false -> iban_match(Files)
	end;
iban_match([]) -> neg.

trid_match() -> text.

trid_match(_, {_Addr, Files}) -> trid_match(Files).

trid_match([File|Files]) ->
	Res = mydlp_regex:match_bin(
	 	trid, 
		File#file.text),
	
	case lists:any(fun(I) -> mydlp_api:is_valid_trid(I) end, Res) of
		true -> pos;
		false -> trid_match(Files)
	end;
trid_match([]) -> neg.

e_archive_match() -> analyzed.

e_archive_match(_, {_Addr, Files}) -> e_archive_match(Files).

e_archive_match([#file{mime_type= <<"application/zip">>, is_encrypted=true}|_Files]) -> pos;
e_archive_match([#file{mime_type= <<"application/x-rar">>, is_encrypted=true}|_Files]) -> pos;
e_archive_match([_File|Files]) -> e_archive_match(Files);
e_archive_match([]) -> neg.

e_file_match() -> analyzed.

e_file_match(_, {_Addr, Files}) -> e_file_match(Files).

e_file_match([#file{is_encrypted=true}|_Files]) -> pos;
e_file_match([_File|Files]) -> e_file_match(Files);
e_file_match([]) -> neg.

shash_match() -> text.

shash_match(Conf, {_Addr, Files}) when is_list(Conf) -> 
	Perc = case lists:keyfind(percentage, 1, Conf) of
		{percentage, P} -> P;
		false -> undefined
	end,
	Count = case lists:keyfind(count, 1, Conf) of
		{count, C} -> C;
		false -> undefined
	end,
	HGIs = case lists:keyfind(group_ids, 1, Conf) of
		{group_ids, G} -> G;
		false -> undefined
	end,
	shash_match(HGIs, Perc, Count, Files).

shash_match(HGIs, Perc, Count, [File|Files]) ->
	Res = mydlp_regex:split_bin(
	 	sentence, 
		File#file.text),
	Res1 = lists:filter(fun(I) -> string:len(I) > 10 end, Res), %%% 10 as string length threshold, shorter strings will be neglacted.
	Res2 = lists:map(fun(I) -> 
			mydlp_api:strhash(
			mydlp_api:norm_str(I)
			) end, Res1),
	Res3 = lists:filter(fun(I) -> mydlp_mnesia:is_shash_of_gid(I, HGIs) end, Res2),
	TotalLen = length(Res2), MatchLen = length(Res3),

	case ((Perc /= undefined) and (Perc < (MatchLen/TotalLen))) or
		((Count /= undefined) and ( Count < MatchLen)) of
		true -> pos;
		false -> shash_match(HGIs, Perc, Count, Files)
	end;
shash_match(_HGIs, _Perc, _Count, []) -> neg.

