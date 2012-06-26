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
	md5_match/0,
	md5_match/1,
	md5_match/3,
	pdm_match/0,
	pdm_match/1,
	pdm_match/3,
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
	said_match/0,
	said_match/1,
	said_match/3,
	e_file_match/0,
	e_file_match/1,
	e_file_match/3,
	e_archive_match/0,
	e_archive_match/1,
	e_archive_match/3,
	p_text_match/0,
	p_text_match/1,
	p_text_match/3,
	scode_match/0,
	scode_match/1,
	scode_match/3,
	scode_ada_match/0,
	scode_ada_match/1,
	scode_ada_match/3,
	cc_match/0,
	cc_match/1,
	cc_match/3,
	pan_match/0,
	pan_match/1,
	pan_match/3,
	cpf_match/0,
	cpf_match/1,
	cpf_match/3
]).

-include_lib("eunit/include/eunit.hrl").

regex_match() -> text.

regex_match(RGIs) -> RGIs.

regex_match(RGIs, _Addr, File) ->
	mydlp_regex:match_count(RGIs, File#file.text).

cc_match() -> text.

cc_match(_Conf) -> none.

cc_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	credit_card, 
		File#file.text),
	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_cc(I) end, Res).

iban_match() -> text.

iban_match(_Conf) -> none.

iban_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	iban, 
		File#file.text),
	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_iban(I) end, Res).

trid_match() -> text.

trid_match(_Conf) -> none.

trid_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	trid, 
		File#file.text),
	
	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_trid(I) end, Res).

pan_match() -> text.

pan_match(_Conf) -> none.

pan_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
		pan,
		File#file.text),

	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_pan(I) end, Res).

cpf_match() -> text.

cpf_match(_Conf) -> none.

cpf_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
		cpf,
		File#file.text),

	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_cpf(I) end, Res).

ssn_match() -> text.

ssn_match(_Conf) -> none.

ssn_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	ssn, 
		File#file.text),
	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_ssn(I) end, Res).

canada_sin_match() -> text.

canada_sin_match(_Conf) -> none.

canada_sin_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	sin, 
		File#file.text),
	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_sin(I) end, Res).

france_insee_match() -> text.

france_insee_match(_Conf) -> none.

france_insee_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	insee, 
		File#file.text),
	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_insee(I) end, Res).

uk_nino_match() -> text.

uk_nino_match(_Conf) -> none.

uk_nino_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	nino, 
		File#file.text),
	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_nino(I) end, Res).

said_match() -> text.

said_match(_Conf) -> none.

said_match(_Conf, _Addr, File) ->
	Res = mydlp_regex:match_bin(
	 	said, 
		File#file.text),
	mydlp_api:filter_count(fun(I) -> mydlp_api:is_valid_said(I) end, Res).

-define(CFILE_MINSIZE, 128).

e_archive_match() -> analyzed.

e_archive_match(_Conf) -> none.

e_archive_match(_Opts, _Addr, #file{mime_type=MimeType, data=Data, is_encrypted=true}) -> 
	case size(Data) > ?CFILE_MINSIZE of
		true -> case mydlp_api:is_compression_mime(MimeType) of
			true -> 1;
			false -> 0 end;
		false -> 0 end;
e_archive_match(_Opts, _Addr, _File) -> 0.

-define(EFILE_MINSIZE, 256).

e_file_match() -> analyzed.

e_file_match(_Conf) -> none.

e_file_match(_Opts, _Addr, #file{data=Data, mime_type=MimeType, is_encrypted=true}) -> 
	case size(Data) > ?EFILE_MINSIZE of
		% compressed files which are marked as encrypted, should not be handled here. (fun e_archive_match)
		true -> case mydlp_api:is_compression_mime(MimeType) of 
			true -> 0;
			false -> 1 end;
		false -> 0 end;
e_file_match(_Opts, _Addr, _File) -> 0.

p_text_match() -> analyzed.

p_text_match(_Conf) -> none.

p_text_match(_Conf, _Addr, #file{mime_type=MimeType} = File) ->
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
md5_match() -> raw.

md5_match([HGI]) -> HGI.

md5_match(HGI, _Addr, File) ->
	Hash = erlang:md5(File#file.data),
	case mydlp_mnesia:is_hash_of_gid(Hash, HGI) of
		true -> 1;
		false -> 0 end.

pdm_match() -> text.

pdm_match([FGI]) -> FGI.

pdm_match(FGI, _Addr, File) ->
	FList = mydlp_pdm:fingerprint(File#file.text),
	mydlp_mnesia:pdm_of_gid(FList, FGI).

scode_match() -> text.

scode_match(_Conf) -> none.

scode_match(_Conf, _Addr, File) ->
	mydlp_regex:score_suite(scode, File#file.text).

scode_ada_match() -> text.

scode_ada_match(_Conf) -> none.

scode_ada_match(_Conf, _Addr, File) ->
	mydlp_regex:score_suite(scode_ada, File#file.text).

