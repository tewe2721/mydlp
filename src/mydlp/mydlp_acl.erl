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
%%% @doc ACL for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-module(mydlp_acl).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_acl.hrl").

%% API
-export([start_link/0,
	stop/0]).

-ifdef(__MYDLP_NETWORK).

-export([
	get_remote_rule_tables/1,
	q/2,
	qr/2
	]).

-endif.

-ifdef(__MYDLP_ENDPOINT).

-endif.

-export([
	qi/2,
	qe/2,
	qe/3
	]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
	is_multisite=false
	}).

%%%%%%%%%%%%% MyDLP ACL API

-ifdef(__MYDLP_NETWORK).

get_remote_rule_tables(EndpointId) -> acl_call({get_remote_rule_tables, EndpointId}).

q(AclQ, Files) -> acl_call({q, AclQ}, Files).

qr(RuleId, Files) when is_integer(RuleId) -> acl_call({qr, RuleId}, Files);

qr(none, _) -> ?ERROR_LOG("Rule not found. Discovery Rule may removed.", []).

-endif.

-ifdef(__MYDLP_ENDPOINT).

-endif.

% For handling inbound request.
qi(Channel, Files) -> acl_call({qi, Channel}, Files).

qe(Channel, Files) -> acl_call({qe, Channel}, Files).

qe(Channel, Files, RuleIndex) -> acl_call({qe, Channel, RuleIndex}, Files).

-ifdef(__MYDLP_NETWORK).

acl_call(Query) -> acl_call(Query, none).

-endif.

acl_call(Query, Files) -> acl_call(Query, Files, 1500000).

acl_call(_Query, [], _Timeout) -> pass;
acl_call(Query, none, Timeout) -> acl_call1(Query, none, Timeout);
acl_call(Query, [#file{mime_type= <<"mydlp-internal/usb-device", _/binary>>}] = Files, Timeout) -> acl_call1(Query, Files, Timeout);
acl_call(Query, Files, Timeout) -> 
	FileSizes = lists:map(fun(F) -> ?BB_S(F#file.dataref) end, Files),
	TotalSize = lists:sum(FileSizes),
	case TotalSize > ?CFG(maximum_object_size) of
		true ->	{log, mydlp_api:empty_aclr(Files, max_size_exceeded)};
		false -> acl_call1(Query, Files, Timeout) end.

% no need to call acl server for inbound requests.
acl_call1({qi, inbound}, Files, _Timeout) -> 
	{RuleId, Action} = mydlp_mnesia:get_inbound_rule(),
	case Action of
		archive -> {archive, mydlp_api:empty_aclr(RuleId, Files, archive_inbound)};
		log -> {log, mydlp_api:empty_aclr(RuleId, Files, archive_inbound)};
		pass -> pass end;
acl_call1(Query, Files, Timeout) -> gen_server:call(?MODULE, {acl, Query, Files, Timeout}, Timeout).

%%%%%%%%%%%%%% gen_server handles

-ifdef(__MYDLP_NETWORK).

acl_exec(_SpawnOpts, none, [], _) -> pass;
acl_exec(_SpawnOpts, _RuleTables, [], _) -> pass;
acl_exec(SpawnOpts, RuleTables, Files, IsOcrActive) ->
	acl_exec2(SpawnOpts, RuleTables, Files, IsOcrActive).

-endif.

acl_exec2(_SpawnOpts, none, _Files, _) -> pass;
acl_exec2(SpawnOpts, {Req, {_Id, DefaultAction}, Rules}, Files, IsOcrActive) ->
	case { DefaultAction, acl_exec3(SpawnOpts, Req, Rules, Files, IsOcrActive) } of
		{DefaultAction, return} -> DefaultAction;
		{_DefaultAction, Action} -> Action end.

acl_exec3(_SpawnOpts, _Req, [], _Files, _) -> return;
acl_exec3(_SpawnOpts, _Req, _AllRules, [], _) -> return;
acl_exec3(SpawnOpts, Req, AllRules, Files, IsOcrActive) ->
	acl_exec3(SpawnOpts, Req, AllRules, Files, [], false, IsOcrActive).

acl_exec3(_SpawnOpts, _Req, _AllRules, [], [], _CleanFiles, _IsOcrActive) -> return;

acl_exec3(SpawnOpts, Req, AllRules, [], ExNewFiles, false, IsOcrActive) ->
	acl_exec3(SpawnOpts, Req, AllRules, [], ExNewFiles, true, IsOcrActive);

acl_exec3(SpawnOpts, Req, AllRules, [], ExNewFiles, CleanFiles, IsOcrActive) ->
	acl_exec3(SpawnOpts, Req, AllRules, ExNewFiles, [], CleanFiles, IsOcrActive);
	
acl_exec3(SpawnOpts, Req, AllRules, Files, ExNewFiles, CleanFiles, IsOcrActive) ->
	{InChunk, RestOfFiles} = mydlp_api:get_chunk(Files),
	
	{FFiles, NewFiles} = prepare_query_files(InChunk, Req),

	CTX = ctx_cache(),
	AclR = case apply_rules(CTX, SpawnOpts, AllRules, FFiles, IsOcrActive, Req) of
		return -> acl_exec3(SpawnOpts, Req, AllRules, RestOfFiles,
				lists:append(ExNewFiles, NewFiles), CleanFiles, IsOcrActive);
		Else -> Else end,
	ctx_cache_stop(CTX),

	case AclR of
		return -> mydlp_api:clean_files(FFiles);
		{_, {{rule, _}, {file, TargetFiles}, {itype, _}, {misc, _}}} ->
			mydlp_api:clean_files_excluding(FFiles, TargetFiles) end,

	AclR.

prepare_query_files(Files, Req) ->
	Files1 = mydlp_api:load_files(Files),

	{PFiles1, NewFiles} = mydlp_api:analyze(Files1),
	
	PFiles2 = mydlp_api:drop_nodata(PFiles1),
	PFiles3 = case Req of
		#mining_req{normal_text = true} -> pl_text(PFiles2, normalized);
		#mining_req{raw_text = true} -> pl_text(PFiles2, raw_text);
		_Else2 -> PFiles2 end,

	PFiles4 = case Req of 
		#mining_req{mc_pd = true, mc_kw = true} -> mc_text(PFiles3, all);
		#mining_req{mc_pd = true} -> mc_text(PFiles3, pd);
		#mining_req{mc_kw = true} -> mc_text(PFiles3, kw);
		_Else3 -> mc_text(PFiles3, none) end,

	{PFiles4, NewFiles}.

-ifdef(__MYDLP_NETWORK).

handle_acl({q, #aclq{} = AclQ}, Files, SpawnOpts, _State) ->
	CustomerId = mydlp_mnesia:get_dfid(),
	Rules = mydlp_mnesia:get_rules(CustomerId, AclQ),
	IsOCRActive = is_ocr_active(AclQ),
	acl_exec(SpawnOpts, Rules, Files, IsOCRActive);

handle_acl({get_remote_rule_tables, EndpointId}, _Files, _SpawnOpts, _State) ->
	CustomerId = mydlp_mnesia:get_dfid(),
	% TODO: change needed for multi-site use
	mydlp_mnesia:get_remote_rule_tables(CustomerId, EndpointId);

handle_acl({qr, RuleId}, Files, SpawnOpts, _State) when is_integer(RuleId) ->
	CustomerId = mydlp_mnesia:get_dfid(),
	Rules = mydlp_mnesia:get_rule_table(CustomerId, [RuleId]),
	acl_exec(SpawnOpts, Rules, Files, false); %last argument for ocr

handle_acl(Q, _Files, _SpawnOpts, _State) -> throw({error, {undefined_query, Q}}).

-endif.

-ifdef(__MYDLP_ENDPOINT).

handle_acl({qe, _Channel}, [#file{mime_type= <<"mydlp-internal/usb-device;id=unknown">>}] = Files, _SpawnOpts, _State) ->
	{?CFG(error_action), mydlp_api:empty_aclr(Files, usb_device_id_unknown)};

handle_acl({qe, _Channel}, [#file{mime_type= <<"mydlp-internal/usb-device;id=", DeviceId/binary>>}] = Files, _SpawnOpts, _State) ->
	case mydlp_mnesia:is_valid_usb_device_id(DeviceId) of % TODO: need refinements for multi-user usage.
		true -> pass;
		false -> {block, mydlp_api:empty_aclr(Files, usb_device_rejected)} end;

handle_acl({qe, Channel}, Files, SpawnOpts, _State) ->
	Rules = mydlp_mnesia:get_rule_table(Channel),
	acl_exec2(SpawnOpts, Rules, Files, false);

handle_acl({qe, Channel, RuleIndex}, Files, SpawnOpts, _State) ->
	Rules = mydlp_mnesia:get_rule_table(Channel, RuleIndex),
	acl_exec2(SpawnOpts, Rules, Files, false);

handle_acl(Q, _Files, _SpawnOpts, _State) -> throw({error, {undefined_query, Q}}).

-endif.

handle_call({acl, Query, Files, Timeout}, From, State) ->
	Worker = self(),
	SpawnOpts = get_spawn_opts(Query),
	mydlp_api:mspawn(fun() ->
		Return = try 
			Result = handle_acl(Query, Files, SpawnOpts, State),
			{ok, Result}
		catch throw:{error,eacces} -> {error, {throw, {error,eaccess}}};
		      Class:Error ->
			?ERROR_LOG("Error occured on ACL query: ["?S"]. Class: ["?S"]. Error: ["?S"].~nStack trace: "?S"~n",
				[Query, Class, Error, erlang:get_stacktrace()]),
			{error, {Class,Error}} end,
		Worker ! {async_acl_q, Return, From} 
	end, Timeout),
	{noreply, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_acl_q, Res, From}, State) ->
	Reply = case Res of
		{ok, R} -> R;
		{error, _} -> ?CFG(error_action) end, % TODO conf

	?SAFEREPLY(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

-ifdef(__MYDLP_NETWORK).

init([]) ->
	IsMS = mydlp_mysql:is_multisite(),
	{ok, #state{is_multisite=IsMS}}.

-endif.

-ifdef(__MYDLP_ENDPOINT).

init([]) ->
	{ok, #state{is_multisite=false}}.

-endif.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%% helper func
get_spawn_opts(#aclq{channel=mail}) -> [{priority, low}];
get_spawn_opts(#aclq{channel=discovery}) -> [{priority, low}, {fullsweep_after, 0}];
get_spawn_opts(#aclq{channel=remote_discovery}) -> [{priority, low}, {fullsweep_after, 0}];
get_spawn_opts(_Else) -> [].


apply_rules(_CTX, _SpawnOpts, [], _Files, _, _) -> return;
apply_rules(_CTX, _SpawnOpts, _Rules, [], _, _) -> return;
apply_rules(CTX, SpawnOpts, Rules, Files, IsOcrActive, Req) ->
	Result = mydlp_api:pmap(fun(R) -> execute_itypes_pr(CTX, SpawnOpts, R, Files, IsOcrActive, Req) end, Rules, 1200000, SpawnOpts),
	generate_aclret(Result, Rules).

generate_aclret(Result, Rules) -> generate_aclret(Result, Rules, return, -1, -1, "", []).

generate_aclret([_|RestOfResults], [_|RestOfRules], error, CurRuleId, CurITypeId, CurMisc, Files) ->
	generate_aclret(RestOfResults, RestOfRules, error, CurRuleId, CurITypeId, CurMisc, Files);
generate_aclret([{RuleId, IResults}|RestOfResults], [{RuleId, Action, _ITypes}|RestOfRules], CurAction, CurRuleId, CurITypeId, CurMisc, Files) ->
	NonNegResults = lists:filter(fun(IR) -> ( not ( IR == neg ) ) end, IResults),
	case NonNegResults of
		[] -> 	generate_aclret(RestOfResults, RestOfRules, CurAction, CurRuleId, CurITypeId, CurMisc, Files);
		_ -> 	{CurAction1, CurRuleId1, CurITypeId1, CurMisc1, Files1} =
				generate_aclret_pr(NonNegResults, RuleId, Action, CurAction, CurRuleId, CurITypeId, CurMisc, Files),
			generate_aclret(RestOfResults, RestOfRules, CurAction1, CurRuleId1, CurITypeId1, CurMisc1, Files1)
			end;
generate_aclret([], [], return, _CurRuleId, _CurITypeId, _CurMisc, _Files) -> return;
generate_aclret([], [], error, CurRuleId, CurITypeId, CurMisc, Files) ->
	case ?CFG(error_action) of
		pass -> return;
		EAction -> {EAction, {{rule, CurRuleId}, {file, Files}, {itype, CurITypeId}, {misc, CurMisc}}} end;
generate_aclret([], [], CurAction, CurRuleId, CurITypeId, CurMisc, Files) ->
	{CurAction, {{rule, CurRuleId}, {file, Files}, {itype, CurITypeId}, {misc, CurMisc}}}.

generate_aclret_pr([_|NonNegResults], RuleId, Action, error, CurRuleId, CurITypeId, CurMisc, CurFiles) ->
	generate_aclret_pr(NonNegResults, RuleId, Action, error, CurRuleId, CurITypeId, CurMisc, CurFiles);
generate_aclret_pr([{pos, {file, File}, {itype, ITypeOrigId}, {misc, Misc}}|NonNegResults], 
			RuleId, Action, _CurAction, -1, -1, "", CurFiles) ->
	generate_aclret_pr(NonNegResults, RuleId, Action, Action, RuleId, ITypeOrigId, Misc, mydlp_api:merge_md_of_same_file(CurFiles, File));
generate_aclret_pr([{pos, {file, File}, {itype, _ITypeOrigId}, {misc, _Misc}}|NonNegResults], 
			RuleId, Action, CurAction, CurRuleId, CurITypeId, CurMisc, CurFiles) ->
	generate_aclret_pr(NonNegResults, RuleId, Action, CurAction, CurRuleId, CurITypeId, CurMisc, mydlp_api:merge_md_of_same_file(CurFiles, File));
generate_aclret_pr([{error, {file, File}, {itype, ITypeOrigId}, {misc, Misc}}|NonNegResults], 
			RuleId, Action, _CurAction, _CurRuleId, _CurITypeId, _CurMisc, _CurFiles) ->
	generate_aclret_pr(NonNegResults, RuleId, Action, error, RuleId, ITypeOrigId, Misc, [File]);
generate_aclret_pr([error|NonNegResults], RuleId, Action, _CurAction, _CurRuleId, _CurITypeId, _CurMisc, _CurFiles) ->
	generate_aclret_pr(NonNegResults, RuleId, Action, error, RuleId, -1, "internal_error", []);
generate_aclret_pr([], _RuleId, _Action, CurAction, CurRuleId, CurITypeId, CurMisc, CurFiles) ->
	{CurAction, CurRuleId, CurITypeId, CurMisc, lists:flatten(CurFiles)}.

execute_itypes_pr(CTX, SpawnOpts, {Id, _Action, ITypes} = RS, Files, IsOcrActive, Req) ->
	Result = try execute_itypes(CTX, SpawnOpts, ITypes, Files, IsOcrActive, Req)
		catch Class:Error -> 
			?ERROR_LOG("Internal error. Class: "?S", Error: "?S".~nRS: "?S"~nStacktrace: "?S, 
				[Class, Error, RS, erlang:get_stacktrace()]),
		[error] end,
	{Id, lists:flatten(Result)}.

execute_itypes(_CTX, _SpawnOpts, [], _Files, _, _) -> neg;
execute_itypes(_CTX, _SpawnOpts, _ITypes, [], _, _) -> neg;
execute_itypes(CTX, SpawnOpts, ITypes, Files, IsOcrActive, Req) ->
	mydlp_api:pmap(fun(F) -> execute_itypes_pf(CTX, SpawnOpts, ITypes, F, IsOcrActive, Req) end, Files, 900000, SpawnOpts).

execute_itypes_pf(CTX, SpawnOpts, ITypes, File, IsOcrActive, Req) -> 
        File1 = case File#file.mime_type of 
                undefined -> 	MT = mydlp_tc:get_mime(File#file.filename, File#file.data),
				File#file{mime_type=MT};
                _Else ->	File end,
	mydlp_api:pmap(fun(T) -> execute_itype_pf(CTX, SpawnOpts, T, File1, IsOcrActive, Req) end, ITypes, 850000, SpawnOpts).
	
execute_itype_pf(CTX, SpawnOpts, {ITypeOrigId, all, Distance, IFeatures}, File, IsOcrActive, Req) ->
	execute_itype_pf_image_filtering(CTX, SpawnOpts, {ITypeOrigId, Distance, IFeatures}, File, IsOcrActive, Req);
execute_itype_pf(CTX, SpawnOpts, {ITypeOrigId, DataFormats, Distance, IFeatures},
		#file{mime_type=MT} = File, IsOcrActive, Req) ->
        case mydlp_mnesia:is_mime_of_dfid(MT, DataFormats) of
                false -> neg;
		true -> execute_itype_pf_image_filtering(CTX, SpawnOpts, {ITypeOrigId, Distance, IFeatures}, File, IsOcrActive, Req) end.

execute_itype_pf_image_filtering(CTX, SpawnOpts, {ITypeOrigId, Distance, IFeatures}, File, IsOcrActive, Req) ->
	File1 = case is_image_file(File) and IsOcrActive of
				true -> case mydlp_ocr:ocr(File) of
						"error" -> File;
						FT ->
							{[FT1], _} = prepare_query_files([FT], Req),
							FT1 end;
				false -> File end, 
	Res = execute_itype_pf1(CTX, SpawnOpts, ITypeOrigId, Distance, IFeatures, File1),
	case Res of
		{pos, {file, FileR}, Itype, Misc} -> 
			case is_image_file(File) and IsOcrActive of
				true -> {pos, {file, File#file{matching_detail=FileR#file.matching_detail}}, Itype, Misc};
				false -> Res end;
		_ -> Res end.

execute_itype_pf1(CTX, SpawnOpts, ITypeOrigId, Distance, IFeatures, File) ->
	%here_for_ocr
	case execute_ifeatures(CTX, SpawnOpts, Distance, IFeatures, File) of
		neg -> neg;
		{pos, MatchingDetails} ->
				File1 = File#file{matching_detail=MatchingDetails},
				{pos, {file, File1}, {itype, ITypeOrigId}, {misc, ""}};
		{error, {file, File}, {misc, Misc}} ->
				{error, {file, File}, {itype, ITypeOrigId}, {misc, Misc}};
		E -> E end.

execute_ifeatures(_CTX, _SpawnOpts, _Distance, [], _File) -> neg;
execute_ifeatures(CTX, SpawnOpts, Distance, IFeatures, File) ->
	try	UseDistance = case Distance of
			undefined -> false;
			_Else -> lists:all(fun({_Threshold, {_MId, Func, _FuncParams}}) ->
						is_distance_applicable(Func) end, IFeatures) end,

		PAllRet = mydlp_api:pall(fun({Threshold, {MId, Func, FuncParams}}) -> 
						case apply_m(CTX, Threshold, Distance, UseDistance, {MId, Func, FuncParams, File}) of
							{pos, T, SI} -> {pos, T, SI, Func};
							R -> R
						end end,
					IFeatures, 800000, SpawnOpts),
		%%%% TODO: Check for PAnyRet whether contains error
		case {PAllRet, UseDistance} of
			{false, _} -> neg;
			{{ok, Results}, false} -> MatchingDetails0 = generate_matching_details(Results),
						MatchingDetails = pp_matching_details(MatchingDetails0, File, UseDistance),
						{pos, MatchingDetails};
			{{ok, Results}, true } -> 
						case is_distance_satisfied(Results, Distance) of
							{pos, MatchingDetails0} -> 
								MatchingDetails = pp_matching_details(MatchingDetails0, File, UseDistance),
								{pos, MatchingDetails};
							neg -> neg;
							false -> neg
						end end
	catch _:{timeout, _F, _T} -> {error, {file, File}, {misc, timeout}} end.

pp_matching_details(MatchingDetails, File, true = _UseDistance) -> 
	case lists:all(fun(#matching_detail{matcher_func=Func}) -> Func == pdm_match end, MatchingDetails) of
		true ->	group_pdm_mds(MatchingDetails, File);
		false -> MatchingDetails end;
pp_matching_details(MatchingDetails, File, false = _UseDistance) -> 
	generate_phrase_for_pdm_mds(MatchingDetails, File).

group_pdm_mds(MatchingDetails, File) -> group_pdm_mds(MatchingDetails, File, undefined, undefined).

group_pdm_mds([#matching_detail{index=Idx}|Rest], File, MinI, MaxI) ->
	MinI1 = case {Idx, MinI} of
		{I, undefined} -> I;
		{I, M} when I < M -> I;
		{_, M} -> M end,
	MaxI1 = case {Idx, MaxI} of
		{I2, undefined} -> I2;
		{I2, M2} when I2 > M2 -> I2;
		{_, M2} -> M2 end,
	group_pdm_mds(Rest, File, MinI1, MaxI1);
group_pdm_mds([], File, MinI0, MaxI0) ->
	TextSize = byte_size(File#file.text),
	MinI = case MinI0 - 50 of
		M when M > 0 -> M;
		_ -> 0 end,
	MaxI = case MaxI0 + 50 of
		M2 when M2 < TextSize -> M2;
		_ -> TextSize end,
	BinLength = case MaxI - MinI of
		L when L > 0 -> L;
		_ -> 0 end,
	<<_:MinI/binary, Phrase:BinLength/binary, _/binary>> = File#file.text,
	Phrase1 = mydlp_api:ensure_unicode(Phrase),
	[#matching_detail{index=MinI, pattern=Phrase1, matcher_func=pdm_match}].

generate_phrase_for_pdm_mds(MatchingDetails, File) -> generate_phrase_for_pdm_mds(MatchingDetails, File, []).

generate_phrase_for_pdm_mds([#matching_detail{index=I, matcher_func=pdm_match} = MD|Rest], File, Acc) ->
	TextSize = byte_size(File#file.text),
	MinI = case I - 50 of
		M when M > 0 -> M;
		_ -> 0 end,
	MaxI = case I + 50 of
		M2 when M2 < TextSize -> M2;
		_ -> TextSize end,
	BinLength = case MaxI - MinI of
		L when L > 0 -> L;
		_ -> 0 end,
	<<_:MinI/binary, Phrase:BinLength/binary, _/binary>> = File#file.text,
	Phrase1 = mydlp_api:ensure_unicode(Phrase),
	NewMD = MD#matching_detail{pattern=Phrase1},
	generate_phrase_for_pdm_mds(Rest, File, [NewMD|Acc]);
generate_phrase_for_pdm_mds([MD|Rest], File, Acc) ->
	generate_phrase_for_pdm_mds(Rest, File, [MD|Acc]);
generate_phrase_for_pdm_mds([], _File, Acc) -> lists:reverse(Acc).
	

generate_matching_details(Results) -> generate_matching_details(Results, []).

generate_matching_details([], Acc) -> Acc;
generate_matching_details([{pos, _Threshold, {_Score, IndexWithPatterns}, MFunc}|Tail], Acc) when is_list(IndexWithPatterns) ->
	Acc1 = lists:map(fun({I, Pattern}) -> #matching_detail{index=I, pattern=Pattern, matcher_func=MFunc} end, IndexWithPatterns),
	generate_matching_details(Tail, lists:append(Acc, Acc1));
generate_matching_details(_, Acc) -> Acc.

%% Controls information feature is applicable for distance property.
is_distance_applicable(Func) ->
	{_, {distance, IsDistance}, {pd, _}, {kw, _}} = apply(mydlp_matchers, Func, []), IsDistance.

is_distance_satisfied(Results, Distance) ->
	[ListOfIndexesWithPatterns, ListOfThresholdReqs] = regulate_results(Results),
	%time to distance control
	[{I, _Pattern, _MFunc, _T}|_Tail] = ListOfIndexesWithPatterns,
	{TailOfIndexList, SubList} = find_in_distance(ListOfIndexesWithPatterns, Distance, I),
	is_in_valid_distance(TailOfIndexList, SubList, ListOfThresholdReqs, Distance).

%% Controls whether fetching indexes are in a suitable distance or not. In addition; iterates all indexes.
is_in_valid_distance([], DistanceList,  ListOfThresholdReqs, _Distance) -> 
	case is_all_thresholds_satisfied(DistanceList, ListOfThresholdReqs) of
		{true, MatchingDetails} -> {pos, MatchingDetails};
		false -> neg
	end;


is_in_valid_distance(ListOfIndexes, DistanceList, ListOfThresholdReqs, Distance) ->
	SumOfThresholds = lists:sum(ListOfThresholdReqs),
	%[_H1,{IndexValue, _Pattern, _MFunc, _T}|_] = DistanceList,
	[{IndexValue, _Pattern, _MFunc, _T}|_] = ListOfIndexes,
	EarlyNeg = (length(DistanceList) < SumOfThresholds),
	case EarlyNeg of 
		true -> {TailOfIndexList, NewDistanceList} = find_in_distance(ListOfIndexes, Distance, IndexValue),
			is_in_valid_distance(TailOfIndexList, NewDistanceList, ListOfThresholdReqs, Distance);
		false -> case is_all_thresholds_satisfied(DistanceList, ListOfThresholdReqs) of
				{true, MatchingDetails} -> {pos, MatchingDetails};
				false -> {TailOfIndexList, NewDistanceList} = find_in_distance(ListOfIndexes, Distance, IndexValue),
					is_in_valid_distance(TailOfIndexList, NewDistanceList, ListOfThresholdReqs, Distance)
			 end
	end.

%% Controls whether list, which contains indexes in a certain distance, includes all information features in a certain amount of threshold.
is_all_thresholds_satisfied(Results, ThresholdList) -> is_all_thresholds_satisfied(Results, ThresholdList, []).

is_all_thresholds_satisfied([], Acc, PatternAcc) ->
	case lists:all(fun(I) -> I =< 0 end, Acc) of
		true -> {true, PatternAcc};
		false -> false
	end;
is_all_thresholds_satisfied([{Index, Pattern, MFunc, T}|Tail], ThresholdList, PatternAcc) ->
	Acc1 = lists:sublist(ThresholdList, T-1) ++ [lists:nth(T, ThresholdList) - 1] ++ lists:nthtail(T, ThresholdList),
	PatternAcc1 = [#matching_detail{index=Index, pattern=Pattern, matcher_func=MFunc}|PatternAcc],
	is_all_thresholds_satisfied(Tail, Acc1, PatternAcc1).

%% Returns remaining index list and the list which is in predefined distance.
find_in_distance(Results, Distance, IndexValue) -> find_in_distance(Results, Distance, IndexValue, []). 

find_in_distance([], _Distance, _IndexValue, Acc) -> {[],lists:reverse(Acc)};

find_in_distance([{IV, _Pattern, _MFunc, _T} = Head|Tail] = RestOfResults, Distance, IndexValue, Acc) ->
	case (IV =< (IndexValue+Distance)) of
		true -> find_in_distance(Tail, Distance, IndexValue, [Head|Acc]);
		false -> [_SelectedHead|SelectedTail] = Selected = lists:reverse(Acc), %% Dropping First Item
			{SelectedTail ++ RestOfResults, Selected} end.

%% Puts flags to the index list, which index comes from which information feature.
regulate_results(Results) -> regulate_results(Results, 1, [], []).

regulate_results([], _Number, AccIndex, AccThreshold) ->
	IndexList = lists:keysort(1, lists:flatten(AccIndex)),
	[IndexList, lists:reverse(AccThreshold)];
		
regulate_results([Head|Tail], Number, AccIndex, AccThreshold) ->
	NewNumber = Number + 1,
	{pos, Threshold, {_Score, IndexListWithPatterns}, MFunc} = Head,
	IndexesWithNumbers = lists:map(fun({I, Pattern}) -> {I, Pattern, MFunc, Number} end, IndexListWithPatterns),
	regulate_results(Tail, NewNumber, [IndexesWithNumbers|AccIndex], [Threshold|AccThreshold]).

is_early_distance_satisfied([], _Threshold, _Distance) -> false;

is_early_distance_satisfied(_Indexes, 1 = _Threshold, _Distance) -> true;

is_early_distance_satisfied([_] = _Indexes, 1 = _Threshold, _Distance) -> true;

is_early_distance_satisfied([_] = _Indexes, _Threshold, _Distance) -> false;

is_early_distance_satisfied([{Index, _Phrase}|Tail], Threshold, Distance)->
	[{Index2, _Phrase2}|_Tail2] = Tail,
	case ((Index2 - Index) =< Distance) of
		true -> true;
		false -> is_early_distance_satisfied(Tail, Threshold, Distance)
	end.

apply_m(_CTX, _Threshold, _Distance, _IsDistanceApplicable, {_MatcherId, all, _FuncParams, _File}) -> pos; %% match directly.
apply_m(CTX, Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}) ->
	IsCachable = case File of
		#file{md5_hash=undefined} -> false;
		#file{md5_hash=_Else} -> true;
		_Else -> false end,
	case IsCachable of
		true -> apply_m_cached(CTX, Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File});
		false -> apply_func(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}) end.

apply_m_cached(CTX, Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}) ->
	CacheKey = {File#file.md5_hash, Func, FuncParams},
	case ctx_start(CTX, CacheKey) of
		{ok, reserved} ->	{Result, IRet} = apply_func_iret(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}),
					ctx_finish(CTX, CacheKey, IRet), Result;
		{error, wait} ->	case ctx_wait(CTX, CacheKey) of
						{ok, IRet} -> apply_func(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}, IRet);
						{error, process_down} -> apply_m_cached(CTX, Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File})
					end;
		{error, iret, IRet} -> apply_func(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}, IRet) end.

apply_func(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}) ->
	{Result, _IRet} = apply_func_iret(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}),
	Result.

apply_func(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}, IRet) ->
	{Result, IRet} = apply_func_iret(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}, IRet),
	Result.

apply_func_iret(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}) ->
	apply_func_iret(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}, undefined).

apply_func_iret(Threshold, Distance, IsDistanceApplicable, {MatcherId, Func, FuncParams, File}, IndexRet0) ->
	EarlyNeg = case is_text_func(Func) of
		false -> false;
		true -> not mydlp_api:has_text(File) end,
	case EarlyNeg of
		true -> {neg, undefined};
		false ->
			FuncOpts = get_func_opts(Func, FuncParams),
			IndexRet = case IndexRet0 of
				undefined -> case is_mc_func(Func) of
					true ->  apply(mydlp_matchers, mc_match, [MatcherId, Func, FuncOpts, File]);
					false -> apply(mydlp_matchers, Func, [FuncOpts, File]) end;
				_Else -> IndexRet0 end,
			Result = case IndexRet of
				{Score, IndexListWithPhrase} ->
						EarlyNegForDistance = case IsDistanceApplicable of
										false -> true;
										true -> is_early_distance_satisfied(IndexListWithPhrase, Threshold, Distance)
									end,
						case ((Score >= Threshold) and EarlyNegForDistance) of
							true -> {pos, Threshold, {Score, IndexListWithPhrase}}; % TODO: Scores should be logged.
							false -> neg
						end;
				Score -> 
						case (Score >= Threshold) of
							true -> {pos, Threshold, {Score, dna}}; % TODO: Scores should be logged.
							false -> neg
						end
			end,
			{Result, IndexRet}
	end.

is_mc_func(Func) ->
	case apply(mydlp_matchers, Func, []) of
		{_, {distance, _}, {pd, true}, {kw, _}} -> true;
		{_, {distance, _}, {pd, _}, {kw, true}} -> true;
		{_, {distance, _}, {pd, _}, {kw, _}} -> false end.

is_text_func(Func) ->
	case apply(mydlp_matchers, Func, []) of
		{raw, {distance, _}, {pd, _}, {kw, _}} -> false;
		{analyzed, {distance, _}, {pd, _}, {kw, _}} -> false;
		{text, {distance, _}, {pd, _}, {kw, _}} -> true;
		{normalized, {distance, _}, {pd, _}, {kw, _}} -> true end.

get_func_opts(Func, FuncParams) -> apply(mydlp_matchers, Func, [{conf, FuncParams}]).

pl_text(Files, Opts) -> lists:map(fun(F) -> pl_text_f(F, Opts) end, Files). % TODO: may be pmap used, should check thrift client high load conc

pl_text_f(#file{text=undefined} = File, Opts) -> 
	File1 = case mydlp_api:get_text(File) of
		{ok, Text} -> File#file{text = Text};
		{error, compression} -> File;
		{error, audio} -> File;
		{error, video} -> File;
		{error, image} -> File;
		{error, encrypted} -> File#file{is_encrypted=true};
		_Else -> File#file{is_encrypted=true}
	end,
	File2 = case {File1#file.text, Opts} of
		{_, raw_text} -> File1;
		{undefined, _} -> File1;
		{RawText, normalized} ->
			NormalText = mydlp_nlp:normalize(RawText),
			File1#file{normal_text=NormalText};
		_Else2 -> File1 end,
	File2.

mc_text(Files, Opts) -> lists:map(fun(F) -> F#file{mc_table=mc_text_f(F, Opts)} end, Files). % TODO: may be pmap used, should check thrift client high load conc

mc_text_f(_, none) -> [];
mc_text_f(#file{normal_text=undefined}, _Opts) -> [];
mc_text_f(#file{normal_text=NormalText}, all) -> mydlp_mc:mc_search(NormalText);
mc_text_f(#file{normal_text=NormalText}, kw) -> mydlp_mc:mc_search(kw, NormalText);
mc_text_f(#file{normal_text=NormalText}, pd) -> mydlp_mc:mc_search(pd, NormalText).

%%%%%%%%%%%%%%%%%%%%% CTX Cache Start

-define(CTX_TIMEOUT, 900000).

ctx_cache() -> ctx_cache(?CTX_TIMEOUT).

ctx_cache(Timeout) -> mydlp_api:mspawn(fun() -> ctx_cache_func(Timeout) end, Timeout + 1000).

ctx_cache_stop(CTX) -> ok = ctx_query(CTX, stop).

ctx_cache_func(Timeout) ->
	receive
                {From, {start, Key}} -> ctx_cache_func_start(Timeout, From, Key);
                {From, {finish, Key, Result}} -> ctx_cache_func_finish(Timeout, From, Key, Result);
                {From, {wait, Key}} -> ctx_cache_func_wait(Timeout, From, Key);
		{'DOWN', _MonitorRef, process, Pid, _Info} -> ctx_cache_func_down(Timeout, Pid);
                {From, stop} -> ctx_cache_func_reply(From, ok);
		Else -> exit({error, unexpected_message_for_cache_main, Else})
        after Timeout + 500 ->
                exit({timeout, ctx_cache})
        end.

ctx_cache_func_reply(From, Message) -> From ! Message.

ctx_cache_func_down(Timeout, Pid) ->
	case get_keys({processing, Pid}) of
		[] -> ok;
		KeyList -> [ctx_cache_func_notify_down(K) || K <- KeyList] end,
	ctx_cache_func(Timeout).

ctx_cache_func_notify_down(Key) ->
	QueueList = case get({queue, Key}) of
		undefined -> 	[];
		L ->		L end,
	erase({queue, Key}),
	erase(Key),
	[P ! process_down || P <- QueueList], ok.
	

ctx_cache_func_start(Timeout, From, Key) ->
	case get(Key) of
		undefined -> 		monitor(process, From),
					put(Key, {processing, From}),
					ctx_cache_func_reply(From, {ok, reserved});
		{processing, _From} -> 	ctx_cache_func_reply(From, {error, wait});
		{iret, Result} -> 	ctx_cache_func_reply(From, {error, iret, Result}) end,
	ctx_cache_func(Timeout).

ctx_cache_func_notify_waiting(Key, Result) ->
	QueueList = case get({queue, Key}) of
		undefined -> 	[];
		L ->		L end,
	erase({queue, Key}),
	[P ! {finished, Result} || P <- QueueList], ok.
		
ctx_cache_func_finish(Timeout, From, Key, Result) ->
	case get(Key) of
		undefined -> 		exit({error, finishing_non_present_key});
		{processing, From} ->	put(Key, {iret, Result}),
					ctx_cache_func_notify_waiting(Key, Result),
					ctx_cache_func_reply(From, ok);
		{processing, _Else} ->	exit({error, finishing_some_other_processes_key});
		{iret, _Result} -> 	exit({error, finishing_alrady_finished_key}) end,
	ctx_cache_func(Timeout).

ctx_cache_func_add_queue(Key, From) ->
	case get({queue, Key}) of
		undefined -> 	put({queue, Key}, [From]);
		QueueList ->	put({queue, Key}, [From|QueueList]) end.
	
ctx_cache_func_wait(Timeout, From, Key) ->
	case get(Key) of
		undefined -> 		exit({error, waiting_for_non_present_key});
		{processing, _From} ->	ctx_cache_func_add_queue(Key, From),
					ctx_cache_func_reply(From, ok);
		{iret, Result} -> 	ctx_cache_func_reply(From, {error, iret, Result}) end,
	ctx_cache_func(Timeout).


ctx_start(CTX, Key) -> ctx_query(CTX, {start, Key}).

ctx_wait(CTX, Key) -> 
	case ctx_query(CTX, {wait, Key}) of
		{error, iret, Result} -> {ok, Result};
		ok ->	receive
				{finished, Result} -> {ok, Result};
				process_down -> {error, process_down};
				Else -> exit({error, unexpected_message_when_waiting, Else})
			after ?CTX_TIMEOUT ->
				exit({timeout, ctx_query_wait})
			end
	end.

ctx_finish(CTX, Key, Result) -> ok = ctx_query(CTX, {finish, Key, Result}).

ctx_query(CTX, Message) -> ctx_query(CTX, Message, ?CTX_TIMEOUT).

ctx_query(CTX, Message, Timeout) ->
	CTX ! {self(), Message},
	receive
		Reply -> Reply
        after Timeout ->
                exit({timeout, ctx_query})
        end.

-ifdef(__MYDLP_ENDPOINT).
is_image_file(_File) -> false.
-endif.

-ifdef(__MYDLP_NETWORK).
is_ocr_active(#aclq{channel=mail}) -> ?CFG(ocr_active);
is_ocr_active(#aclq{channel=web}) -> ?CFG(ocr_active);
is_ocr_active(_) -> false. 

is_image_file(#file{mime_type= <<"image/png">>}) -> true;
is_image_file(#file{mime_type= <<"image/x-icon">>}) -> true;
is_image_file(#file{mime_type= <<"image/gif">>}) -> true;
is_image_file(#file{mime_type= <<"image/bmp">>}) -> true;
is_image_file(#file{mime_type= <<"image/x-xcf">>}) -> true;
is_image_file(#file{mime_type= <<"image/x-ms-bmp">>}) -> true;
is_image_file(#file{mime_type= <<"image/vnd.wap.wbmp">>}) -> true;
is_image_file(#file{mime_type= <<"image/vnd.adobe.photoshop">>}) -> true;
is_image_file(#file{mime_type= <<"image/tiff">>}) -> true;
is_image_file(#file{mime_type= <<"image/jpeg">>}) -> true;
is_image_file(#file{mime_type= <<"image/svg+xml">>}) -> true;
is_image_file(_File) -> false.

-endif.

%%%%%%%%%%%%%%%%%%% CTX Cache end
