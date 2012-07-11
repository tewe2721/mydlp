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

%% API
-export([start_link/0,
	stop/0]).

-ifdef(__MYDLP_NETWORK).

-export([
	get_remote_rule_tables/2,
	q/2
	]).

-endif.

-ifdef(__MYDLP_ENDPOINT).

-endif.

-export([
	qi/2,
	qe/2
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

get_remote_rule_tables(Addr, UserH) -> acl_call({get_remote_rule_tables, Addr, UserH}).

q(AclQ, Files) -> acl_call({q, AclQ}, Files).

-endif.

-ifdef(__MYDLP_ENDPOINT).

-endif.

% For handling inbound request.
qi(Channel, Files) -> acl_call({qi, Channel}, Files).

qe(Channel, Files) -> acl_call({qe, Channel}, Files).

-ifdef(__MYDLP_NETWORK).

acl_call(Query) -> acl_call(Query, none).

-endif.

acl_call(Query, Files) -> acl_call(Query, Files, 1500000).

acl_call(Query, none, Timeout) -> acl_call1(Query, none, Timeout);
acl_call(Query, [#file{mime_type= <<"mydlp-internal/usb-device", _/binary>>}] = Files, Timeout) -> acl_call1(Query, Files, Timeout);
acl_call(Query, Files, Timeout) -> 
	FileSizes = lists:map(fun(F) -> ?BB_S(F#file.dataref) end, Files),
	TotalSize = lists:sum(FileSizes),
	case TotalSize > ?CFG(maximum_object_size) of
		true -> {log, mydlp_api:empty_aclr(Files, max_size_exceeded)};
		false -> acl_call1(Query, Files, Timeout) end.

% no need to call acl server for inbound requests.
acl_call1({qi, _Channel}, _Files, _Timeout) -> 
	case ?CFG(archive_inbound) of
		true -> {archive, mydlp_api:empty_aclr(none, archive_inbound)};
		false -> pass end;
acl_call1(Query, Files, Timeout) -> gen_server:call(?MODULE, {acl, Query, Files, Timeout}, Timeout).

%%%%%%%%%%%%%% gen_server handles

-ifdef(__MYDLP_NETWORK).

acl_exec(none, _Source, []) -> pass;
acl_exec(_RuleTables, _Source, []) -> pass;
acl_exec(RuleTables, Source, Files) ->
	acl_exec2(RuleTables, Source, Files).

-endif.

acl_exec2(none, _Source, _Files) -> pass;
acl_exec2({ACLOpts, {_Id, DefaultAction}, Rules}, Source, Files) ->
	case { DefaultAction, acl_exec3(ACLOpts, Rules, Source, Files) } of
		{DefaultAction, return} -> DefaultAction;
		{_DefaultAction, Action} -> Action end.

acl_exec3(_ACLOpts, [], _Source, _Files) -> return;
acl_exec3(_ACLOpts, _AllRules, _Source, []) -> return;
acl_exec3(ACLOpts, AllRules, Source, Files) ->
	acl_exec3(ACLOpts, AllRules, Source, Files, [], false).

acl_exec3(_ACLOpts, _AllRules, _Source, [], [], _CleanFiles) -> return;

acl_exec3(ACLOpts, AllRules, Source, [], ExNewFiles, false) ->
	acl_exec3(ACLOpts, AllRules, Source, [], ExNewFiles, true);

acl_exec3(ACLOpts, AllRules, Source, [], ExNewFiles, CleanFiles) ->
	acl_exec3(ACLOpts, AllRules, Source, ExNewFiles, [], CleanFiles);
	
acl_exec3({TextExtraction} = ACLOpts, AllRules, Source, Files, ExNewFiles, CleanFiles) ->
	{InChunk, RestOfFiles} = mydlp_api:get_chunk(Files),
	Files1 = mydlp_api:load_files(InChunk),
	
	Files3 = drop_whitefile(Files1),

	{PFiles, NewFiles} = mydlp_api:analyze(Files3),
	PFiles1 = case CleanFiles of
		true -> mydlp_api:clean_files(PFiles); % Cleaning newly created files.
		false -> PFiles end,

	PFiles2 = drop_nodata(PFiles1),
	FFiles = case TextExtraction of
		true -> pl_text(PFiles2);
		false -> PFiles2 end,

	case apply_rules(AllRules, Source, FFiles) of
		return -> acl_exec3(ACLOpts, AllRules, Source, RestOfFiles,
				lists:append(ExNewFiles, NewFiles), CleanFiles);
		Else -> Else end.

-ifdef(__MYDLP_NETWORK).

handle_acl({q, #aclq{src_addr=Addr} = AclQ}, Files, _State) ->
	CustomerId = mydlp_mnesia:get_dfid(),
	Rules = mydlp_mnesia:get_rules(CustomerId, AclQ),
	acl_exec(Rules, [{cid, CustomerId}, {addr, Addr}], Files);

handle_acl({get_remote_rule_tables, Addr, UserH}, _Files, _State) ->
	CustomerId = mydlp_mnesia:get_dfid(),
	% TODO: change needed for multi-site use
	mydlp_mnesia:get_remote_rule_tables(CustomerId, Addr, UserH);

handle_acl(Q, _Files, _State) -> throw({error, {undefined_query, Q}}).

-endif.

-ifdef(__MYDLP_ENDPOINT).

handle_acl({qe, _Channel}, [#file{mime_type= <<"mydlp-internal/usb-device;id=unknown">>}] = Files, _State) ->
	{?CFG(error_action), mydlp_api:empty_aclr(Files, usb_device_id_unknown)};

handle_acl({qe, _Channel}, [#file{mime_type= <<"mydlp-internal/usb-device;id=", DeviceId/binary>>}] = Files, _State) ->
	case mydlp_mnesia:is_valid_usb_device_id(DeviceId) of % TODO: need refinements for multi-user usage.
		true -> pass;
		false -> {block, mydlp_api:empty_aclr(Files, usb_device_rejected)} end;

handle_acl({qe, Channel}, Files, _State) ->
	Rules = mydlp_mnesia:get_rule_table(Channel),
	acl_exec2(Rules, [{cid, mydlp_mnesia:get_dfid()}], Files);

handle_acl(Q, _Files, _State) -> throw({error, {undefined_query, Q}}).

-endif.


handle_call({acl, Query, Files, Timeout}, From, State) ->
	Worker = self(),
	mydlp_api:mspawn(fun() ->
		Return = try 
			Result = handle_acl(Query, Files, State),
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

	gen_server:reply(From, Reply),
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
apply_rules([], _Addr, _Files) -> return;
apply_rules(_Rules, _Addr, []) -> return;
apply_rules([{Id, Action, ITypes}|Rules], Addr, Files) ->
	case execute_itypes(ITypes, Addr, Files) of
		neg -> apply_rules(Rules, Addr, Files);
		{pos, {file, File}, {itype, ITypeOrigId}, {misc, Misc}} -> 
			{Action, {{rule, Id}, {file, File}, {itype, ITypeOrigId}, {misc, Misc}}};
		{error, {file, File}, {itype, ITypeOrigId}, {misc, Misc}} -> 
			{?CFG(error_action), {{rule, Id}, {file, File}, {itype, ITypeOrigId}, {misc, Misc}}}
	end.

execute_itypes([], _Addr, _Files) -> neg;
execute_itypes(_ITypes, _Addr, []) -> neg;
execute_itypes(ITypes, Addr, Files) ->
	PAnyRet = mydlp_api:pany(fun(F) -> execute_itypes_pf(ITypes, Addr, F) end, Files, 180000),
	case PAnyRet of
		false -> neg;
		{ok, _File, Ret} -> Ret end.


execute_itypes_pf(ITypes, Addr, File) -> 
        File1 = case File#file.mime_type of 
                undefined -> 	MT = mydlp_tc:get_mime(File#file.data),
				File#file{mime_type=MT};
                _Else ->	File end,

	PAnyRet = mydlp_api:pany(fun(T) -> execute_itype_pf(T, Addr, File1) end, ITypes, 150000),
	case PAnyRet of
		false -> neg;
		{ok, _IType, Ret} -> Ret end.

execute_itype_pf({ITypeOrigId, all, IFeatures}, Addr, File) ->
	execute_itype_pf1(ITypeOrigId, IFeatures, Addr, File);
execute_itype_pf({ITypeOrigId, DataFormats, IFeatures}, Addr, 
		#file{mime_type=MT} = File) ->
        case mydlp_mnesia:is_mime_of_dfid(MT, DataFormats) of
                false -> neg;
		true -> execute_itype_pf1(ITypeOrigId, IFeatures, Addr, File) end.

execute_itype_pf1(ITypeOrigId, IFeatures, Addr, File) ->
	case execute_ifeatures(IFeatures, Addr, File) of
		neg -> neg;
		pos -> {pos, {file, File}, {itype, ITypeOrigId}, {misc, ""}};
		{error, {file, File}, {misc, Misc}} ->
				{error, {file, File}, {itype, ITypeOrigId}, {misc, Misc}};
		E -> E end.

execute_ifeatures([], _Addr, _File) -> 0;
execute_ifeatures(IFeatures, Addr, File) -> 
	try	PAllRet = mydlp_api:pall(fun({Threshold, {Func, FuncParams}}) ->
						apply_m(Threshold, Func, [FuncParams, Addr, File]) end,
					IFeatures, 120000),
		%%%% TODO: Check for PAnyRet whether contains error
		case PAllRet of
			false -> neg;
			%%% TODO: Unused results data can be used for more detailed logging.
			{ok, Results} -> is_distance_satisfied(Results) end
	catch _:{timeout, _F, _T} -> {error, {file, File}, {misc, timeout}} end.

is_distance_satisfied(Results) ->
	[ListOfIndexes, ListOfThresholds] = regulate_results(Results, 1),
	lists:keysort(1, ListOfIndexes),
	%time to distance control
	is_in_valid_distance(ListOfIndexes, ListOfThresholds).

is_in_valid_distance(ListOfIndexes, ListOfThresholds) when length(ListOfIndexes) == 1 ->
	Ret = is_all_thresholds_satisfied(ListOfIndexes, ListOfThresholds, 1),
	case Ret of 
		true -> pos;
		false -> neg
	end;

is_in_valid_distance([Head|Tail], ListOfThresholds) ->
	SubList = find_in_distance([Head|Tail]),
	SumOfThresholds = lists:sum(ListOfThresholds),
	EarlyNeg = (length(SubList) < SumOfThresholds),
	case EarlyNeg of 
		true -> is_in_valid_distance(Tail, ListOfThresholds);
		false -> case is_all_thresholds_satisfied(SubList, ListOfThresholds, 1) of
				true -> pos;
				false -> is_in_valid_distance(Tail, ListOfThresholds)
			 end
	end.

is_all_thresholds_satisfied(SubList, Thresholds, Index) when length(Thresholds) == Index ->
	is_threshold_satisfied(Index, SubList, lists:nth(Index, Thresholds));

is_all_thresholds_satisfied(SubList, Thresholds, Index) ->
	Ret = is_threshold_satisfied(Index, SubList, lists:nth(Index, Thresholds)),
	case Ret of
		true -> is_all_thresholds_satisfied(SubList, Thresholds, Index+1);
		false -> false
	end.

is_threshold_satisfied(Index, SubIndexList, Threshold) ->
	SubElements = lists:filter(fun({_I, W}) -> W == Index end, SubIndexList),
	(length(SubElements) >= Threshold).		

find_in_distance([Head|Tail]) ->
	{IndexValue, _WIT} = Head,
	lists:takewhile(fun({I, _W}) -> I < (IndexValue +100) end, [Head|Tail]).
	
regulate_results(Results, Number) when length(Results) == 1 ->
	[Head|_Tail] = Results,
	{pos, Threshold, {_Score, IndexList}} = Head,
	IndexesWithNumbers = lists:map(fun(I) -> {I, Number} end, IndexList),
	[IndexesWithNumbers, [Threshold]];
		
regulate_results([Head|Tail], Number) ->
	NewNumber = Number + 1,
	[OtherIndexes, OtherThresholds] = regulate_results(Tail, NewNumber),
	{pos, Threshold, {_Score, IndexList}} = Head, 
	IndexesWithNumbers = lists:map(fun(I) -> {I, Number} end, IndexList),
	[lists:append(IndexesWithNumbers, OtherIndexes), lists:append([Threshold], OtherThresholds)].

apply_m(_Threshold, all, [_FuncParams, _Addr, _File]) -> pos; %% match directly.
apply_m(Threshold, Func, [FuncParams, Addr, File]) ->
	Distance = 100,
	EarlyNeg = case get_matcher_req(Func) of
		raw -> false;
		analyzed -> false;
		text -> not mydlp_api:has_text(File) end,
	case EarlyNeg of
		true -> neg;
		false -> FuncOpts = get_func_opts(Func, FuncParams),
			{Score, IndexList} = apply(mydlp_matchers, Func, [FuncOpts, Addr, File]),
			EarlyNegForDistance = (length(IndexList) >= Threshold),
			case ((Score >= Threshold) and EarlyNegForDistance) of
				true -> {pos, Threshold, {Score, IndexList}}; % TODO: Scores should be logged.
				false -> neg
			end
	end.

get_matcher_req(Func) -> apply(mydlp_matchers, Func, []).

get_func_opts(Func, FuncParams) -> apply(mydlp_matchers, Func, [FuncParams]).

pl_text(Files) -> pl_text(Files, []).
pl_text([#file{text=undefined} = File|Files], Rets) -> 
	File1 = case mydlp_api:get_text(File) of
		{ok, Text} -> File#file{text = Text};
		{error, compression} -> File;
		{error, audio} -> File;
		{error, video} -> File;
		{error, image} -> File;
		_Else -> File#file{is_encrypted=true}
	end,
	pl_text(Files, [ File1 |Rets]);
pl_text([File|Files], Rets) -> pl_text(Files, [File|Rets]);
pl_text([], Rets) -> lists:reverse(Rets).

is_whitefile(_File) ->
	%Hash = erlang:md5(File#file.data),
	%mydlp_mnesia:is_fhash_of_gid(Hash, [mydlp_mnesia:get_pgid()])
	false.

drop_whitefile(Files) -> lists:filter(fun(F) -> not is_whitefile(F) end, Files).

has_data(#file{dataref={cacheref, _Ref}}) -> true;
has_data(#file{dataref={memory, Bin}}) -> size(Bin) > 0;
has_data(#file{data=Data}) when is_binary(Data)-> size(Data) > 0;
has_data(Else) -> throw({error, unexpected_obj, Else}).

drop_nodata(Files) -> lists:filter(fun(F) -> has_data(F) end, Files).

