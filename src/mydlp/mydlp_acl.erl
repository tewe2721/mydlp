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
	get_remote_rule_tables/1,
	q/5,
	qu/4,
	qa/3,
	qm/2
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
	is_multisite=false,
	error_action=pass
	}).

%%%%%%%%%%%%% MyDLP ACL API

-ifdef(__MYDLP_NETWORK).

get_remote_rule_tables(Addr) -> acl_call({get_remote_rule_tables, Addr}).

q(Channel, Site, Addr, DestList, Files) -> acl_call({q, Channel, Site, DestList, Addr}, Files).

qu(Channel, User, _Dest, Files) -> acl_call({qu, Channel, site, User}, Files).

qa(Channel, DestList, Files) -> acl_call({qa, Channel, site, DestList}, Files).

qm(Channel, Files) -> acl_call({qm, Channel, site}, Files).

-endif.

-ifdef(__MYDLP_ENDPOINT).

-endif.

% For handling inbound request.
qi(Channel, Files) -> acl_call({qi, Channel}, Files).

qe(Channel, Files) -> acl_call({qe, Channel, site}, Files).

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
		true -> archive;
		false -> pass end;
acl_call1(Query, Files, Timeout) -> gen_server:call(?MODULE, {acl, Query, Files, Timeout}, Timeout).

%%%%%%%%%%%%%% gen_server handles

-ifdef(__MYDLP_NETWORK).

acl_exec(_RuleTables, _Source, []) -> pass;
acl_exec(RuleTables, Source, Files) ->
	acl_exec2(RuleTables, Source, Files).

-endif.

% acl_exec2([{{_Id, DefaultAction}, Rules}| Rest], Source, Files)  % Cannot be more than one filter
acl_exec2({ACLOpts, {_Id, DefaultAction}, Rules}, Source, Files) ->
	case { DefaultAction, acl_exec3(ACLOpts, Rules, Source, Files) } of
		% {return, return}-> acl_exec2(Rest, Source, Files);  % Cannot be more than one filter
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

%% it needs refactoring for trusted domains
handle_acl({q, Channel, SAddr, DestList, Addr}, Files, #state{is_multisite=true}) ->
	case mydlp_mnesia:get_fid(SAddr) of
		nofilter -> block;
		CustomerId -> 
			Rules = mydlp_mnesia:get_rules_for_fid(Channel, CustomerId, DestList, Addr),
			acl_exec(Rules, [{cid, CustomerId}, {addr, Addr}], Files) end;

handle_acl({q, Channel, _Site, DestList, Addr}, Files, #state{is_multisite=false}) ->
	%Rules = mydlp_mnesia:get_rules(Addr),
	CustomerId = mydlp_mnesia:get_dfid(),
	Rules = mydlp_mnesia:get_rules_for_fid(Channel, CustomerId, DestList, Addr),
	acl_exec(Rules, [{cid, CustomerId}, {addr, Addr}], Files);

handle_acl({get_remote_rule_tables, Addr}, _Files, _State) ->
	CustomerId = mydlp_mnesia:get_dfid(),
	% TODO: change needed for multi-site use
	mydlp_mnesia:get_remote_rule_tables(CustomerId, Addr); 

%% now this is used for only SMTP, and in SMTP domain part of, mail adresses itself a siteid for customer.
%% it needs refactoring for both multisite and trusted domains
handle_acl({qu, Channel, _Site, User}, Files, _State) ->
	Rules = mydlp_mnesia:get_rules_by_user(Channel, User),
	acl_exec(Rules, [{cid, mydlp_mnesia:get_dfid()}, {user, User}], Files);

handle_acl({qa, Channel, _Site, DestList}, Files, _State) ->
	Rules = mydlp_mnesia:get_all_rules(Channel, DestList),
	acl_exec(Rules, [{cid, mydlp_mnesia:get_dfid()}], Files);

handle_acl({qm, Channel, _Site}, Files, _State) ->
	Rules = mydlp_mnesia:get_all_rules(Channel),
	acl_exec(Rules, [{cid, mydlp_mnesia:get_dfid()}], Files);

handle_acl({qe, Channel, Site}, Files, State) -> handle_acl({qm, Channel, Site}, Files, State);

handle_acl(Q, _Files, _State) -> throw({error, {undefined_query, Q}}).

-endif.

-ifdef(__MYDLP_ENDPOINT).

handle_acl({qe, _Channel, _Site}, [#file{mime_type= <<"mydlp-internal/usb-device;id=unknown">>}] = Files, _State) ->
	{?CFG(error_action), mydlp_api:empty_aclr(Files, usb_device_id_unknown)};

handle_acl({qe, _Channel, _Site}, [#file{mime_type= <<"mydlp-internal/usb-device;id=", DeviceId/binary>>}] = Files, _State) ->
	case mydlp_mnesia:is_valid_usb_device_id(DeviceId) of % TODO: need refinements for multi-user usage.
		true -> pass;
		false -> {block, mydlp_api:empty_aclr(Files, usb_device_rejected)} end;

handle_acl({qe, Channel, _Site}, Files, _State) ->
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
		catch Class:Error ->
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

handle_info({async_acl_q, Res, From}, #state{error_action=Action} = State) ->
	Reply = case Res of
		{ok, R} -> R;
		{error, _} -> Action end, % TODO conf

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
%	IsMS = mydlp_mysql:is_multisite(),
%	{ok, #state{is_multisite=IsMS, error_action=?CFG(error_action)}}.
	{ok, #state{is_multisite=false, error_action=?CFG(error_action)}}.

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

execute_itype_pf({ITypeOrigId, Threshold, all, IFeatures}, Addr, File) ->
	execute_itype_pf1(ITypeOrigId, Threshold, IFeatures, Addr, File);
execute_itype_pf({ITypeOrigId, Threshold, DataFormats, IFeatures}, Addr, 
		#file{mime_type=MT} = File) ->
        case mydlp_mnesia:is_mime_of_dfid(MT, DataFormats) of
                false -> neg;
		true -> execute_itype_pf1(ITypeOrigId, Threshold, IFeatures, Addr, File) end.

execute_itype_pf1(ITypeOrigId, Threshold, IFeatures, Addr, File) ->
	case execute_ifeatures(IFeatures, Addr, File) of
		I when is_integer(I), I >= Threshold -> 
				{pos, {file, File}, {itype, ITypeOrigId}, 
				{misc, "score=" ++ integer_to_list(I)}};
		I when is_integer(I) -> neg;
		{error, {file, File}, {misc, Misc}} ->
				{error, {file, File}, {itype, ITypeOrigId}, {misc, Misc}};
		E -> E end.

execute_ifeatures([], _Addr, _File) -> 0;
execute_ifeatures(IFeatures, Addr, File) -> 
	try	PMapRet = mydlp_api:pmap(fun({Weight, {Func, FuncParams}}) ->
						apply_m(Weight, Func, [FuncParams, Addr, File]) end,
					IFeatures, 120000),
		%%%% TODO: Check for PMapRet whether contains error
		lists:sum(PMapRet)
	catch _:{timeout, _F, T} -> {error, {file, File}, {misc, "timeout=" ++ integer_to_list(T)}} end.

apply_m(Weight, Func, [FuncParams, Addr, File]) ->
	EarlyNeg = case get_matcher_req(Func) of
		raw -> false;
		analyzed -> false;
		text -> not mydlp_api:has_text(File) end,
	case EarlyNeg of
		true -> 0;
		false -> FuncOpts = get_func_opts(Func, FuncParams),
			Count = apply(mydlp_matchers, Func, [FuncOpts, Addr, File]),
			Count * Weight end.

get_matcher_req(Func) -> apply(mydlp_matchers, Func, []).

get_func_opts(Func, FuncParams) -> apply(mydlp_matchers, Func, [FuncParams]).

pl_text(Files) -> pl_text(Files, []).
pl_text([#file{text=undefined} = File|Files], Rets) -> 
	File1 = case mydlp_api:get_text(File) of
		{ok, Text} -> File#file{text = Text};
		{error, cobject} -> File;
		{error, compression} -> File;
		{error, binary_format} -> File;
		{error, image} -> File;
		_Else -> File#file{is_encrypted=true}
	end,
	pl_text(Files, [ File1 |Rets]);
pl_text([File|Files], Rets) -> pl_text(Files, [File|Rets]);
pl_text([], Rets) -> lists:reverse(Rets).

is_whitefile(File) ->
	Hash = erlang:md5(File#file.data),
	mydlp_mnesia:is_fhash_of_gid(Hash, [mydlp_mnesia:get_pgid()]).

drop_whitefile(Files) -> lists:filter(fun(F) -> not is_whitefile(F) end, Files).

has_data(#file{dataref={cacheref, _Ref}}) -> true;
has_data(#file{dataref={memory, Bin}}) -> size(Bin) > 0;
has_data(#file{data=Data}) when is_binary(Data)-> size(Data) > 0;
has_data(Else) -> throw({error, unexpected_obj, Else}).

drop_nodata(Files) -> lists:filter(fun(F) -> has_data(F) end, Files).

