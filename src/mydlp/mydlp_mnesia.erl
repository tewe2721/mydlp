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
%%% @doc Persistency api for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-module(mydlp_mnesia).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").
-include("mydlp_acl.hrl").

%% API
-export([start_link/0,
	stop/0]).

%% API common
-export([
	get_unique_id/1,
	compile_regex/0,
	get_cgid/0,
	get_pgid/0,
	get_dfid/0,
	get_drid/0,
	wait_for_tables/0,
	get_regexes/1,
	get_mc_module/0,
	get_config_value/1,
	is_mime_of_dfid/2,
	is_hash_of_gid/2,
	get_fs_entry/1,
	del_fs_entry/1,
	del_fs_entries_by_rule_id/1,
	add_fs_entry/1,
	fs_entry_list_dir/1,
	pdm_of_gid/2,
	get_record_fields/1,
	dump_tables/1,
	dump_client_tables/0,
	truncate_all/0,
	truncate_nondata/0,
	write/1,
	delete/1,
	flush_cache/0,
	post_start/1,
	post_start/0
	]).

-ifdef(__MYDLP_NETWORK).

%API network
-export([
	new_authority/1,
	get_mnesia_nodes/0,
	get_rule_table/2,
	get_rules/2,
	get_rule_ids/2,
	get_remote_user_rule_ids/0,
	get_remote_ipr_rule_ids/0,
	get_remote_default_rule_ids/0,
	get_remote_rule_tables/3,
	get_remote_rule_ids/3,
	get_notification_items/1,
	get_notification_queue_items/1,
	get_early_notification_queue_items/0,
	update_notification_queue_item/2,
	get_remote_mc_module/3,
	get_fid/1,
	remove_site/1,
	add_fhash/3,
	save_user_address/3,
	remove_old_user_address/0,
	get_user_from_address/1,
	save_endpoint_command/3,
	save_endpoint_command/2,
	remove_old_endpoint_command/0,
	remove_endpoint_command/3,
	get_endpoint_commands/1,
	get_keywords/1,
	get_matchers/0,
	get_matchers/1,
	get_remote_storages/0,
	get_remote_storage_by_id/1,
	get_remote_storages_by_rule_id/1,
	get_rule_id_by_web_server_id/1,
	get_web_server/1,
	get_web_servers_by_rule_id/1,
	get_web_entry/1,
	add_web_entry/1,
	web_entry_list_links/1,
	del_web_entries_by_rule_id/1,
	get_user_message/2,
	get_schedules_by_hour/1,
	get_availabilty_by_rule_id/1,
	register_schedule/2,
	get_waiting_schedule_by_rule_id/1,
	get_rule_id_by_orig_id/1,
	get_orig_id_by_rule_id/1,
	get_rule_channel/1,
	get_discovery_rule_ids/2,
	update_ep_schedules/2,
	%update_rfs_and_web_schedules/1,
	get_endpoints_by_rule_id/1,
	get_remote_document_databases/0,
	add_dd_file_entry/1,
	get_dd_file_entry/1
	]).

-endif.

-ifdef(__MYDLP_ENDPOINT).

% API endpoint 
-export([
	get_rule_table/1,
	get_rule_table/2,
	get_rule_table_destination/1,
	get_discovery_directory/1,
	get_prtscr_app_name/0,
	get_inbound_rule/0,
	is_valid_usb_device_id/1
	]).

-endif.

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(state, {}).

%%%%%%%%%%%%%%%% Table definitions

-define(CLIENT_TABLES, [
	config,
	mime_type,
	file_hash,
	file_fingerprint,
	usb_device,
	regex
]).


-ifdef(__MYDLP_NETWORK).

-define(OTHER_DATA_TABLES,[
]).

-endif.

-ifdef(__MYDLP_ENDPOINT).

-define(OTHER_DATA_TABLES,[]).

-endif.

-define(DATA_TABLES, ?OTHER_DATA_TABLES).

-ifdef(__MYDLP_NETWORK).

-define(NONDATA_FUNCTIONAL_TABLES, [
	filter,
	rule,
	ipr,
	dest,
	notification, 
	notification_queue,
	remote_storage,
	remote_storage_dd,
	discovery_schedule,
	discovery_targets,
	waiting_schedules,
	{m_user, ordered_set, 
		fun() -> mnesia:add_table_index(m_user, un_hash) end},
	{source_domain, ordered_set, 
		fun() -> mnesia:add_table_index(source_domain, domain_name) end},
	itype,
	ifeature,
	match, 
	{keyword, ordered_set, 
		fun() -> mnesia:add_table_index(keyword, group_id) end},
	site_desc,
	{user_address, ordered_set, 
		fun() -> mnesia:add_table_index(user_address, last_seen) end},
	{endpoint_command, ordered_set, 
		fun() -> mnesia:add_table_index(endpoint_command, endpoint_id) end},
	user_message,
	{web_entry, ordered_set,
		fun() -> mnesia:add_table_index(web_entry, parent_id) end},
	web_server
]).

-endif.

-ifdef(__MYDLP_ENDPOINT).

-define(NONDATA_FUNCTIONAL_TABLES, [
	{rule_table, ordered_set, 
		fun() -> mnesia:add_table_index(rule_table, head) end}
]).

-endif.

-define(NONDATA_COMMON_TABLES, [
	config,
	mc_module, 
	{fs_entry, ordered_set, 
		fun() -> mnesia:add_table_index(fs_entry, parent_id),
			 mnesia:add_table_index(fs_entry, entry_id) end},
	{dd_file_entry, ordered_set,
		fun() -> mnesia:add_table_index(dd_file_entry, filepath) end},
	{mime_type, ordered_set, 
		fun() -> mnesia:add_table_index(mime_type, mime) end},
	{regex, ordered_set, 
		fun() -> mnesia:add_table_index(regex, group_id) end},
	file_hash,
	file_fingerprint,
	{usb_device, ordered_set, 
		fun() -> mnesia:add_table_index(usb_device, device_id) end}
]).

-define(NONDATA_TABLES, lists:append(?NONDATA_FUNCTIONAL_TABLES, ?NONDATA_COMMON_TABLES)).

-define(TABLES, lists:append(?DATA_TABLES, ?NONDATA_TABLES)).

get_record_fields_common(Record) -> 
        case Record of
		unique_ids -> record_info(fields, unique_ids);
		config -> record_info(fields, config);
		fs_entry -> record_info(fields, fs_entry);
		dd_file_entry -> record_info(fields, dd_file_entry);
		usb_device -> record_info(fields, usb_device);
		file_hash -> record_info(fields, file_hash);
		file_fingerprint -> record_info(fields, file_fingerprint);
		mime_type -> record_info(fields, mime_type);
		regex -> record_info(fields, regex);
		mc_module -> record_info(fields, mc_module);
		_Else -> not_found
	end.

-ifdef(__MYDLP_NETWORK).

get_record_fields_functional(Record) ->
        case Record of
		filter -> record_info(fields, filter);
		rule -> record_info(fields, rule);
		ipr -> record_info(fields, ipr);
		dest -> record_info(fields, dest);
		remote_storage -> record_info(fields, remote_storage);
		remote_storage_dd -> record_info(fields, remote_storage_dd);
		discovery_schedule -> record_info(fields, discovery_schedule);
		discovery_targets -> record_info(fields, discovery_targets);
		waiting_schedules -> record_info(fields, waiting_schedules);
		notification -> record_info(fields, notification);
		notification_queue -> record_info(fields, notification_queue);
		source_domain -> record_info(fields, source_domain);
		m_user -> record_info(fields, m_user);
		itype -> record_info(fields, itype);
		ifeature -> record_info(fields, ifeature);
		match -> record_info(fields, match);
		keyword -> record_info(fields, keyword);
		site_desc -> record_info(fields, site_desc);
		user_address -> record_info(fields,user_address);
		endpoint_command -> record_info(fields,endpoint_command);
		web_server -> record_info(fields,web_server);
		web_entry -> record_info(fields,web_entry);
		user_message -> record_info(fields, user_message);
		_Else -> not_found
	end.

-endif.

-ifdef(__MYDLP_ENDPOINT).

get_record_fields_functional(Record) ->
        case Record of
		rule_table -> record_info(fields, rule_table);
		_Else -> not_found
	end.

-endif.

get_record_fields(Record) -> 
	case get_record_fields_common(Record) of
		not_found -> get_record_fields_functional(Record);
		Else -> Else end.

get_copy_media(user_address) -> ram_copies;
get_copy_media(_Else) -> disc_copies.

%-define(QLCQ(ListC), qlc:q(ListC, [{cache, ets}])).

%-define(QLCE(Query), qlc:e(Query, [{cache_all, ets}])).

-define(QLCQ(ListC), qlc:q(ListC)).

-define(QLCQU(ListC), qlc:q(ListC, {unique,true})).

-define(QLCE(Query), qlc:e(Query)).

%%%%%%%%%%%%% MyDLP Mnesia API

get_cgid() -> -1.

get_pgid() -> -2.

get_dfid() -> 1.

get_drid() -> 0.

wait_for_tables() -> wait_for_tables(15000).

wait_for_tables(Timeout) when Timeout > 0 ->
	TableList = lists:map( fun
			({RecordAtom,_,_}) -> RecordAtom;
			({RecordAtom,_}) -> RecordAtom;
			(RecordAtom) when is_atom(RecordAtom) -> RecordAtom
		end, ?TABLES),
	CurTables = mnesia:system_info(tables),
	DoTablesExist = lists:all(fun(T) -> lists:member(T, CurTables) end, TableList),

	case DoTablesExist of
		true -> mnesia:wait_for_tables(TableList, Timeout);
		_Else -> timer:sleep(500), wait_for_tables(Timeout-500) end;

wait_for_tables(_Else) -> timeout.

-ifdef(__MYDLP_NETWORK).

get_rules(FilterId, AclQ) -> RuleIDs = get_rule_ids(FilterId, AclQ), get_rule_table(FilterId, RuleIDs).

get_rule_ids(FilterId, AclQ) -> aqc({get_rule_ids, FilterId, AclQ}, cache).

get_rule_table(FilterId, RuleIDs) -> aqc({get_rule_table, FilterId, RuleIDs}, cache).

get_remote_rule_tables(FilterId, Addr, UserH) -> aqc({get_remote_rule_tables, FilterId, Addr, UserH}, cache).

get_remote_rule_ids(FilterId, Addr, UserH) -> aqc({get_remote_rule_ids, FilterId, Addr, UserH}, cache).

get_remote_user_rule_ids() -> aqc(get_remote_user_rule_ids, nocache, dirty).

get_remote_ipr_rule_ids() -> aqc(get_remote_ipr_rule_ids, nocache, dirty).

get_remote_default_rule_ids() -> aqc(get_remote_default_rule_ids, nocache, dirty).

get_notification_items(RuleId) -> aqc({get_notification_items, RuleId}, cache). 

get_notification_queue_items(RuleId) -> aqc({get_notification_queue_items, RuleId}, nocache). 

get_early_notification_queue_items() -> aqc(get_early_notification_queue_items, nocache).

update_notification_queue_item(RuleId, NewStatus) -> aqc({update_notification_queue_item, RuleId, NewStatus}, nocache). 

get_remote_mc_module(FilterId, Addr, UserH) -> 
	RuleIDs = get_remote_rule_ids(FilterId, Addr, UserH),
	Mods = case get_mc_module(RuleIDs) of
		[] -> ?ERROR_LOG("Cannot find mc module for remote. Addr: "?S", UserH: "?S", Rule Ids: "?S , 
			[Addr, UserH, RuleIDs]), get_mc_module();
		ML -> ML end,
	#mc_module{target=local, modules=Mods}.

get_fid(SIpAddr) -> aqc({get_fid, SIpAddr}, cache).

remove_site(FilterId) -> aqc({remove_site, FilterId}, flush, dirty, 90000).

add_fhash(Hash, FileId, GroupId) when is_binary(Hash) -> 
	aqc({add_fhash, Hash, FileId, GroupId}, flush).

new_authority(Node) -> gen_server:call(?MODULE, {new_authority, Node}, 30000).

save_user_address(IpAddress, UserHash, UserName) -> aqc({save_user_address, IpAddress, UserHash, UserName}, nocache, dirty).

remove_old_user_address() -> aqc(remove_old_user_address, nocache, dirty).

get_user_from_address(IpAddress) -> aqc({get_user_from_address, IpAddress}, nocache, dirty).

save_endpoint_command(EndpointId, Command, Args) -> aqc({save_endpoint_command, EndpointId, Command, Args}, nocache, dirty).

save_endpoint_command(EndpointId, Command) -> aqc({save_endpoint_command, EndpointId, Command}, nocache, dirty).

remove_old_endpoint_command() -> aqc(remove_old_endpoint_command, nocache, dirty).

remove_endpoint_command(EndpointId, Command, Args) -> aqc({remove_endpoint_command, EndpointId, Command, Args}, nocache, dirty).

get_endpoint_commands(EndpointId) -> aqc({get_endpoint_commands, EndpointId}, nocache, dirty).

get_keywords(GroupId) -> aqc({get_keywords, GroupId}, nocache).

get_matchers() -> get_matchers(all).

get_matchers(Source) -> aqc({get_matchers, Source}, nocache).

get_user_message(OrigRuleId, Format) -> aqc({get_user_message, OrigRuleId, Format}, cache).

get_remote_storages() -> aqc(get_remote_storages, cache).

get_remote_storage_by_id(Id) -> aqc({get_remote_storage_by_id, Id}, cache).

get_remote_storages_by_rule_id(RuleId) -> aqc({get_remote_storages_by_rule_id, RuleId}, cache).

get_rule_id_by_web_server_id(Id) -> aqc({get_rule_id_by_web_server_id, Id}, cache).

get_web_servers_by_rule_id(RuleId) -> aqc({get_web_servers_by_rule_id, RuleId}, cache).

get_web_server(WebServerId) -> aqc({get_web_server, WebServerId}, cache).

get_web_entry(EntryId) -> aqc({get_web_entry, EntryId}, nocache).

add_web_entry(Record) when is_tuple(Record) -> write(Record, nocache).

web_entry_list_links(EntryId) -> aqc({web_entry_list_links, EntryId}, nocache).

del_web_entries_by_rule_id(RuleId) -> aqc({del_web_entries_by_rule_id, RuleId}, nocache).

get_schedules_by_hour(Day) -> aqc({get_schedules_by_hour, Day}, nocache).

get_availabilty_by_rule_id(RuleId) -> aqc({get_availabilty_by_rule_id, RuleId}, nocache).

register_schedule(RuleId, GroupId) -> aqc({register_schedule, RuleId, GroupId}, nocache).

get_waiting_schedule_by_rule_id(RuleId) -> aqc({get_waiting_schedule_by_rule_id, RuleId}, nocache).

get_rule_id_by_orig_id(OrigRuleId) -> aqc({get_rule_id_by_orig_id, OrigRuleId}, nocache).

get_orig_id_by_rule_id(RuleId) -> aqc({get_orig_id_by_rule_id, RuleId}, nocache).

get_rule_channel(RuleId) -> aqc({get_rule_channel, RuleId}, nocache).

get_discovery_rule_ids(Ip, Username) -> aqc({get_discovery_rule_ids, Ip, Username}, nocache).

update_ep_schedules({EndpointId, Ip, Username}, TargetRuleId) ->
	SrcUserH = mydlp_api:hash_un(Username),
	ClientIp = mydlp_api:str_to_ip(binary_to_list(Ip)),
	AclQ = #aclq{src_addr=ClientIp, src_user_h=SrcUserH},
	RuleIds = get_rule_ids(get_dfid(), AclQ#aclq{channel=discovery}),
	aqc({update_ep_schedules, EndpointId, RuleIds, TargetRuleId}, nocache, dirty).

get_endpoints_by_rule_id(RuleId) -> aqc({get_endpoints_by_rule_id, RuleId}, nocache).

get_remote_document_databases() -> aqc(get_remote_document_databases, nocache).

add_dd_file_entry(#dd_file_entry{filepath=FilePath}=Record) ->
	aqc({remove_redundant_dd_file_entries, FilePath}, nocache),
	write(Record).

get_dd_file_entry(FilePath) -> aqc({get_dd_file_entry, FilePath}, nocache).

-endif.

-ifdef(__MYDLP_ENDPOINT).

get_rule_table(Channel) -> aqc({get_rule_table, Channel}, cache).

get_rule_table(Channel, RuleIndex) -> aqc({get_rule_table, Channel, RuleIndex}, cache).

get_rule_table_destination(Channel) -> aqc({get_rule_table_destination, Channel}, cache).

get_discovery_directory(RuleId) -> aqc({get_discovery_directory, RuleId}, cache).

get_prtscr_app_name() -> aqc({get_prtscr_app_name}, cache).

get_inbound_rule() -> aqc(get_inbound_rule, cache). 

is_valid_usb_device_id(DeviceId) -> aqc({is_valid_usb_device_id, DeviceId}, cache).

-endif.

get_fs_entry(FilePath) -> aqc({get_fs_entry, FilePath}, nocache).

del_fs_entry(FilePath) -> aqc({del_fs_entry, FilePath}, nocache).

del_fs_entries_by_rule_id(RuleId) -> aqc({del_fs_entries_by_rule_id, RuleId}, nocache).

fs_entry_list_dir(EntryId) -> aqc({fs_entry_list_dir, EntryId}, nocache).

add_fs_entry(Record) when is_tuple(Record) -> write(Record, nocache).


dump_tables(Tables) when is_list(Tables) -> aqc({dump_tables, Tables}, cache);
dump_tables(Table) -> dump_tables([Table]).

dump_client_tables() -> dump_tables(?CLIENT_TABLES).

get_regexes(GroupId) ->	aqc({get_regexes, GroupId}, cache).

get_mc_module() -> get_mc_module(local).

get_mc_module(Target) -> aqc({get_mc_module, Target}, nocache).

get_config_value(KeyB) -> aqc({get_config_value, KeyB}, nocache).

is_mime_of_dfid(Mime, DataFormatIds) -> 
	aqc({is_mime_of_dfid, Mime, DataFormatIds}, cache).

is_hash_of_gid(Hash, GroupId) -> aqc({is_hash_of_gid, Hash, GroupId}, nocache, dirty).

pdm_of_gid(Fingerprints, GroupId) -> aqc({pdm_of_gid, Fingerprints, GroupId}, nocache, dirty, 60000).

write(RecordList, CacheOption) when is_list(RecordList) -> 
	T = 15000 + ( length(RecordList) * 10 ),
	Timeout = case T > 150000 of true -> 150000; _ -> T end,
	aqc({write, RecordList}, CacheOption, dirty, Timeout);
write(Record, CacheOption) -> write([Record], CacheOption).

write(Item) -> write(Item, flush).

delete(Item) -> aqc({delete, Item}, flush).

truncate_all() -> gen_server:call(?MODULE, truncate_all, 15000).

truncate_nondata() -> gen_server:call(?MODULE, truncate_nondata, 15000).

mnesia_dir_cleanup() -> gen_server:cast(?MODULE, mnesia_dir_cleanup).

flush_cache() -> cache_clean0().



%%%%%%%%%%%%%% gen_server handles

-ifdef(__MYDLP_NETWORK).


handle_result({get_user_from_address, _IpAddress}, {atomic, Result}) -> 
	case Result of
		[] -> {nil, unknown};
		[#user_address{username=UserName, un_hash=UserHash}] -> {UserName, UserHash} end;

handle_result({get_user_message, _OrigRuleId, Format}, {atomic, Result}) -> 
	Message = case Result of
		[] -> <<>>;
		[M] -> M end,
	case Format of
		html -> mydlp_denied_page:get_raw(Message);
		html_base64_str -> mydlp_denied_page:get_base64_str(Message) end;

handle_result(get_remote_storages, {atomic, Result}) ->
	case Result of 
		[] -> none;
		_ -> Result end;

handle_result({get_remote_storage_by_id, _Id}, {atomic, Result}) ->
	case Result of 
		[] -> none;
		[Table] -> Table end;

%handle_result({get_remote_storages_by_rule_id, _RuleId}, {atomic, Result}) ->
%	case Result of 
%		[] -> none;
%		_ -> Result end;

handle_result({get_rule_id_by_web_server_id, _Id}, {atomic, Result}) ->
	case Result of 
		[] -> none;
		[Table] -> Table end;

handle_result({get_schedules_by_hour, _}, {atomic, Result}) ->
	case Result of
		[] -> none;
		_ -> filter_for_day(Result, []) end;

handle_result({get_availabilty_by_rule_id, _}, {atomic, Result}) ->
	case Result of
		[] -> false;
		_ -> is_available(Result) end;

handle_result({get_rule_id_by_orig_id, _}, {atomic, Result}) ->
	case Result of
		[] -> none;
		[R] -> R end;

handle_result({get_orig_id_by_rule_id, _}, {atomic, Result}) ->
	case Result of
		[] -> none;
		[R] -> R end;

handle_result({get_rule_channel, _}, {atomic, Result}) ->
	case Result of
		[] -> none;
		[R] -> R end;

handle_result({get_endpoint_commands, _EntryId}, {atomic, Result}) -> 
	lists:map(fun(#endpoint_command{command=C, args=Args}) ->
			case C of
				{set_enc_key, _} -> {command, C};
				_ -> {command, C, Args} end end , Result);
	%[ {command, C, Args} || #endpoint_command{command=C, args=Args} <- Result ];

handle_result({get_matchers, _Source}, {atomic, Result}) -> lists:usort(Result);

handle_result({get_web_server, WebServerId}, {atomic, Result}) ->
	case Result of
		[#web_server{id=WebServerId} = W] -> W;
		_Else -> none end;

handle_result({get_web_entry, _EntryId}, {atomic, Result}) -> 
	case Result of
		[] -> none;
		[WebEntry] -> WebEntry end;

handle_result({web_entry_list_links, _EntryId}, {atomic, Result}) -> 
	[ LP || #web_entry{entry_id={_WebServerId, LP}} <- Result ];

handle_result({del_web_entries_by_rule_id, RuleId}, {atomic, Result}) ->
	remove_reduntant_web_entries(Result, RuleId);

handle_result({get_dd_file_entry, FilePath}, {atomic, Result}) ->
	case Result of
		[] -> none;
		[DDFileEntry] -> DDFileEntry;
		R -> ?ERROR_LOG("Unexpected dd file entry result: ["?S"]", R)
	end;

handle_result(Query, Result) -> handle_result_common(Query, Result).

-endif.

-ifdef(__MYDLP_ENDPOINT).

handle_result({get_rule_table, _Channel}, {atomic, Result}) -> 
	case Result of
		[] -> none;
		[Table] -> Table end;

handle_result({get_rule_table, _Channel, RuleIndex}, {atomic, Result}) -> 
	Res = case Result of
		[] -> none;
		[{Req, IdAndDefaultAction, RuleTables}] ->
			UniqueRule = get_rule_with_id(RuleTables, RuleIndex),
			%UniqueRule = lists:nth(RuleIndex+1, RuleTables),
			{Req, IdAndDefaultAction, [UniqueRule]} end,
	Res;

handle_result({get_rule_table_destination, _Channel}, {atomic, Result}) -> 
	case Result of
		[] -> [];
		[D] -> D end;


handle_result({get_discovery_directory, RuleId}, {atomic, Result}) ->
	FilePaths = case Result of
			[] -> none;
			[{FileaPaths, {_, _, Rules}}] -> get_directory_by_rule_id(FileaPaths, Rules, RuleId, 0)
		end,
	lists:map(fun(P) ->
			case P of
				all -> <<"C:/">>;
				_ -> P
			end
		end, FilePaths);	

handle_result({get_prtscr_app_name}, {atomic, Result}) -> 
	case Result of
		[] -> none;
		[Table] -> Table end;

handle_result(get_inbound_rule, {atomic, Result}) ->
	case Result of
		[{_,_,[]}] -> {-1, pass};
		[{_,_,[{RuleId, Action,_}|_]}|_] -> {RuleId,Action} end;

handle_result({is_valid_usb_device_id, _DeviceId}, {atomic, Result}) -> 
	case Result of
		[] -> false;
		[_|_] -> true end;


handle_result(Query, Result) -> handle_result_common(Query, Result).

-endif.

handle_result_common({del_fs_entries_by_rule_id, RuleId}, {atomic, Result}) ->
	remove_reduntant_fs_entries(Result, RuleId);

handle_result_common({is_mime_of_dfid, _Mime, DFIs}, {atomic, MDFIs}) -> 
	lists:any(fun(I) -> lists:member(I, DFIs) end, MDFIs);

handle_result_common({is_hash_of_gid, Hash, _GroupId}, {atomic, Set}) -> 
	gb_sets:is_member(Hash, Set);

handle_result_common({pdm_of_gid, Fingerprints, _GroupId}, {atomic, Set}) -> 
	pdm_hit_count(Fingerprints, Set, 0);

% TODO: instead of case statements, refining function definitions will make queries faster.
handle_result_common({get_fid, _SIpAddr}, {atomic, Result}) -> 
	case Result of
		[] -> nofilter;
		[FilterId] -> FilterId end;

handle_result_common({get_config_value, _}, {atomic, Result}) -> 
	case Result of
		[] -> none;
		[ValB] -> ValB end;

handle_result_common({get_mc_module, _Target}, {atomic, Result}) -> 
	case Result of
		[] -> [];
		[#mc_module{modules=Mods}] -> Mods end;

handle_result_common({get_fs_entry, _FilePath}, {atomic, Result}) -> 
	case Result of
		[] -> none;
		[FSEntry] -> FSEntry end;

handle_result_common({fs_entry_list_dir, _EntryId}, {atomic, Result}) -> 
	[ FP || #fs_entry{file_id={FP,_RuleIndex}} <- Result ];


handle_result_common(_Query, {atomic, Objects}) -> Objects.

-ifdef(__MYDLP_NETWORK).

is_applicable_destination(Destinations, UserDestination) ->
	R = mydlp_api:reverse_binary(UserDestination),
	A = lists:filter(fun(D) -> R1 = mydlp_api:reverse_binary(D),
				is_sub_destination(R, R1) end, 
				Destinations),
	length(A) > 0.

is_sub_destination(<<>>, _Dest2) -> true;
is_sub_destination(<<C/utf8, R/binary>>, <<C1/utf8, R1/binary>>) when C == C1 -> is_sub_destination(R, R1);
is_sub_destination(_, _) -> false.

select_rule_ids_by_source(FilterId, #aclq{channel=Channel} = AclQ) ->
	Q0 = ?QLCQ([R#rule.id || 
		R <- mnesia:table(rule),
		I <- mnesia:table(ipr),
		R#rule.filter_id == FilterId,
		R#rule.channel == Channel,
		I#ipr.rule_id == R#rule.id,
		I#ipr.ipbase == {0,0,0,0}, 
		I#ipr.ipmask == {0,0,0,0}
	]),
	RulesD = ?QLCE(Q0),

	RulesI = case AclQ#aclq.src_addr of
		unknown -> [];
		Addr -> Q = ?QLCQ([R#rule.id || 
				R <- mnesia:table(rule),
				I <- mnesia:table(ipr),
				R#rule.filter_id == FilterId,
				R#rule.channel == Channel,
				I#ipr.rule_id == R#rule.id,
				I#ipr.ipbase /= {0,0,0,0}, 
				I#ipr.ipmask /= {0,0,0,0},
				ip_band(I#ipr.ipbase, I#ipr.ipmask) == ip_band(Addr, I#ipr.ipmask)
				]), ?QLCE(Q) end,

	RulesU = case AclQ#aclq.src_user_h of
		unknown -> [];
		UserH -> Q2 = ?QLCQ([R#rule.id || 
				R <- mnesia:table(rule),
				U <- mnesia:table(m_user),
				R#rule.filter_id == FilterId,
				R#rule.channel == Channel,
				U#m_user.rule_id == R#rule.id,
				U#m_user.un_hash == UserH
				]), ?QLCE(Q2) end,

	RulesDU = case AclQ#aclq.src_domain of
		undefined -> [];
		DomainName -> Q4 = ?QLCQ([R#rule.id ||
				R <- mnesia:table(rule),
				U <- mnesia:table(source_domain),
				R#rule.filter_id == FilterId,
				R#rule.channel == Channel,
				U#source_domain.rule_id == R#rule.id,
				U#source_domain.domain_name == DomainName
				]), ?QLCE(Q4) end, 

	RuleIds = lists:append([RulesD, RulesI, RulesU, RulesDU]),
	RuleIds.

filter_rule_ids_by_dest(RuleIds, AclQ) -> %TODO: domain names stored as binary. Unicode characters should be examined wheter it is problem or not.
	Q0 = ?QLCQ([R ||
		D <- mnesia:table(dest),
		R <- RuleIds,
		D#dest.rule_id == R,
		D#dest.destination == all
	]),
	RuleaD = ?QLCE(Q0),
	
	Q1 = ?QLCQ([R ||
		D <- mnesia:table(dest),
		R <- RuleIds,
		D#dest.rule_id == R,
		D#dest.destination /= all,
		D#dest.destination /= has_bcc,
		is_applicable_destination(AclQ#aclq.destinations, D#dest.destination)
	]),
	RulenD = ?QLCE(Q1),

	RuleHD = case AclQ#aclq.has_hidden_destinations of
		true -> case AclQ#aclq.channel of
			mail -> Q2 = ?QLCQ([R ||
					D <- mnesia:table(dest),
					R <- RuleIds,
					D#dest.rule_id == R,
					D#dest.destination == has_bcc
				]),
				?QLCE(Q2);
			_Else -> [] end;
		false -> [] end,

	lists:append([RuleaD, RulenD, RuleHD]).

get_rule_destinations(RuleIds) -> 
	DL =get_rule_destinations(RuleIds, 0, []),
	lists:reverse(DL).

get_rule_destinations([Id|RuleIds], Index, Acc) ->
	Q0 = ?QLCQ([{D#dest.destination, Index} ||
		D <- mnesia:table(dest),
		D#dest.rule_id == Id
	]),
	Q1 = ?QLCE(Q0),
	get_rule_destinations(RuleIds, Index+1, [Q1|Acc]);
get_rule_destinations([], _Index, Acc) ->  lists:flatten(Acc).

handle_query({get_notification_items, OrigRuleId}) ->
	Q = ?QLCQ([{N#notification.type, N#notification.target} ||
		N <- mnesia:table(notification),
		R <- mnesia:table(rule),
		N#notification.rule_id == R#rule.id,
		R#rule.orig_id == OrigRuleId
	]),
	?QLCE(Q);

handle_query({get_notification_queue_items, OrigRuleId}) ->
	Q = ?QLCQ([N#notification_queue.status ||
		N <- mnesia:table(notification_queue),
		N#notification_queue.rule_id == OrigRuleId
	]),
	?QLCE(Q);

handle_query(get_early_notification_queue_items) ->
	{_Me, S, _Mi} = erlang:now(),
	BeforeHalfHour = S - 1800,
	Q0 = ?QLCQ([N#notification_queue.rule_id ||
		N <- mnesia:table(notification_queue),
		N#notification_queue.is_shadow == true
	]),
	ShadowRuleIds = ?QLCE(Q0),
	lists:foreach(fun(I) -> mnesia:delete({notification_queue, I}) end, ShadowRuleIds),

	Q1 = ?QLCQ([N ||
		N <- mnesia:table(notification_queue),
		N#notification_queue.status == true
	]),
	InactiveNotificationQueueItems = ?QLCE(Q1),
	lists:foreach(fun(E) -> mnesia:write(E#notification_queue{is_shadow=true}) end, InactiveNotificationQueueItems),

	Q2 = ?QLCQ([N#notification_queue.rule_id ||
		N <- mnesia:table(notification_queue),
		N#notification_queue.date < BeforeHalfHour,
		N#notification_queue.status /= true
	]),
	?QLCE(Q2);

handle_query({update_notification_queue_item, RuleId, Status}) ->
	[I] = mnesia:wread({notification_queue, RuleId}),
	EventThreshold = I#notification_queue.event_threshold,
	{NewStatus, NewEventThreshold, Action} = case Status == EventThreshold of
					true -> {true, EventThreshold*EventThreshold, notify};
					false -> {Status, EventThreshold, ok}
				end,
	mnesia:write(I#notification_queue{status=NewStatus, event_threshold=NewEventThreshold, is_shadow=false}),
	Action;

handle_query({get_remote_rule_tables, FilterId, Addr, UserH}) ->
	AclQ = #aclq{src_addr=Addr, src_user_h=UserH},
	RemovableStorageRuleTable = get_rules(FilterId, AclQ#aclq{channel=removable}),
	PrinterRuleTable = get_rules(FilterId, AclQ#aclq{channel=printer}),
	InboundRuleTable = get_rules(FilterId, AclQ#aclq{channel=inbound}),
	DiscoveryRuleIds = get_rule_ids(FilterId, AclQ#aclq{channel=discovery}),
	ScreenshotRuleIds = get_rule_ids(FilterId, AclQ#aclq{channel=screenshot}),
	ApplicationNames = get_rule_destinations(ScreenshotRuleIds),
	Directories = get_rule_destinations(DiscoveryRuleIds),
	DiscoveryRuleTable = get_rule_table(FilterId, DiscoveryRuleIds),
	ScreenshotRuleTable = get_rule_table(FilterId, ScreenshotRuleIds),
	EncryptionRuleTable = get_rules(FilterId, AclQ#aclq{channel=encryption}),
	[
		{removable, none, RemovableStorageRuleTable},
		{printer, none, PrinterRuleTable},
		{discovery, Directories, DiscoveryRuleTable},
		{screenshot, ApplicationNames, ScreenshotRuleTable},
		{inbound, none, InboundRuleTable},
		{encryption, none, EncryptionRuleTable}
	];

handle_query({get_remote_rule_ids, FilterId, Addr, UserH}) ->
	AclQ = #aclq{src_addr=Addr, src_user_h=UserH},
	RemovableStorageRuleIds = get_rule_ids(FilterId, AclQ#aclq{channel=removable}),
	PrinterRuleIds = get_rule_ids(FilterId, AclQ#aclq{channel=printer}),
	DiscoveryRuleIds = get_rule_ids(FilterId, AclQ#aclq{channel=discovery}),
	ScreenshotRuleIds = get_rule_ids(FilterId, AclQ#aclq{channel=screenshot}),
	InboundRuleIds = get_rule_ids(FilterId, AclQ#aclq{channel=inbound}),
	EncryptionRuleIds = get_rule_ids(FilterId, AclQ#aclq{channel=encryption}),
	R = lists:flatten([RemovableStorageRuleIds, PrinterRuleIds, DiscoveryRuleIds, ScreenshotRuleIds, InboundRuleIds, EncryptionRuleIds]),
	lists:usort(R);

handle_query({get_rule_ids, FilterId, AclQ}) ->
	RuleIds = select_rule_ids_by_source(FilterId, AclQ),

	FinalRuleIds = case AclQ#aclq.channel of
		web -> filter_rule_ids_by_dest(RuleIds, AclQ);
		mail -> filter_rule_ids_by_dest(RuleIds, AclQ);
		_ -> RuleIds end,

	lists:usort(FinalRuleIds);

handle_query({get_rule_table, FilterId, RuleIDs}) ->
	Rules = lists:map(fun(I) ->
			[R] = mnesia:read(rule, I),
			{R#rule.id, R#rule.orig_id, R#rule.action}
		end, RuleIDs),
	Rules1 = lists:usort(fun({FId,_,_},{SId,_,_}) -> FId =< SId end, Rules),
	resolve_all(Rules1, FilterId);

handle_query(get_remote_user_rule_ids) ->
	Q1 = ?QLCQU([U#m_user.un_hash || 
		U <- mnesia:table(m_user)
		]),
	UniqUserHList = ?QLCE(Q1),

	PUSRL = lists:map(fun(UH) -> 
			Q2 = ?QLCQ([R#rule.id || 
				R <- mnesia:table(rule),
				U <- mnesia:table(m_user),
				R#rule.channel /= api,
				R#rule.channel /= web,
				R#rule.channel /= mail,
				U#m_user.rule_id == R#rule.id,
				U#m_user.un_hash == UH
				]),
			RL = ?QLCE(Q2),
			lists:usort(RL) end, UniqUserHList),

	lists:usort([[]|PUSRL]);

handle_query(get_remote_ipr_rule_ids) ->
	Q1 = ?QLCQU([I#ipr.ipbase || 
		I <- mnesia:table(ipr)
		]),
	UniqIPBaseList = ?QLCE(Q1),

	PIPRL = lists:map(fun(A) -> 
			Q2 = ?QLCQ([R#rule.id || 
				R <- mnesia:table(rule),
				I <- mnesia:table(ipr),
				R#rule.channel /= api,
				R#rule.channel /= web,
				R#rule.channel /= mail,
				I#ipr.rule_id == R#rule.id,
				I#ipr.ipbase /= {0,0,0,0}, 
				I#ipr.ipmask /= {0,0,0,0},
				ip_band(I#ipr.ipbase, I#ipr.ipmask) == ip_band(A, I#ipr.ipmask)
				]),
			RL = ?QLCE(Q2),
			lists:usort(RL) end, UniqIPBaseList),

	lists:usort([[]|PIPRL]);

handle_query(get_remote_default_rule_ids) ->
	Q0 = ?QLCQ([R#rule.id || 
		R <- mnesia:table(rule),
		I <- mnesia:table(ipr),
		R#rule.channel /= api,
		R#rule.channel /= web,
		R#rule.channel /= mail,
		I#ipr.rule_id == R#rule.id,
		I#ipr.ipbase == {0,0,0,0}, 
		I#ipr.ipmask == {0,0,0,0}
	]),
	RulesD = ?QLCE(Q0),
	lists:usort(RulesD);

handle_query({get_matchers, all}) ->
	Q = ?QLCQ([{M#match.id, M#match.func, M#match.func_params} ||
		M <- mnesia:table(match)
		]),
	?QLCE(Q);

handle_query({get_web_servers_by_rule_id, RuleId}) ->
	Q = ?QLCQ([W ||
		W <- mnesia:table(web_server),
		W#web_server.rule_id == RuleId
		]),
	?QLCE(Q);

%handle_query({get_web_servers_id_by_rule_id, RuleId}) ->
%	Q = ?QLCQ([W#web_server.id ||
%		W <- mnesia:table(web_server),
%		W#web_server.rule_id == RuleId
%		]),
%	?QLCE(Q);

handle_query({get_web_server, WebServerId}) ->
	mnesia:read(web_server, WebServerId);

handle_query({get_web_entry, EntryId}) ->
	mnesia:read(web_entry, EntryId);

handle_query({web_entry_list_links, EntryId}) ->
	mnesia:match_object(#web_entry{entry_id='_', parent_id=EntryId, is_html='_', size='_', maxage='_', expires='_', last_modified='_'});

handle_query({get_user_message, OrigRuleId, _Format}) ->
	Q = ?QLCQ([U#user_message.message ||
		U <- mnesia:table(user_message),
		U#user_message.rule_orig_id == OrigRuleId
		]),
	?QLCE(Q);

handle_query(get_remote_storages) ->
	Q = ?QLCQ([{R#remote_storage.id, R#remote_storage.rule_id, R#remote_storage.type, R#remote_storage.details} ||
		R <- mnesia:table(remote_storage)
		]),
	?QLCE(Q);

handle_query({get_remote_storage_by_id, Id}) ->
	Q = ?QLCQ([{R#remote_storage.type, R#remote_storage.details} ||
		R <- mnesia:table(remote_storage),
		R#remote_storage.id == Id
		]),
	?QLCE(Q);

handle_query({get_remote_storages_by_rule_id, RuleId}) ->
	Q = ?QLCQ([{R#remote_storage.id, R#remote_storage.rule_id, R#remote_storage.type, R#remote_storage.details} ||
		R <- mnesia:table(remote_storage),
		R#remote_storage.rule_id == RuleId
		]),
	?QLCE(Q);

%handle_query({get_remote_storages_id_by_rule_id, RuleId}) ->
%	Q = ?QLCQ([R#remote_storage.id ||
%		R <- mnesia:table(remote_storage),
%		R#remote_storage.rule_id ==  RuleId
%		]),
%	?QLCE(Q);

handle_query({get_rule_id_by_web_server_id, Id}) ->
	Q = ?QLCQ([R#web_server.rule_id ||
		R <- mnesia:table(web_server),
		R#web_server.id == Id
		]),
	?QLCE(Q);

handle_query({get_schedules_by_hour, Hour}) ->
	Q = ?QLCQ([{D#discovery_schedule.rule_id, D#discovery_schedule.details} ||
		D <- mnesia:table(discovery_schedule),
		D#discovery_schedule.schedule_hour == Hour
	]),
	?QLCE(Q);

handle_query({get_availabilty_by_rule_id, RuleId}) ->
	Q = ?QLCQ([D#discovery_schedule.available_intervals ||
		D <- mnesia:table(discovery_schedule),
		D#discovery_schedule.rule_id == RuleId
	]),
	?QLCE(Q);

handle_query({register_schedule, RuleId, GroupId}) ->
	Id = get_unique_id(endpoint_command),
	WS = #waiting_schedules{id=Id, rule_id=RuleId, group_id=GroupId},
	mnesia:dirty_write(WS);

handle_query({get_waiting_schedule_by_rule_id, RuleId}) ->
	Q = ?QLCQ([W ||
		W <- mnesia:table(waiting_schedules),
		W#waiting_schedules.rule_id == RuleId
	]),
	case ?QLCE(Q) of
		[] -> none;
		[WS] -> mnesia:dirty_delete({waiting_schedules, WS#waiting_schedules.id}),
			WS#waiting_schedules.group_id
	end;

handle_query({get_rule_id_by_orig_id, OrigRuleId}) ->
	Q = ?QLCQ([D#rule.id ||
		D <- mnesia:table(rule),
		D#rule.orig_id == OrigRuleId
	]),
	?QLCE(Q);

handle_query({get_orig_id_by_rule_id, RuleId}) ->
	Q = ?QLCQ([D#rule.orig_id ||
		D <- mnesia:table(rule),
		D#rule.id == RuleId
	]),
	?QLCE(Q);

handle_query({get_rule_channel, RuleId}) ->
	Q = ?QLCQ([D#rule.channel ||
		D <- mnesia:table(rule),
		D#rule.id == RuleId
	]),
	?QLCE(Q);

handle_query({get_discovery_rule_ids, Ip, Username}) ->
	AclQ = #aclq{src_addr=Ip, src_user_h=Username},
	get_rule_ids(get_dfid(), AclQ#aclq{channel=discovery});

handle_query({update_ep_schedules, EndpointId, RuleIds, TargetRuleId}) -> 
	case lists:member(TargetRuleId, RuleIds) of
		true ->
			[I] = mnesia:match_object(#discovery_targets{id='_', channel='_', rule_id=TargetRuleId, orig_id='_', group_id='_', targets='_'}),
			EpList = I#discovery_targets.targets,
			case lists:member(EndpointId, EpList) of %% TODO: use gb_sets instead of b-in list
				true -> ok;
				false -> mnesia:dirty_write(I#discovery_targets{channel=discovery, targets=[EndpointId|EpList]})
			end;
		false -> ok
	end;

%handle_query({update_rfs_and_web_schedules, RuleId, WebServers, Rfs}) ->
%	[I] = mnesia:match_object(#discovery_targets{id='_', rule_id=RuleId, orig_id='_', targets='_'}),
%	mnesia:dirty_write(I#discovery_targets{channel=remote_discovery, targets=[{web, WebServers},{rfs, Rfs}]});

handle_query({get_endpoints_by_rule_id, RuleId}) ->
	Q = ?QLCQ([D#discovery_targets.targets ||
		D <- mnesia:table(discovery_targets),
		D#discovery_targets.rule_id == RuleId
	]),
	?QLCE(Q);

handle_query(get_remote_document_databases) ->
	Q = ?QLCQ([{R#remote_storage_dd.document_id, R#remote_storage_dd.details, R#remote_storage_dd.rs_id, R#remote_storage_dd.exclude_files} ||
		R <- mnesia:table(remote_storage_dd)
	]),
	?QLCE(Q);

handle_query({get_dd_file_entry, FilePath}) ->
	Q = ?QLCQ([D ||
		D <-  mnesia:table(dd_file_entry),
		D#dd_file_entry.filepath == FilePath
	]),
	?QLCE(Q);

handle_query({remove_redundant_dd_file_entries, FilePath}) ->
	Q = ?QLCQ([D#dd_file_entry.id ||	
		D <- mnesia:table(dd_file_entry),
		D#dd_file_entry.filepath == FilePath
		]),
	Ids = ?QLCE(Q),
	lists:foreach(fun(I) -> mnesia:delete({dd_file_entry, I}) end, Ids);
	
handle_query({get_matchers, RuleIDs}) ->
	ML = lists:map(fun(RId) ->
		Q1 = ?QLCQ([F#ifeature.match_id ||
			F <- mnesia:table(ifeature),
			T <- mnesia:table(itype),
			T#itype.rule_id == RId,
			F#ifeature.itype_id == T#itype.id
			]),
		MIds = ?QLCE(Q1),
		lists:map(fun(MId) ->
			Q2 = ?QLCQ([{M#match.id, M#match.func, M#match.func_params} ||
				M <- mnesia:table(match),
				M#match.id == MId
				]),
			?QLCE(Q2) 
		end, MIds)
	end, RuleIDs),
	lists:usort(lists:flatten(ML));

handle_query({get_keywords, GroupId}) ->
	Q = ?QLCQ([ K#keyword.keyword ||
		K <- mnesia:table(keyword),
		K#keyword.group_id == GroupId
		]),
	?QLCE(Q);

handle_query({get_fid, SIpAddr}) ->
	Q = ?QLCQ([S#site_desc.filter_id ||
		S <- mnesia:table(site_desc),
		S#site_desc.ipaddr == SIpAddr
		]),
	?QLCE(Q);

handle_query({remove_site, FI}) ->

	Q1 = ?QLCQ([C#config.id ||	
		C <- mnesia:table(config),
		C#config.filter_id == FI
		]),
	CIs = ?QLCE(Q1),

	Q4 = ?QLCQ([S#site_desc.filter_id ||	
		S <- mnesia:table(site_desc),
		S#site_desc.filter_id == FI
		]),
	RQ4 = ?QLCE(Q4),

	Q7 = ?QLCQ([U#usb_device.id ||	
		U <- mnesia:table(usb_device),
		U#usb_device.filter_id == FI
		]),
	UDIs = ?QLCE(Q7),

	Q8 = ?QLCQ([M#mc_module.target ||	
		M <- mnesia:table(mc_module)
		]),
	MCTs = ?QLCE(Q8),

	Q9 = ?QLCQ([F#fs_entry.file_id ||	
		F <- mnesia:table(fs_entry)
		]),
	FSIs = ?QLCE(Q9),

	Q10 = ?QLCQ([R#remote_storage_dd.id ||	
		R <- mnesia:table(remote_storage_dd)
		]),
	RDDs = ?QLCE(Q10),

	case RQ4 of
		[] -> ok;
		[SDI] -> mnesia:delete({site_desc, SDI}) end,

	remove_filters([FI]),
	lists:foreach(fun(T) -> mnesia:delete({mc_module, T}) end, MCTs),
	lists:foreach(fun(Id) -> mnesia:delete({config, Id}) end, CIs),
	lists:foreach(fun(Id) -> mnesia:delete({fs_entry, Id}) end, FSIs),
	lists:foreach(fun(Id) -> mnesia:delete({usb_device, Id}) end, UDIs),
	lists:foreach(fun(Id) -> mnesia:delete({remote_storage_dd, Id}) end, RDDs);

handle_query({save_user_address, IpAddress, UserHash, UserName}) ->
	{MegaSecs, Secs, _MicroSecs} = erlang:now(),
        Born = 1000000*MegaSecs + Secs,
	U = #user_address{ipaddr=IpAddress, un_hash=UserHash, username=UserName, last_seen=Born},
	mnesia:dirty_write(U);

handle_query(remove_old_user_address) ->
	{MegaSecs, Secs, _MicroSecs} = erlang:now(),
        AgeLimit = 1000000*MegaSecs + Secs - 900,
	Q = ?QLCQ([U#user_address.ipaddr ||
		U <- mnesia:table(user_address),
		U#user_address.last_seen < AgeLimit
		]),
	UAIs = ?QLCE(Q),
	lists:foreach(fun(Id) -> mnesia:dirty_delete({user_address, Id}) end, UAIs);

handle_query({get_user_from_address, IpAddress}) ->
	mnesia:dirty_read(user_address, IpAddress);

handle_query({save_endpoint_command, EndpointId, Command, [{ruleId, RuleId}, {groupId, GroupId}]=Args}) ->
	{MegaSecs, Secs, _MicroSecs} = erlang:now(),
        Born = 1000000*MegaSecs + Secs,
        L = mnesia:match_object(#endpoint_command{id='_', endpoint_id=EndpointId, command=Command, date='_', args=Args}),
	EC = case L of
		[] -> 	Id = get_unique_id(endpoint_command),
			#endpoint_command{id=Id, endpoint_id=EndpointId, command=Command, date=Born, args=Args};
		[E] ->	E#endpoint_command{date=Born} end,
	Time = erlang:universaltime(),
	OprLog = #opr_log{time=Time, channel=discovery, rule_id=RuleId, message_key="command_created", group_id=GroupId},
	?DISCOVERY_OPR_LOG(OprLog),
	mnesia:dirty_write(EC),
	case Command of
		stop_discovery -> remove_endpoint_command(EndpointId, [start_discovery, pause_discovery, continue_discovery], Args);
		start_discovery -> remove_endpoint_command(EndpointId, [stop_discovery, pause_discovery, continue_discovery], Args);
		pause_discovery -> remove_endpoint_command(EndpointId, [start_discovery, stop_discovery, continue_discovery], Args);
		continue_discovery -> remove_endpoint_command(EndpointId, [start_discovery, pause_discovery, stop_discovery], Args);
		_Else -> ok end,
	ok;

handle_query({save_endpoint_command, EndpointId, Command}) ->
	{MegaSecs, Secs, _MicroSecs} = erlang:now(),
        Born = 1000000*MegaSecs + Secs,
        L = mnesia:match_object(#endpoint_command{id='_', endpoint_id=EndpointId, command=Command, date='_'}),
	EC = case L of
		[] -> 	Id = get_unique_id(endpoint_command),
			#endpoint_command{id=Id, endpoint_id=EndpointId, command=Command, date=Born};
		[E] ->	E#endpoint_command{date=Born} end,
	mnesia:dirty_write(EC),
	ok;

handle_query({get_endpoint_commands, EndpointId}) ->
        Items = mnesia:match_object(#endpoint_command{id='_', endpoint_id=EndpointId, command='_', date='_', args='_'}),
	Time=erlang:universaltime(),
	lists:foreach(fun(#endpoint_command{command=Command, id=Id, args=Args}) -> 
		case Command of
			{set_enc_key, _} -> ok;
			_ ->
				[{ruleId, RuleId}, {groupId, GroupId}] = Args,
				OprLog = #opr_log{time=Time, channel=discovery, rule_id=RuleId, message_key="command_sent", group_id=GroupId},
				?DISCOVERY_OPR_LOG(OprLog) end,
		mnesia:dirty_delete({endpoint_command, Id}) end, Items),
	Items;

handle_query(remove_old_endpoint_command) ->
	{MegaSecs, Secs, _MicroSecs} = erlang:now(),
        AgeLimit = 1000000*MegaSecs + Secs - 9000,
	Q = ?QLCQ([E#endpoint_command.id ||
		E <- mnesia:table(endpoint_command),
		E#endpoint_command.date < AgeLimit
		]),
	ECIs = ?QLCE(Q),
	lists:foreach(fun(Id) -> mnesia:dirty_delete({endpoint_command, Id}) end, ECIs);

handle_query({remove_endpoint_command, EndpointId, CommandList, Args}) ->
        lists:map(fun(Command) -> Items = mnesia:match_object(#endpoint_command{id='_', endpoint_id=EndpointId, command=Command, date='_', args=Args}),
				lists:foreach(fun(#endpoint_command{id=Id}) -> mnesia:dirty_delete({endpoint_command, Id}) end, Items)
				end, CommandList);

handle_query({del_web_entries_by_rule_id, _RuleId}) ->
	Q = ?QLCQ([F ||
		F <- mnesia:table(fs_entry)
		]),
	?QLCE(Q);

handle_query(Query) -> handle_query_common(Query).

-endif.

-ifdef(__MYDLP_ENDPOINT).

% TODO: should be refined for multi-site usage
handle_query({get_rule_table, Channel}) ->
	Q = ?QLCQ([ R#rule_table.table ||
		R <- mnesia:table(rule_table),
		R#rule_table.channel == Channel
		]),
	?QLCE(Q);

handle_query({get_rule_table, Channel, _RuleIndex}) ->
	Q = ?QLCQ([R#rule_table.table ||
		R <- mnesia:table(rule_table),
		R#rule_table.channel == Channel
		]),
	?QLCE(Q);

handle_query({get_rule_table_destination, Channel}) ->
	Q = ?QLCQ([ R#rule_table.destination ||
		R <- mnesia:table(rule_table),
		R#rule_table.channel == Channel
		]),
	?QLCE(Q);

handle_query({get_discovery_directory, _RuleId}) ->
	Q = ?QLCQ([{R#rule_table.destination, R#rule_table.table} ||
		R <- mnesia:table(rule_table),
		R#rule_table.channel == discovery
		]),
	?QLCE(Q);

handle_query({get_prtscr_app_name}) ->
	Q = ?QLCQ([R#rule_table.destination ||
		R <- mnesia:table(rule_table),
		R#rule_table.channel == screenshot
		]),
	?QLCE(Q);

handle_query(get_inbound_rule) ->
	Q = ?QLCQ([R#rule_table.table ||
		R <- mnesia:table(rule_table),
		R#rule_table.channel == inbound
		]),
	?QLCE(Q);

% TODO: should be refined for multi-site usage
handle_query({is_valid_usb_device_id, DeviceId}) ->
	Q = ?QLCQ([ U#usb_device.id ||
		U <- mnesia:table(usb_device),
		U#usb_device.device_id == DeviceId,
		U#usb_device.filter_id == mydlp_mnesia:get_dfid(),
		U#usb_device.action == pass
		]),
	?QLCE(Q);



handle_query(Query) -> handle_query_common(Query).

-endif.

handle_query_common({del_fs_entries_by_rule_id, _RuleId}) ->
	Q = ?QLCQ([F ||
		F <- mnesia:table(fs_entry)
		]),
	?QLCE(Q);

handle_query_common({is_mime_of_dfid, Mime, _DFIs}) ->
	Q = ?QLCQ([M#mime_type.data_format_id ||
		M <- mnesia:table(mime_type),
		M#mime_type.mime == Mime
		]),
	?QLCE(Q);

handle_query_common({is_hash_of_gid, _Hash, GroupId}) ->
	case mnesia:dirty_match_object(file_hash, #file_hash{group_id=GroupId, gb_set='_'}) of
		[H] -> H#file_hash.gb_set;
		[] -> gb_sets:empty() end;

handle_query_common({pdm_of_gid, _Fingerprints, GroupId}) ->
	case mnesia:dirty_match_object(file_fingerprint, #file_fingerprint{group_id=GroupId, gb_set='_'}) of
		[F] -> F#file_fingerprint.gb_set;
		[] -> gb_sets:empty() end;

handle_query_common({get_regexes, GroupId}) ->
	Q = ?QLCQ([ R#regex.compiled ||
		R <- mnesia:table(regex),
		R#regex.group_id == GroupId
		]),
	?QLCE(Q);

handle_query_common({get_mc_module, Target}) ->
	Q = ?QLCQ([M ||
		M <- mnesia:table(mc_module),
		M#mc_module.target == Target
		]),
	?QLCE(Q);

handle_query_common({get_config_value, KeyB}) ->
	Q = ?QLCQ([ C#config.value ||
		C <- mnesia:table(config),
		C#config.key == KeyB
		]),
	?QLCE(Q);

handle_query_common({get_fs_entry, FileId}) ->
	mnesia:read(fs_entry, FileId);

handle_query_common({del_fs_entry, FileId}) ->
	mnesia:delete({fs_entry, FileId});

handle_query_common({fs_entry_list_dir, EntryId}) ->
	mnesia:match_object(#fs_entry{file_id='_', entry_id='_', parent_id=EntryId, file_size='_', last_modified='_'});

handle_query_common({dump_tables, Tables}) ->
	L1 = [ {T, mnesia:all_keys(T)} || T <- Tables],
	L2 = [ [ mnesia:read({T,K}) || K <- Keys ]  || {T, Keys} <- L1 ],
	L3 = lists:append(L2),
	lists:append(L3);

handle_query_common({write, RecordList}) when is_list(RecordList) ->
	lists:foreach(fun(R) -> mnesia:write(R) end, RecordList);

handle_query_common({delete, Item}) ->
	mnesia:delete(Item);

handle_query_common(Query) -> throw({error,{unhandled_query,Query}}).

handle_async_query(flush, Context, Query) ->
	Return = evaluate_query(Context, Query),
	cache_clean(),
	Return;

handle_async_query(cache, Context, Query) ->
	case cache_lookup(Query) of
		{hit, {Query, R}} -> R;
		miss ->	R = evaluate_query(Context, Query),
			cache_insert(Query, R),
			R end;

handle_async_query(nocache, Context, Query) ->
	evaluate_query(Context, Query).

handle_call({async_query, CacheOption, Context, Query, Timeout}, From, State) ->
	Worker = self(),
	mydlp_api:mspawn(fun() ->
		Return= try	handle_async_query(CacheOption, Context, Query)
			catch  	Class:Error ->
				?ERROR_LOG("MNESIAQ: Error occured: Class: ["?S"]. Error: ["?S"].~n"
						"Stack trace: "?S"~n"
						"CacheOption: ["?S"]. Context: ["?S"]. Query: ["?S"]~n"
						"State: "?S"~n ",
					[Class, Error, erlang:get_stacktrace(), CacheOption, Context, Query, State]),
					{ierror, {Class, Error}} end,
		Worker ! {async_reply, Return, From}
	end, Timeout + 250),
	{noreply, State};

handle_call(truncate_all, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
		lists:foreach(fun(T) -> mnesia:clear_table(T) end, all_tab_names()),
		lists:foreach(fun(T) -> mydlp_mnesia:delete({unique_ids, T}) end, all_tab_names()),
		cache_clean(),
		Worker ! {async_reply, ok, From}
	end, 15000),
	{noreply, State};

handle_call(truncate_nondata, From, State) ->
	Worker = self(),
	?ASYNC(fun() ->
		lists:foreach(fun(T) -> mnesia:clear_table(T) end, nondata_tab_names()),
		lists:foreach(fun(T) -> mydlp_mnesia:delete({unique_ids, T}) end, nondata_tab_names()),
		cache_clean(),
		Worker ! {async_reply, ok, From}
	end, 15000),
	{noreply, State};

handle_call({new_authority, AuthorNode}, _From, State) ->
	MnesiaNodes = get_mnesia_nodes(),
	case lists:member(AuthorNode, MnesiaNodes) of
		false -> force_author(AuthorNode);
		true -> ok end,
	{reply, ok, State};

handle_call(ping, _From, State) ->
	{reply, pong, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(cleanup_now, State) ->
	?ASYNC(fun() ->
		cache_cleanup_handle(),
		call_timer()
	end, 15000),
	{noreply, State};

handle_info({mnesia_system_event,{mnesia_down, _Node}}, State) ->
	?ERROR_LOG("MNESIA Stopped unexpectedly.", []),
	mnesia_dir_cleanup(),
	{noreply, State};

handle_info({mnesia_system_event,{mnesia_fatal, Format, Args, _BinaryCore}}, State) ->
	?ERROR_LOG("MNESIA FATAL: " ++ io_lib:format(Format, Args), []),
	mnesia_dir_cleanup(),
	{noreply, State};

handle_info({mnesia_system_event,{mnesia_error, Format, Args}}, State) ->
	?ERROR_LOG("MNESIA ERROR: " ++ io_lib:format(Format, Args), []),
	mnesia_dir_cleanup(),
	{noreply, State};

handle_info({mnesia_system_event,{inconsistent_database, _Context, _Node}}, State) ->
	?ERROR_LOG("MNESIA Inconsistant database. Cleaning up and restarting.", []),
	mnesia_dir_cleanup(),
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

init([]) -> 
	schedule_boot_mnesia(),
	{ok, #state{}}.

handle_cast(mnesia_dir_cleanup, State) ->
	try 	(catch mnesia_stop()),
		{ok, MnesiaDir} = application_controller:get_env(mnesia, dir),
		{ok, MnesiaFiles} = file:list_dir(MnesiaDir),
		lists:foreach(fun(FN) ->
				AbsFileName = filename:absname(FN, MnesiaDir),
				file:delete(AbsFileName) end
			, MnesiaFiles),
		boot_mnesia()
	catch  	Class:Error ->
		?ERROR_LOG("MNESIA_CLEANUP: Error occured: Class: ["?S"]. Error: ["?S"].~n"
			"Stack trace: "?S"~n", [Class, Error, erlang:get_stacktrace()]) end,
	{noreply, State};

handle_cast(schedule_after_tables_ops, State) ->
	WaitTimeout = 30000,
	case catch wait_for_tables(WaitTimeout) of
		ok -> 	boot_after_tables_ops();
		Else -> mnesia_dir_cleanup(),
			?ERROR_LOG("MNESIA didn't started within "?S"ms. Scheduled cleanup. Err: "?S, [WaitTimeout, Else]) end,
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	mnesia_stop(),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%%

schedule_boot_mnesia() ->
	?ASYNC0(fun() -> boot_mnesia() end), ok.

boot_mnesia() ->
	mnesia_configure(),
	case is_mydlp_distributed() of
		true -> start_distributed();
		false -> start_single() end,
	ok.

boot_after_tables_ops() ->
	cache_start(),
	call_timer(15000),
	ok.

mnesia_configure() ->
        MnesiaDir = case os:getenv("MYDLP_MNESIA_DIR") of
                false -> ?CFG(mnesia_dir);
                Path -> Path end,
	application:load(mnesia),
	application_controller:set_env(mnesia, dir, MnesiaDir),
	ok.

get_mnesia_nodes() -> mnesia:system_info(db_nodes).

-ifdef(__MYDLP_NETWORK).

is_mydlp_distributed() -> mydlp_distributor:is_distributed().

-endif.

-ifdef(__MYDLP_ENDPOINT).

is_mydlp_distributed() -> false.

-endif.

start_single() ->
	start_mnesia_simple(),
	start_tables(false),
	ok.

start_distributed() ->
	IsAlreadyDistributed = is_mnesia_distributed(),
	case start_mnesia_distributed(IsAlreadyDistributed) of
		ok -> start_tables(true);
		{error, _} -> start_tables(false) end,
	MnesiaNodes = get_mnesia_nodes(),
	mydlp_distributor:bcast_cluster(MnesiaNodes),
	ok.

force_author(AuthorNode) -> 
	mnesia_stop(),
	case start_mnesia_with_author(AuthorNode) of
		ok -> start_tables(true);
		{error, _} -> start_tables(false) end,
	ok.

start_mnesia_distributed(true = _IsAlreadyDistributed) -> 
	start_mnesia_simple(),
	ok;

start_mnesia_distributed(false = _IsAlreadyDistributed) -> 
	case mydlp_distributor:find_authority() of
		none -> start_mnesia_simple(), {error, cannot_find_an_authority};
		AuthorNode -> start_mnesia_with_author(AuthorNode) end.

start_mnesia_simple() ->
	mnesia:create_schema([node()]), 
	mnesia_start().

start_mnesia_with_author(AuthorNode) ->
	mnesia:delete_schema([node()]),
	mnesia_start(),
	case mnesia:change_config(extra_db_nodes, [AuthorNode]) of
		{ok, []} -> {error, cannot_connect_to_any_other_node};
		{ok, [_|_]} -> mnesia:change_table_copy_type(schema, node(), disc_copies), ok;
		Else -> {error, Else} end.

is_mnesia_distributed() ->
	ThisNode = node(),
	case mnesia:system_info(db_nodes) of
		[ThisNode] -> false;
		DBNodeList -> lists:member(ThisNode, DBNodeList) end.

cache_start() ->
	case ets:info(query_cache) of
                undefined -> ets:new(query_cache, [
					public,
					named_table,
					{write_concurrency, true}
					%{read_concurrency, true}
				]);
                _Else -> ok end.

-ifdef(__MYDLP_NETWORK).

repopulate_mnesia() -> mydlp_mysql:repopulate_mnesia().

-endif.

-ifdef(__MYDLP_ENDPOINT).

repopulate_mnesia() -> schedule_post_start().

-endif.

wait_ready() -> 
	case gen_server:call(?MODULE, ping, 60000) of
		pong -> ok;
		Err -> ?ERROR_LOG("Mnesia didn't get ready in 60000ms. Err: "?S, [Err]) end.

post_start() ->
	wait_ready(),
	post_start0(mnesia),
	post_start0(mc),
	ok.

post_start(Mod) ->
	wait_ready(),
	post_start0(Mod).

post_start0(mnesia) ->
	consistency_chk(),
	mydlp_dynamic:load(),
	ok;
post_start0(mc) ->
	mydlp_mc:mc_load_mnesia(),
	ok.

schedule_post_start() ->
	?ASYNC0(fun() -> post_start() end), ok.

start_tables(IsDistributionInit) ->
	start_table(IsDistributionInit, {unique_ids, set}),
	StartResult =  start_tables(IsDistributionInit, ?TABLES),

	case StartResult of
		{ok, no_change} -> schedule_post_start();
		{ok, schema_changed} -> repopulate_mnesia() end,
	ok.

start_table(IsDistributionInit, RecordAtom) when is_atom(RecordAtom) ->
	start_table(IsDistributionInit, {RecordAtom, ordered_set});

start_table(IsDistributionInit, {RecordAtom, TableType}) ->
	start_table(IsDistributionInit, {RecordAtom, TableType, fun() -> ok end});

start_table(false = _IsDistributionInit, Table) -> init_table(Table);

start_table(true = _IsDistributionInit, {RecordAtom, _, _}) -> 
	LocalTables = mnesia:system_info(local_tables),
	case lists:member(RecordAtom, LocalTables) of
		false -> mnesia:add_table_copy(RecordAtom, node(), get_copy_media(RecordAtom));
		true -> ok end, ok.

init_table({RecordAtom, TableType, InitFun}) ->
	RecordAttributes = get_record_fields(RecordAtom),

	TabState = try
		case mnesia:table_info(RecordAtom, attributes) of
			RecordAttributes -> ok;
			_Else -> recreate  % it means that schema had been updated, should recreate tab.
		end 
	catch
		exit: _ -> create % it means that there is no tab in database as specified.
	end,

	case TabState of
		ok -> 		ok;
		create -> 	create_table(RecordAtom, RecordAttributes, TableType, InitFun), 
				changed;
		recreate -> 	mnesia:wait_for_tables([RecordAtom], 5000),
				delete_table(RecordAtom),
				create_table(RecordAtom, RecordAttributes, TableType, InitFun), 
				changed 
	end.

delete_table(RecordAtom) -> mnesia:delete_table(RecordAtom).

create_table(RecordAtom, RecordAttributes, TableType, InitFun) ->
	mnesia:create_table(RecordAtom,
			[{attributes, 
				RecordAttributes },
				{type, TableType},
				{get_copy_media(RecordAtom), [node()]}]),

	transaction(InitFun).

start_tables(IsDistributionInit, RecordAtomList) ->
	start_tables(IsDistributionInit, RecordAtomList, false).

start_tables(IsDistributionInit, [RecordAtom|RAList], false = _IsSchemaChanged) ->
	StartResult = start_table(IsDistributionInit, RecordAtom),
	IsSchemaChanged = case StartResult of
		ok -> false;
		changed -> true end,
	start_tables(IsDistributionInit, RAList, IsSchemaChanged);
start_tables(IsDistributionInit, [RecordAtom|RAList], true = _IsSchemaChanged) ->
	start_table(IsDistributionInit, RecordAtom),
	start_tables(IsDistributionInit, RAList, true);
start_tables(_IsDistributionInit, [], false = _IsSchemaChanged) -> {ok, no_change};
start_tables(_IsDistributionInit, [], true = _IsSchemaChanged) -> {ok, schema_changed}.

mnesia_stop() ->
	mnesia:unsubscribe(system),
	mnesia:stop().

mnesia_start() ->
	mnesia:start(),
	mnesia_subscribe_system(),
	gen_server:cast(?MODULE, schedule_after_tables_ops).

mnesia_subscribe_system() ->
	case mnesia:subscribe(system) of
		{error,{node_not_running, _}} ->
			?ERROR_LOG("Mnesia didn't started. Can not subscribe. Waiting 100ms to retry.",[]),
			timer:sleep(100),
			mnesia_subscribe_system();
		{error, _} = Error ->
			?ERROR_LOG("Can not subscribe to mnesia. Error: "?S, [Error]),
			Error;
		{ok, _} -> ok end.

%get_unique_id(TableName) ->
%	mnesia:dirty_update_counter(unique_ids, TableName, 1).

transaction(F) ->
	try {atomic, mnesia:activity(transaction, F)}
	catch
		_:Reason ->
			{aborted, Reason}
	end.

dirty(F) ->
	try {atomic, mnesia:activity(async_dirty, F)}
	catch
		_:Reason ->
			{aborted, Reason}
	end.

handle_mnesia_error(Reason) ->
        case is_no_exists_error(Reason) of
                true -> mnesia_dir_cleanup();
                false -> throw({error, {error_in_transaction, Reason}}) end.

is_no_exists_error({no_exists, _}) -> true;
is_no_exists_error({aborted, Reason}) -> is_no_exists_error(Reason);
is_no_exists_error(_Else) -> false.

evaluate_query(transaction, Query) ->
        F = fun() -> handle_query(Query) end,
        case transaction(F) of
                {atomic, _} = Result -> handle_result(Query, Result);
                {aborted, Reason} -> handle_mnesia_error(Reason), ok end;
evaluate_query(dirty, Query) ->
        F = fun() -> handle_query(Query) end,
        case dirty(F) of
                {atomic, _} = Result -> handle_result(Query, Result);
                {aborted, Reason} -> handle_mnesia_error(Reason), ok end.

cache_lookup(Query) ->
	case ets:lookup(query_cache, Query) of
		[] -> miss;
		[I|_] -> {hit, I} end.

cache_insert(Query, Return) ->
	ets:insert(query_cache, {Query, Return}),
	ok.

cache_clean() -> 
	cache_clean0(),
	MnesiaNodes = get_mnesia_nodes(),
	case is_mnesia_distributed() of
		true -> mydlp_distributor:flush_cache(MnesiaNodes);
		false -> ok end,
	ok.

cache_clean0() ->
	ets:delete_all_objects(query_cache),
	ok.

cache_cleanup_handle() ->
	MaxSize = ?CFG(query_cache_maximum_size),
	case ets:info(query_cache, memory) of
		I when I > MaxSize -> cache_clean();
		_Else -> ok end.
	
call_timer() -> call_timer(?CFG(query_cache_cleanup_interval)).
call_timer(Time) -> timer:send_after(Time, cleanup_now).

-ifdef(__MYDLP_NETWORK).

ip_band({A1,B1,C1,D1}, {A2,B2,C2,D2}) -> {A1 band A2, B1 band B2, C1 band C2, D1 band D2}.

%resolve_all(Rules) -> resolve_all(Rules, get_dfid()).

resolve_all(Rules, FilterId) ->
	Q = ?QLCQ([{F#filter.id, F#filter.default_action} || 
			F <- mnesia:table(filter),
			F#filter.id == FilterId
			]),
	case ?QLCE(Q) of
		[FilterKey] -> 	
			Rules1 = lists:usort(Rules),
			RRules = resolve_rules(Rules1),
			Req = get_mining_req(RRules),
			{Req, FilterKey, RRules};
		_Else -> {{false}, {0, pass}, []} end.

get_mining_req(Rules) -> predict_req_rules(#mining_req{}, Rules).

predict_req_rules(Req, []) -> Req;
predict_req_rules(Req, [{_RId, _RAction, ITypes}|Rules]) ->
	Req1 = predict_req_itypes(Req, ITypes),
	predict_req_rules(Req1, Rules).

predict_req_itypes(Req, []) -> Req;
predict_req_itypes(Req, [{_ITId, _DataFormats, _Distance, IFeatures}|ITypes]) ->
	Req1 = predict_req_ifeatures(Req, IFeatures),
	predict_req_itypes(Req1, ITypes).

predict_req_ifeatures(Req, []) -> Req;
predict_req_ifeatures(Req, [{_Threshold, {_Id, all, _FuncParams}}|IFeatures]) ->
	predict_req_ifeatures(Req, IFeatures);
predict_req_ifeatures(Req, [{_Threshold, {_Id, Func, _FuncParams}}|IFeatures]) ->
	Req1 = predict_req(Req, Func),
	predict_req_ifeatures(Req1, IFeatures).

predict_req(#mining_req{} = Req, Func) ->
	Req1 = predict_req1(Req, Func),
	Req2 = predict_req2(Req1, Func),
	Req3 = predict_req3(Req2, Func),
	Req3.

predict_req1(#mining_req{raw_text=undefined} = Req, Func) -> predict_req1(Req#mining_req{raw_text=false}, Func);
predict_req1(#mining_req{raw_text=false} = Req, Func) -> predict_req_te(Req, Func);
predict_req1(#mining_req{normal_text=undefined} = Req, Func) -> predict_req1(Req#mining_req{normal_text=false}, Func);
predict_req1(#mining_req{normal_text=false} = Req, Func) -> predict_req_te(Req, Func);
predict_req1(#mining_req{} = Req, _Func) -> Req.

predict_req2(#mining_req{mc_kw=undefined} = Req, Func) -> predict_req2(Req#mining_req{mc_kw=false}, Func);
predict_req2(#mining_req{mc_kw=false} = Req, Func) -> predict_req_mc_kw(Req, Func);
predict_req2(#mining_req{} = Req, _Func) -> Req.

predict_req3(#mining_req{mc_pd=undefined} = Req, Func) -> predict_req3(Req#mining_req{mc_pd=false}, Func);
predict_req3(#mining_req{mc_pd=false} = Req, Func) -> predict_req_mc_pd(Req, Func);
predict_req3(#mining_req{} = Req, _Func) -> Req.


predict_req_te(#mining_req{} = Req, Func) ->
	case get_matcher_req(Func) of
		raw -> Req;
		analyzed -> Req#mining_req{raw_text=true};
		text -> Req#mining_req{raw_text=true};
		normalized -> Req#mining_req{raw_text=true, normal_text=true} end.

predict_req_mc_pd(#mining_req{} = Req, Func) ->
	{_, {distance, _}, {pd, IsPD}, {kw, _}} = apply(mydlp_matchers, Func, []),
	Req#mining_req{mc_pd=IsPD}.

predict_req_mc_kw(#mining_req{} = Req, Func) ->
	{_, {distance, _}, {pd, _}, {kw, IsKW}} = apply(mydlp_matchers, Func, []),
	Req#mining_req{mc_kw=IsKW}.

get_matcher_req(Func) -> 
	{MReq, {distance, _}, {pd, _}, {kw, _}} = apply(mydlp_matchers, Func, []), MReq.

resolve_rules(PS) -> resolve_rules(PS, []).
resolve_rules([{RId, ROrigId,RAction}|PS], Rules) -> 
	resolve_rules(PS, [{ROrigId, RAction, find_itypes(RId)}| Rules]);
resolve_rules([], Rules) -> lists:reverse(Rules).

find_itypes(RuleId) ->
	QM = ?QLCQ([{T#itype.orig_id, T#itype.data_formats, T#itype.distance, find_ifeatures(T#itype.id)} ||
			T <- mnesia:table(itype),
			T#itype.rule_id == RuleId
		]),
	?QLCE(QM).

find_ifeatures(ITypeId) ->
	QM = ?QLCQ([{F#ifeature.threshold, find_func(F#ifeature.match_id)} ||
			F <- mnesia:table(ifeature),
			F#ifeature.itype_id == ITypeId
		]),
	?QLCE(QM).

find_func(MatchId) ->
	QM = ?QLCQ([{M#match.id, M#match.func, M#match.func_params} ||
			M <- mnesia:table(match),
			M#match.id == MatchId
		]),
	case ?QLCE(QM) of
		[FuncTuple] -> FuncTuple;
		_Else -> throw({ierror, cannot_be_more_than_one_matcher}) end.

-endif.

consistency_chk() -> 
	compile_regex().

compile_regex() ->
	mnesia:wait_for_tables([regex], 5000),
	RegexC = fun() ->
		Q = ?QLCQ([R || R <- mnesia:table(regex),
			R#regex.plain /= undefined,
			R#regex.compiled == undefined,
			R#regex.error == undefined
			]),
		[mnesia:write(R) || R <- compile_regex(?QLCE(Q))]
	end,
	transaction(RegexC).

compile_regex(Regexs) -> compile_regex(Regexs, []).

compile_regex([R|RS], Ret) -> 
	R1 = case re:compile(R#regex.plain, [unicode, caseless]) of
		{ok, C} -> R#regex{compiled=C};
		{error, Err} -> R#regex{error=Err}
	end,
	compile_regex(RS, [R1|Ret]);
compile_regex([], Ret) -> lists:reverse(Ret).

get_unique_id(TableName) -> mnesia:dirty_update_counter(unique_ids, TableName, 1).

% aqc(Query) -> aqc(Query, nocache).

aqc(Query, CacheOption) -> aqc(Query, CacheOption, transaction).

aqc(Query, CacheOption, Context) -> aqc(Query, CacheOption, Context, 15000).

aqc(Query, CacheOption, Context, Timeout) -> async_query_call(Query, CacheOption, Context, Timeout).

async_query_call(Query, CacheOption, Context, Timeout) -> 
	case gen_server:call(?MODULE, {async_query, CacheOption, Context, Query, Timeout}, Timeout) of
		{ierror, {Class, Error}} -> mydlp_api:exception(Class, Error);
		Else -> Else end.

all_tab_names() -> tab_names1(?TABLES, []).

nondata_tab_names() -> tab_names1(?NONDATA_TABLES, []).

%tab_names() -> tab_names1(?TABLES, [unique_ids]).

tab_names1([{Tab,_,_}|Tabs], Returns) -> tab_names1(Tabs, [Tab|Returns]);
tab_names1([{Tab,_}|Tabs], Returns) -> tab_names1(Tabs, [Tab|Returns]);
tab_names1([Tab|Tabs], Returns) when is_atom(Tab) ->  tab_names1(Tabs, [Tab|Returns]);
tab_names1([], Returns) -> lists:reverse(Returns).

pdm_hit_count([Fingerprint|Rest], Set, Acc) ->
	case gb_sets:is_member(Fingerprint, Set) of
		false -> pdm_hit_count(Rest, Set, Acc);
		true -> pdm_hit_count(Rest, Set, Acc + 1) end;
pdm_hit_count([], _Set, Acc) -> Acc.

remove_reduntant_fs_entries([#fs_entry{file_id={_, RuleId1}}=Item|Rest], RuleId) ->
	case RuleId1 of	
		RuleId -> mnesia:dirty_delete_object(Item);
		_ -> ok
	end,
	remove_reduntant_fs_entries(Rest, RuleId);
%remove_reduntant_fs_entries([_|Rest], RuleId) ->
%	mnesia:dirty_delete_object(Rest, RuleId);
remove_reduntant_fs_entries([], _RuleId) -> ok.


-ifdef(__MYDLP_NETWORK).

%% File Group functions
remove_reduntant_web_entries([#web_entry{entry_id={_, _, RuleId1}}=Item|Rest], RuleId) ->
	case RuleId1 of	
		RuleId -> mnesia:dirty_delete_object(Item);
		_ -> ok
	end,
	remove_reduntant_web_entries(Rest, RuleId);
%remove_reduntant_fs_entries([_|Rest], RuleId) ->
%	mnesia:dirty_delete_object(Rest, RuleId);
remove_reduntant_web_entries([], _RuleId) -> ok.

remove_filters(FIs) -> lists:foreach(fun(Id) -> remove_filter(Id) end, FIs), ok.

remove_filter(FI) ->
	Q = ?QLCQ([{R#rule.id, R#rule.orig_id} ||	
		R <- mnesia:table(rule),
		R#rule.filter_id == FI
		]),
	AllRIs = ?QLCE(Q),
	RIs = [X || {X, _Y} <- AllRIs],
	OrigRIs = [Y || {_X, Y} <- AllRIs],
	remove_notification_queue_items(OrigRIs),
	remove_user_messages(OrigRIs),
	remove_rules(RIs),
	mnesia:delete({filter, FI}).

remove_user_messages(OrigRIs) -> lists:foreach(fun(Id) -> remove_user_message(Id) end, OrigRIs), ok.

remove_user_message(OrigRI) ->
	Q = ?QLCQ([N#user_message.rule_orig_id ||
		N <- mnesia:table(user_message),
		N#user_message.rule_orig_id == OrigRI
	]),
	UI = ?QLCE(Q),
	lists:foreach(fun(I) -> mnesia:delete({user_message, I}) end, UI).

remove_notification_queue_items(OrigRIs) -> lists:foreach(fun(Id) -> remove_notification_queue_item(Id) end, OrigRIs), ok.

remove_notification_queue_item(RI) ->
	Q = ?QLCQ([N#notification_queue.rule_id ||
		N <- mnesia:table(notification_queue),
		N#notification_queue.rule_id == RI
	]),
	NI = ?QLCE(Q),
	lists:foreach(fun(I) -> mydlp_incident:notify_users_now(I),
				mnesia:delete({notification_queue, I}) end, NI).

remove_rules(RIs) -> lists:foreach(fun(Id) -> remove_rule(Id) end, RIs), ok.

remove_rule(RI) ->
	Q1 = ?QLCQ([T#itype.data_formats ||	
		T <- mnesia:table(itype),
		T#itype.data_formats /= all,
		T#itype.rule_id == RI
		]),
	DFIs = lists:flatten(?QLCE(Q1)),

	Q2 = ?QLCQ([T#itype.id ||	
		T <- mnesia:table(itype),
		T#itype.rule_id == RI
		]),
	ITIs = ?QLCE(Q2),

	Q3 = ?QLCQ([I#ipr.id ||	
		I <- mnesia:table(ipr),
		I#ipr.rule_id == RI
		]),
	IIs = ?QLCE(Q3),

	Q4 = ?QLCQ([U#m_user.id ||	
		U <- mnesia:table(m_user),
		U#m_user.rule_id == RI
		]),
	UIs = ?QLCE(Q4),

	Q5 = ?QLCQ([I#dest.id ||	
		I <- mnesia:table(dest),
		I#dest.rule_id == RI
		]),
	DIs = ?QLCE(Q5),

	Q6 = ?QLCQ([N#notification.id ||
		N <- mnesia:table(notification),
		N#notification.rule_id == RI
		]),
	NIs = ?QLCE(Q6),

	Q7 = ?QLCQ([S#source_domain.id ||	
		S <- mnesia:table(source_domain),
		S#source_domain.rule_id == RI
		]),
	SDs = ?QLCE(Q7),

	Q8 = ?QLCQ([RS#remote_storage.id ||	
		RS <- mnesia:table(remote_storage),
		RS#remote_storage.rule_id == RI
		]),
	RSs = ?QLCE(Q8),

	Q9 = ?QLCQ([RS#discovery_schedule.id ||	% Removing these two table may cause inconsistency in policy compilation
		RS <- mnesia:table(discovery_schedule),
		RS#discovery_schedule.rule_id == RI
		]),
	DSs = ?QLCE(Q9),

	Q10 = ?QLCQ([DES#discovery_targets.id ||	
		DES <- mnesia:table(discovery_targets),
		DES#discovery_targets.rule_id == RI
		]),
	DESs = ?QLCE(Q10),

	Q11 = ?QLCQ([WS#waiting_schedules.id ||	
		WS <- mnesia:table(waiting_schedules),
		WS#waiting_schedules.rule_id == RI
		]),
	WSCs = ?QLCE(Q11),

	Q12 = ?QLCQ([RS#web_server.id ||	
		RS <- mnesia:table(web_server),
		RS#web_server.rule_id == RI
		]),
	WSs = ?QLCE(Q12),

	lists:foreach(fun(Id) -> mnesia:delete({ipr, Id}) end, IIs),
	lists:foreach(fun(Id) -> mnesia:delete({m_user, Id}) end, UIs),
	lists:foreach(fun(Id) -> mnesia:delete({dest, Id}) end, DIs),
	lists:foreach(fun(Id) -> mnesia:delete({notification, Id}) end, NIs),
	lists:foreach(fun(Id) -> mnesia:delete({source_domain, Id}) end, SDs),
	lists:foreach(fun(Id) -> mnesia:delete({remote_storage, Id}) end, RSs),
	lists:foreach(fun(Id) -> mnesia:delete({discovery_schedule, Id}) end, DSs),
	lists:foreach(fun(Id) -> mnesia:delete({discovery_targets, Id}) end, DESs),
	lists:foreach(fun(Id) -> mnesia:delete({waiting_schedules, Id}) end, WSCs),
	lists:foreach(fun(Id) -> mnesia:delete({web_server, Id}) end, WSs),

	remove_data_formats(DFIs),
	remove_itypes(ITIs),
	mnesia:delete({rule, RI}).

remove_data_formats(DFIs) -> lists:foreach(fun(Id) -> remove_data_format(Id) end, DFIs), ok.

remove_data_format(DFI) -> 
	Q1 = ?QLCQ([MT#mime_type.id ||	
		MT <- mnesia:table(mime_type),
		MT#mime_type.data_format_id == DFI
		]),
	MTIs = ?QLCE(Q1),

	remove_mime_types(MTIs).

remove_mime_types(MTIs) -> lists:foreach(fun(Id) -> remove_mime_type(Id) end, MTIs), ok.

remove_mime_type(MTI) -> mnesia:delete({mime_type, MTI}).

remove_itypes(ITIs) -> lists:foreach(fun(Id) -> remove_itype(Id) end, ITIs), ok.

remove_itype(ITI) ->
	Q = ?QLCQ([F#ifeature.id ||	
		F <- mnesia:table(ifeature),
		F#ifeature.itype_id == ITI
		]),
	IFIs = ?QLCE(Q),
	remove_ifeatures(IFIs),
	mnesia:delete({itype, ITI}).

remove_ifeatures(IFIs) -> lists:foreach(fun(Id) -> remove_ifeature(Id) end, IFIs), ok.

remove_ifeature(IFI) ->
	Q1 = ?QLCQ([M#match.func_params ||	
		M <- mnesia:table(match),
		F <- mnesia:table(ifeature),
		F#ifeature.id == IFI,
		M#match.id == F#ifeature.match_id,
		M#match.func == md5_match
		]),
	FHGIs = ?QLCE(Q1),
	remove_filehashes(lists:usort(lists:flatten(FHGIs))),

	Q2 = ?QLCQ([M#match.func_params ||	
		M <- mnesia:table(match),
		F <- mnesia:table(ifeature),
		F#ifeature.id == IFI,
		M#match.id == F#ifeature.match_id,
		M#match.func == pdm_match
		]),
	FFGIs = ?QLCE(Q2),
	remove_filefingerprints(lists:usort(lists:flatten(FFGIs))),

	Q3 = ?QLCQ([M#match.func_params ||	
		M <- mnesia:table(match),
		F <- mnesia:table(ifeature),
		F#ifeature.id == IFI,
		M#match.id == F#ifeature.match_id,
		M#match.func == keyword_match
		]),
	KGIs = ?QLCE(Q3),
	remove_keywords(lists:usort(lists:flatten(KGIs))),

	Q4 = ?QLCQ([M#match.func_params ||	
		M <- mnesia:table(match),
		F <- mnesia:table(ifeature),
		F#ifeature.id == IFI,
		M#match.id == F#ifeature.match_id,
		M#match.func == regex_match
		]),
	RGIs = ?QLCE(Q4),
	remove_regexes(lists:usort(lists:flatten(RGIs))),

	Q0 = ?QLCQ([M#match.id ||	
		M <- mnesia:table(match),
		F <- mnesia:table(ifeature),
		F#ifeature.id == IFI,
		M#match.id == F#ifeature.match_id
		]),
	MIs = ?QLCE(Q0),
	remove_matches(MIs),
	mnesia:delete({ifeature, IFI}).

remove_matches(MIs) -> lists:foreach(fun(Id) -> remove_match(Id) end, MIs), ok.

remove_match(MI) -> mnesia:delete({match, MI}).

remove_filehashes(FHGIs) -> lists:foreach(fun(GroupId) -> remove_filehashes1(GroupId) end, FHGIs), ok.

remove_filehashes1(GroupId) ->
	FileHashes = mnesia:match_object(#file_hash{group_id=GroupId, gb_set='_'}),
	lists:foreach(fun(#file_hash{group_id=Id}) -> mnesia:delete({file_hash, Id}) end, FileHashes),
	ok.

remove_filefingerprints(FHGIs) -> lists:foreach(fun(GroupId) -> remove_filefingerprints1(GroupId) end, FHGIs), ok.

remove_filefingerprints1(GroupId) ->
	Fingerprints = mnesia:match_object(#file_fingerprint{group_id=GroupId, gb_set='_'}),
	lists:foreach(fun(#file_fingerprint{group_id=Id}) -> mnesia:delete({file_fingerprint, Id}) end, Fingerprints),
	ok.

remove_keywords(KGIs) -> lists:foreach(fun(GroupId) -> remove_keyword(GroupId) end, KGIs), ok.

remove_keyword({group_id, GroupId}) ->
	Keywords = mnesia:match_object(#keyword{id='_', group_id=GroupId, keyword='_'}),
	lists:foreach(fun(#keyword{id=Id}) -> mnesia:delete({keyword, Id}) end, Keywords), ok;
remove_keyword(_) -> ok.

remove_regexes(RGIs) -> lists:foreach(fun(GroupId) -> remove_regex(GroupId) end, RGIs), ok.

remove_regex(GroupId) ->
	Regexes = mnesia:match_object(#regex{id='_', group_id=GroupId, plain='_', compiled='_', error='_'}),
	lists:foreach(fun(#regex{id=Id}) -> mnesia:delete({regex, Id}) end, Regexes), ok.

filter_for_day([{R, daily}|Rows], Acc) -> 
	filter_for_day(Rows, [R|Acc]);
filter_for_day([{R, {weekly, DayList}}|Rows], Acc) ->
	DayAsInt = calendar:day_of_the_week(date()),
	case lists:nth(DayAsInt, DayList) of
		1 -> filter_for_day(Rows, [R|Acc]);
		_ -> filter_for_day(Rows, Acc)
	end;
filter_for_day([], []) -> none;
filter_for_day([], Acc) -> Acc.

is_available([DayIntervals]) ->
	{D, {H, _, _}} = erlang:localtime(),
	DayAsInt = calendar:day_of_the_week(D),
	DayInterval = lists:nth(DayAsInt, DayIntervals),
	case lists:nth(H+1, DayInterval) of
		1 -> true;
		_ -> false
	end.
-endif.

-ifdef(__MYDLP_ENDPOINT).

get_rule_with_id([{RuleId, _, _}=Rule|Rest], TargetRuleId) ->
	case RuleId of
		TargetRuleId -> Rule;
		_ -> get_rule_with_id(Rest, TargetRuleId)
	end;
get_rule_with_id([], TargetRuleId) ->
	?ERROR_LOG("Unknown rule id for discovery channel: ["?S"]", [TargetRuleId]).

get_directory_by_rule_id(FilePaths, [{RuleId, _, _}|Rest], TargetRuleId, Index) ->
	case RuleId of
		TargetRuleId -> get_file_paths(FilePaths, Index);
		_ -> get_directory_by_rule_id(FilePaths, Rest, TargetRuleId, Index+1)
	end;
get_directory_by_rule_id(_FilePath, [], TargetRuleId, _Index) ->
	?ERROR_LOG("Unknown rule id for discovery channel: ["?S"]", [TargetRuleId]).

get_file_paths(FilePaths, TargetRuleIndex) -> get_file_paths(FilePaths, TargetRuleIndex, []).
get_file_paths([{Path, Index}|Rest], TargetIndex, Acc) ->
	case Index of
		TargetIndex -> get_file_paths(Rest, TargetIndex, [Path|Acc]);
		_ -> get_file_paths(Rest, TargetIndex, Acc)
	end;
get_file_paths([], _TargetIndex, Acc) -> Acc.
-endif.
