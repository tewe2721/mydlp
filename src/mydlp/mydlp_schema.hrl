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

-ifndef(_MYDLP_SCHEMA_HRL).
-define(_MYDLP_SCHEMA_HRL, true).

-record(unique_ids, {type, id}).

-ifdef(__MYDLP_NETWORK).

-record(filter, {
	id,
	default_action
}).

-record(rule, {
	id,
	orig_id,
	filter_id,
	channel,
	action
}).

-record(ipr, {
	id,
	rule_id,
	ipbase,
	ipmask
}).

-record(dest, { % used for destination domain recors too
	id,
	rule_id,
	destination
}).

-record(notification, {
	id,
	rule_id,
	type,
	target
}).

-record(notification_queue, {
	rule_id,
	date,
	status, % if notification sent, status=true; else status=#of notificaitons 
	event_threshold=10, % threshold value for waiting notification
	is_shadow=false % true means rule does not occur for specified time interval.
}).

-record(rule_details, {
	id,
	rule_id,
	rule_orig_id,
	hr_name,
	channel,
	action
}).

-record(source_domain, {
	id,
	rule_id,
	domain_name
}).

-record(remote_storage, {
	id,
	orig_id,
	rule_id,
	type,
	details
}).

-record(remote_storage_dd, {
	id,
	document_id,
	rs_id, %Remote Storage Id
	details,
	exclude_files
}).

-record(discovery_schedule, {
	id,
	rule_id,
	rule_orig_id,
	schedule_hour, 
	details,
	available_intervals
}).

-record(waiting_schedules, {
	id,
	rule_id,
	group_id
}).

-record(discovery_targets, {
	id,
	channel, 
	rule_id,
	orig_id,
	group_id,
	targets=[]
}).

-record(dd_file_entry, {
	id,
	filepath,
	file_entry_id,
	dd_id_list=[]
}).

-record(m_user, {
	id,
	rule_id,
	un_hash
}).

-record(m_endpoint_id, {
	id,
	rule_id,
	endpoint_id
}).

-record(m_hostname, {
	id,
	rule_id,
	hostname
}).

-record(destination_user, {
	id,
	rule_id,
	un_hash
}).

-record(itype, {
	id,
	rule_id,
	orig_id,
	distance,
	data_formats=[]
}).

-record(ifeature, {
	id,
	itype_id,
	match_id,
	threshold
}).

-record(match, {
	id,
	func,
	func_params=[]
}).

-record(site_desc, {
	ipaddr,
	filter_id
}).

-record(user_address, {
	endpoint_id,
	ipaddr,
	un_hash,
	username,
	hostname,
	last_seen
}).

-record(user_message, {
	rule_orig_id,
	message
}).

-record(endpoint_command, {
	id,
	endpoint_id,
	command,
	args=[],
	date
}).

-record(web_server, {
	id,
	rule_id,
	proto,
	address,
	port,
	dig_depth,
	start_path
}).

-record(web_entry, {
	entry_id,
	rule_id,
	parent_id,
	is_html,
	size,
	maxage,
	expires,
	last_modified
}).

-record(license_email, {
                mail_address,
		register_time
        }).

-record(license_endpoint, {
                ep_key,
		register_time
        }).

-record(license_remote_storage, {
                rs_key,
		register_time
        }).

-endif.


-ifdef(__MYDLP_ENDPOINT).

-record(rule_table, {
	channel,
	destination,% target parent file path for discovery. "none" (atom) for endpoint and printer.
	table=[]
}).

-endif.

-record(config, {
	id,
	filter_id,
	key,
	value
}).

-record(fs_entry, {
	file_id,
	entry_id,
	parent_id,
	file_size,
	last_modified
}).

-record(discovery_status, {
	id,
	rule_id,
	group_id,
	status
}).

-record(usb_device, {
	id,
	filter_id,
	device_id,
	action
}).

-record(regex, {
	id,
	group_id,
	plain,
	compiled,
	error
}).

-record(keyword, {
	id,
	group_id,
	keyword
}).

-record(mc_module, {
	target,
	modules
}).

%% refine match_object statements after any change
%% TODO: problems in multisite use
-record(file_hash, {
	group_id,
	gb_set
}).

%% TODO: problems in multisite use
%% refine match_object statements after any change
-record(file_fingerprint, {
	group_id,
	gb_set
}).

-record(mime_type, {
	id,
	data_format_id,
	mime
}).

-endif.

