%%
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

-record(dest, {
	id,
	rule_id,
	destination
}).

-record(m_user, {
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
	ipaddr,
	un_hash,
	username,
	last_seen
}).

-endif.


-ifdef(__MYDLP_ENDPOINT).

-record(rule_table, {
	channel,
	table=[]
}).

-record(fs_entry, {
	file_path,
	entry_id,
	parent_id,
	file_size,
	last_modified
}).

-endif.

-record(config, {
	id,
	filter_id,
	key,
	value
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
-record(file_hash, {
	id,
	file_id,
	group_id,
	hash
}).

%% refine match_object statements after any change
-record(file_fingerprint, {
	id,
	file_id,
	group_id,
	fingerprint
}).

-record(mime_type, {
	id,
	data_format_id,
	mime
}).

-endif.

