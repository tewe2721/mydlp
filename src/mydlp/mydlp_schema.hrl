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

-record(m_user, {
	id,
	rule_id,
	username
}).

-record(itype, {
	id,
	rule_id,
	orig_id,
	data_formats=[],
	threshold
}).

-record(ifeature, {
	id,
	itype_id,
	weight
}).

-record(match, {
	id,
	orig_id,
	ifeature_id,
	func,
	func_params=[]
}).

-record(site_desc, {
	ipaddr,
	filter_id
}).

-endif.

-ifdef(__MYDLP_ENDPOINT).

-record(rule_table, {
	id,
	table=[]
}).

-endif.

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

% TODO: refine
-record(file_hash, {
	id,
	file_id,
	md5
}).

% TODO: refine
-record(sentence_hash, {
	id,
	file_id,
	phash2
}).

% TODO: refine
-record(file_group, {
	id,
	file_id,
	group_id
}).

-record(mime_type, {
	id,
	data_format_id,
	mime
}).

% TODO: refine
-record(bayes_item_count, {
	type,
	count
}).

% TODO: refine
-record(bayes_positive, {
	word_hash,
	count
}).

% TODO: refine
-record(bayes_negative, {
	word_hash,
	count
}).

-endif.

