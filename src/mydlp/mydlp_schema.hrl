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

-record(filter, {
	id,
	name
}).

-record(rule, {
	id,
	filter_id,
	action
}).

-record(ipr, {
	id,
	parent,
	ipbase,
	ipmask
}).

-record(match, {
	id,
	parent,
	func,
	func_params=[]
}).

-record(match_group, {
	id,
	parent,
	name
}).

-record(file_hash, {
	id,
	file_id,
	group_id,
	md5
}).

-record(regex, {
	id,
	group_id,
	plain,
	compiled,
	error
}).

-record(sentence_hash, {
	id,
	file_id,
	group_id,
	phash2
}).

-record(mime_type, {
	id,
	group_id,
	extension,
	mime
}).

-endif.
