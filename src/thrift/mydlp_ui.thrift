/**
 *     Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@medra.com.tr>
 * 
 ***************************************************************************
 *     This file is part of MyDLP.
 * 
 *     MyDLP is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 * 
 *     MyDLP is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 * 
 *     You should have received a copy of the GNU General Public License
 *     along with MyDLP.  If not, see <http://www.gnu.org/licenses/>.
 ***************************************************************************/

enum ptype {
	rule = 0,
	mgroup = 1
}

struct parent {
	1: ptype type,
	2: i32 pid
}

struct filter {
	1: i32 id,
	2: string name,
	3: bool is_active
}

enum raction {
	rpass = 0,
	block = 1
}

struct rule {
	1: i32 id,
	2: i32 filter_id,
	3: raction action
	4: bool is_active
}

struct ipaddr {
	1: byte ip1,
	2: byte ip2,
	3: byte ip3,
	4: byte ip4
}

struct ipr {
	1: i32 id,
	2: parent parent,
	3: ipaddr ipbase,
	4: ipaddr ipmask
}

struct match {
	1: i32 id,
	2: parent parent,
	3: string func,
	4: list<string> func_params=[]
}

struct match_group {
	1: i32 id,
	2: parent parent,
	3: string name
}

service Mydlp_ui {

	list<filter> list_filter(),
	list<rule> list_rule_by_filter_id(1: i32 filter_id),
	list<match_group> list_match_group_by_parent(1: parent parent),
	list<match> list_match_by_parent(1: parent parent),
	list<ipr> list_ipr_by_parent(1: parent parent),

	filter get_filter_by_id(1: i32 id),
	rule get_rule_by_id(1: i32 id),
	ipr get_ipr_by_id(1: i32 id),
	match get_match_by_id(1: i32 id),
	match_group get_match_group_by_id(1: i32 id),

	void delete_filter_by_id(1: i32 id),
	void delete_rule_by_id(1: i32 id),
	void delete_ipr_by_id(1: i32 id),
	void delete_match_by_id(1: i32 id),
	void delete_match_group_by_id(1: i32 id),
	
	void add_filter(1: filter filter)
	void add_rule(1: rule rule)
	void add_ipr(1: ipr ipr)
	void add_match(1: match match)
	void add_match_group(1: match_group match_group)
	
}
