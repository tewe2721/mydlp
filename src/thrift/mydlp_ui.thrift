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

service Mydlp_ui {

	// Web UI calls
	oneway void trainConfidential(1: binary Data, 2: i32 Fileid)
	oneway void setConfidentialGroup(1: i32 Fileid, 2: i32 Groupid)
	oneway void trainPublic(1: binary Data, 2: i32 Fileid)

	oneway void removeFile(1: i32 Fileid)
	oneway void removeGroup(1: i32 Groupid)

	oneway void removeFileFromGroup(1: i32 Fileid, 2: i32 Groupid)

	void compileFilters()

	void compileCustomer(1: i32 Customerid)

	//moddlp calls.
	i32 initEntity()
	void pushData(1: i32 Entityid, 2: binary Data)
	bool analyze(1: i32 Entityid)
	oneway void closeEntity(1: i32 Entityid)
}
