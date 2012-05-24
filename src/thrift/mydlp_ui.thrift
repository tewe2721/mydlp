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

namespace java com.mydlp.ui.thrift

service Mydlp_ui {

	void compileCustomer(1: i32 Customerid)

	binary getRuletable(1: string Ipaddress, 2: string Userh, 3: string Revisionid)

	string receiveBegin(1: string Ipaddress)
	string receiveChunk(1: string Ipaddress, 2: i64 Itemid, 3: binary Chunkdata, 4: i32 Chunknum, 5: i32 Chunknumtotal)

	list<i64> getFingerprints(1: string Filename, 2: binary Data)

	oneway void requeueIncident(1: i64 Incidentid)

	oneway void registerUserAddress(1: string Ipaddress, 2: string Userh, 3: binary Payload)
}
