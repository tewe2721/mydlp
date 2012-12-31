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

namespace java com.mydlp.backend.thrift

service Mydlp {
	string getMime(1 : string FileName, 2 : binary Data)
	binary getText(1 : string FileName, 2: string MimeType, 3 : binary Data)
	binary getUnicodeText(1 : string Encoding, 2 : binary Data)

	string secloreInitialize(	1 : string SecloreAppPath,
					2 : string SecloreAddress,
					3 : i32 SeclorePort,
					4 : string SecloreAppName,
					5 : i32 SecloreHotFolderCabinetId,
					6 : string SecloreHotFolderCabinetPassphrase,
					7 : i32 SeclorePoolSize )

	string secloreProtect(		1 : string FilePath,
					2 : i32 HotFolderId,
					3 : string ActivityComments )

	string secloreTerminate()
}
