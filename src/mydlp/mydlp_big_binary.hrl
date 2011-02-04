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

-ifndef(_MYDLP_BIG_BINARY_HRL).
-define(_MYDLP_BIG_BINARY_HRL, true).

% Big binary create
-define(BB_C(RawData),
	mydlp_workdir:raw_to_obj(RawData)
).

% Big binary read
-define(BB_R(Ref),
	mydlp_workdir:read_obj(Ref)
).

% Big binary get filepath
-define(BB_P(Ref),
	mydlp_workdir:get_obj_fp(Ref)
).


% Big binary delete
-define(BB_D(Ref),
	mydlp_workdir:delete_obj(Ref)
).

-endif.
