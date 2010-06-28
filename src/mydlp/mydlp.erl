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

-module(mydlp).
-author('kerem@medratech.com').

-include("mydlp.hrl").

-export([start/0]).

start() ->
	mydlp_loglevel:set(4),
	application:start(ssl),
	application:start(crypto),
	application:load(thrift),
	application:load(sasl),
	application:load(mydlp),
	error_logger:add_report_handler(mydlp_logger_h, get_log_path()),
	application:start(mydlp).

%% @spec () -> string()
%% @doc Returns the full path to the ejabberd log file.
%% It first checks for application configuration parameter 'log_path'.
%% If not defined it checks the environment variable EJABBERD_LOG_PATH.
%% And if that one is neither defined, returns the default value:
%% "ejabberd.log" in current directory.
get_log_path() ->
	case application:get_env(mydlp, log_path) of
	{ok, Path} ->
		Path;
	undefined ->
		case os:getenv("MYDLP_LOG_PATH") of
			false -> ?LOG_PATH;
			Path -> Path
		end
	end.
