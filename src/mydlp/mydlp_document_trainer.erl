%%%
%%%    Copyright (C) 2010 Ozgen Muzac <ozgen@mydlp.com>
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

%%%-------------------------------------------------------------------
%%% @author Ozgen Muzac <ozgen@mydlp.com>
%%% @copyright 2013, Ozgen Muzac
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-module(mydlp_document_trainer).
-author("ozgen@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").


%% API
-export([start_link/0,
	start_fingerprinting/0,
	stop/0
	]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
}).

-define(MOUNT_PATH, "/var/lib/mydlp/ddmounts").
-define(SSH_COMMAND, "/usr/bin/sshfs").
-define(FTP_COMMAND, "/usr/bin/curlftpfs").
-define(SMB_COMMAND, "/usr/bin/smbmount").
-define(MOUNT_COMMAND, "/bin/mount").
-define(MOUNTPOINT_COMMAND, "/bin/mountpoint").
-define(UMOUNT_COMMAND, "/bin/umount").
-define(TRY_COUNT, 5).

%%%%%%%%%%%%%  API


%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({handle_remotes, RDDs}, State) ->
	mount_and_generate_fingerprints(RDDs),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(startup, State) ->
	start_fingerprinting(),
	{noreply, State};

handle_info(control_remote_storages, State) ->
	start_fingerprinting(),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions


start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	timer:send_after(6000, startup),
	{ok, #state{}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

mount_path(MountPath, Command, Args, Envs, Stdin, 1) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", MountPath]) of
		true ->	ok;
		false ->
			case mydlp_api:cmd(Command, Args, Envs, Stdin) of
				ok -> ok;
				E -> ?ERROR_LOG("Remote Discovery: Error Occcured on mount: "
                                                "FilePath: "?S"~nError: "?S"~n ", [MountPath, E]),
					none end
	end;
						
mount_path(MountPath, Command, Args, Envs, Stdin, TryCount) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", MountPath]) of
		true -> ok;
		false ->
			case mydlp_api:cmd(Command, Args, Envs, Stdin) of
				ok ->ok;
				_ -> timer:sleep(500),
					mount_path(MountPath, Command, Args, Envs, Stdin, TryCount-1) end
	end.
						

create_and_mount_path(MountPath, Command, Args, Envs, Stdin) ->
	case filelib:is_dir(MountPath) of 
		true -> ok;
		false -> file:make_dir(MountPath)
	end, 
	mount_path(MountPath, Command, Args, Envs, Stdin, ?TRY_COUNT).

handle_each_mount({sshfs, [Address, Password, Path, Port, Username]}, Id) ->
	PortS = integer_to_list(Port),
	Stdin = binary_to_list(Password) ++ "\n",
	UsernameS = binary_to_list(Username),
	PathS = binary_to_list(Path),
	AddressS = binary_to_list(Address),
	ConnectionString = UsernameS ++ "@" ++ AddressS ++ ":" ++ PathS, 
	MountPath = filename:join(?MOUNT_PATH, integer_to_list(Id)),
	Args = ["-p", PortS, ConnectionString, MountPath, "-o", "password_stdin"],
	create_and_mount_path(MountPath, ?SSH_COMMAND, Args, [], Stdin);

handle_each_mount({ftpfs, [Address, Password, Path, Username]}, Id) ->
	PasswordS = binary_to_list(Password),
	UsernameS = binary_to_list(Username),
	PathS = binary_to_list(Path),
	AddressS = binary_to_list(Address),
	UandP = case UsernameS of 
			[] -> "anonymous:anonymous";
			_ -> UsernameS ++ ":" ++ PasswordS
		end,
	AddressPath = UandP ++ "@" ++ AddressS ++ "/" ++ PathS,
	MountPath = filename:join(?MOUNT_PATH, integer_to_list(Id)),
	Args = ["-o", "ro,utf8", AddressPath, MountPath],
	create_and_mount_path(MountPath, ?FTP_COMMAND, Args, [], none);

handle_each_mount({cifs, Details}, Id) ->
	handle_windows_share(Details, Id);

handle_each_mount({dfs, Details}, Id) ->
	handle_windows_share(Details, Id);

handle_each_mount({nfs, [Address, Path]}, Id) ->
	PathS = binary_to_list(Path),
	AddressS = binary_to_list(Address),
	AddressPath = AddressS ++ ":" ++ PathS,
	MountPath = filename:join(?MOUNT_PATH, integer_to_list(Id)),
	Args = ["-o", "ro,soft,intr,rsize=8192,wsize=8192", AddressPath, MountPath],
	create_and_mount_path(MountPath, ?MOUNT_COMMAND, Args, [], none).

handle_windows_share([WindowsShare, Password, Path, Username], Id) ->
	PasswordS = binary_to_list(Password),
	UsernameS = binary_to_list(Username),
	PathS = binary_to_list(Path),
	WindowsSharePath =  "//" ++ binary_to_list(WindowsShare) ++ "/" ++ PathS,
	MountPath = filename:join(?MOUNT_PATH, integer_to_list(Id)),
	Args = ["-o","ro", WindowsSharePath, MountPath],
	case UsernameS of
		[] -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [], "\n");
		_ ->
			case PasswordS of
				[] -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [{"USER", UsernameS}], "\n");
				_ -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [{"USER", UsernameS}, {"PASSWD", PasswordS}], none)
			end
	end.

mount_and_generate_fingerprints([{DDId, RemoteStorage, RSId}|Rest]) ->
	handle_each_mount(RemoteStorage, RSId),
	mount_and_generate_fingerprints(Rest);
mount_and_generate_fingerprints([]) -> ok.

start_fingerprinting() ->
	RDDs = mydlp_mnesia:get_remote_document_databases(),
	erlang:display(RDDs),
	gen_server:cast(?MODULE, {handle_remotes, RDDs}),
	timer:send_after(60*60*1000, control_remote_storages).
