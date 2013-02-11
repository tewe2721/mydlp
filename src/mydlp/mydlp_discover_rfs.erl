%%%
%%%    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@mydlp.com>
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
%%% @author H. Kerem Cevahir <kerem@mydlp.com>
%%% @copyright 2011, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-module(mydlp_discover_rfs).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").


%% API
-export([start_link/0,
	start_remote_discovery/0,
	finished/0,
	stop/0]).

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

-define(MOUNTH_PATH, "/home/ozgen/mounts/").
-define(SSH_COMMAND, "/usr/bin/sshfs").
-define(FTP_COMMAND, "/usr/bin/curlftpfs").
-define(SMB_COMMAND, "/usr/bin/smbmount").
-define(MOUNT_COMMAND, "/bin/mount").
-define(MOUNTPOINT_COMMAND, "/bin/mountpoint").
-define(UMOUNT_COMMAND, "/bin/umount").
-define(TRY_COUNT, 5).

%%%%%%%%%%%%%  API

start_remote_discovery() ->
	RemoteStorages = mydlp_mnesia:get_remote_storages(),
	consume(RemoteStorages).

finished() -> gen_server:cast(?MODULE, finished).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({consume, RemoteStorages}, State) ->
	discover_each_mount(RemoteStorages, []),
	{noreply, State};

handle_cast(finished, State) ->
	release_mounts(true),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(startup, State) ->
	release_mounts(?CFG(discover_rfs_on_startup)),
	{noreply, State};

handle_info(schedule_now, State) ->
	start_remote_discovery(),
	{noreply, State};

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

consume(RemoteStorages) -> gen_server:cast(?MODULE, {consume, RemoteStorages}).

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	timer:send_after(60000, startup),
	{ok, #state{}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

concat_mount_list(none, Acc) -> Acc;
concat_mount_list({MountPath, RuleId}, Acc) ->
	[{MountPath, RuleId}|Acc].


mount_path(MountPath, Command, Args, Envs, Stdin, RuleId, 1) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", MountPath]) of
		true -> {MountPath, RuleId};
		false ->
			case mydlp_api:cmd(Command, Args, Envs, Stdin) of
				ok -> {MountPath, RuleId};
				E -> ?ERROR_LOG("Remote Discovery: Error Occcured on mount: "
                                                "FilePath: "?S"~nError: "?S"~n ", [MountPath, E]),
					none end
	end;
						
mount_path(MountPath, Command, Args, Envs, Stdin, RuleId, TryCount) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", MountPath]) of
		true -> {MountPath, RuleId};
		false ->
			case mydlp_api:cmd(Command, Args, Envs, Stdin) of
				ok -> {MountPath, RuleId};
				_ -> timer:sleep(500),
					mount_path(MountPath, Command, Args, Envs, Stdin, RuleId, TryCount-1) end
	end.
						

create_and_mount_path(MountPath, Command, Args, Envs, Stdin, RuleId) ->
	case filelib:is_dir(MountPath) of 
		true -> ok;
		false -> file:make_dir(MountPath)
	end, 
	mount_path(MountPath, Command, Args, Envs, Stdin, RuleId, ?TRY_COUNT).

discover_each_mount([{Id, RuleId, sshfs, {Address, Port, Path, Username, Password}}|RemoteStorages], Acc) ->
	PortS = integer_to_list(Port),
	Stdin = binary_to_list(Password) ++ "\n",
	UsernameS = binary_to_list(Username),
	PathS = binary_to_list(Path),
	AddressS = binary_to_list(Address),
	ConnectionString = UsernameS ++ "@" ++ AddressS ++ ":" ++ PathS, 
	MountPath = filename:join(?MOUNTH_PATH, integer_to_list(Id)),
	Args = ["-p", PortS, ConnectionString, MountPath, "-o", "password_stdin"],
	MountTuple = create_and_mount_path(MountPath, ?SSH_COMMAND, Args, [], Stdin, RuleId),
	Acc1 = concat_mount_list(MountTuple, Acc),
	discover_each_mount(RemoteStorages, Acc1);
discover_each_mount([{Id, RuleId, ftpfs, {Address, Path, Username, Password}}|RemoteStorages], Acc) ->
	PasswordS = binary_to_list(Password),
	UsernameS = binary_to_list(Username),
	PathS = binary_to_list(Path),
	AddressS = binary_to_list(Address),
	UandP = case UsernameS of 
			[] -> "anonymous:anonymous";
			_ -> UsernameS ++ ":" ++ PasswordS
		end,
	AddressPath = UandP ++ "@" ++ AddressS ++ "/" ++ PathS,
	MountPath = filename:join(?MOUNTH_PATH, integer_to_list(Id)),
	Args = ["-o", "ro,utf8", AddressPath, MountPath],
	MountTuple = create_and_mount_path(MountPath, ?FTP_COMMAND, Args, [], none, RuleId),
	Acc1 = concat_mount_list(MountTuple, Acc),
	discover_each_mount(RemoteStorages, Acc1);
discover_each_mount([{Id, RuleId, cifs, Details}|RemoteStorages], Acc) ->
	Acc1 = discover_windows_share(Id, RuleId, Details, Acc),
	discover_each_mount(RemoteStorages, Acc1);
discover_each_mount([{Id, RuleId, dfs, Details}|RemoteStorages], Acc) ->
	Acc1 = discover_windows_share(Id, RuleId, Details, Acc),
	discover_each_mount(RemoteStorages, Acc1);
discover_each_mount([{Id, RuleId, nfs, {Address, Path}}|RemoteStorages], Acc) ->
	PathS = binary_to_list(Path),
	AddressS = binary_to_list(Address),
	AddressPath = AddressS ++ ":" ++ PathS,
	MountPath = filename:join(?MOUNTH_PATH, integer_to_list(Id)),
	Args = ["-o", "ro,soft,intr,rsize=8192,wsize=8192", AddressPath, MountPath],
	MountTuple = create_and_mount_path(MountPath, ?MOUNT_COMMAND, Args, [], none, RuleId),
	Acc1 = concat_mount_list(MountTuple, Acc),
	discover_each_mount(RemoteStorages, Acc1);
discover_each_mount([], Acc) -> mydlp_discover_fs:ql(Acc).

discover_windows_share(Id, RuleId, {WindowsShare, Path, Username, Password}, Acc) ->
	PasswordS = binary_to_list(Password),
	UsernameS = binary_to_list(Username),
	PathS = binary_to_list(Path),
	WindowsSharePath =  "//" ++ binary_to_list(WindowsShare) ++ "/" ++ PathS,
	MountPath = filename:join(?MOUNTH_PATH, integer_to_list(Id)),
	Args = ["-o","ro", WindowsSharePath, MountPath],
	MountTuple = case UsernameS of
			[] -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [], "\n", RuleId);
			_ ->
				case PasswordS of
					[] -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [{"USER", UsernameS}], "\n", RuleId);
					_ -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [{"USER", UsernameS}, {"PASSWD", PasswordS}], none, RuleId)
				end
		end,
	concat_mount_list(MountTuple, Acc).

release_mounts(IsScheduled) -> 
	case file:list_dir(?MOUNTH_PATH) of
		{ok, FileList} -> release_mount(FileList, IsScheduled);
		{error, E} -> ?ERROR_LOG("Remote Discovery: Error Occured listing mount directory. MountPath: ["?S"]~n. Error: ["?S"]~n", [?MOUNTH_PATH, E])
	end.

release_mount([File|Rest], IsScheduled) ->
	FilePath = filename:join(?MOUNTH_PATH, File),
	umount_path(FilePath, ?TRY_COUNT),
	release_mount(Rest, IsScheduled);
release_mount([], IsScheduled) -> 
	case IsScheduled of 
		true ->
			case timer:send_after(?CFG(discover_rfs_interval), schedule_now) of
				{ok, _} -> ok;
				{error, E} -> ?ERROR_LOG("Remote discovery: Can not create timer. Reason: "?S, [E])
			end;
		false -> ok end.

umount_path(FilePath, TryCount) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", FilePath]) of %Checks whether File path is a mountpoint or not.
		false -> file:del_dir(FilePath);
		true ->	
			case mydlp_api:cmd(?UMOUNT_COMMAND, [FilePath]) of
				ok -> 
					case file:del_dir(FilePath) of
						ok -> ok;
						ER -> ?ERROR_LOG("Remote Discovery: Error Occured rm directory. MountPath: ["?S"]~n. Error: ["?S"]~n", [FilePath, ER]) end;
				E ->
					case TryCount of
						1 -> ?ERROR_LOG("Remote Discovery: Error Occured umount directory. MountPath: ["?S"]~n. Error: ["?S"]~n", [FilePath, E]) ;
						_ -> timer:sleep(400),
							umount_path(FilePath, TryCount-1)
					end
			end
	end.

