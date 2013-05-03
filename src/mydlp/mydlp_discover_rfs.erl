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
%%% @author Ozgen Muzac <ozgen@mydlp.com>
%%% @copyright 2011, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-module(mydlp_discover_rfs).
-author("kerem@mydlp.com").
-author("ozgen@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").


%% API
-export([start_link/0,
	start_remote_discovery/0,
	finished/0,
	release_mount_by_rule_id/1,
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
	mount_dict
}).

-define(MOUNT_PATH, "/var/lib/mydlp/mounts").
-define(SSH_COMMAND, "/usr/bin/sshfs").
-define(FTP_COMMAND, "/usr/bin/curlftpfs").
-define(SMB_COMMAND, "/usr/bin/smbmount").
-define(MOUNT_COMMAND, "/bin/mount").
-define(MOUNTPOINT_COMMAND, "/bin/mountpoint").
-define(UMOUNT_COMMAND, "/bin/umount").
-define(TRY_COUNT, 5).

-define(REPORT_STATUS_DISC, "running").
-define(REPORT_STATUS_STOPPED, "stopped").
-define(REPORT_STATUS_PAUSED, "paused").
-define(REPORT_STATUS_ERROR, "error").

-define(RFS_DISC_FINISHED, "rfs_finished").

%%%%%%%%%%%%%  API

release_mount_by_rule_id(RuleId) -> gen_server:call(?MODULE, {release_mount_by_rule_id, RuleId}).

start_remote_discovery() -> ok.
	%RemoteStorages = mydlp_mnesia:get_remote_storages().
	%consume(RemoteStorages).

finished() -> gen_server:cast(?MODULE, finished).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

%handle_call({stop_discovery,RuleId}, _From, State) ->
%	Reply = gen_server:call(mydlp_discover_fs, {stop_discovery_by_rule_id, RuleId}, 60000),
%	{reply, Reply, State};

handle_call({release_mount_by_rule_id, RuleId}, _From, #state{mount_dict=Dict}=State) ->
	Dict1 = case dict:find(RuleId, Dict) of
			{ok, {FilePaths, _R}} -> release_mount(FilePaths),
					dict:erase(RuleId, Dict);
			_ -> ?OPR_LOG("mydlp_discover_rfs: Unknown Rule Id: ["?S"]", [RuleId]), 
				Dict
		end,
	{reply, ok, State#state{mount_dict=Dict1}};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({start_by_rule_id, RuleId, GroupId}, #state{mount_dict=Dict}=State) ->
	RemoteStorages = mydlp_mnesia:get_remote_storages_by_rule_id(RuleId),
	Dict1 = case dict:find(RuleId, Dict) of
			{ok, {FilePaths, _R}} -> release_mount(FilePaths),
					dict:erase(RuleId, Dict);
			_ -> ?OPR_LOG("mydlp_discover_rfs: Unknown Rule Id: ["?S"]", [RuleId]), 
				Dict
		end,
	consume(RemoteStorages, GroupId, RuleId),
	{noreply, State#state{mount_dict=Dict1}};

%handle_cast({continue_discovering, RuleId}, State) ->
%	mydlp_discover_fs:continue_paused_discovery(RuleId),
%	{noreply, State};	

handle_cast({consume, RemoteStorages, GroupId, RuleId}, #state{mount_dict=Dict}=State) ->
	Dict1 = discover_each_mount(RemoteStorages, Dict, GroupId),
	case dict:find(RuleId, Dict1) of
		{ok, {MountPaths, GroupId}} -> mydlp_discover_fs:ql([{RuleId, MountPath, GroupId}|| MountPath <- MountPaths]);
		_ ->	Time = erlang:universaltime(),
			OprLog = #opr_log{time=Time, channel=remote_discovery, rule_id=RuleId, message_key=?RFS_DISC_FINISHED, group_id=GroupId},
			?DISCOVERY_OPR_LOG(OprLog)
	end,
	{noreply, State#state{mount_dict=Dict1}};

handle_cast(finished, State) ->
	release_mounts(),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(startup, State) ->
	release_mounts(),
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

consume(RemoteStorages, GroupId, RuleId) -> gen_server:cast(?MODULE, {consume, RemoteStorages, GroupId, RuleId}).

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	%timer:send_after(60000, startup),
	filelib:ensure_dir(?MOUNT_PATH),
	{ok, #state{mount_dict=dict:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

add_mount_path_to_dict(none, Dict, _GroupId) -> Dict;
add_mount_path_to_dict({MountPath, RuleId}, Dict, GroupId) -> 
	case dict:find(RuleId, Dict) of
		error -> dict:store(RuleId, {[MountPath], GroupId}, Dict);
		{ok, {Paths, GId}} -> dict:store(RuleId, {[MountPath|Paths], GId}, Dict)
	end.

mount_path(MountPath, Command, Args, Envs, Stdin, RuleId, _GroupId, 1) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", MountPath]) of
		true ->	{MountPath, RuleId};
		false ->
			case mydlp_api:cmd(Command, Args, Envs, Stdin) of
				ok -> {MountPath, RuleId};
				E -> ?ERROR_LOG("Remote Discovery: Error Occcured on mount: "
                                                "FilePath: "?S"~nError: "?S"~n ", [MountPath, E]),
					none end
	end;
						
mount_path(MountPath, Command, Args, Envs, Stdin, RuleId, GroupId, TryCount) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", MountPath]) of
		true -> {MountPath, RuleId};
		false ->
			case mydlp_api:cmd(Command, Args, Envs, Stdin) of
				ok ->{MountPath, RuleId};
				_ -> timer:sleep(500),
					mount_path(MountPath, Command, Args, Envs, Stdin, RuleId, GroupId, TryCount-1) end
	end.
						

create_and_mount_path(MountPath, Command, Args, Envs, Stdin, RuleId, GroupId) ->
	case filelib:is_dir(MountPath) of 
		true -> ok;
		false -> file:make_dir(MountPath)
	end, 
	mount_path(MountPath, Command, Args, Envs, Stdin, RuleId, GroupId, ?TRY_COUNT).

discover_each_mount([{Id, RuleId, sshfs, {Address, Port, Path, Username, Password}}|RemoteStorages], Dict, GroupId) ->
	PortS = integer_to_list(Port),
	Stdin = binary_to_list(Password) ++ "\n",
	UsernameS = binary_to_list(Username),
	PathS = binary_to_list(Path),
	AddressS = binary_to_list(Address),
	ConnectionString = UsernameS ++ "@" ++ AddressS ++ ":" ++ PathS, 
	MountPath = filename:join(?MOUNT_PATH, integer_to_list(Id)),
	Args = ["-p", PortS, ConnectionString, MountPath, "-o", "password_stdin"],
	MountTuple = create_and_mount_path(MountPath, ?SSH_COMMAND, Args, [], Stdin, RuleId, GroupId),
	Dict1 = add_mount_path_to_dict(MountTuple, Dict, GroupId),
	discover_each_mount(RemoteStorages, Dict1, GroupId);
discover_each_mount([{Id, RuleId, ftpfs, {Address, Path, Username, Password}}|RemoteStorages], Dict, GroupId) ->
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
	MountTuple = create_and_mount_path(MountPath, ?FTP_COMMAND, Args, [], none, RuleId, GroupId),
	Dict1 = add_mount_path_to_dict(MountTuple, Dict, GroupId),
	discover_each_mount(RemoteStorages, Dict1, GroupId);
discover_each_mount([{Id, RuleId, cifs, Details}|RemoteStorages], Dict, GroupId) ->
	Dict1 = discover_windows_share(Id, RuleId, Details, Dict, GroupId),
	discover_each_mount(RemoteStorages, Dict1, GroupId);
discover_each_mount([{Id, RuleId, dfs, Details}|RemoteStorages], Dict, GroupId) ->
	Dict1 = discover_windows_share(Id, RuleId, Details, Dict, GroupId),
	discover_each_mount(RemoteStorages, Dict1, GroupId);
discover_each_mount([{Id, RuleId, nfs, {Address, Path}}|RemoteStorages], Dict, GroupId) ->
	PathS = binary_to_list(Path),
	AddressS = binary_to_list(Address),
	AddressPath = AddressS ++ ":" ++ PathS,
	MountPath = filename:join(?MOUNT_PATH, integer_to_list(Id)),
	Args = ["-o", "ro,soft,intr,rsize=8192,wsize=8192", AddressPath, MountPath],
	MountTuple = create_and_mount_path(MountPath, ?MOUNT_COMMAND, Args, [], none, RuleId, GroupId),
	Dict1 = add_mount_path_to_dict(MountTuple, Dict, GroupId),
	discover_each_mount(RemoteStorages, Dict1, GroupId);
discover_each_mount([], Dict, _GroupId) -> Dict.

discover_windows_share(Id, RuleId, {WindowsShare, Path, Username, Password}, Dict, GroupId) ->
	PasswordS = binary_to_list(Password),
	UsernameS = binary_to_list(Username),
	PathS = binary_to_list(Path),
	WindowsSharePath =  "//" ++ binary_to_list(WindowsShare) ++ "/" ++ PathS,
	MountPath = filename:join(?MOUNT_PATH, integer_to_list(Id)),
	Args = ["-o","ro", WindowsSharePath, MountPath],
	MountTuple = case UsernameS of
			[] -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [], "\n", RuleId, GroupId);
			_ ->
				case PasswordS of
					[] -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [{"USER", UsernameS}], "\n", RuleId, GroupId);
					_ -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [{"USER", UsernameS}, {"PASSWD", PasswordS}], none, RuleId, GroupId)
				end
		end,
	add_mount_path_to_dict(MountTuple, Dict, GroupId).

release_mounts() -> 
	case file:list_dir(?MOUNT_PATH) of
		{ok, FileList} -> release_mount(FileList);
		{error, E} -> ?ERROR_LOG("Remote Discovery: Error Occured listing mount directory. MountPath: ["?S"]~n. Error: ["?S"]~n", [?MOUNT_PATH, E])
	end.

release_mount([File|Rest]) ->
	FilePath = filename:join(?MOUNT_PATH, File),
	umount_path(FilePath, ?TRY_COUNT),
	release_mount(Rest);
release_mount([]) -> ok.

umount_path(FilePath, TryCount) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", FilePath]) of %Checks whether File path is a mountpoint or not.
		false -> file:del_dir(FilePath);
		true ->	
			case mydlp_api:cmd(?UMOUNT_COMMAND, [FilePath]) of
				ok -> 
					case file:del_dir(FilePath) of
						ok -> ok;
						ER -> ?ERROR_LOG("Remote Discovery: Error Occured rm directory. MountPath: ["?S"]~n. Error: ["?S"]~n", [FilePath, ER]),
							 error 
					end;
				E ->
					case TryCount of
						1 -> ?ERROR_LOG("Remote Discovery: Error Occured umount directory. MountPath: ["?S"]~n. Error: ["?S"]~n", [FilePath, E]) ;
						_ -> timer:sleep(1000),
							umount_path(FilePath, TryCount-1)
					end
			end
	end.

