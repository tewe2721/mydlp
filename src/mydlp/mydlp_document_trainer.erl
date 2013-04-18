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

-include_lib("kernel/include/file.hrl").

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
	queue,
	in_prog=false
}).

-define(MOUNT_PATH, "/var/lib/mydlp/ddmounts").
-define(SSH_COMMAND, "/usr/bin/sshfs").
-define(FTP_COMMAND, "/usr/bin/curlftpfs").
-define(SMB_COMMAND, "/usr/bin/smbmount").
-define(MOUNT_COMMAND, "/bin/mount").
-define(MOUNTPOINT_COMMAND, "/bin/mountpoint").
-define(UMOUNT_COMMAND, "/bin/umount").
-define(TRY_COUNT, 5).


q(MountPath, ExcludeFiles, DDId) -> gen_server:cast(?MODULE, {q, MountPath, ExcludeFiles, DDId}).

consume() -> gen_server:cast(?MODULE, consume).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({handle_remotes, RDDs}, State) ->
	mount_and_generate_fingerprints(RDDs),
	{noreply, State};

handle_cast({q, MountPath, ExcludeFiles, DDId}, #state{queue=Q, in_prog=false}=State) ->
	Q1 = queue:in(Q, {MountPath, ExcludeFiles, DDId}),
	consume(),
	{noreply, State#state{queue=Q1, in_prog=true}};	

handle_cast({q, MountPath, ExcludeFiles, DDId}, #state{queue=Q, in_prog=true}=State) ->
	Q1 = queue:in(Q, {MountPath, ExcludeFiles, DDId}),
	{noreply, State#state{queue=Q1}};

handle_cast(consume, #state{queue=Q}=State) ->
	case queue:out(Q) of
		{value, {FilePath, ExcludeFiles, DDId}, Q1} ->
			case filename:split(FilePath) of
				["/", "var", "lib", "mydlp", "ddmounts", _Id, Filename|_Rest] -> 
					case lists:member(Filename, ExcludeFiles) of
						false -> generate_fingerprints(FilePath, DDId);
						true -> ok end;
				_ -> ?ERROR_LOG("Unknown file in document training: ["?S"]", [FilePath]) end,
			consume(),
			{noreply, State#state{queue=Q1}};
		{empty, Q1} ->
			{noreply, State#state{queue=Q1}}
	end;

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
	reset_discover_cache(),
	timer:send_after(6000, startup),
	{ok, #state{queue=queue:new(), in_prog=false}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

mount_path(MountPath, Command, Args, Envs, Stdin, 1) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", MountPath]) of
		true ->	MountPath;
		false ->
			case mydlp_api:cmd(Command, Args, Envs, Stdin) of
				ok -> MountPath;
				E -> ?ERROR_LOG("Document Trainer: Error Occcured on mount: "
                                                "FilePath: "?S"~nError: "?S"~n ", [MountPath, E]),
					none end
	end;
						
mount_path(MountPath, Command, Args, Envs, Stdin, TryCount) ->
	case mydlp_api:cmd_bool(?MOUNTPOINT_COMMAND, ["-q", MountPath]) of
		true -> MountPath;
		false ->
			case mydlp_api:cmd(Command, Args, Envs, Stdin) of
				ok -> MountPath;
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

generate_fingerprints_file(#fs_entry{file_id=FP}, DDId) ->
	ok.

generate_fingerprints_dir(#fs_entry{file_id=FP, entry_id=EId}, DDId) ->
	CList = case file:list_dir(FP) of
		{ok, LD} -> LD;
		{error, _} -> [] end,
	OList = mydlp_mnesia:fs_entry_list_dir(EId),
	MList = lists:umerge([CList, OList]),
	[ q(filename:absname(FN, FP), [], DDId) || FN <- MList ],
	ok.

generate_fingerprints_dir_dir(#fs_entry{file_id=FP, entry_id=EId}, DDId) ->
	OList = mydlp_mnesia:fs_entry_list_dir(EId),
	[ q(filename:absname(FN, FP), [], DDId) || FN <- OList ],
	ok.

generate_fingerprints(FilePath, DDId) ->
	case is_cached({FilePath, DDId}) of
		true -> ok;
		false -> generate_fingerprints1(FilePath, DDId) end.

generate_fingerprints1(FilePath, DDId) ->
	case filelib:is_regular(FilePath) of
		true -> E = fs_entry(FilePath),
			case is_changed(E) of
				true -> generate_fingerprints_file(FilePath, DDId);
				false -> 
					case mydlp_mnesia:get_dd_file_entry(FilePath) of 
						none -> generate_fingerprints_file(FilePath, DDId);  
						#dd_file_entry{dd_id_list=DDList} = DDFileEntry ->
							case lists:member(DDId, DDList) of
								true -> ok;
								_ -> add_dd_to_file_entry(DDFileEntry, DDId)
							end
					end
			 end;
	false -> case filelib:is_dir(FilePath) of
		true -> E = fs_entry(FilePath),
			case is_changed(E) of
				true -> generate_fingerprints_dir(E, DDId);
				false -> generate_fingerprints_dir_dir(E, DDId) end;
	false -> ?ERROR_LOG("DISCOVER: File or directory does not exists. Filename: "?S, [FilePath]),
		mydlp_mnesia:del_fs_entry(FilePath) end end, % Means file does not exists
	ok.

add_dd_to_file_entry(#dd_file_entry{dd_id_list=DDList, file_entry_id=FileEntryId}=Entry, DDId) ->
	NewList = [DDId|DDList],
	mydlp_mnesia:add_dd_file_entry(Entry#dd_file_entry{dd_id_list=NewList}),
	mydlp_mysql:insert_dd_file_entry(FileEntryId, DDId).

meta(FilePath) ->
	{ok, FileInfo} = file:read_file_info(FilePath),
	{FileInfo#file_info.mtime, FileInfo#file_info.size}.

is_changed(#fs_entry{file_id=FilePath, file_size=FSize, last_modified=LMod} = E) ->
	{MTime, CSize} = meta(FilePath),
	case ( (LMod /= MTime) or (CSize /= FSize) ) of
		true -> mydlp_mnesia:add_fs_entry(E#fs_entry{file_size=CSize, last_modified=MTime}), % update mnesia entry
			true;
		false -> false end.

fs_entry(FilePath) ->
	case mydlp_mnesia:get_fs_entry(FilePath) of
		none -> 
			Id = mydlp_mnesia:get_unique_id(fs_entry),
			E = #fs_entry{entry_id=Id, file_id=FilePath},
			mydlp_mnesia:add_fs_entry(E),
			E;
		#fs_entry{} = F -> F 
	end.

is_cached(Element) ->
	CS = get(cache),
	case gb_sets:is_element(Element, CS) of
		true -> true;
		false -> CS1 = gb_sets:add(Element, CS),
			put(cache, CS1),
			false end.

reset_discover_cache() ->
	put(cache, gb_sets:new()), ok.

mount_and_generate_fingerprints([{DDId, RemoteStorage, RSId, ExcludeFiles}|Rest]) ->
	MountPath = case RemoteStorage of
		none -> none;
		_ -> handle_each_mount(RemoteStorage, RSId) end,
	case MountPath of 
		none -> ok;
		_ -> q(MountPath, ExcludeFiles, DDId) end,
	mount_and_generate_fingerprints(Rest);
mount_and_generate_fingerprints([]) -> ok.

start_fingerprinting() ->
	RDDs = mydlp_mnesia:get_remote_document_databases(),
	erlang:display(RDDs),
	gen_server:cast(?MODULE, {handle_remotes, RDDs}),
	timer:send_after(60*60*1000, control_remote_storages).
