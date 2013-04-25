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
	start_fingerprinting/1,
	get_remote_storage_dir/1,
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
	document_ids,
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

get_remote_storage_dir(RSId) -> gen_server:call(?MODULE, {get_remote_storage_dir, RSId}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call({get_remote_storage_dir, RSId}, _From, State) ->
	RemoteStorage = mydlp_mysql:get_remote_storage_by_id(RSId),
	Filename = integer_to_list(RSId) ++ "_dir",
	MountPath = handle_each_mount(RemoteStorage, Filename),
	DirList = case MountPath of
		none -> {reply, [none], State};
		_ -> case file:list_dir(MountPath) of
			{ok, FileList} -> {reply, FileList, State};
			{error, E} -> ?ERROR_LOG("Document Discovery: Error Occured listing directory. MountPath: ["?S"]~n. Error: ["?S"]~n", [MountPath, E]),
					{reply, [], State} end
	end,
	release_mount([Filename]),
	DirList;

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({handle_remotes, RDDs, DDId}, #state{document_ids=Ids}=State) ->
	Ids1 = case lists:member(DDId, Ids) of
			true -> Ids;
			false -> [DDId|Ids] end,
	mount_and_generate_fingerprints(RDDs),
	{noreply, State#state{document_ids=Ids1}};

handle_cast({q, MountPath, ExcludeFiles, DDId}, #state{queue=Q, in_prog=false}=State) ->
	Q1 = queue:in({MountPath, ExcludeFiles, DDId}, Q),
	consume(),
	{noreply, State#state{queue=Q1, in_prog=true}};	

handle_cast({q, MountPath, ExcludeFiles, DDId}, #state{queue=Q, in_prog=true}=State) ->
	Q1 = queue:in({MountPath, ExcludeFiles, DDId}, Q),
	{noreply, State#state{queue=Q1}};

handle_cast(consume, #state{queue=Q, document_ids=Ids}=State) ->
	case queue:out(Q) of
		{{value, {FilePath, ExcludeFiles, DDId}}, Q1} ->
			case lists:member(FilePath, ExcludeFiles) of
				false -> generate_fingerprints(FilePath, DDId, ExcludeFiles);
				true -> ok end,
			consume(),
			{noreply, State#state{queue=Q1}};
		{empty, Q1} ->
			reset_discover_cache(),
			release_mounts(),
			mark_fingerprinting_as_finished(Ids),
			{noreply, State#state{queue=Q1, in_prog=false, document_ids=[]}}
	end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(startup, State) ->
	%start_fingerprinting(),
	{noreply, State};

handle_info(control_remote_storages, State) ->
	%start_fingerprinting(),
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
	release_mounts(),
	timer:send_after(6000, startup),
	{ok, #state{queue=queue:new(), in_prog=false, document_ids=[]}}.

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
	MountPath = get_mount_path(Id),
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
	MountPath = get_mount_path(Id),
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
	MountPath = get_mount_path(Id),
	Args = ["-o", "ro,soft,intr,rsize=8192,wsize=8192", AddressPath, MountPath],
	create_and_mount_path(MountPath, ?MOUNT_COMMAND, Args, [], none).

handle_windows_share([WindowsShare, Password, Path, Username], Id) ->
	PasswordS = binary_to_list(Password),
	UsernameS = binary_to_list(Username),
	PathS = binary_to_list(Path),
	WindowsSharePath =  "//" ++ binary_to_list(WindowsShare) ++ "/" ++ PathS,
	MountPath = get_mount_path(Id),
	Args = ["-o","ro", WindowsSharePath, MountPath],
	case UsernameS of
		[] -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [], "\n");
		_ ->
			case PasswordS of
				[] -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [{"USER", UsernameS}], "\n");
				_ -> create_and_mount_path(MountPath, ?SMB_COMMAND, Args, [{"USER", UsernameS}, {"PASSWD", PasswordS}], none)
			end
	end.

get_mount_path(Id) ->
	case is_integer(Id) of
		true -> filename:join(?MOUNT_PATH, integer_to_list(Id));
		false -> filename:join(?MOUNT_PATH, Id) end.

generate_fingerprints_file(#fs_entry{file_id=FP}, DDId) ->
	try
		Filename = filename:basename(FP),
		CreatedDate = erlang:universaltime(),
		{ok, Bin} = file:read_file(FP),
		File = ?BF_C(#file{filename=Filename}, Bin),
		Md5Hash = File#file.md5_hash,
		FileId = mydlp_mysql:insert_file_entry(Filename, Md5Hash, CreatedDate),
		Text = mydlp_api:concat_texts(File),
		FList = mydlp_pdm:fingerprint(Text),
		mydlp_api:clean_files(File),
		FList1 = lists:usort(lists:map(fun(#kgram{hash=Hash}) -> Hash end, FList)),
		mydlp_mysql:save_fingerprints(FileId, FList1),
		
		DDFileEntry =case mydlp_mnesia:get_dd_file_entry(FP) of
				none ->	Id = mydlp_mnesia:get_unique_id(dd_file_entry),
					#dd_file_entry{id=Id, filepath=FP, dd_id_list=[]};
				R -> R end,
		add_dd_to_file_entry(DDFileEntry#dd_file_entry{file_entry_id=FileId}, DDId)

	catch Class:Error ->
		?ERROR_LOG("Document Trainer: Error occured while reading file. Class: ["?S"]. Error: ["?S"]. ~n"
				"Stack trace: "?S"~n FilePath: ["?S"].~n", 
				[Class, Error, erlang:get_stacktrace(), FP])
	end,
	ok.

generate_fingerprints_dir(#fs_entry{file_id=FP, entry_id=EId}, DDId, ExcludeFiles) ->
	CList = case file:list_dir(FP) of
		{ok, LD} -> LD;
		{error, _} -> [] end,
	OList = mydlp_mnesia:fs_entry_list_dir(EId),
	MList = lists:umerge([CList, OList]),
	[ q(filename:absname(FN, FP), ExcludeFiles, DDId) || FN <- MList ],
	ok.

generate_fingerprints_dir_dir(#fs_entry{file_id=FP, entry_id=EId}, DDId, ExcludeFiles) ->
	OList = mydlp_mnesia:fs_entry_list_dir(EId),
	[ q(filename:absname(FN, FP), ExcludeFiles, DDId) || FN <- OList ],
	ok.

generate_fingerprints(FilePath, DDId, ExcludeFiles) ->
	case is_cached({FilePath, DDId}) of
		true -> ok;
		false -> generate_fingerprints1(FilePath, DDId, ExcludeFiles) end.

generate_fingerprints1(FilePath, DDId, ExcludeFiles) ->
	case filelib:is_regular(FilePath) of
		true -> E = fs_entry(FilePath),
			case is_changed(E) of
				true -> generate_fingerprints_file(E, DDId);
				false -> 
					case mydlp_mnesia:get_dd_file_entry(FilePath) of 
						none -> generate_fingerprints_file(E, DDId);  
						#dd_file_entry{} = DDFileEntry -> add_dd_to_file_entry(DDFileEntry, DDId)
					end
			 end;
	false -> case filelib:is_dir(FilePath) of
		true -> E = fs_entry(FilePath),
			case is_changed(E) of
				true -> generate_fingerprints_dir(E, DDId, ExcludeFiles);
				false -> generate_fingerprints_dir_dir(E, DDId, ExcludeFiles) end;
	false -> ?ERROR_LOG("DISCOVER: File or directory does not exists. Filename: "?S, [FilePath]),
		mydlp_mnesia:del_fs_entry(FilePath) end end, % Means file does not exists
	ok.

mark_fingerprinting_as_finished(DocumentIds) ->
	mydlp_mysql:update_document_fingerprinting_as_finished(DocumentIds).

add_dd_to_file_entry(#dd_file_entry{dd_id_list=DDList, file_entry_id=FileEntryId}=Entry, DDId) ->
	NewList = case lists:member(DDId, DDList) of
		true -> DDList;
		false -> [DDId|DDList] end,
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
		_ -> q(MountPath, lists:map(fun(I) -> filename:join(MountPath, binary_to_list(I)) end, ExcludeFiles), DDId) end,
	mount_and_generate_fingerprints(Rest);
mount_and_generate_fingerprints([]) -> ok.

%start_fingerprinting() ->
%	RDDs = mydlp_mnesia:get_remote_document_databases(),
%	gen_server:cast(?MODULE, {handle_remotes, RDDs}),
%	timer:send_after(60*60*1000, control_remote_storages).

start_fingerprinting(DDId) ->
	RDDs = mydlp_mnesia:get_remote_document_databases_by_id(DDId),
	gen_server:cast(?MODULE, {handle_remotes, RDDs, DDId}).

release_mounts() -> 
	case file:list_dir(?MOUNT_PATH) of
		{ok, FileList} -> release_mount(FileList);
		{error, E} -> ?ERROR_LOG("Document Discovery: Error Occured listing mount directory. MountPath: ["?S"]~n. Error: ["?S"]~n", [?MOUNT_PATH, E])
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
