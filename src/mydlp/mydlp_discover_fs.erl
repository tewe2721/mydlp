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

-ifdef(__MYDLP_ENDPOINT).

-module(mydlp_discover_fs).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0,
	q/1,
	q/2,
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
	discover_queue,
	discover_inprog=false
}).

%%%%%%%%%%%%%  API

q(FilePath) -> q(none, FilePath).

q(ParentId, FilePath) -> gen_server:cast(?MODULE, {q, ParentId, FilePath}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({q, ParentId, FilePath}, #state{discover_queue=Q, discover_inprog=false} = State) ->
	Q1 = queue:in({ParentId, FilePath}, Q),
	consume(),
	{noreply, State#state{discover_queue=Q1, discover_inprog=true}};

handle_cast({q, ParentId, FilePath}, #state{discover_queue=Q, discover_inprog=true} = State) ->
	Q1 = queue:in({ParentId, FilePath}, Q),
	{noreply,State#state{discover_queue=Q1}};

handle_cast(consume, #state{discover_queue=Q} = State) ->
	case queue:out(Q) of
		{{value, {ParentId, FilePath}}, Q1} ->
			try	discover(ParentId, FilePath),
				consume(),
				{noreply, State#state{discover_queue=Q1}}
			catch Class:Error ->
				?ERROR_LOG("Discover Queue Consume: Error occured: "
						"Class: ["?S"]. Error: ["?S"].~n"
						"Stack trace: "?S"~n.FilePath: "?S"~nState: "?S"~n ",	
						[Class, Error, erlang:get_stacktrace(), FilePath, State]),
					{noreply, State#state{discover_queue=Q1}} end;
		{empty, _} ->
			schedule(?CFG(discover_fs_interval)),
			{noreply, State#state{discover_inprog=false}}
	end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(schedule_now, State) ->
	schedule(),
	{noreply, State};

handle_info(schedule_startup, State) ->
	case ?CFG(discover_fs_on_startup) of
		true -> schedule();
		false -> schedule(?CFG(discover_fs_interval)) end,
	{noreply, State};

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

schedule(Interval) -> timer:send_after(Interval, schedule_now).

schedule() ->
	Paths = ?CFG(discover_fs_paths),
	PathList = string:tokens(Paths,";"),
	lists:foreach(fun(P) -> q(P) end, PathList),
	ok.

consume() -> gen_server:cast(?MODULE, consume).

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	timer:send_after(60000, schedule_startup),
	gen_server:call(?MODULE, stop).

init([]) ->
	{ok, #state{discover_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal

meta(FilePath) ->
	{ok, FileInfo} = file:read_file_info(FilePath),
	{FileInfo#file_info.mtime, FileInfo#file_info.size}.

is_changed(#fs_entry{file_path=FP, file_size=FSize, last_modified=LMod} = E) ->
	{MTime, CSize} = meta(FP),
	case ( (LMod /= MTime) or (CSize /= FSize) ) of
		true -> mydlp_mnesia:add_fs_entry(E#fs_entry{file_size=CSize, last_modified=MTime}), % update mnesia entry
			true;
		false -> false end.

fs_entry(ParentId, FilePath, IsDir) ->
	case mydlp_mnesia:get_fs_entry(FilePath) of
		none -> Id = mydlp_mnesia:get_unique_id(fs_entry),
			E = #fs_entry{file_path=FilePath, entry_id=Id, parent_id=ParentId, is_dir=IsDir},
			mydlp_mnesia:add_fs_entry(E), %% bulk write may improve performance
			E;
		#fs_entry{} = FS -> FS end.

discover_file(#fs_entry{file_path=FP}) -> 
	?ASYNC(fun() ->
		try	{ok, ObjId} = mydlp_container:new(),
			ok = mydlp_container:setprop(ObjId, "channel", "discovery"),
			ok = mydlp_container:pushfile(ObjId, {raw, FP}),
			{ok, Action} = mydlp_container:aclq(ObjId),
			case Action of
				block -> ok = file:delete(FP);
				pass -> ok end
		catch Class:Error ->
			?ERROR_LOG("ACLQ: Error occured: Class: ["?S"]. Error: ["?S"].~n"
					"Stack trace: "?S"~nFilePath: ["?S"].~n",
				[Class, Error, erlang:get_stacktrace(), FP])
		end
	end, 1800000),
	ok.

discover_dir(#fs_entry{file_path=FP, entry_id=EId}) ->
	{ok, CList} = file:list_dir(FP),
	OList = mydlp_mnesia:fs_entry_list_dir(EId),
	MList = lists:umerge([CList, OList]),
	[ q(EId, filename:absname(FN, FP)) || FN <- MList ],
	ok.

discover_dir_dir(#fs_entry{file_path=FP, entry_id=EId}) ->
	OList = mydlp_mnesia:fs_entry_list_dir_dir(EId),
	[ q(EId, filename:absname(FN, FP)) || FN <- OList ],
	ok.

discover(ParentId, FilePath) ->
	case filelib:is_regular(FilePath) of
		true -> E = fs_entry(ParentId, FilePath, false),
			case is_changed(E) of
				true -> discover_file(E);
				false -> ok end;
	false -> case filelib:is_dir(FilePath) of
		true -> E = fs_entry(ParentId, FilePath, true),
			case is_changed(E) of
				true -> discover_dir(E);
				false -> discover_dir_dir(E) end;
	false -> mydlp_mnesia:del_fs_entry(FilePath) end end, % Means file does not exists
	ok.
	
-endif.

