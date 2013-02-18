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

-module(mydlp_sync).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	set_policy_id/1,
	set_enc_key/1,
	get_enc_key/0,
	sync_now/0,
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
	policy_id,
	enc_key
	}).

%%%% API
set_policy_id(PolicyId) -> gen_server:cast(?MODULE, {set_policy_id, PolicyId}).

set_enc_key(EncKey) when is_binary(EncKey), size(EncKey) == 64 -> gen_server:cast(?MODULE, {set_enc_key, EncKey}).

get_enc_key() -> gen_server:call(?MODULE, get_enc_key).

sync_now() -> gen_server:cast(?MODULE, sync).

%%%%%%%%%%%%%% gen_server handles

handle_call(get_enc_key, _From, #state{enc_key=EncKey} = State) ->
	{reply, EncKey, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(sync, #state{policy_id=undefined} = State) ->
	PolicyId = try mydlp_api:get_client_policy_revision_id()
	catch Class:Error -> ?ERROR_LOG("GET Revision ID: "
		"Class: ["?S"]. Error: ["?S"].~n"
		"Stack trace: "?S"~n", [Class, Error, erlang:get_stacktrace()]),
		0 end,
	handle_cast(sync, State#state{policy_id=PolicyId});

handle_cast(sync, #state{policy_id=PolicyId} = State) ->
	sync(PolicyId),
        {noreply, State};

handle_cast({set_policy_id, PolicyId}, State) ->
        {noreply, State#state{policy_id=PolicyId}};

handle_cast({set_enc_key, EncKey}, State) when is_binary(EncKey), size(EncKey) == 64 ->
	mydlp_container:set_ep_meta("has_enc_key", "yes"),
        {noreply, State#state{enc_key=EncKey}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(sync_now, #state{policy_id=undefined} = State) ->
	PolicyId = mydlp_api:get_client_policy_revision_id(),
	handle_info(sync_now, State#state{policy_id=PolicyId});

handle_info(sync_now, #state{policy_id=PolicyId} = State) ->
	try	mydlp_container:set_general_meta(),
		timer:sleep(1000),
		sync(PolicyId)
	catch Class:Error -> ?ERROR_LOG("SYNC Handle: "
		"Class: ["?S"]. Error: ["?S"].~n"
		"Stack trace: "?S"~n", [Class, Error, erlang:get_stacktrace()]) end,
	call_timer(),
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
	inets:start(),
	call_timer(15000),
	{ok, #state{}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

call_timer() -> call_timer(?CFG(sync_interval)).

call_timer(Interval) -> timer:send_after(Interval, sync_now).

sync(PolicyId) ->
	RevisionS = integer_to_list(PolicyId),
	MetaDict = mydlp_container:get_ep_meta_dict(),
	User = mydlp_container:get_ep_meta("user"),
	UserHI = mydlp_api:hash_un(User),
	UserHS = integer_to_list(UserHI),
	Data0 = erlang:term_to_binary(MetaDict),
	case mydlp_api:encrypt_payload(Data0) of
		retry -> ok;
		Data when is_binary(Data)-> 
			Url = "https://" ++ ?CFG(management_server_address) ++ "/sync?rid=" ++ RevisionS ++ "&uh=" ++ UserHS,
			case catch httpc:request(post, {Url, [], "application/octet-stream", Data}, [], []) of
				{ok, {{_HttpVer, Code, _Msg}, _Headers, Body}} -> 
					case {Code, Body} of
						{200, []} -> ?ERROR_LOG("SYNC: Empty response: Url="?S"~n", [Url]);
						{200, "up-to-date" ++ _Rest} -> ok;
						{200, Payload} -> process_payload(Payload);
						{Else1, _Data} -> ?ERROR_LOG("SYNC: An error occured during HTTP req: Code="?S"~n", [Else1]) end;
				Else -> ?ERROR_LOG("SYNC: An error occured during HTTP req: Obj="?S"~n", [Else]) end end,
	ok.

process_payload(Payload) ->
	case mydlp_api:decrypt_payload(Payload) of
		retry -> ok;
		<<"up-to-date", _/binary>> -> ok;
		CDBBin -> mydlp_api:use_client_policy(CDBBin) end.

-endif.

