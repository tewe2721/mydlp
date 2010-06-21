%%
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

%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@medratech.com>
%%% @copyright 2010, H. Kerem Cevahir
%%% @doc ACL for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(mydlp_acl).
-author("kerem@medra.com.tr").
-behaviour(gen_server).

-include("mydlp.hrl").

%% API
-export([start_link/0,
	q/3,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(NOTEXT, [mime_match]).

-record(state, {}).

%%%%%%%%%%%%% MyDLP ACL API

q(Addr, _Dest, Files) ->
	gen_server:call(?MODULE, {acl_q, {Addr, Files}}, 60000).

%%%%%%%%%%%%%% gen_server handles

handle_call({acl_q, {Addr, Files}}, From, State) ->
	Worker = self(),
	spawn_link(fun() ->
		Rules = mydlp_mnesia:get_rules(Addr),
		Param = {Addr, df_to_files(Files)},

		Result = apply_rules(Rules, Param),
		Worker ! {async_acl_q, Result, From}
	end),
	{noreply, State, 60000};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_acl_q, Res, From}, State) ->
	gen_server:reply(From, Res),
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
	{ok, #state{}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%% helper func
apply_rules([{_Id, Action, Matchers}|Rules], Params) ->
	case execute_matchers(Matchers, Params) of
		pos -> Action;
		neg -> apply_rules(Rules, Params)
	end;
apply_rules([], _Params) -> pass.

execute_matchers(Matchers, Params) -> execute_matchers(Matchers, Params, false).

execute_matchers([{Func, FuncParams}|Matchers], Params, true) ->
	case apply(mydlp_matchers, Func, [FuncParams, Params]) of
		pos -> execute_matchers(Matchers, Params, true);
		neg -> neg
	end;
execute_matchers([{Func, FuncParams}|Matchers], {Addr, Files} = Params, false) ->
	{PLT, Params1} = case lists:member(Func, ?NOTEXT) of
		true -> {false, Params};
		false -> {true, {Addr, pl_text(Files)}}
	end,

	case apply(mydlp_matchers, Func, [FuncParams, Params1]) of
		pos -> execute_matchers(Matchers, Params1, PLT);
		neg -> neg
	end;
execute_matchers([], _Params, _PLTexted) -> pos.

pl_text(Files) -> pl_text(Files, []).
pl_text([#file{text=undefined} = File|Files], Rets) -> 
	pl_text(Files, [ File#file{text = mydlp_api:get_text(File)} |Rets]);
pl_text([File|Files], Rets) -> pl_text(Files, [File|Rets]);
pl_text([], Rets) -> lists:reverse(Rets).

df_to_files(Files) ->
	Files1 = df_to_files1(Files, []),
	Files2 =comp_to_files(Files1),
	lists:flatten(Files2).

df_to_files1([#file{mime_type=undefined} = File|Files], Returns) -> 
	MT = mydlp_tc:get_mime(File#file.data),
	df_to_files1(Files, [File#file{mime_type=MT}|Returns]);
df_to_files1([File|Files], Returns) -> 
	df_to_files1(Files, [File|Returns]);
df_to_files1([], Returns) -> lists:reverse(Returns).

comp_to_files(Files) -> comp_to_files(Files, []).
comp_to_files([#file{mime_type= <<"application/zip">>, is_encrypted=false} = File|Files], Returns) -> 
	case zip:extract(File#file.data, [memory]) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			comp_to_files(Files, [df_to_files(ExtFiles)|Returns]);
		{error, _ShouldBeLogged} -> 
			comp_to_files(Files, [File#file{is_encrypted=true}|Returns])
	end;
comp_to_files([#file{mime_type= <<"application/x-rar">>, is_encrypted=false} = File|Files], Returns) -> 
	case mydlp_api:unrar(File#file.data) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			comp_to_files(Files, [df_to_files(ExtFiles)|Returns]);
		{error, _ShouldBeLogged} -> 
			comp_to_files(Files, [File#file{is_encrypted=true}|Returns])
	end;
comp_to_files([#file{mime_type= <<"application/octet-stream">>} = File|Files], Returns) -> % Needs refinement for better ODF handling
	case zip:extract(File#file.data, [memory]) of
		{ok, Ext} -> 
			ExtFiles = ext_to_file(Ext),
			comp_to_files(Files, [df_to_files(ExtFiles)|Returns]);
		{error, _ShouldBeLogged} -> comp_to_files(Files, [File|Returns])
	end;
comp_to_files([File|Files], Returns) -> comp_to_files(Files, [File|Returns]);
comp_to_files([], Returns) -> lists:reverse(Returns).

ext_to_file(Ext) ->
	[#file{name= <<"extracted file">>, 
		filename=Filename, 
		data=Data} 
		|| {Filename,Data} <- Ext].

