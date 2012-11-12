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
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------

-ifdef(__MYDLP_NETWORK).

-module(mydlp_smtpc).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_smtp.hrl").

%% API
-export([start_link/0,
	mail/1,
	mail/2,
	mail/3,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {}).

-define(SMTP_LOG(Type, Param), smtp_msg(Type,Param)).

%%%%%%%%%%%%% MyDLP Thrift RPC API

mail(Message) -> gen_server:cast(?MODULE, {mail, Message}).

mail(Ref, Message) -> gen_server:cast(?MODULE, {mail, Ref, Message}).

mail(From, Rcpt, MessageS) -> gen_server:cast(?MODULE, {mail, From, Rcpt, MessageS}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From,  State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

% INSERT INTO log_incedent (id, rule_id, protocol, src_ip, destination, action, matcher, filename, misc)
handle_cast({mail, #message{mail_from=From, rcpt_to=Rcpt, message=MessageS}}, State) ->
	?ASYNC(fun() -> 
		?SMTP_LOG(send_start, {From, Rcpt}),
		smtpc:sendmail(	?CFG(smtp_next_hop_host), 
				?CFG(smtp_next_hop_port), 
				?CFG(smtp_helo_name), 
				From, Rcpt, MessageS),
		mydlp_spool:consume_next("smtp"),
		?SMTP_LOG(sent_success, {From, Rcpt}),
		ok
	end, 600000),
	{noreply, State};

handle_cast({mail, Ref, #message{mail_from=From, rcpt_to=Rcpt, message=MessageS}}, State) ->
	?ASYNC(fun() -> 
		?SMTP_LOG(send_start, {From, Rcpt}),
		smtpc:sendmail(	?CFG(smtp_next_hop_host), 
				?CFG(smtp_next_hop_port), 
				?CFG(smtp_helo_name), 
				From, Rcpt, MessageS),
		mydlp_spool:delete(Ref),
		mydlp_spool:release(Ref),
		mydlp_spool:consume_next("smtp"),
		?SMTP_LOG(sent_success, {From, Rcpt}),
		ok
	end, 600000),
	{noreply, State};

handle_cast({mail, From, Rcpt, MessageS}, State) ->
	?ASYNC(fun() -> 
		?SMTP_LOG(send_start, {From, Rcpt}),
		smtpc:sendmail(	?CFG(smtp_next_hop_host), 
				?CFG(smtp_next_hop_port), 
				?CFG(smtp_helo_name), 
				From, Rcpt, MessageS),
		mydlp_spool:consume_next("smtp"),
		?SMTP_LOG(sent_success, {From, Rcpt}),
		ok
	end, 600000),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
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

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

smtp_msg(Type, Param) ->
        {Format, Args} = create_smtp_msg(Type, Param),
        mydlp_api:smtp_msg(Format, Args).

create_smtp_msg(send_start, {From, Rcpt}) ->
        {
                "Sending mail. FROM=~s TO='~s' ",
                [From, Rcpt]
        };
create_smtp_msg(send_success, {From, Rcpt}) ->
        {
                "Mail has been sent successfully. FROM=~s TO='~s' ",
                [From, Rcpt]
        };
create_smtp_msg(Type, Param) ->
        {
                "Type=~w Param=~w",
                [Type, Param]
        }.

-endif.

