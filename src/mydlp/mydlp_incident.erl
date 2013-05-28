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

-ifdef(__MYDLP_NETWORK).

-module(mydlp_incident).
-author("kerem@mydlp.com").
-behaviour(gen_server).

-include("mydlp.hrl").
-include("mydlp_schema.hrl").

%% API
-export([start_link/0,
	l/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	notify_users_now/1,
	code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
	logger_queue,
	logger_inprog = false
}).

%%%%%%%%%%%%%  API

l(LogRecord) -> gen_server:cast(?MODULE, {l, LogRecord}).

%%%%%%%%%%%%%% gen_server handles

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast({l, Item}, #state{logger_queue=Q, logger_inprog=false} = State) ->
	Q1 = queue:in(Item, Q),
	consume(),
	{noreply, State#state{logger_queue=Q1, logger_inprog=true}};

handle_cast({l, Item}, #state{logger_queue=Q, logger_inprog=true} = State) ->
	Q1 = queue:in(Item, Q),
	{noreply,State#state{logger_queue=Q1}};

handle_cast(consume, #state{logger_queue=Q} = State) ->
	case queue:out(Q) of
		{{value, Item}, Q1} ->
			try	process_log_tuple(Item)
			catch Class:Error ->
				?ERROR_LOG("Logger Queue Consume: Error occured: "
						"Class: ["?S"]. Error: ["?S"].~n"
						"Stack trace: "?S"~n.Item: "?S"~nState: "?S"~n ",	
						[Class, Error, erlang:get_stacktrace(), Item, State]) end,
				%%% TODO: for some errors we many return original queue
			consume(),
			{noreply, State#state{logger_queue=Q1}};
		{empty, _} ->
			{noreply, State#state{logger_inprog=false}}
	end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	?SAFEREPLY(From, Reply),
	{noreply, State};

handle_info(check_notifications, State) ->
	check_notification_queue(),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

consume() -> gen_server:cast(?MODULE, consume).

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	timer:send_after(1800000, check_notifications),
	{ok, #state{logger_queue=queue:new()}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%% internal
check_notification_queue() ->
	NQI = mydlp_mnesia:get_early_notification_queue_items(),
	lists:foreach(fun(R) -> notify_users_now(R, false), 
				update_notification_queue_item(R, true)		
				end, NQI),
	timer:send_after(1800000, check_notifications).

regulate_notifications(RuleId, Details) ->
	NQI = mydlp_mnesia:get_notification_queue_items(RuleId),
	case NQI of
		[] ->	notify_users_now(RuleId, {true, Details}),
		 	{_Me, S, _Mi} = erlang:now(),
			N = #notification_queue{rule_id=RuleId, date=S, status=true},
			mydlp_mnesia:write(N);
		[I] -> NewStatus = case I of
					true -> 1;
					_ -> I+1
				end,
				update_notification_queue_item(RuleId, NewStatus)
	end.

update_notification_queue_item(RuleId, NewStatus) ->
	Result = mydlp_mnesia:update_notification_queue_item(RuleId, NewStatus),
	case Result of
		notify -> notify_users_now(RuleId, false);
		_ -> ok
	end.

notify_users(RuleId, Ip, User, Time) ->
	NI = mydlp_mnesia:get_notification_items(RuleId),
	case NI of
		[] -> ok;
		_ -> regulate_notifications(RuleId, {Ip, User, Time})
	end.

notify_users_now(RuleId) -> notify_users_now(RuleId, false).

notify_users_now(RuleId, IsDetailed) ->
	Notifications = mydlp_mnesia:get_notification_items(RuleId),
	notify_user(Notifications, RuleId, IsDetailed).

notify_user([{email, EmailAddress}|Notifications], RuleId, IsDetailed) ->
	Replace = case IsDetailed of
			{true, Details} -> get_detalied_message(RuleId, Details);
			false -> get_backoff_message(RuleId)
	end,
	CFGBody = unicode:characters_to_binary(?CFG(email_notification_message)),
	ReplacedMessage = re:replace(CFGBody, "%%DETAILS%%", Replace, [global, {return, binary}]),
	
	EmailBody = [	"From: ", ?CFG(email_notification_message_from),
			"\n",
			"To: ", EmailAddress,
			"\n",
			"Subject: ",
			?CFG(email_notification_message_subject),
			"\n",
			"Content-Type: text/plain; charset=utf-8",
			"\n",
			"Content-Transfer-Encoding: base64",
			"\n",
			"\n",
			base64:encode(ReplacedMessage),
			"\n"],
	mydlp_smtpc:mail("support@mydlp.com", binary_to_list(EmailAddress), EmailBody),
	notify_user(Notifications, RuleId, IsDetailed);
notify_user([{other, _Target}|Notifications], RuleId, IsDetailed) -> 
	notify_user(Notifications, RuleId, IsDetailed);
notify_user([], _, _) -> ok.

get_rule_detail_field(RuleId) ->
	Name = mydlp_mnesia:get_rule_name_by_id(RuleId),
	case Name of
		undefined -> integer_to_list(RuleId);
		R -> binary_to_list(R) end.

get_detalied_message(RuleId, {{I1, I2, I3, I4}, User, Time}) ->
	IpS = integer_to_list(I1) ++ "." ++ integer_to_list(I2) ++ "." ++ integer_to_list(I3) ++ "." ++ integer_to_list(I4),
	UserS = case User of
			nil -> "none";
			R when is_binary(R) -> binary_to_list(R);
			RS -> RS end,
	{{Yr,Mt,Dy},{Hr,Mnt,Scnd}} = Time,
	TimeD = io_lib:format('~2..0b.~2..0b.~4..0b', [Mt, Dy, Yr]),
	TimeT = io_lib:format('~2..0b:~2..0b:~2..0b', [Hr, Mnt, Scnd]),
	Details = "Date: " ++ TimeD ++ " " ++ TimeT ++ "\n" ++ 
		"User: " ++ UserS ++ "\n" ++ 
		"IP: " ++ IpS ++ "\n" ++
		"Rule: " ++ get_rule_detail_field(RuleId) ++ "\n",
	unicode:characters_to_binary(Details).

get_backoff_message(RuleId) ->
	Count = integer_to_list(mydlp_mnesia:get_number_of_incidents(RuleId)),
	Details = "Rule:" ++ get_rule_detail_field(RuleId) ++ " causes " ++ Count ++ " incident",
	unicode:characters_to_binary(Details).

process_log_tuple(#log{channel=web, action=archive, rule_id=-1, file=Files} = Log) ->
	Files1 = lists:filter(fun(F) -> 
		?BB_S(F#file.dataref) > ?CFG(archive_minimum_size)
		end, Files),
	process_log_tuple1(Log#log{file=Files1});
process_log_tuple(#log{channel=mail, destination={_RcptTo, CompleteRcpts}} = Log) ->
	process_log_tuple1(Log#log{destination=CompleteRcpts});
process_log_tuple(Log) -> process_log_tuple1(Log).

process_log_tuple1(#log{file=[]}) -> ok;
process_log_tuple1(#log{time=Time, channel=Channel, rule_id=RuleId, action=Action, ip=Ip, user=User, destination=To, file=Files, itype_id = ITypeId, misc=Misc, payload=Payload, group_id=GroupId, matching_details=MatchingDetails}) ->
	IsLogData = mydlp_api:is_store_action(Action),
	LogId = mydlp_mysql:push_log(Time, Channel, RuleId, Action, Ip, User, To, ITypeId, Misc, GroupId),
	notify_users(RuleId, Ip, User, Time),
	process_log_files(LogId, IsLogData, Files),
	process_matching_details(LogId, MatchingDetails),
	case {Channel, Action} of
		{mail, quarantine} -> 	process_payload(LogId, Payload),
					mydlp_mysql:insert_log_requeue(LogId);
		_Else2 -> ok end,
	ok.

process_payload(_LogId, none) -> ok;
process_payload(LogId, Payload) ->
	Data = erlang:term_to_binary(Payload, [compressed]),
	mydlp_quarantine:s(payload, LogId, Data),
	ok.

get_meta(#file{} = File) ->
	Size = File#file.size,
	Hash = File#file.md5_hash,
	MimeType = File#file.mime_type,
	Filename = mydlp_api:file_to_str(File),
	{Filename, MimeType, Size, Hash}.

process_log_files(LogId, false = IsLogData, [File|Files]) ->
	{Filename, MimeType, Size, Hash} = get_meta(File),
	mydlp_api:clean_files(File),

	mydlp_mysql:insert_log_blueprint(LogId, Filename, MimeType, Size, Hash),
	process_log_files(LogId, IsLogData, Files);
process_log_files(LogId, true = IsLogData, [File|Files]) ->
	File1 = case ( File#file.size < ?CFG(maximum_object_size) ) of 
		true -> mydlp_api:load_files(File);
		false -> ?ERROR_LOG("Unexpected big item. File: "?S, [File]),
			File#file{data=undefined} end,

	case File1#file.data of
		undefined -> process_log_files(LogId, false, [File1]);
		<<>> -> process_log_files(LogId, false, [File1]);
		D when is_binary(D), size(D) == 0 -> process_log_files(LogId, false, [File1]);
		_Else -> 
			{Filename, MimeType, Size, Hash} = get_meta(File1),
			{ok, Path} = mydlp_api:quarantine(File1),
			mydlp_api:clean_files(File1),
			mydlp_mysql:insert_log_data(LogId, Filename, MimeType, Size, Hash, Path)
	end,

	process_log_files(LogId, IsLogData, Files);
process_log_files(_LogId, _IsLogData, []) -> ok.

process_matching_details(LogId, MatchingDetails) ->
	mydlp_mysql:insert_log_detail(LogId, MatchingDetails).

-endif.

