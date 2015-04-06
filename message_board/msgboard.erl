-module(msgboard).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3]).
-export([login/1, send/2, fetch/0, user_list/0, greet/0]).

-record(state, {userlist = [],
				useraddr = dict:new(),
				userboard = dict:new()}).

-define(DEF_TIMEOUT, 30000).
-define(CHAT_SERVER, {global, 'chatser'}).

start_link() ->
	gen_server:start_link(?CHAT_SERVER, ?MODULE, [], []).

init([])->
	{ok, #state{}}.

check_user(From, UserAddr) ->
	Resp = dict:find(From, UserAddr),
	case Resp of
		{ok, Name} ->
			{ok, From, Name};
		error ->
			{error, From, "Please log in first"}
	end.

handle_call({login, Name}, {From, _}, State = #state{userlist = NameList, useraddr = UserAddr}) ->
	NewState = State#state{userlist = [Name] ++ NameList, useraddr = dict:store(From, Name, UserAddr)},
	{reply, {ok, From, Name}, NewState};
handle_call({greet}, {From, _}, State = #state{useraddr = UserAddr}) ->
	Check = check_user(From, UserAddr),
	{reply, Check, State};
handle_call({fetch}, {From, _}, State = #state{useraddr = UserAddr, userboard = UserBoard}) ->
	Check = check_user(From, UserAddr),
	case Check of
		{ok, _, Name} ->
			RecvBoardResp = dict:find(Name, UserBoard),
			case RecvBoardResp of
				{ok, Board} ->
					NewState = State#state{userboard = dict:erase(Name,	UserBoard)},
					{reply, {ok, Board}, NewState};
				_ ->
					{reply, {ok, "No Msgs"}, State}
				end;
		_ ->
			{reply, Check, State}
	end;
handle_call({send, Name, Msg}, {From, _}, State = #state{useraddr = UserAddr, userboard = UserBoard}) ->
	Check = check_user(From, UserAddr),
	case Check of
		{ok, _, FromName} ->
			MsgF = lists:concat(["From ", FromName, ": ", Msg]),
%			io:format("~p~n", [MsgF]),
			RecvBoardResp = dict:find(Name, UserBoard),
			case RecvBoardResp of
				{ok, Board} ->
					NewBoard = Board ++ [MsgF];
				_ ->
					NewBoard = [MsgF]
			end,
			NewState = State#state{userboard = dict:store(Name, NewBoard, UserBoard)},
			{reply, {ok, "Sent"}, NewState};
		_ ->
			{reply, Check, State}
	end;
handle_call({list}, {From, _}, State = #state{useraddr = UserAddr}) ->
	{reply, {ok, State#state.userlist}, State}.

user_list() ->
	gen_server:call(?CHAT_SERVER, {list}, ?DEF_TIMEOUT).

login(Name) ->
	gen_server:call(?CHAT_SERVER, {login, Name}, ?DEF_TIMEOUT).

send(Name, Msg) ->
	gen_server:call(?CHAT_SERVER, {send, Name, Msg}, ?DEF_TIMEOUT).

fetch() ->
	gen_server:call(?CHAT_SERVER, {fetch}, ?DEF_TIMEOUT).

greet() ->
	gen_server:call(?CHAT_SERVER, {greet}, ?DEF_TIMEOUT).
