-module(server).
-compile(export_all).

-record(state, {userlist = [],
				peerlist = dict:new(),
				userid = 0}).

-define(SERVER, {global, 'server'}).

start()->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

handle_call({getlist}, _, State=#state{peerlist = PeerList}) ->
	{reply, {ok, PeerList}, State};

handle_call({connect, Name}, _, State=#state{userlist = UserList, peerlist = PeerList, userid = UserId}) ->
	NewPeerList = dict:store(Name, UserId, PeerList),
	NewUserList = UserList ++ [Name],
	NewUserId = UserId + 1, 
	NewState = State#state{userlist = NewUserList, peerlist = NewPeerList, userid = NewUserId},
	io:format("Registered ~p[~p]~n", [Name, UserId]),
	{reply, {ok, UserId, lists:concat(["Hello ", Name])}, NewState}.

init([])->
	{ok, #state{}}.

get_list()->
	gen_server:call(?SERVER, {getlist}).
connect(Name)->
	gen_server:call(?SERVER, {connect, Name}).
