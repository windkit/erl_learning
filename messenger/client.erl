-module(client).
-compile(export_all).

-record(state, {name = ""}).

-define(SERVER, 'server@ubuntu1404d').
-define(LOCAL, client).

start(Name, UserId) ->
	gen_server:start_link({global, UserId}, ?MODULE, [Name], []).

handle_call({send, Name, Msg}, _, State=#state{name = OwnName}) ->
	case Name of
		OwnName ->
			io:format("You cannot message yourself!~n"),
			{reply, {failed, "Self Messaging"}, State};
		_ ->
			io:format("Message To ~p: ~p~n", [Name, Msg]),
			NodeId = get_peer(Name),
			case NodeId of
				{ok, TargetId} ->
					%Resp = rpc:call({global, TargetId}, client, recv, [OwnName, Msg]),
					Resp = gen_server:call({global, TargetId}, {recv, OwnName, Msg}),
					{reply, Resp, State};
				{failed, Msg} ->
					{reply, {failed, Msg}, State}
				end
		end;
handle_call({recv, FromName, Msg}, _, State=#state{}) ->
	io:format("Message From ~p:~n~p~n", [FromName, Msg]),
	{reply, {ok, "Message Received"}, State}.

init([Name]) ->
	register(?LOCAL, self()),
	{ok, #state{name = Name}}.

login(Name) ->
	Resp = rpc:call(?SERVER, server, connect, [Name]),
	case Resp of
		{ok, UserId, Msg} ->
			start(Name, UserId),	
			io:format("Welcome Message from Server: ~n~p~n", [Msg]);
		_ ->
			error
	end.

get_peer(Name) ->
	try get_list() of
		PeerList ->
			try dict:fetch(Name, PeerList) of
				TargetId ->
					{ok, TargetId}
			catch
				_ -> 
					{failed, "Target Not Found"}
			end
	catch
		throw:not_found -> 
			{failed, "Target Not Found"}
	end.

get_list() ->
	Resp = rpc:call(?SERVER, server, get_list, []),
	case Resp of
		{ok, PeerList} ->
			PeerList;	
		_ ->
			throw(not_found)
		end.
	
recv(FromName, Msg) ->
	gen_server:call(?LOCAL, {recv, FromName, Msg}).

send(Name, Msg) ->
	gen_server:call(?LOCAL, {send, Name, Msg}).
