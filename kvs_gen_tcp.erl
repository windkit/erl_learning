-module(kvs_server).
 
-export([start/0,stop/1]).
 
-include_lib("eunit/include/eunit.hrl").
-define(PORT, 12321).
-define(STORE, store).

start() ->
    Options = [binary, {packet, raw}, {active, true}, {reuseaddr, true}],
	ets:new(?STORE, [set, public, named_table]),	
    case gen_tcp:listen(?PORT, Options) of
        {ok, Listen} -> spawn(fun() -> acceptor_loop(Listen) end),
                        {ok, Listen};
        {error, _} ->
            stop
    end.
 
stop(Listen) ->
    gen_tcp:close(Listen),
    ok.
 
acceptor_loop(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            Pid = spawn(fun() -> server_loop(Client) end),
            gen_tcp:controlling_process(Client, Pid),
            acceptor_loop(Listen);
        {error, closed} ->
            stop;
        {error, timeout} ->
            io:format("accept failed: timeout~n"),
            stop;
        {error, Reason} ->
            io:format("accept failed: ~p~n", [Reason]),
            stop
    end.
 
server_loop(Client) ->
    receive
        {tcp, Client, Data} ->
            gen_tcp:send(Client, handle_request(parse_request(Data))),
            inet:setopts(Client, [{active, once}]),
            server_loop(Client);
        {tcp_closed, _} ->
            gen_tcp:close(Client),
            ok;
        _ ->
            gen_tcp:close(Client),
            error
    end.

% Exercise 1. Complete implementation of the handlers for set and get requests
% Exercise 1. Implement a handler for delete requests
handle_request([<<"help">>]) ->
    <<"command list\n  set <key> <value>\n  get <key>\n  delete <key>\n">>;
handle_request([<<"set">>, Key, Val]) ->
    ?debugVal(Key),
    ?debugVal(Val),
	try ets:insert(?STORE, {Key, Val}) of
		true ->
			<< "ok\n" >>
	catch
		_ : _ ->
			<< "failed\n" >>
	end;
    %<<"set request!\n">>;
handle_request([<<"get">>, Key]) ->
    ?debugVal(Key),
	Ret = ets:lookup(?STORE, Key),
	case Ret of
		[{Key, Val}] ->
			?debugVal(Val),
			<< Val/binary , "\n">>;
		_ ->
			<< "not found\n" >>
	end;
%    <<"get request!\n">>;
handle_request([<<"delete">>, Key]) ->
	?debugVal(Key),
	ets:delete(?STORE, Key),
	<< "ok\n" >>;
handle_request(Req) ->
    ?debugVal(Req),
    <<"parse error\n">>.

parse_request(Data) ->
    % remove the last newline characters
    Data2 = re:replace(Data, <<"\\s+$">>, <<"">>, [{newline, any}, {return, binary}]),
    re:split(Data2, "\\s+").
