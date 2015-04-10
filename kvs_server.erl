-module(kvs_server).

-behaviour(gen_server).

-include("kvs.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(STORE, store).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket}).

start_link(Client) ->
    gen_server:start_link(?MODULE, [Client], []).

init([Client]) ->
    ets:new(?STORE, [set, public, named_table]),
    {ok, #state{socket=Client}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State=#state{socket=Socket}) ->
	gen_tcp:send(Socket, kvs_binary(Data)),
	{noreply, State, ?CLIENT_TIMEOUT};
handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.


% Exercise. Complete implementation of the handlers for set, get and delete requests
kvs_binary(<< ?Magic_Request:8, ?OP_Version:8, KeySize:16, ExtrasSize:8, _:8, _:16, _:32, _:32, _:64,
              _:ExtrasSize/binary, _:KeySize/binary, _/binary >>) ->
    BodySize = byte_size(?Memcache_Protocol_Version),
    << ?Magic_Response:8, ?OP_Version:8, 0:16, 0:8, 0:8, 0:16, BodySize:32, 0:32, 0:64,
       ?Memcache_Protocol_Version/binary >>;
kvs_binary(<< ?Magic_Request:8, ?OP_GetK:8, KeySize:16, ExtrasSize:8, _:8, _:16, TotalBodySize:32, _:32, _:64, Extras:ExtrasSize/binary, Key:KeySize/binary, _/binary >>) ->
    Ret = ets:lookup(?STORE, Key),
    case Ret of
        [{Key, Val}] ->
            ?debugVal(Val),
            BodySize = byte_size(Val) + KeySize + 4,
            << ?Magic_Response:8, ?OP_GetK:8, KeySize:16, 16#04:8, 0:8, 0:16, BodySize:32, 0:32, 0:64, 16#DEADBEEF:32, Key/binary, Val/binary >>;
        [] ->
            io:format("Not Found~n"),
            << ?Magic_Response:8, ?OP_GetK:8, 0:16, 0:8, 0:8, 16#01:16, 0:32, 0:32, 0:64 >>
    end;
kvs_binary(<< ?Magic_Request:8, ?OP_Set:8, KeySize:16, ExtrasSize:8, _:8, _:16, TotalBodySize:32, _:32, _:64, Flags:32, Expiry:32, Body/binary >>) ->
    Key = binary:part(Body, {0, KeySize}),
    Val = binary:part(Body, {KeySize, TotalBodySize - KeySize - ExtrasSize}),
    ?debugVal(Key),
    ?debugVal(Val),
    ets:insert(?STORE, {Key, Val}),
    << ?Magic_Response:8, ?OP_Set:8, 0:16, 0:8, 0:8, 0:16, 0:32, 0:32, 0:64 >>;
kvs_binary(<< ?Magic_Request:8, ?OP_Delete:8, KeySize:16, ExtrasSize:8, _:8, _:16, BodySize:32, _:32, _:64, Extras:ExtrasSize/binary, Key:KeySize/binary, _/binary >>) ->
    ?debugVal(Key),
    ets:delete(?STORE, Key),
    << ?Magic_Response:8, ?OP_Delete:8, 0:16, 0:8, 0:8, 0:16, 0:32, 0:32, 0:64 >>;
kvs_binary(Packet) ->
    io:format("<<~s>>~n", [[io_lib:format("~2.16.0B|",[X]) || <<X:8>> <= Packet ]]),
    error_message(<<"Not implemented">>).

error_message(Cause) ->
    BodySize = byte_size(Cause),
    << ?Magic_Response:8, 0:8, 0:16, 0:8, 0:8, 1:16, BodySize:32, 0:32, 0:64,
       Cause/binary >>.
