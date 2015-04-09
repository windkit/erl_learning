-module(file_sender_app).

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    file_sender_sup:start_link().

prep_stop(_State) ->
	ok = file_sender_sup:stop(),
	ok.

stop(_State) ->
    ok.


-ifdef(TEST).

simple_test() ->
	ok = application:start(file_sender),
	?assertNot(undefined == whereis(file_sender_sup)).

-endif.
