-module(erlbus_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([start_chatroom_test/1]).

all() -> [start_chatroom_test].

init_per_suite(Config) ->
    %% Initialize resources needed for the entire suite
    {ok, Pid} = chatroom:start_link([]),
    [{pid, Pid} | Config].

end_per_suite(Config) ->
    %% Clean up resources used for the entire suite
    Pid = proplists:get_value(pid, Config),
    chatroom:stop(Pid).

init_per_testcase(_TestCase, Config) ->
    %% Initialize resources needed for each test case
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Clean up resources used for each test case
    ok.

start_chatroom_test(Config) ->
    %% Example test code
    _Pid = proplists:get_value(pid, Config),
    ?assertEqual("Hello World!", "Hello World!"),
    ok.