-module(chatroom_tests).
-export([chatroom_tests/0]).
-include_lib("eunit/include/eunit.hrl").

chatroom_tests() ->
    [fun join_chatroom_test/0,
     fun send_message_test/0,
     fun receive_message_test/0].

join_chatroom_test() ->
    Room = chatroom:create(),
    ok = chatroom:join(Room, user1),
    ?assertEqual([user1], chatroom:members(Room)).

send_message_test() ->
    Room = chatroom:create(),
    ok = chatroom:join(Room, user1),
    ok = chatroom:send_message(Room, user1, "Hello, world!"),
    ?assertEqual(["Hello, world!"], chatroom:get_messages(Room)).

receive_message_test() ->
    Room = chatroom:create(),
    ok = chatroom:join(Room, user1),
    ok = chatroom:join(Room, user2),
    ok = chatroom:send_message(Room, user1, "Hello, user2!"),
    ?assertEqual(["Hello, user2!"], chatroom:get_messages(Room)).