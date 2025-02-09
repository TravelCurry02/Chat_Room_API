-module(chat_room_broadcast).

-export([new/0, new/1, new/2, new/3]).
-export([from_map/1]).

-type t() :: #{
  topic   => binary() | nil,
  event   => binary() | nil,
  payload => term(),
  chat_room_t  => broadcast
}.

-export_type([t/0]).

new() ->
  new(nil).

new(Topic) ->
  new(Topic, nil).

new(Topic, Event) ->
  new(Topic, Event, nil).

-spec new(binary() | nil, binary() | nil, term()) -> t().
new(Topic, Event, Payload)
    when (is_binary(Topic) orelse Topic =:= nil)
    andalso (is_binary(Event) orelse Event =:= nil) ->
  #{
    topic   => Topic,
    event   => Event,
    payload => Payload,
    chat_room_t  => broadcast
  }.

-spec from_map(map()) -> t().
from_map(Map) ->
  BinKey = fun(K, V, Acc) -> Acc#{chat_room_common:to_bin(K) => V} end,
  BinMap = maps:fold(BinKey, #{}, Map),
  #{
    topic   => maps:get(<<"topic">>, BinMap, nil),
    event   => maps:get(<<"event">>, BinMap, nil),
    payload => maps:get(<<"payload">>, BinMap, nil),
    chat_room_t  => broadcast
  }.
