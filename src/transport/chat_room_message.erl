-module(chat_room_message).

-export([new/0, new/1, new/2, new/3, new/4]).
-export([from_map/1]).

-type t() :: #{
  topic   => binary() | nil,
  event   => binary() | nil,
  payload => term(),
  ref     => binary() | nil,
  chat_room_t  => message
}.

-export_type([t/0]).

new() ->
  new(nil).

new(Topic) ->
  new(Topic, nil).

new(Topic, Event) ->
  new(Topic, Event, nil).

new(Topic, Event, Payload) ->
  new(Topic, Event, Payload, nil).

-spec new(binary() | nil, binary() | nil, term(), binary() | nil) -> t().
new(Topic, Event, Payload, Ref)
    when (is_binary(Topic) orelse Topic =:= nil)
    andalso (is_binary(Event) orelse Event =:= nil) ->
  #{
    topic   => Topic,
    event   => Event,
    payload => Payload,
    ref     => Ref,
    chat_room_t  => message
  }.

-spec from_map(map()) -> t().
from_map(Map) ->
  BinKey = fun(K, V, Acc) -> Acc#{chat_room_common:to_bin(K) => V} end,
  BinMap = maps:fold(BinKey, #{}, Map),
  #{
    topic   => maps:get(<<"topic">>, BinMap, nil),
    event   => maps:get(<<"event">>, BinMap, nil),
    payload => maps:get(<<"payload">>, BinMap, nil),
    ref     => maps:get(<<"ref">>, BinMap, nil),
    chat_room_t  => message
  }.
