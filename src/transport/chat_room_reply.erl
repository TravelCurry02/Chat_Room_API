-module(chat_room_reply).

-export([new/0, new/1, new/2, new/3, new/4]).
-export([from_map/1]).

-type t() :: #{
  topic   => binary() | nil,
  status  => atom(),
  payload => term(),
  ref     => binary() | nil,
  chat_room_t  => reply
}.

-export_type([t/0]).

new() ->
  new(nil).

new(Topic) ->
  new(Topic, nil).

new(Topic, Status) ->
  new(Topic, Status, nil).

new(Topic, Status, Payload) ->
  new(Topic, Status, Payload, nil).

-spec new(binary() | nil, atom(), term(), binary() | nil) -> t().
new(Topic, Status, Payload, Ref)
    when (is_binary(Topic) orelse Topic =:= nil) andalso is_atom(Status) ->
  #{
    topic   => Topic,
    status  => Status,
    payload => Payload,
    ref     => Ref,
    chat_room_t  => reply
  }.

-spec from_map(map()) -> t().
from_map(Map) ->
  BinKey = fun(K, V, Acc) -> Acc#{chat_room_common:to_bin(K) => V} end,
  BinMap = maps:fold(BinKey, #{}, Map),
  #{
    topic   => maps:get(<<"topic">>, BinMap, nil),
    status  => chat_room_common:to_atom(maps:get(<<"status">>, BinMap, nil)),
    payload => maps:get(<<"payload">>, BinMap, nil),
    ref     => maps:get(<<"ref">>, BinMap, nil),
    chat_room_t  => reply
  }.
