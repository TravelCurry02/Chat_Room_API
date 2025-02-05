-module(chat_room_broadcast).

%% API
-export([new/0, new/1, new/2, new/3]).
-export([from_map/1]).

%%%===================================================================
%%% Type definitions
%%%===================================================================

%% Defines a message sent from the chat room system to channels and vice-versa.
%% The message format requires the following keys:
%% <ul>
%% <li>`topic': The binary topic or `topic:subtopic` pair namespace,
%% for example `<<"chat">>', `<<"chat:123">>'.</li>
%% <li>`event': The binary event name, for example `<<"chat_join">>'.</li>
%% <li>`payload': The message payload.</li>
%% </ul>
-type t() :: #{
  topic   => binary() | nil,
  event   => binary() | nil,
  payload => term(),
  chat_t  => broadcast
}.

-export_type([t/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% Create a new chat message with no topic.
new() -> 
  new(nil).

%% Create a new chat message with a topic and no event.
new(Topic) -> 
  new(Topic, nil).

%% Create a new chat message with a topic and event, but no payload.
new(Topic, Event) -> 
  new(Topic, Event, nil).

%% Create a new chat message with topic, event, and payload.
-spec new(binary() | nil, binary() | nil, term()) -> t().
new(Topic, Event, Payload)
    when (is_binary(Topic) orelse Topic =:= nil)
    andalso (is_binary(Event) orelse Event =:= nil) ->
  #{
    topic   => Topic,
    event   => Event,
    payload => Payload,
    chat_t  => broadcast
  }.

%% Convert a map to a chat message, extracting topic, event, and payload.
-spec from_map(map()) -> t().
from_map(Map) -> 
  BinKey = fun(K, V, Acc) -> Acc#{chat_common:to_bin(K) => V} end,
  BinMap = maps:fold(BinKey, #{}, Map),
  #{
    topic   => maps:get(<<"topic">>, BinMap, nil),
    event   => maps:get(<<"event">>, BinMap, nil),
    payload => maps:get(<<"payload">>, BinMap, nil),
    chat_t  => broadcast
  }.
