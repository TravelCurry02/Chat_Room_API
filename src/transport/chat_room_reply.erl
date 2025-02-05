-module(chat_room_reply).

%% API
-export([new/0, new/1, new/2, new/3, new/4]).
-export([from_map/1]).

%%%===================================================================
%%% Type definitions
%%%===================================================================

%% Defines a reply sent from chat room channels to the transport layer.
%% The message format requires the following keys:
%% <ul>
%% <li>`topic': The binary topic or `topic:subtopic` pair namespace,
%% for example `<<"chat">>', `<<"chat:123">>'.</li>
%% <li>`status': The reply status as an atom.</li>
%% <li>`payload': The message payload.</li>
%% <li>`ref': The unique binary ref.</li>
%% </ul>
-type t() :: #{
  topic   => binary() | nil,
  status  => atom(),
  payload => term(),
  ref     => binary() | nil,
  chat_t  => reply
}.

-export_type([t/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% Create a new chat reply with no topic.
new() -> 
  new(nil).

%% Create a new chat reply with a topic and no status.
new(Topic) -> 
  new(Topic, nil).

%% Create a new chat reply with a topic and status, but no payload.
new(Topic, Status) -> 
  new(Topic, Status, nil).

%% Create a new chat reply with topic, status, and payload.
%% Optionally, you can include a reference.
-spec new(binary() | nil, atom(), term(), binary() | nil) -> t().
new(Topic, Status, Payload, Ref)
    when (is_binary(Topic) orelse Topic =:= nil) andalso is_atom(Status) ->
  #{
    topic   => Topic,
    status  => Status,
    payload => Payload,
    ref     => Ref,
    chat_t  => reply
  }.

%% Convert a map to a chat reply, extracting topic, status, payload, and ref.
-spec from_map(map()) -> t().
from_map(Map) -> 
  BinKey = fun(K, V, Acc) -> Acc#{chat_common:to_bin(K) => V} end,
  BinMap = maps:fold(BinKey, #{}, Map),
  #{
    topic   => maps:get(<<"topic">>, BinMap, nil),
    status  => chat_common:to_atom(maps:get(<<"status">>, BinMap, nil)),
    payload => maps:get(<<"payload">>, BinMap, nil),
    ref     => maps:get(<<"ref">>, BinMap, nil),
    chat_t  => reply
  }.
