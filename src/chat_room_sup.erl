-module(chat_room_sup).

-export([start_link/0]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  PubSub = application:get_env(chat_room, pubsub, []),
  Name = chat_room_common:keyfind(name, PubSub, chat_room:default_ps_server()),
  Adapter = chat_room_common:keyfind(adapter, PubSub, chat_room_ps_pg2),
  Adapter:start_link(Name, PubSub).
