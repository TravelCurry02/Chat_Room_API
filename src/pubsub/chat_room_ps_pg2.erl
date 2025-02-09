-module(chat_room_ps_pg2).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-spec start_link(atom(), [term()]) -> supervisor:startlink_ret().
start_link(Name, Opts) ->
  SupName = chat_room_common:build_name([Name, <<"sup">>], <<"_">>),
  supervisor:start_link({local, SupName}, ?MODULE, [Name, Opts]).

init([Server, Opts]) ->
  PoolSize = chat_room_common:keyfind(pool_size, Opts, 1),
  DispatchRules = [{broadcast, chat_room_ps_pg2_server, [Server, PoolSize]}],

  Children = [
    chat_room_ps_pg:child_spec(),
    chat_room_supervisor_spec:supervisor(
      chat_room_ps_local_sup, [Server, PoolSize, DispatchRules]
    ),
    chat_room_supervisor_spec:worker(chat_room_ps_pg2_server, [Server])
  ],

  chat_room_supervisor_spec:supervise(
    [C || C <- Children, C /= undefined],
    #{strategy => rest_for_one}
  ).
