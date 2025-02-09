-module(chat_room_ps_local_sup).

-behaviour(supervisor).

-export([start_link/3]).

-export([init/1]).

-type command()       :: broadcast | subscribe | unsubscribe.
-type dispatch_rule() :: {command(), module(), [term()]}.

-export_type([dispatch_rule/0]).

-spec start_link(
  atom(), pos_integer(), [dispatch_rule()]
) -> supervisor:startlink_ret().
start_link(Server, PoolSize, DispatchRules) ->
  supervisor:start_link(?MODULE, [Server, PoolSize, DispatchRules]).

init([Server, PoolSize, DispatchRules]) ->

  Server = ets:new(Server, [set, named_table, {read_concurrency, true}]),

  true =
    ets:insert(Server, [
      {subscribe, chat_room_ps_local, [Server, PoolSize]},
      {unsubscribe, chat_room_ps_local, [Server, PoolSize]},
      {subscribers, chat_room_ps_local, [Server, PoolSize]},
      {list, chat_room_ps_local, [Server, PoolSize]}
      | DispatchRules
    ]),

  ChildrenFun = fun(Shard) ->
    LocalShardName = chat_room_ps_local:local_name(Server, Shard),
    GCShardName    = chat_room_ps_local:gc_name(Server, Shard),

    true = ets:insert(Server, {Shard, LocalShardName, GCShardName}),

    ShardChildren = [
      chat_room_supervisor_spec:worker(chat_room_ps_gc, [GCShardName, LocalShardName]),
      chat_room_supervisor_spec:worker(chat_room_ps_local, [LocalShardName, GCShardName])
    ],

    chat_room_supervisor_spec:supervisor(
      chat_room_supervisor,
      [ShardChildren, #{strategy => one_for_all}],
      #{id => Shard}
    )
  end,

  Children = [ChildrenFun(C) || C <- lists:seq(0, PoolSize - 1)],

  chat_room_supervisor_spec:supervise(Children, #{strategy => one_for_one}).
