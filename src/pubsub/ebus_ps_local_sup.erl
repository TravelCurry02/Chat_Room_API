-module(ebus_ps_local_sup).

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
      {subscribe, ebus_ps_local, [Server, PoolSize]},
      {unsubscribe, ebus_ps_local, [Server, PoolSize]},
      {subscribers, ebus_ps_local, [Server, PoolSize]},
      {list, ebus_ps_local, [Server, PoolSize]}
      | DispatchRules
    ]),

  ChildrenFun = fun(Shard) ->
    LocalShardName = ebus_ps_local:local_name(Server, Shard),
    GCShardName    = ebus_ps_local:gc_name(Server, Shard),

    true = ets:insert(Server, {Shard, LocalShardName, GCShardName}),

    ShardChildren = [
      ebus_supervisor_spec:worker(ebus_ps_gc, [GCShardName, LocalShardName]),
      ebus_supervisor_spec:worker(ebus_ps_local, [LocalShardName, GCShardName])
    ],

    ebus_supervisor_spec:supervisor(
      ebus_supervisor,
      [ShardChildren, #{strategy => one_for_all}],
      #{id => Shard}
    )
  end,

  Children = [ChildrenFun(C) || C <- lists:seq(0, PoolSize - 1)],

  ebus_supervisor_spec:supervise(Children, #{strategy => one_for_one}).
