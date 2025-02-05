-module(chat_room_ps_local_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type command()       :: broadcast | subscribe | unsubscribe.
-type dispatch_rule() :: {command(), module(), [term()]}.

-export_type([dispatch_rule/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(
  atom(), pos_integer(), [dispatch_rule()]
) -> supervisor:startlink_ret().
start_link(Server, PoolSize, DispatchRules) ->
  supervisor:start_link(?MODULE, [Server, PoolSize, DispatchRules]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([Server, PoolSize, DispatchRules]) ->
  % Define a dispatch table for routing requests efficiently.
  Server = ets:new(Server, [set, named_table, {read_concurrency, true}]),

  % Store metadata in the dispatch table.
  true =
    ets:insert(Server, [
      {subscribe, chat_room_ps_local, [Server, PoolSize]},
      {unsubscribe, chat_room_ps_local, [Server, PoolSize]},
      {subscribers, chat_room_ps_local, [Server, PoolSize]},
      {list, chat_room_ps_local, [Server, PoolSize]}
      | DispatchRules
    ]),

  % Define children for each shard.
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

  % Create children for all shards.
  Children = [ChildrenFun(C) || C <- lists:seq(0, PoolSize - 1)],

  % Supervise the children with a one-for-one strategy.
  chat_room_supervisor_spec:supervise(Children, #{strategy => one_for_one}).
