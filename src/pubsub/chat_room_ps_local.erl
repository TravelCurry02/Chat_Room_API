-module(chat_room_ps_local).

-behaviour(gen_server).

-export([
  start_link/2,
  subscribe/4,
  subscribe/5,
  unsubscribe/4,
  broadcast/5,
  subscribers/3,
  subscribers_by_shard/3,
  subscribers_with_fastlanes/3,
  list/2,
  list_by_shard/2,
  subscription/3,
  local_name/2,
  gc_name/2
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-type fastlane() :: {
  FastlanePid     :: pid(),
  Serializer      :: module(),
  EventIntercepts :: [term()]
}.

-type option()  :: {link, _} | {fastlane, fastlane()}.

-type options() :: [option()].

-export_type([fastlane/0, option/0, options/0]).

-spec start_link(atom(), atom()) -> gen_server:start_ret().
start_link(ServerName, GCName) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [ServerName, GCName], []).

subscribe(Server, PoolSize, Pid, Topic) ->
  subscribe(Server, PoolSize, Pid, Topic, []).

-spec subscribe(atom(), pos_integer(), pid(), binary(), options()) -> ok.
subscribe(Server, PoolSize, Pid, Topic, Opts) when is_atom(Server) ->
  {ok, {Topics, Pids}} = gen_server:call(
    local_for_pid(Server, Pid, PoolSize),
    {subscribe, Pid, Topic, Opts}
  ),
  Fastlane = chat_room_common:keyfind(fastlane, Opts),
  true = ets:insert(Topics, {Topic, {Pid, Fastlane}}),
  true = ets:insert(Pids, {Pid, Topic}),
  ok.

-spec unsubscribe(atom(), pos_integer(), pid(), binary()) -> ok.
unsubscribe(Server, PoolSize, Pid, Topic) when is_atom(Server) ->
  {LocalServer, GCServer} = pools_for_shard(
    pid_to_shard(Pid, PoolSize), Server
  ),
  ok = chat_room_ps_gc:unsubscribe(Pid, Topic, LocalServer, GCServer).

-spec broadcast(atom(), pos_integer(), pid(), binary(), term()) -> ok.
broadcast(Server, 1, From, Topic, Msg) when is_atom(Server) ->
  do_broadcast(Server, 0, From, Topic, Msg),
  ok;
broadcast(Server, PoolSize, From, Topic, Msg) when is_atom(Server) ->
  lists:foreach(fun(Shard) ->
    do_broadcast(Server, Shard, From, Topic, Msg)
  end, lists:seq(0, PoolSize - 1)).

do_broadcast(Server, Shard, From, Topic,
             #{chat_room_t := broadcast, event := Event} = Msg) ->
  Reduce = fun
    ({Pid, _Fastlanes}, Cache) when Pid == From ->
      Cache;
    ({Pid, nil}, Cache) ->
      Pid ! Msg,
      Cache;
    ({Pid, {FastlanePid, Serializer, EventIntercepts}}, Cache) ->
      case lists:member(Event, EventIntercepts) of
        true ->
          Pid ! Msg,
          Cache;
        _ ->
          case maps:get(Serializer, Cache, nil) of
            nil ->
              EncodedMsg = Serializer:fastlane(Msg),
              FastlanePid ! EncodedMsg,
              maps:put(Serializer, EncodedMsg, Cache);
            EncodedMsg ->
              FastlanePid ! EncodedMsg,
              Cache
          end
      end
  end,
  Subscribers = subscribers_with_fastlanes(Server, Topic, Shard),
  lists:foldl(Reduce, #{}, Subscribers);
do_broadcast(Server, Shard, From, Topic, Msg) ->
  lists:foreach(fun
    (Pid) when Pid == From ->
      noop;
    (Pid) ->
      Pid ! Msg
  end, subscribers_by_shard(Server, Topic, Shard)).

-spec subscribers(atom(), pos_integer(), binary()) -> [pid()].
subscribers(Server, PoolSize, Topic) when is_atom(Server) ->
  lists:foldl(fun(Shard, Acc) ->
    Acc ++ subscribers_by_shard(Server, Topic, Shard)
  end, [], lists:seq(0, PoolSize - 1)).

-spec subscribers_by_shard(atom(), binary(), non_neg_integer()) -> [pid()].
subscribers_by_shard(Server, Topic, Shard) when is_atom(Server) ->
  Pids = subscribers_with_fastlanes(Server, Topic, Shard),
  [Pid || {Pid, _Fastlanes} <- Pids].

-spec subscribers_with_fastlanes(
  atom(), binary(), non_neg_integer()
) -> [{pid(), nil | term()}].
subscribers_with_fastlanes(Server, Topic, Shard) when is_atom(Server) ->
  try
    ets:lookup_element(local_for_shard(Shard, Server), Topic, 2)
  catch
    error:badarg -> []
  end.

-spec list(atom(), pos_integer()) -> [binary()].
list(Server, PoolSize) ->
  lists:foldl(fun(Shard, Acc) ->
    Acc ++ list_by_shard(Server, Shard)
  end, [], lists:seq(0, PoolSize - 1)).

-spec list_by_shard(atom(), non_neg_integer()) -> [binary()].
list_by_shard(Server, Shard) when is_atom(Server) ->
  lists:usort(ets:select(
    local_for_shard(Shard, Server),
    [{{'$1', '_'}, [], ['$1']}]
  )).

-spec subscription(atom(), non_neg_integer(), pid()) -> [binary()].
subscription(Server, PoolSize, Pid) when is_atom(Server) ->
  {_Local, GCServer} = pools_for_shard(
    pid_to_shard(Pid, PoolSize), Server
  ),
  gen_server:call(GCServer, {subscription, Pid}).

-spec local_name(atom(), non_neg_integer()) -> atom().
local_name(Server, Shard) ->
  chat_room_common:build_name([Server, <<"local">>, Shard], <<"_">>).

-spec gc_name(atom(), non_neg_integer()) -> atom().
gc_name(Server, Shard) ->
  chat_room_common:build_name([Server, <<"gc">>, Shard], <<"_">>).

init([Local, GC]) ->
  TabOpts = [
    duplicate_bag,
    named_table,
    public,
    {read_concurrency, true},
    {write_concurrency, true}
  ],
  Local = ets:new(Local, TabOpts),
  GC = ets:new(GC, TabOpts),
  process_flag(trap_exit, true),
  {ok, #{topics => Local, pids => GC, gc_server => GC}}.

handle_call({subscribe, Pid, _Topic, Opts}, _From,
            #{topics := Topics, pids := Pids} = State) ->
  case chat_room_common:keyfind(link, Opts) of
    nil -> ok;
    _   -> link(Pid)
  end,
  erlang:monitor(process, Pid),
  {reply, {ok, {Topics, Pids}}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, _Type, Pid, _Info},
            #{gc_server := GCServer} = State) ->
  chat_room_ps_gc:down(GCServer, Pid),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

local_for_pid(Server, Pid, PoolSize) ->
  local_for_shard(pid_to_shard(Pid, PoolSize), Server).

local_for_shard(Shard, Server) ->
  {LocalServer, _GCServer} = pools_for_shard(Shard, Server),
  LocalServer.

pools_for_shard(Shard, Server) ->
  [{Shard, ShardServer, GCServer}] = ets:lookup(Server, Shard),
  {ShardServer, GCServer}.

pid_to_shard(Pid, ShardSize) ->
  pid_id(Pid) rem ShardSize.

pid_id(Pid) ->
  Binary = term_to_binary(Pid),
  Prefix = (byte_size(Binary) - 9) * 8,
  <<_:Prefix, Id:32, _:40>> = Binary,
  Id.
