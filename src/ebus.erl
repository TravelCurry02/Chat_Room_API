-module(ebus).

-behaviour(application).

-export([sub/2, sub/3, sub/4]).
-export([unsub/2, unsub/3]).
-export([pub/2, pub/3, pub_from/3, pub_from/4]).
-export([subscribers/1, subscribers/2]).
-export([local_subscribers/1, local_subscribers/2]).
-export([topics/0, topics/1, local_topics/0, local_topics/1]).
-export([dispatch/2, dispatch/3, dispatch/4]).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

-export([server/0, default_ps_server/0]).

-type topic() :: iodata().

-type handler() :: pid().

-type dispatch_fun() :: fun(([term()]) -> term()).

-type dispatch_opt() :: {scope, local | global} |
                        {dispatch_fun, dispatch_fun()}.

-type dispatch_opts() :: [dispatch_opt()].

-type options() :: ebus_ps_local:options().

-export_type([
  topic/0,
  handler/0,
  dispatch_fun/0,
  dispatch_opts/0,
  options/0
]).

sub(Handler, Topic) ->
  sub(server(), Handler, Topic).

sub(Server, Handler, Topic) ->
  sub(Server, Handler, Topic, []).

-spec sub(atom(), handler(), topic(), options()) -> ok | {error, term()}.
sub(Server, Handler, Topic, Opts) ->
  ebus_ps:subscribe(Server, Handler, ebus_common:to_bin(Topic), Opts).

unsub(Handler, Topic) ->
  unsub(server(), Handler, Topic).

-spec unsub(atom(), handler(), topic()) -> ok | {error, term()}.
unsub(Server, Handler, Topic) ->
  ebus_ps:unsubscribe(Server, Handler, ebus_common:to_bin(Topic)).

pub(Topic, Message) ->
  pub(server(), Topic, Message).

-spec pub(atom(), topic(), term()) -> ok | {error, term()}.
pub(Server, Topic, Message) ->
  ebus_ps:broadcast(Server, ebus_common:to_bin(Topic), Message).

pub_from(From, Topic, Message) ->
  pub_from(server(), From, Topic, Message).

-spec pub_from(atom(), handler(), topic(), term()) -> ok | {error, term()}.
pub_from(Server, FromHandler, Topic, Message) ->
  BinTopic = ebus_common:to_bin(Topic),
  ebus_ps:broadcast_from(Server, FromHandler, BinTopic, Message).

subscribers(Topic) ->
  subscribers(server(), Topic).

-spec subscribers(atom(), topic()) -> [pid()].
subscribers(Server, Topic) ->
  BinTopic = ebus_common:to_bin(Topic),
  {ResL, _} = rpc:multicall(?MODULE, local_subscribers, [Server, BinTopic]),
  lists:merge(ResL).

local_subscribers(Topic) ->
  local_subscribers(server(), Topic).

-spec local_subscribers(atom(), topic()) -> [pid()].
local_subscribers(Server, Topic) ->
  ebus_ps:subscribers(Server, ebus_common:to_bin(Topic)).

topics() ->
  topics(server()).

-spec topics(atom()) -> [binary()].
topics(Server) ->
  {ResL, _} = rpc:multicall(?MODULE, local_topics, [Server]),
  lists:usort(lists:merge(ResL)).

local_topics() ->
  local_topics(server()).

-spec local_topics(atom()) -> [binary()].
local_topics(Server) ->
  ebus_ps:list(Server).

dispatch(Topic, Message) ->
  dispatch(Topic, Message, []).

dispatch(Topic, Message, Opts) ->
  dispatch(server(), Topic, Message, Opts).

-spec dispatch(atom(), topic(), term(), dispatch_opts()) -> ok.
dispatch(Server, Topic, Message, Opts) ->
  BinTopic = ebus_common:to_bin(Topic),
  Subscribers = case ebus_common:keyfind(scope, Opts, local) of
    local -> local_subscribers(Server, BinTopic);
    _     -> subscribers(Server, BinTopic)
  end,
  DispatchFun = case ebus_common:keyfind(dispatch_fun, Opts) of
    nil -> fun ebus_common:rand_elem/1;
    Fun -> Fun
  end,
  case Subscribers of
    [] -> throw(no_subscribers_available);
    _  -> DispatchFun(Subscribers) ! Message, ok
  end.

-spec start() -> {ok, _} | {error, term()}.
start() -> application:ensure_all_started(ebus).

-spec stop() -> ok | {error, term()}.
stop() -> application:stop(ebus).

start(_StartType, _StartArgs) -> ebus_sup:start_link().

stop(_State) -> ok.

-spec server() -> atom().
server() ->
  PubSub = application:get_env(ebus, pubsub, []),
  ebus_common:keyfind(name, PubSub, default_ps_server()).

-spec default_ps_server() -> ebus_ps.
default_ps_server() -> ebus_ps.
