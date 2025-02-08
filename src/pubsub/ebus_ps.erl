-module(ebus_ps).

-export([
  subscribe/3,
  subscribe/4,
  unsubscribe/3,
  broadcast/3,
  broadcast_from/4,
  subscribers/2,
  list/1
]).

-type options() :: ebus_ps_local:options().

subscribe(Server, Pid, Topic) ->
  subscribe(Server, Pid, Topic, []).

-spec subscribe(atom(), pid(), binary(), options()) -> ok | {error, term()}.
subscribe(Server, Pid, Topic, Opts) when is_atom(Server) ->
  call(Server, subscribe, [Pid, Topic, Opts]).

-spec unsubscribe(atom(), pid(), binary()) -> ok | {error, term()}.
unsubscribe(Server, Pid, Topic) when is_atom(Server) ->
  call(Server, unsubscribe, [Pid, Topic]).

-spec broadcast(atom(), binary(), term()) -> ok | {error, term()}.
broadcast(Server, Topic, Msg) when is_atom(Server) ->
  call(Server, broadcast, [none, Topic, Msg]).

-spec broadcast_from(atom(), pid(), binary(), term()) -> ok | {error, term()}.
broadcast_from(Server, FromPid, Topic, Msg)
    when is_atom(Server), is_pid(FromPid) ->
  call(Server, broadcast, [FromPid, Topic, Msg]).

-spec subscribers(atom(), binary()) -> [pid()].
subscribers(Server, Topic) when is_atom(Server) ->
  call(Server, subscribers, [Topic]).

-spec list(atom()) -> [binary()].
list(Server) when is_atom(Server) ->
  call(Server, list, []).

call(Server, Kind, Args) ->
  [{Kind, Module, Head}] = ets:lookup(Server, Kind),
  apply(Module, Kind, Head ++ Args).
