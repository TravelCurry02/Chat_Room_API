-module(chat_room_ps).

%% API
-export([
  subscribe/3,
  subscribe/4,
  unsubscribe/3,
  broadcast/3,
  broadcast_from/4,
  subscribers/2,
  list/1
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type options() :: chat_room_ps_local:options().

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv subscribe(Server, Pid, Topic, [])
subscribe(Server, Pid, Topic) -> 
  subscribe(Server, Pid, Topic, []).

%% Subscribes the pid to the PubSub adapter's topic.
-spec subscribe(atom(), pid(), binary(), options()) -> ok | {error, term()}.
subscribe(Server, Pid, Topic, Opts) when is_atom(Server) ->
  call(Server, subscribe, [Pid, Topic, Opts]).

%% Unsubscribes the pid from the PubSub adapter's topic.
-spec unsubscribe(atom(), pid(), binary()) -> ok | {error, term()}.
unsubscribe(Server, Pid, Topic) when is_atom(Server) ->
  call(Server, unsubscribe, [Pid, Topic]).

%% Broadcasts message on the given topic.
-spec broadcast(atom(), binary(), term()) -> ok | {error, term()}.
broadcast(Server, Topic, Msg) when is_atom(Server) ->
  call(Server, broadcast, [none, Topic, Msg]).

%% Broadcasts message to all but `FromPid` on the given topic.
-spec broadcast_from(atom(), pid(), binary(), term()) -> ok | {error, term()}.
broadcast_from(Server, FromPid, Topic, Msg)
    when is_atom(Server), is_pid(FromPid) ->
  call(Server, broadcast, [FromPid, Topic, Msg]).

%% Returns a set of subscribers' pids for the given topic.
-spec subscribers(atom(), binary()) -> [pid()].
subscribers(Server, Topic) when is_atom(Server) ->
  call(Server, subscribers, [Topic]).

%% Returns the topic list. DO NOT USE IN PROD.
-spec list(atom()) -> [binary()].
list(Server) when is_atom(Server) ->
  call(Server, list, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
call(Server, Kind, Args) -> 
  [{Kind, Module, Head}] = ets:lookup(Server, Kind),
  apply(Module, Kind, Head ++ Args).
