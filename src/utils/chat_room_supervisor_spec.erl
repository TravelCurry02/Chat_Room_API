%%%-------------------------------------------------------------------
%%% Utility module for supervisor and worker specs in the chat room.
%%%-------------------------------------------------------------------
-module(chat_room_supervisor_spec).

%% API
-export([supervise/2]).
-export([supervisor/2, supervisor/3]).
-export([worker/2, worker/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% Supervises a list of children with options.
-spec supervise([supervisor:child_spec()], supervisor:sup_flags()) -> {ok, tuple()}.
supervise(Children, SupFlags) ->
  assert_unique_ids([Id || #{id := Id} <- Children]),
  {ok, {SupFlags, Children}}.

%% Defines a supervisor with module and arguments.
-spec supervisor(module(), [term()], map()) -> supervisor:child_spec().
supervisor(Module, Args) ->
  supervisor(Module, Args, #{}).

%% Defines supervisor options with default values.
-spec supervisor(module(), [term()], map()) -> supervisor:child_spec().
supervisor(Module, Args, Spec) when is_map(Spec) ->
  child(supervisor, Module, Args, Spec#{shutdown => infinity});
supervisor(_, _, _) -> throw(invalid_child_spec).

%% Defines a worker with module and arguments.
-spec worker(module(), [term()], map()) -> supervisor:child_spec().
worker(Module, Args) ->
  worker(Module, Args, #{}).

%% Defines worker options with default values.
-spec worker(module(), [term()], map()) -> supervisor:child_spec().
worker(Module, Args, Spec) when is_map(Spec) ->
  child(worker, Module, Args, Spec);
worker(_, _, _) -> throw(invalid_child_spec).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Checks for unique IDs.
assert_unique_ids([]) -> ok;
assert_unique_ids([Id | Rest]) ->
  case lists:member(Id, Rest) of
    true -> throw({badarg, duplicated_id});
    _    -> assert_unique_ids(Rest)
  end.

%% Creates a child spec for supervisor/worker.
child(Type, Module, Args, Spec) when is_map(Spec) ->
  #{
    id       => maps:get(id, Spec, Module),
    start    => maps:get(start, Spec, {Module, start_link, Args}),
    restart  => maps:get(restart, Spec, permanent),
    shutdown => maps:get(shutdown, Spec, 5000),
    type     => Type,
    modules  => maps:get(modules, Spec, [Module])
  }.
