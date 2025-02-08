-module(ebus_supervisor).

-behaviour(supervisor).

-export([start_link/2, start_link/3]).

-export([init/1]).

-type name() :: atom() | {global, term()} | {via, module(), term()}.

-type options() :: #{
  name      => name(),
  strategy  => supervisor:strategy(),
  intensity => non_neg_integer(),
  period    => pos_integer()
}.

-spec start_link(
  [supervisor:child_spec()], options()
) -> supervisor:startlink_ret().
start_link(Children, Options) when is_list(Children) ->
  Spec = ebus_supervisor_spec:supervise(Children, Options),
  start_link(?MODULE, Spec, Options).

-spec start_link(module(), term(), options()) -> supervisor:startlink_ret().
start_link(Module, Arg, Options) ->
  case maps:get(name, Options, nil) of
    nil ->
      supervisor:start_link(Module, Arg);
    Atom when is_atom(Atom) ->
      supervisor:start_link({local, Atom}, Module, Arg);
    Other when is_tuple(Other) ->
      supervisor:start_link(Other, Module, Arg)
  end.

init(Args) -> Args.
