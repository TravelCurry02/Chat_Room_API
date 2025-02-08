-module(ebus_ps_pg2).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-spec start_link(atom(), [term()]) -> supervisor:startlink_ret().
start_link(Name, Opts) ->
  SupName = ebus_common:build_name([Name, <<"sup">>], <<"_">>),
  supervisor:start_link({local, SupName}, ?MODULE, [Name, Opts]).

init([Server, Opts]) ->
  PoolSize = ebus_common:keyfind(pool_size, Opts, 1),
  DispatchRules = [{broadcast, ebus_ps_pg2_server, [Server, PoolSize]}],

  Children = [
    ebus_ps_pg:child_spec(),
    ebus_supervisor_spec:supervisor(
      ebus_ps_local_sup, [Server, PoolSize, DispatchRules]
    ),
    ebus_supervisor_spec:worker(ebus_ps_pg2_server, [Server])
  ],

  ebus_supervisor_spec:supervise(
    [C || C <- Children, C /= undefined],
    #{strategy => rest_for_one}
  ).
