-module(ebus_ps_pg2_server).

-behaviour(gen_server).

-export([
  start_link/1,
  broadcast/5
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-spec start_link(atom()) -> gen_server:start_ret().
start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, Name, []).

-spec broadcast(
  atom(), pos_integer(), pid(), binary(), any()
) -> ok | {error, no_such_group}.
broadcast(Name, PoolSize, FromPid, Topic, Msg) ->
  lists:foreach(fun
    (Pid) when node(Pid) == node() ->
      ebus_ps_local:broadcast(Name, PoolSize, FromPid, Topic, Msg);
    (Pid) ->
      Pid ! {forward_to_local, FromPid, PoolSize, Topic, Msg}
  end, ebus_ps_pg:get_members(Name)).

init(Name) ->
  ok = ebus_ps_pg:join(Name, self()),
  {ok, Name}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({forward_to_local, FromPid, PoolSize, Topic, Msg}, Name) ->

  ebus_ps_local:broadcast(Name, PoolSize, FromPid, Topic, Msg),
  {noreply, Name};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
