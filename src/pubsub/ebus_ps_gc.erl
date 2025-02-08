-module(ebus_ps_gc).

-behaviour(gen_server).

-export([
  start_link/2,
  down/2,
  unsubscribe/4
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-spec start_link(atom(), atom()) -> gen_server:start_ret().
start_link(ServerName, LocalName) ->
  gen_server:start_link(
    {local, ServerName}, ?MODULE, [ServerName, LocalName], []
  ).

-spec down(atom(), pid()) -> ok.
down(GCServer, Pid) when is_atom(GCServer) ->
  gen_server:cast(GCServer, {down, Pid}).

-spec unsubscribe(pid(), binary(), atom(), atom()) -> ok.
unsubscribe(Pid, Topic, TopicsTable, PidsTable) ->
  true = ets:match_delete(TopicsTable, {Topic, {Pid, '_'}}),
  true = ets:delete_object(PidsTable, {Pid, Topic}),
  ok.

init([ServerName, LocalName]) ->
  {ok, #{topics => LocalName, pids => ServerName}}.

handle_call({subscription, Pid}, _From, #{pids := Pids} = State) ->
  try
    {reply, ets:lookup_element(Pids, Pid, 2), State}
  catch
    error:badarg -> {reply, [], State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({down, Pid}, #{pids := Pids, topics := Topics} = State) ->
  try
    Topics0 = ets:lookup_element(Pids, Pid, 2),
    lists:foreach(fun(Topic) ->
      true = ets:match_delete(Topics, {Topic, {Pid, '_'}})
    end, Topics0),
    true = ets:match_delete(Pids, {Pid, '_'})
  catch
    error:badarg -> badarg
  end,
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
