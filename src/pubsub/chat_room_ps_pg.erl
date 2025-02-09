-module(chat_room_ps_pg).

-export([
  child_spec/0,
  join/2,
  leave/2,
  get_members/1
]).

-ifndef(OTP_RELEASE).

  -define(OTP_RELEASE, 20).
-endif.

-if(?OTP_RELEASE >= 23).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
  #{
    id => ?MODULE,
    start => {pg, start_link, [?MODULE]}
  }.

-spec join(Group :: atom(), Pid :: pid()) -> ok.
join(Group, Pid) when is_atom(Group), is_pid(Pid) ->
  pg:join(?MODULE, Group, Pid).

-spec leave(Group :: atom(), Pid :: pid()) -> ok.
leave(Group, Pid) when is_atom(Group), is_pid(Pid) ->
  pg:leave(?MODULE, Group, Pid).

-spec get_members(Group :: atom()) -> [pid()].
get_members(Group) when is_atom(Group) ->
  pg:get_members(?MODULE, Group).

-else.

child_spec() -> undefined.

-spec join(Group :: atom(), Pid :: pid()) -> ok.
join(Group, Pid) when is_atom(Group), is_pid(Pid) ->
  pg2:join(ensure_namespace(Group), Pid).

-spec leave(Group :: atom(), Pid :: pid()) -> ok.
leave(Group, Pid) when is_atom(Group), is_pid(Pid) ->
  pg2:leave(ensure_namespace(Group), Pid).

-spec get_members(Group :: atom()) -> [pid()].
get_members(Group) when is_atom(Group) ->
  pg2:get_members(ensure_namespace(Group)).

ensure_namespace(Group) ->
  Namespace = {chat_room, Group},
  ok = pg2:create(Namespace),
  Namespace.

-endif.
