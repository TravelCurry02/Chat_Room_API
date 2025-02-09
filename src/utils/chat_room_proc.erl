-module(chat_room_proc).

-export([wait_for_msg/0, wait_for_msg/1]).
-export([messages/0, messages/1, r_messages/1]).
-export([spawn_timer_fun/1]).
-export([spawn_handler/1, spawn_handler/2, spawn_handler/3]).
-export([spawn_callback_handler/3, spawn_callback_handler/4]).

wait_for_msg() ->
  wait_for_msg(infinity).

-spec wait_for_msg(timeout()) -> term() | {error, timeout}.
wait_for_msg(Timeout) ->
  receive
    Msg -> Msg
  after
    Timeout -> {error, timeout}
  end.

messages() ->
  messages(self()).

-spec messages(pid()) -> [term()].
messages(Pid) ->
  {messages, Messages} = erlang:process_info(Pid, messages),
  Messages.

-spec r_messages(pid()) -> [term()].
r_messages(Pid) ->
  {messages, Messages} = rpc:pinfo(Pid, messages),
  Messages.

-spec spawn_timer_fun(timeout()) -> pid().
spawn_timer_fun(Timeout) ->
  spawn_link(fun() -> timer:sleep(Timeout) end).

spawn_handler(Fun) ->
  spawn_handler(Fun, []).

spawn_handler(Fun, Args) ->
  spawn_handler(Fun, Args, []).

spawn_handler(Fun, Args, Opts) ->
  spawn_callback_handler(erlang, apply, [Fun, Args], Opts).

spawn_callback_handler(Module, Fun, Args) ->
  spawn_callback_handler(Module, Fun, Args, []).

-spec spawn_callback_handler(
  module(), atom(), [term()], [term()]
) -> pid() | {pid(), reference()}.
spawn_callback_handler(Module, Fun, Args, Opts)
    when is_atom(Module), is_atom(Fun) ->
  spawn_opt(fun() -> handle(Module, Fun, Args) end, Opts).

handle(erlang, apply, [Fun, FunArgs] = Args) ->
  receive
    Message ->
      apply(erlang, apply, [Fun, [Message | FunArgs]]),
      handle(erlang, apply, Args)
  end;
handle(Module, Fun, Args) ->
  receive
    Message ->
      apply(Module, Fun, [Message | Args]),
      handle(Module, Fun, Args)
  end.
