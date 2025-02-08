-module(ebus_common).

-export([keyfind/2, keyfind/3, rand_elem/1]).
-export([build_name/1, build_name/2]).
-export([to_bin/1, to_atom/1, to_int/1, to_float/1, to_list/1]).
-export([wait_until/1, wait_until/2, wait_until/3]).

keyfind(Key, TupleList) ->
  keyfind(Key, TupleList, nil).

-spec keyfind(term(), [tuple()], term()) -> term().
keyfind(Key, TupleList, Default) ->
  case lists:keyfind(Key, 1, TupleList) of
    {Key, V} -> V;
    _        -> Default
  end.

-spec rand_elem([term()]) -> term().
rand_elem(L) when is_list(L), length(L) > 0 ->
  N = (erlang:phash2(os:timestamp()) rem length(L)) + 1,
  lists:nth(N, L).

build_name(L) ->
  build_name(L, <<"_">>).

-spec build_name([any()], iodata()) -> atom().
build_name(L, Separator) when is_list(L) ->
  Fun = fun
    (X, <<"">>) ->
      <<(to_bin(X))/binary>>;
    (X, Acc) ->
      <<Acc/binary, (to_bin(Separator))/binary, (to_bin(X))/binary>>
  end,
  binary_to_atom(lists:foldl(Fun, <<"">>, L), utf8).

-spec to_bin(any()) -> binary().
to_bin(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_bin(Data) when is_float(Data) ->
  float_to_binary(Data);
to_bin(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_bin(Data) when is_list(Data) ->
  iolist_to_binary(Data);
to_bin(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  integer_to_binary(erlang:phash2(Data));
to_bin(Data) ->
  Data.

-spec to_atom(any()) -> atom().
to_atom(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  list_to_atom(integer_to_list(erlang:phash2(Data)));
to_atom(Data) ->
  Data.

-spec to_int(any()) -> integer().
to_int(Data) when is_binary(Data) ->
  binary_to_integer(Data);
to_int(Data) when is_list(Data) ->
  list_to_integer(Data);
to_int(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_int(Data) ->
  Data.

-spec to_float(any()) -> float().
to_float(Data) when is_binary(Data) ->
  binary_to_float(Data);
to_float(Data) when is_list(Data) ->
  list_to_float(Data);
to_float(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_float(Data) ->
  Data.

-spec to_list(any()) -> list().
to_list(Data) when is_binary(Data) ->
  binary_to_list(Data);
to_list(Data) when is_integer(Data) ->
  integer_to_list(Data);
to_list(Data) when is_float(Data) ->
  float_to_list(Data);
to_list(Data) when is_atom(Data) ->
  atom_to_list(Data);
to_list(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  integer_to_list(erlang:phash2(Data));
to_list(Data) ->
  Data.

wait_until(Fun) ->
  wait_until(Fun, 50).

wait_until(Fun, Retries) ->
  wait_until(Fun, Retries, 100).

-spec wait_until(fun(), non_neg_integer(), timeout()) -> term().
wait_until(Fun, 0, _Timeout) ->
  Fun();
wait_until(Fun, Retries, Timeout) ->
  case Fun() of
    true ->
      true;

    false ->
      ok = timer:sleep(Timeout),
      wait_until(Fun, decr_retries(Retries), Timeout)
  end.

decr_retries(infinity) -> infinity;
decr_retries(Val) when is_integer(Val), Val > 0 -> Val - 1.
