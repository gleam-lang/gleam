-module(gleam__stdlib).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

expect_equal(A, Expected) -> ?assertEqual(Expected, A).
expect_not_equal(A, Expected) -> ?assertNotEqual(Expected, A).
expect_true(A) -> ?assert(A).
expect_false(A) -> ?assertNot(A).

map_fetch(Map, Key) ->
  case maps:find(Key, Map) of
    error -> {error, {}},
    OkFound -> OkFound
  end.

iodata_append(X, Data) -> [X | Y].
iodata_prepend(X, Data) -> [Y, X].

identity(X) -> X.
