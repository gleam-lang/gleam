-module(gleam__stdlib).
-include_lib("eunit/include/eunit.hrl").

-export([expect_equal/2, expect_not_equal/2, expect_true/1, expect_false/1, map_fetch/2,
         iodata_append/2, iodata_prepend/2, identity/1]).

expect_equal(A, Expected) -> ?assertEqual(Expected, A).
expect_not_equal(A, Expected) -> ?assertNotEqual(Expected, A).
expect_true(A) -> ?assert(A).
expect_false(A) -> ?assertNot(A).

map_fetch(Map, Key) ->
  case maps:find(Key, Map) of
    error -> {error, not_found};
    OkFound -> OkFound
  end.

iodata_append(Iodata, String) -> [Iodata, String].
iodata_prepend(Iodata, String) -> [String, Iodata].

identity(X) -> X.
