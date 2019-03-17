-module(gleam__stdlib).
-include_lib("eunit/include/eunit.hrl").

-export([expect_equal/2, expect_not_equal/2, expect_true/1, expect_false/1, expect_is_ok/1,
         expect_is_error/1, atom_from_string/1, atom_create_from_string/1, atom_to_string/1,
         map_fetch/2, iodata_append/2, iodata_prepend/2, identity/1]).

expect_equal(A, Expected) -> ?assertEqual(Expected, A).
expect_not_equal(A, Expected) -> ?assertNotEqual(Expected, A).
expect_true(A) -> ?assert(A).
expect_false(A) -> ?assertNot(A).
expect_is_ok(A) -> ?assertMatch({ok, _}, A).
expect_is_error(A) -> ?assertMatch({error, _}, A).

map_fetch(Map, Key) ->
  case maps:find(Key, Map) of
    error -> {error, not_found};
    OkFound -> OkFound
  end.

atom_create_from_string(S) ->
  binary_to_atom(S, utf8).

atom_to_string(S) ->
  atom_to_binary(S, utf8).

atom_from_string(S) ->
  try {ok, binary_to_existing_atom(S, utf8)} catch
    error:badarg -> {error, atom_not_loaded}
  end.

iodata_append(Iodata, String) -> [Iodata, String].
iodata_prepend(Iodata, String) -> [String, Iodata].

identity(X) -> X.
