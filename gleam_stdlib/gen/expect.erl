-module(expect).

-export([equal/2, not_equal/2, true/1, false/1, fail/0]).

equal(A, B) ->
    gleam__stdlib:expect_equal(A, B).

not_equal(A, B) ->
    gleam__stdlib:expect_not_equal(A, B).

true(A) ->
    gleam__stdlib:expect_true(A).

false(A) ->
    gleam__stdlib:expect_false(A).

fail() ->
    true(false).
