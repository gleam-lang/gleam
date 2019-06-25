-module(gleam@expect).
-compile(no_auto_import).

-export([equal/2, not_equal/2, true/1, false/1, is_ok/1, is_error/1, fail/0]).

equal(A, B) ->
    gleam__stdlib:expect_equal(A, B).

not_equal(A, B) ->
    gleam__stdlib:expect_not_equal(A, B).

true(A) ->
    gleam__stdlib:expect_true(A).

false(A) ->
    gleam__stdlib:expect_false(A).

is_ok(A) ->
    gleam__stdlib:expect_is_ok(A).

is_error(A) ->
    gleam__stdlib:expect_is_error(A).

fail() ->
    true(false).
