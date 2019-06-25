-module(gleam@bool_test).
-compile(no_auto_import).

-export([negate_test/0, compare_test/0, max_test/0, min_test/0, to_int_test/0]).

negate_test() ->
    gleam@expect:false(gleam@bool:negate(true)),
    gleam@expect:true(gleam@bool:negate(false)).

compare_test() ->
    gleam@expect:equal(gleam@bool:compare(true, true), eq),
    gleam@expect:equal(gleam@bool:compare(true, false), gt),
    gleam@expect:equal(gleam@bool:compare(false, false), eq),
    gleam@expect:equal(gleam@bool:compare(false, true), lt).

max_test() ->
    gleam@expect:equal(gleam@bool:max(true, true), true),
    gleam@expect:equal(gleam@bool:max(true, false), true),
    gleam@expect:equal(gleam@bool:max(false, false), false),
    gleam@expect:equal(gleam@bool:max(false, true), true).

min_test() ->
    gleam@expect:equal(gleam@bool:min(true, true), true),
    gleam@expect:equal(gleam@bool:min(true, false), false),
    gleam@expect:equal(gleam@bool:min(false, false), false),
    gleam@expect:equal(gleam@bool:min(false, true), false).

to_int_test() ->
    gleam@expect:equal(gleam@bool:to_int(true), 1),
    gleam@expect:equal(gleam@bool:to_int(false), 0).
