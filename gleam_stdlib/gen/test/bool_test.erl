-module(bool_test).
-compile(no_auto_import).

-export([negate_test/0, max_test/0, min_test/0, to_int_test/0]).

negate_test() ->
    expect:false(bool:negate(true)),
    expect:true(bool:negate(false)).

max_test() ->
    expect:equal(bool:max(true, true), true),
    expect:equal(bool:max(true, false), true),
    expect:equal(bool:max(false, false), false),
    expect:equal(bool:max(false, true), true).

min_test() ->
    expect:equal(bool:min(true, true), true),
    expect:equal(bool:min(true, false), false),
    expect:equal(bool:min(false, false), false),
    expect:equal(bool:min(false, true), false).

to_int_test() ->
    expect:equal(bool:to_int(true), 1),
    expect:equal(bool:to_int(false), 0).
