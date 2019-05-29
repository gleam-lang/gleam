-module(std@bool_test).
-compile(no_auto_import).

-export([negate_test/0, compare_test/0, max_test/0, min_test/0, to_int_test/0]).

negate_test() ->
    std@expect:false(std@bool:negate(true)),
    std@expect:true(std@bool:negate(false)).

compare_test() ->
    std@expect:equal(std@bool:compare(true, true), eq),
    std@expect:equal(std@bool:compare(true, false), gt),
    std@expect:equal(std@bool:compare(false, false), eq),
    std@expect:equal(std@bool:compare(false, true), lt).

max_test() ->
    std@expect:equal(std@bool:max(true, true), true),
    std@expect:equal(std@bool:max(true, false), true),
    std@expect:equal(std@bool:max(false, false), false),
    std@expect:equal(std@bool:max(false, true), true).

min_test() ->
    std@expect:equal(std@bool:min(true, true), true),
    std@expect:equal(std@bool:min(true, false), false),
    std@expect:equal(std@bool:min(false, false), false),
    std@expect:equal(std@bool:min(false, true), false).

to_int_test() ->
    std@expect:equal(std@bool:to_int(true), 1),
    std@expect:equal(std@bool:to_int(false), 0).
