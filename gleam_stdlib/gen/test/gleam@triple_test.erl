-module(gleam@triple_test).
-compile(no_auto_import).

-export([first_test/0, second_test/0, third_test/0]).

first_test() ->
    gleam@expect:equal(gleam@triple:first({1, 2, 3}), 1),
    gleam@expect:equal(gleam@triple:first({[], <<"abc">>, 3}), []).

second_test() ->
    gleam@expect:equal(gleam@triple:second({1, 2, 3}), 2),
    gleam@expect:equal(gleam@triple:second({[], <<"abc">>, 3}), <<"abc">>).

third_test() ->
    gleam@expect:equal(gleam@triple:third({1, 2, 3}), 3),
    gleam@expect:equal(gleam@triple:third({[], <<"abc">>, 3}), 3).
