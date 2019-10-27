-module(hello_test).
-compile(no_auto_import).

-export([app_test/0]).

app_test() ->
    hello_world:x().
