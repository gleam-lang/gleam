-module(hello_world).
-compile(no_auto_import).

-export([x/0]).

id(X, Y) ->
    X.

x() ->
    id(1.0, 1).
