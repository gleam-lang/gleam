-module(hello_world).
-compile(no_auto_import).

-export([x/0]).

fully_typed(First) ->
    First + 1.

id(X, Y) ->
    X.

x() ->
    id(1.0, 1).
