-module(hello_world).
-compile(no_auto_import).

-export([x/0]).

fully_typed(First) ->
    X = <<"This is a string\nThat has a newline">>,
    First + 1.

id(X, Y) ->
    X.

x() ->
    id(1.0, 1).
