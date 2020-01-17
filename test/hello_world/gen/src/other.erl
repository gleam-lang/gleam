-module(other).
-compile(no_auto_import).

id(X) ->
    X.

main() ->
    id(fun id/1).
