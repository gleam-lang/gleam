-module(other).
-compile(no_auto_import).

main(X) ->
    case X of
        Y when X =:= Y ->
            1
    end.
