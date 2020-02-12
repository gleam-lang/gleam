-module(other).
-compile(no_auto_import).

go() ->
    1.

main(X) ->
    B = true,
    case X of
        X1 when (X1 =:= X1) =:= (B =:= B) ->
            1
    end.
