-module(other).
-compile(no_auto_import).

go() ->
    1.

main(X) ->
    B = true,
    case X of
        Y when B =:= X ->
            1
    end.
