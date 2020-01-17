-module(other).
-compile(no_auto_import).

main() ->
    case {1, 2} of
        {_, _} ->
            1
    end.
