-module(other).
-compile(no_auto_import).

run() ->
    case {1, 2.0} of
        {_, _} ->
            1
    end.
