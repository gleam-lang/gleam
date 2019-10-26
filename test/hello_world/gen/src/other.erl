-module(other).
-compile(no_auto_import).

<<<<<<< HEAD
solve_b(Noun) ->
    case Noun of
        99 ->
            Noun1 = Noun,
            1;

        Code ->
            Noun1
=======
run() ->
    case {1, 2.0} of
        {_, _} ->
            1
>>>>>>> Annotate dep modules
    end.
