-module(other).
-compile(no_auto_import).

solve_b(Noun) ->
    case Noun of
        99 ->
            Noun1 = Noun,
            1;

        Code ->
            Noun1
