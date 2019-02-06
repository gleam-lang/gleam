-module(gleam_hello_world).

-export([]).

any(Tree, Predicate) ->
    case Tree of
        {'leaf', I} ->
            Predicate(I);

        {'node', Left, Right} ->
            case any(Left, Predicate) of
                'true' ->
                    'true';

                'false' ->
                    any(Right, Predicate)
            end
    end.
