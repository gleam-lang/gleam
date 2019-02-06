-module(gleam_hello_world).

-export([any/2, has_even_leaf/1]).

any(Tree, Predicate) ->
    case Tree of
        {'leaf', I} ->
            Predicate(I);

        {'node', Left, Right} ->
            any(Left, Predicate) orelse any(Right, Predicate)
    end.

has_even_leaf(Tree) ->
    any(Tree, fun(I) -> I rem 2 =:= 0 end).
