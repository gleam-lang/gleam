-module(gleam_hello_world).

-export([any/2, has_even_leaf/1, person/1, add_age/2, person_with_age/2]).

any(Tree, Predicate) ->
    case Tree of
        {'leaf', I} ->
            Predicate(I);

        {'node', Left, Right} ->
            any(Left, Predicate) orelse any(Right, Predicate)
    end.

has_even_leaf(Tree) ->
    any(Tree, fun(I) -> I rem 2 =:= 0 end).

person(Name) ->
    #{}#{'name' => Name}.

add_age(Record, Age) ->
    Record#{'age' => Age}.

person_with_age(Name, Age) ->
    add_age(person(Name), Age).
