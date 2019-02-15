-module(gleam_hello_world).

-export([any/2, has_even_leaf/1, person/1, put_age/2, person_with_age/2, get_age/1, multiline/0, not/1]).

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

put_age(Record, Age) ->
    Record#{'age' => Age}.

person_with_age(Name, Age) ->
    put_age(person(Name), Age).

get_age(Record) ->
    maps:get('age', Record).

multiline() ->
    <<"hello
  \"
world">>.

not(B) ->
    bool:not(B).
