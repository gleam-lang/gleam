-module(gleam@bitwise).
-compile(no_auto_import).

-export(['and'/2, 'not'/1, 'or'/2, exclusive_or/2, shift_left/2, shift_right/2]).

-spec 'and'(integer(), integer()) -> integer().
'and'(X, Y) ->
    erlang:'band'(X, Y).

-spec 'not'(integer()) -> integer().
'not'(X) ->
    erlang:'bnot'(X).

-spec 'or'(integer(), integer()) -> integer().
'or'(X, Y) ->
    erlang:'bor'(X, Y).

-spec exclusive_or(integer(), integer()) -> integer().
exclusive_or(X, Y) ->
    erlang:'bxor'(X, Y).

-spec shift_left(integer(), integer()) -> integer().
shift_left(X, Y) ->
    erlang:'bsl'(X, Y).

-spec shift_right(integer(), integer()) -> integer().
shift_right(X, Y) ->
    erlang:'bsr'(X, Y).
