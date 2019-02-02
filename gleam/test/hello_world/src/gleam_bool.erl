-module(gleam_bool).

-export([not/1, compare/2, max/2, min/2, to_int/1]).

not(Bool) ->
    case Bool of
        'true' ->
            'false';

        'false' ->
            'true'
    end.

compare(A, B) ->
    case {A, B} of
        {'true', 'true'} ->
            'eq';

        {'true', 'false'} ->
            'gt';

        {'false', 'false'} ->
            'eq';

        {'false', 'true'} ->
            'gt'
    end.

max(A, B) ->
    case A of
        'true' ->
            'true';

        'false' ->
            B
    end.

min(A, B) ->
    case A of
        'false' ->
            'false';

        'true' ->
            B
    end.

to_int(Bool) ->
    case Bool of
        'false' ->
            0;

        'true' ->
            1
    end.
