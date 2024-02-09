-module(gleam@bool).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export(['and'/2, 'or'/2, negate/1, nor/2, nand/2, exclusive_or/2, exclusive_nor/2, compare/2, max/2, min/2, to_int/1, to_string/1, guard/3, lazy_guard/3]).

-spec 'and'(boolean(), boolean()) -> boolean().
'and'(A, B) ->
    A andalso B.

-spec 'or'(boolean(), boolean()) -> boolean().
'or'(A, B) ->
    A orelse B.

-spec negate(boolean()) -> boolean().
negate(Bool) ->
    case Bool of
        true ->
            false;

        false ->
            true
    end.

-spec nor(boolean(), boolean()) -> boolean().
nor(A, B) ->
    case {A, B} of
        {false, false} ->
            true;

        {false, true} ->
            false;

        {true, false} ->
            false;

        {true, true} ->
            false
    end.

-spec nand(boolean(), boolean()) -> boolean().
nand(A, B) ->
    case {A, B} of
        {false, false} ->
            true;

        {false, true} ->
            true;

        {true, false} ->
            true;

        {true, true} ->
            false
    end.

-spec exclusive_or(boolean(), boolean()) -> boolean().
exclusive_or(A, B) ->
    case {A, B} of
        {false, false} ->
            false;

        {false, true} ->
            true;

        {true, false} ->
            true;

        {true, true} ->
            false
    end.

-spec exclusive_nor(boolean(), boolean()) -> boolean().
exclusive_nor(A, B) ->
    case {A, B} of
        {false, false} ->
            true;

        {false, true} ->
            false;

        {true, false} ->
            false;

        {true, true} ->
            true
    end.

-spec compare(boolean(), boolean()) -> gleam@order:order().
compare(A, B) ->
    case {A, B} of
        {true, true} ->
            eq;

        {true, false} ->
            gt;

        {false, false} ->
            eq;

        {false, true} ->
            lt
    end.

-spec max(boolean(), boolean()) -> boolean().
max(A, B) ->
    case A of
        true ->
            true;

        false ->
            B
    end.

-spec min(boolean(), boolean()) -> boolean().
min(A, B) ->
    case A of
        false ->
            false;

        true ->
            B
    end.

-spec to_int(boolean()) -> integer().
to_int(Bool) ->
    case Bool of
        false ->
            0;

        true ->
            1
    end.

-spec to_string(boolean()) -> binary().
to_string(Bool) ->
    case Bool of
        false ->
            <<"False"/utf8>>;

        true ->
            <<"True"/utf8>>
    end.

-spec guard(boolean(), FGU, fun(() -> FGU)) -> FGU.
guard(Requirement, Consequence, Alternative) ->
    case Requirement of
        true ->
            Consequence;

        false ->
            Alternative()
    end.

-spec lazy_guard(boolean(), fun(() -> FGV), fun(() -> FGV)) -> FGV.
lazy_guard(Requirement, Consequence, Alternative) ->
    case Requirement of
        true ->
            Consequence();

        false ->
            Alternative()
    end.
