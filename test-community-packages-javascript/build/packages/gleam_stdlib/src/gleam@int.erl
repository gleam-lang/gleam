-module(gleam@int).
-compile([no_auto_import, nowarn_unused_vars]).

-export([absolute_value/1, compare/2, min/2, max/2, clamp/3, is_even/1, is_odd/1, negate/1, divide/2, remainder/2, modulo/2, floor_divide/2, add/2, multiply/2, subtract/2, parse/1, base_parse/2, to_string/1, to_base_string/2, to_base2/1, to_base8/1, to_base16/1, to_base36/1, to_float/1, power/2, square_root/1, random/2, sum/1, product/1, digits/2, undigits/2]).
-export_type([invalid_base/0]).

-type invalid_base() :: invalid_base.

-spec absolute_value(integer()) -> integer().
absolute_value(X) ->
    case X >= 0 of
        true ->
            X;

        false ->
            X * -1
    end.

-spec compare(integer(), integer()) -> gleam@order:order().
compare(A, B) ->
    case A =:= B of
        true ->
            eq;

        false ->
            case A < B of
                true ->
                    lt;

                false ->
                    gt
            end
    end.

-spec min(integer(), integer()) -> integer().
min(A, B) ->
    case A < B of
        true ->
            A;

        false ->
            B
    end.

-spec max(integer(), integer()) -> integer().
max(A, B) ->
    case A > B of
        true ->
            A;

        false ->
            B
    end.

-spec clamp(integer(), integer(), integer()) -> integer().
clamp(X, Min_bound, Max_bound) ->
    _pipe = X,
    _pipe@1 = min(_pipe, Max_bound),
    max(_pipe@1, Min_bound).

-spec is_even(integer()) -> boolean().
is_even(X) ->
    (X rem 2) =:= 0.

-spec is_odd(integer()) -> boolean().
is_odd(X) ->
    (X rem 2) /= 0.

-spec negate(integer()) -> integer().
negate(X) ->
    -1 * X.

-spec divide(integer(), integer()) -> {ok, integer()} | {error, nil}.
divide(Dividend, Divisor) ->
    case Divisor of
        0 ->
            {error, nil};

        Divisor@1 ->
            {ok, case Divisor@1 of
                    0 -> 0;
                    Gleam@denominator -> Dividend div Gleam@denominator
                end}
    end.

-spec remainder(integer(), integer()) -> {ok, integer()} | {error, nil}.
remainder(Dividend, Divisor) ->
    case Divisor of
        0 ->
            {error, nil};

        Divisor@1 ->
            {ok, case Divisor@1 of
                    0 -> 0;
                    Gleam@denominator -> Dividend rem Gleam@denominator
                end}
    end.

-spec modulo(integer(), integer()) -> {ok, integer()} | {error, nil}.
modulo(Dividend, Divisor) ->
    case Divisor of
        0 ->
            {error, nil};

        _ ->
            Remainder = case Divisor of
                0 -> 0;
                Gleam@denominator -> Dividend rem Gleam@denominator
            end,
            case (Remainder * Divisor) < 0 of
                true ->
                    {ok, Remainder + Divisor};

                false ->
                    {ok, Remainder}
            end
    end.

-spec floor_divide(integer(), integer()) -> {ok, integer()} | {error, nil}.
floor_divide(Dividend, Divisor) ->
    case Divisor of
        0 ->
            {error, nil};

        Divisor@1 ->
            case ((Dividend * Divisor@1) < 0) andalso ((case Divisor@1 of
                0 -> 0;
                Gleam@denominator -> Dividend rem Gleam@denominator
            end) /= 0) of
                true ->
                    {ok, (case Divisor@1 of
                            0 -> 0;
                            Gleam@denominator@1 -> Dividend div Gleam@denominator@1
                        end) - 1};

                false ->
                    {ok, case Divisor@1 of
                            0 -> 0;
                            Gleam@denominator@2 -> Dividend div Gleam@denominator@2
                        end}
            end
    end.

-spec add(integer(), integer()) -> integer().
add(A, B) ->
    A + B.

-spec multiply(integer(), integer()) -> integer().
multiply(A, B) ->
    A * B.

-spec subtract(integer(), integer()) -> integer().
subtract(A, B) ->
    A - B.

-spec parse(binary()) -> {ok, integer()} | {error, nil}.
parse(String) ->
    gleam_stdlib:parse_int(String).

-spec base_parse(binary(), integer()) -> {ok, integer()} | {error, nil}.
base_parse(String, Base) ->
    case (Base >= 2) andalso (Base =< 36) of
        true ->
            gleam_stdlib:int_from_base_string(String, Base);

        false ->
            {error, nil}
    end.

-spec to_string(integer()) -> binary().
to_string(X) ->
    erlang:integer_to_binary(X).

-spec to_base_string(integer(), integer()) -> {ok, binary()} |
    {error, invalid_base()}.
to_base_string(X, Base) ->
    case (Base >= 2) andalso (Base =< 36) of
        true ->
            {ok, erlang:integer_to_binary(X, Base)};

        false ->
            {error, invalid_base}
    end.

-spec to_base2(integer()) -> binary().
to_base2(X) ->
    erlang:integer_to_binary(X, 2).

-spec to_base8(integer()) -> binary().
to_base8(X) ->
    erlang:integer_to_binary(X, 8).

-spec to_base16(integer()) -> binary().
to_base16(X) ->
    erlang:integer_to_binary(X, 16).

-spec to_base36(integer()) -> binary().
to_base36(X) ->
    erlang:integer_to_binary(X, 36).

-spec to_float(integer()) -> float().
to_float(X) ->
    erlang:float(X).

-spec power(integer(), float()) -> {ok, float()} | {error, nil}.
power(Base, Exponent) ->
    _pipe = Base,
    _pipe@1 = to_float(_pipe),
    gleam@float:power(_pipe@1, Exponent).

-spec square_root(integer()) -> {ok, float()} | {error, nil}.
square_root(X) ->
    _pipe = X,
    _pipe@1 = to_float(_pipe),
    gleam@float:square_root(_pipe@1).

-spec random(integer(), integer()) -> integer().
random(Boundary_a, Boundary_b) ->
    {Min, Max} = case {Boundary_a, Boundary_b} of
        {A, B} when A =< B ->
            {A, B};

        {A@1, B@1} when A@1 > B@1 ->
            {B@1, A@1}
    end,
    Min@1 = begin
        _pipe = to_float(Min),
        gleam@float:ceiling(_pipe)
    end,
    Max@1 = begin
        _pipe@1 = to_float(Max),
        gleam@float:floor(_pipe@1)
    end,
    _pipe@2 = gleam@float:random(Min@1, Max@1),
    _pipe@3 = gleam@float:floor(_pipe@2),
    gleam@float:round(_pipe@3).

-spec do_sum(list(integer()), integer()) -> integer().
do_sum(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_sum(Rest, X + Initial)
    end.

-spec sum(list(integer())) -> integer().
sum(Numbers) ->
    _pipe = Numbers,
    do_sum(_pipe, 0).

-spec do_product(list(integer()), integer()) -> integer().
do_product(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_product(Rest, X * Initial)
    end.

-spec product(list(integer())) -> integer().
product(Numbers) ->
    case Numbers of
        [] ->
            1;

        _ ->
            do_product(Numbers, 1)
    end.

-spec do_digits(integer(), integer(), list(integer())) -> list(integer()).
do_digits(X, Base, Acc) ->
    case absolute_value(X) < Base of
        true ->
            [X | Acc];

        false ->
            do_digits(case Base of
                    0 -> 0;
                    Gleam@denominator -> X div Gleam@denominator
                end, Base, [case Base of
                        0 -> 0;
                        Gleam@denominator@1 -> X rem Gleam@denominator@1
                    end | Acc])
    end.

-spec digits(integer(), integer()) -> {ok, list(integer())} |
    {error, invalid_base()}.
digits(X, Base) ->
    case Base < 2 of
        true ->
            {error, invalid_base};

        false ->
            {ok, do_digits(X, Base, [])}
    end.

-spec do_undigits(list(integer()), integer(), integer()) -> {ok, integer()} |
    {error, invalid_base()}.
do_undigits(Numbers, Base, Acc) ->
    case Numbers of
        [] ->
            {ok, Acc};

        [Digit | _] when Digit >= Base ->
            {error, invalid_base};

        [Digit@1 | Rest] ->
            do_undigits(Rest, Base, (Acc * Base) + Digit@1)
    end.

-spec undigits(list(integer()), integer()) -> {ok, integer()} |
    {error, invalid_base()}.
undigits(Numbers, Base) ->
    case Base < 2 of
        true ->
            {error, invalid_base};

        false ->
            do_undigits(Numbers, Base, 0)
    end.
