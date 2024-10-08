-module(gleam@float).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse/1, to_string/1, compare/2, min/2, max/2, clamp/3, ceiling/1, floor/1, truncate/1, absolute_value/1, loosely_compare/3, loosely_equals/3, power/2, square_root/1, negate/1, round/1, to_precision/2, sum/1, product/1, random/0, modulo/2, divide/2, add/2, multiply/2, subtract/2]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 32).
-spec parse(binary()) -> {ok, float()} | {error, nil}.
parse(String) ->
    gleam_stdlib:parse_float(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 49).
-spec to_string(float()) -> binary().
to_string(X) ->
    gleam_stdlib:float_to_string(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 86).
-spec compare(float(), float()) -> gleam@order:order().
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 167).
-spec min(float(), float()) -> float().
min(A, B) ->
    case A < B of
        true ->
            A;

        false ->
            B
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 183).
-spec max(float(), float()) -> float().
max(A, B) ->
    case A > B of
        true ->
            A;

        false ->
            B
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 66).
-spec clamp(float(), float(), float()) -> float().
clamp(X, Min_bound, Max_bound) ->
    _pipe = X,
    _pipe@1 = min(_pipe, Max_bound),
    max(_pipe@1, Min_bound).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 199).
-spec ceiling(float()) -> float().
ceiling(X) ->
    math:ceil(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 216).
-spec floor(float()) -> float().
floor(X) ->
    math:floor(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 262).
-spec truncate(float()) -> integer().
truncate(X) ->
    erlang:trunc(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 310).
-spec absolute_value(float()) -> float().
absolute_value(X) ->
    case X >= +0.0 of
        true ->
            X;

        _ ->
            +0.0 - X
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 116).
-spec loosely_compare(float(), float(), float()) -> gleam@order:order().
loosely_compare(A, B, Tolerance) ->
    Difference = absolute_value(A - B),
    case Difference =< Tolerance of
        true ->
            eq;

        false ->
            compare(A, B)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 149).
-spec loosely_equals(float(), float(), float()) -> boolean().
loosely_equals(A, B, Tolerance) ->
    Difference = absolute_value(A - B),
    Difference =< Tolerance.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 347).
-spec power(float(), float()) -> {ok, float()} | {error, nil}.
power(Base, Exponent) ->
    Fractional = (ceiling(Exponent) - Exponent) > +0.0,
    case ((Base < +0.0) andalso Fractional) orelse ((Base =:= +0.0) andalso (Exponent
    < +0.0)) of
        true ->
            {error, nil};

        false ->
            {ok, math:pow(Base, Exponent)}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 379).
-spec square_root(float()) -> {ok, float()} | {error, nil}.
square_root(X) ->
    power(X, 0.5).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 392).
-spec negate(float()) -> float().
negate(X) ->
    -1.0 * X.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 238).
-spec round(float()) -> integer().
round(X) ->
    erlang:round(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 287).
-spec to_precision(float(), integer()) -> float().
to_precision(X, Precision) ->
    Factor = math:pow(10.0, erlang:float(- Precision)),
    erlang:float(round(case Factor of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> X / Gleam@denominator
            end)) * Factor.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 410).
-spec do_sum(list(float()), float()) -> float().
do_sum(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_sum(Rest, X + Initial)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 405).
-spec sum(list(float())) -> float().
sum(Numbers) ->
    _pipe = Numbers,
    do_sum(_pipe, +0.0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 433).
-spec do_product(list(float()), float()) -> float().
do_product(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_product(Rest, X * Initial)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 426).
-spec product(list(float())) -> float().
product(Numbers) ->
    case Numbers of
        [] ->
            1.0;

        _ ->
            do_product(Numbers, 1.0)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 455).
-spec random() -> float().
random() ->
    rand:uniform().

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 484).
-spec modulo(float(), float()) -> {ok, float()} | {error, nil}.
modulo(Dividend, Divisor) ->
    case Divisor of
        +0.0 ->
            {error, nil};

        _ ->
            {ok, Dividend - (floor(case Divisor of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> Dividend / Gleam@denominator
                    end) * Divisor)}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 505).
-spec divide(float(), float()) -> {ok, float()} | {error, nil}.
divide(A, B) ->
    case B of
        +0.0 ->
            {error, nil};

        B@1 ->
            {ok, case B@1 of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> A / Gleam@denominator
                end}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 536).
-spec add(float(), float()) -> float().
add(A, B) ->
    A + B.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 564).
-spec multiply(float(), float()) -> float().
multiply(A, B) ->
    A * B.

-file("/Users/louis/src/gleam/stdlib/src/gleam/float.gleam", 597).
-spec subtract(float(), float()) -> float().
subtract(A, B) ->
    A - B.
