-module(gleam@float).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([parse/1, to_string/1, compare/2, min/2, max/2, clamp/3, ceiling/1, floor/1, round/1, truncate/1, absolute_value/1, loosely_compare/3, loosely_equals/3, power/2, square_root/1, negate/1, sum/1, product/1, random/2, divide/2, add/2, multiply/2, subtract/2]).

-spec parse(binary()) -> {ok, float()} | {error, nil}.
parse(String) ->
    gleam_stdlib:parse_float(String).

-spec to_string(float()) -> binary().
to_string(X) ->
    gleam_stdlib:float_to_string(X).

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

-spec min(float(), float()) -> float().
min(A, B) ->
    case A < B of
        true ->
            A;

        false ->
            B
    end.

-spec max(float(), float()) -> float().
max(A, B) ->
    case A > B of
        true ->
            A;

        false ->
            B
    end.

-spec clamp(float(), float(), float()) -> float().
clamp(X, Min_bound, Max_bound) ->
    _pipe = X,
    _pipe@1 = min(_pipe, Max_bound),
    max(_pipe@1, Min_bound).

-spec ceiling(float()) -> float().
ceiling(X) ->
    math:ceil(X).

-spec floor(float()) -> float().
floor(X) ->
    math:floor(X).

-spec round(float()) -> integer().
round(X) ->
    erlang:round(X).

-spec truncate(float()) -> integer().
truncate(X) ->
    erlang:trunc(X).

-spec absolute_value(float()) -> float().
absolute_value(X) ->
    case X >= +0.0 of
        true ->
            X;

        _ ->
            +0.0 - X
    end.

-spec loosely_compare(float(), float(), float()) -> gleam@order:order().
loosely_compare(A, B, Tolerance) ->
    Difference = absolute_value(A - B),
    case Difference =< Tolerance of
        true ->
            eq;

        false ->
            compare(A, B)
    end.

-spec loosely_equals(float(), float(), float()) -> boolean().
loosely_equals(A, B, Tolerance) ->
    Difference = absolute_value(A - B),
    Difference =< Tolerance.

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

-spec square_root(float()) -> {ok, float()} | {error, nil}.
square_root(X) ->
    power(X, 0.5).

-spec negate(float()) -> float().
negate(X) ->
    -1.0 * X.

-spec do_sum(list(float()), float()) -> float().
do_sum(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_sum(Rest, X + Initial)
    end.

-spec sum(list(float())) -> float().
sum(Numbers) ->
    _pipe = Numbers,
    do_sum(_pipe, +0.0).

-spec do_product(list(float()), float()) -> float().
do_product(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_product(Rest, X * Initial)
    end.

-spec product(list(float())) -> float().
product(Numbers) ->
    case Numbers of
        [] ->
            1.0;

        _ ->
            do_product(Numbers, 1.0)
    end.

-spec random(float(), float()) -> float().
random(Min, Max) ->
    (rand:uniform() * (Max - Min)) + Min.

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

-spec add(float(), float()) -> float().
add(A, B) ->
    A + B.

-spec multiply(float(), float()) -> float().
multiply(A, B) ->
    A * B.

-spec subtract(float(), float()) -> float().
subtract(A, B) ->
    A - B.
