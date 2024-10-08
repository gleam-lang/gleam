-module(gleam@int).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([absolute_value/1, parse/1, base_parse/2, to_string/1, to_base_string/2, to_base2/1, to_base8/1, to_base16/1, to_base36/1, to_float/1, power/2, square_root/1, compare/2, min/2, max/2, clamp/3, is_even/1, is_odd/1, negate/1, sum/1, product/1, digits/2, undigits/2, random/1, divide/2, remainder/2, modulo/2, floor_divide/2, add/2, multiply/2, subtract/2, bitwise_and/2, bitwise_not/1, bitwise_or/2, bitwise_exclusive_or/2, bitwise_shift_left/2, bitwise_shift_right/2]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 30).
-spec absolute_value(integer()) -> integer().
absolute_value(X) ->
    case X >= 0 of
        true ->
            X;

        false ->
            X * -1
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 107).
-spec parse(binary()) -> {ok, integer()} | {error, nil}.
parse(String) ->
    gleam_stdlib:parse_int(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 145).
-spec base_parse(binary(), integer()) -> {ok, integer()} | {error, nil}.
base_parse(String, Base) ->
    case (Base >= 2) andalso (Base =< 36) of
        true ->
            gleam_stdlib:int_from_base_string(String, Base);

        false ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 165).
-spec to_string(integer()) -> binary().
to_string(X) ->
    erlang:integer_to_binary(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 204).
-spec to_base_string(integer(), integer()) -> {ok, binary()} | {error, nil}.
to_base_string(X, Base) ->
    case (Base >= 2) andalso (Base =< 36) of
        true ->
            {ok, erlang:integer_to_binary(X, Base)};

        false ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 224).
-spec to_base2(integer()) -> binary().
to_base2(X) ->
    erlang:integer_to_binary(X, 2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 237).
-spec to_base8(integer()) -> binary().
to_base8(X) ->
    erlang:integer_to_binary(X, 8).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 250).
-spec to_base16(integer()) -> binary().
to_base16(X) ->
    erlang:integer_to_binary(X, 16).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 263).
-spec to_base36(integer()) -> binary().
to_base36(X) ->
    erlang:integer_to_binary(X, 36).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 286).
-spec to_float(integer()) -> float().
to_float(X) ->
    erlang:float(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 67).
-spec power(integer(), float()) -> {ok, float()} | {error, nil}.
power(Base, Exponent) ->
    _pipe = Base,
    _pipe@1 = to_float(_pipe),
    gleam@float:power(_pipe@1, Exponent).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 87).
-spec square_root(integer()) -> {ok, float()} | {error, nil}.
square_root(X) ->
    _pipe = X,
    _pipe@1 = to_float(_pipe),
    gleam@float:square_root(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 328).
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 348).
-spec min(integer(), integer()) -> integer().
min(A, B) ->
    case A < B of
        true ->
            A;

        false ->
            B
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 364).
-spec max(integer(), integer()) -> integer().
max(A, B) ->
    case A > B of
        true ->
            A;

        false ->
            B
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 303).
-spec clamp(integer(), integer(), integer()) -> integer().
clamp(X, Min_bound, Max_bound) ->
    _pipe = X,
    _pipe@1 = min(_pipe, Max_bound),
    max(_pipe@1, Min_bound).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 385).
-spec is_even(integer()) -> boolean().
is_even(X) ->
    (X rem 2) =:= 0.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 403).
-spec is_odd(integer()) -> boolean().
is_odd(X) ->
    (X rem 2) /= 0.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 416).
-spec negate(integer()) -> integer().
negate(X) ->
    -1 * X.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 434).
-spec do_sum(list(integer()), integer()) -> integer().
do_sum(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_sum(Rest, X + Initial)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 429).
-spec sum(list(integer())) -> integer().
sum(Numbers) ->
    _pipe = Numbers,
    do_sum(_pipe, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 457).
-spec do_product(list(integer()), integer()) -> integer().
do_product(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_product(Rest, X * Initial)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 450).
-spec product(list(integer())) -> integer().
product(Numbers) ->
    case Numbers of
        [] ->
            1;

        _ ->
            do_product(Numbers, 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 486).
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 479).
-spec digits(integer(), integer()) -> {ok, list(integer())} | {error, nil}.
digits(X, Base) ->
    case Base < 2 of
        true ->
            {error, nil};

        false ->
            {ok, do_digits(X, Base, [])}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 520).
-spec do_undigits(list(integer()), integer(), integer()) -> {ok, integer()} |
    {error, nil}.
do_undigits(Numbers, Base, Acc) ->
    case Numbers of
        [] ->
            {ok, Acc};

        [Digit | _] when Digit >= Base ->
            {error, nil};

        [Digit@1 | Rest] ->
            do_undigits(Rest, Base, (Acc * Base) + Digit@1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 513).
-spec undigits(list(integer()), integer()) -> {ok, integer()} | {error, nil}.
undigits(Numbers, Base) ->
    case Base < 2 of
        true ->
            {error, nil};

        false ->
            do_undigits(Numbers, Base, 0)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 549).
-spec random(integer()) -> integer().
random(Max) ->
    _pipe = (rand:uniform() * to_float(Max)),
    _pipe@1 = gleam@float:floor(_pipe),
    gleam@float:round(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 582).
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 634).
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 676).
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 720).
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 754).
-spec add(integer(), integer()) -> integer().
add(A, B) ->
    A + B.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 782).
-spec multiply(integer(), integer()) -> integer().
multiply(A, B) ->
    A * B.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 815).
-spec subtract(integer(), integer()) -> integer().
subtract(A, B) ->
    A - B.

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 827).
-spec bitwise_and(integer(), integer()) -> integer().
bitwise_and(X, Y) ->
    erlang:'band'(X, Y).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 837).
-spec bitwise_not(integer()) -> integer().
bitwise_not(X) ->
    erlang:'bnot'(X).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 847).
-spec bitwise_or(integer(), integer()) -> integer().
bitwise_or(X, Y) ->
    erlang:'bor'(X, Y).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 857).
-spec bitwise_exclusive_or(integer(), integer()) -> integer().
bitwise_exclusive_or(X, Y) ->
    erlang:'bxor'(X, Y).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 867).
-spec bitwise_shift_left(integer(), integer()) -> integer().
bitwise_shift_left(X, Y) ->
    erlang:'bsl'(X, Y).

-file("/Users/louis/src/gleam/stdlib/src/gleam/int.gleam", 877).
-spec bitwise_shift_right(integer(), integer()) -> integer().
bitwise_shift_right(X, Y) ->
    erlang:'bsr'(X, Y).
