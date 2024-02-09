-module(gleam_community@colour).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([from_rgb255/3, from_rgb/3, from_rgba/4, from_hsla/4, from_hsl/3, from_rgb_hex/1, from_rgb_hex_string/1, from_rgba_hex/1, from_rgba_hex_string/1, to_rgba/1, to_hsla/1, to_css_rgba_string/1, to_rgba_hex/1, to_rgba_hex_string/1, to_rgb_hex/1, to_rgb_hex_string/1]).
-export_type([colour/0]).

-opaque colour() :: {rgba, float(), float(), float(), float()} |
    {hsla, float(), float(), float(), float()}.

-spec valid_colour_value(float()) -> {ok, float()} | {error, nil}.
valid_colour_value(C) ->
    case (C > 1.0) orelse (C < +0.0) of
        true ->
            {error, nil};

        false ->
            {ok, C}
    end.

-spec hue_to_rgb(float(), float(), float()) -> float().
hue_to_rgb(Hue, M1, M2) ->
    H = case Hue of
        _ when Hue < +0.0 ->
            Hue + 1.0;

        _ when Hue > 1.0 ->
            Hue - 1.0;

        _ ->
            Hue
    end,
    H_t_6 = H * 6.0,
    H_t_2 = H * 2.0,
    H_t_3 = H * 3.0,
    case H of
        _ when H_t_6 < 1.0 ->
            M1 + (((M2 - M1) * H) * 6.0);

        _ when H_t_2 < 1.0 ->
            M2;

        _ when H_t_3 < 2.0 ->
            M1 + (((M2 - M1) * ((2.0 / 3.0) - H)) * 6.0);

        _ ->
            M1
    end.

-spec hex_string_to_int(binary()) -> {ok, integer()} | {error, nil}.
hex_string_to_int(Hex_string) ->
    Hex = case Hex_string of
        <<"#"/utf8, Hex_number/binary>> ->
            Hex_number;

        <<"0x"/utf8, Hex_number@1/binary>> ->
            Hex_number@1;

        _ ->
            Hex_string
    end,
    _pipe = Hex,
    _pipe@1 = gleam@string:lowercase(_pipe),
    _pipe@2 = gleam@string:to_graphemes(_pipe@1),
    _pipe@3 = gleam@list:reverse(_pipe@2),
    gleam@list:index_fold(
        _pipe@3,
        {ok, 0},
        fun(Total, Char, Index) -> case Total of
                {error, nil} ->
                    {error, nil};

                {ok, V} ->
                    gleam@result:then(case Char of
                            <<"a"/utf8>> ->
                                {ok, 10};

                            <<"b"/utf8>> ->
                                {ok, 11};

                            <<"c"/utf8>> ->
                                {ok, 12};

                            <<"d"/utf8>> ->
                                {ok, 13};

                            <<"e"/utf8>> ->
                                {ok, 14};

                            <<"f"/utf8>> ->
                                {ok, 15};

                            _ ->
                                gleam@int:parse(Char)
                        end, fun(Num) ->
                            gleam@result:then(
                                gleam@int:power(16, gleam@int:to_float(Index)),
                                fun(Base) ->
                                    {ok,
                                        V + gleam@float:round(
                                            gleam@int:to_float(Num) * Base
                                        )}
                                end
                            )
                        end)
            end end
    ).

-spec hsla_to_rgba(float(), float(), float(), float()) -> {float(),
    float(),
    float(),
    float()}.
hsla_to_rgba(H, S, L, A) ->
    M2 = case L =< 0.5 of
        true ->
            L * (S + 1.0);

        false ->
            (L + S) - (L * S)
    end,
    M1 = (L * 2.0) - M2,
    R = hue_to_rgb(H + (1.0 / 3.0), M1, M2),
    G = hue_to_rgb(H, M1, M2),
    B = hue_to_rgb(H - (1.0 / 3.0), M1, M2),
    {R, G, B, A}.

-spec rgba_to_hsla(float(), float(), float(), float()) -> {float(),
    float(),
    float(),
    float()}.
rgba_to_hsla(R, G, B, A) ->
    Min_colour = gleam@float:min(R, gleam@float:min(G, B)),
    Max_colour = gleam@float:max(R, gleam@float:max(G, B)),
    H1 = case true of
        _ when Max_colour =:= R ->
            gleam@float:divide(G - B, Max_colour - Min_colour);

        _ when Max_colour =:= G ->
            _pipe = gleam@float:divide(B - R, Max_colour - Min_colour),
            gleam@result:then(_pipe, fun(D) -> {ok, 2.0 + D} end);

        _ ->
            _pipe@1 = gleam@float:divide(R - G, Max_colour - Min_colour),
            gleam@result:then(_pipe@1, fun(D@1) -> {ok, 4.0 + D@1} end)
    end,
    H2 = case H1 of
        {ok, V} ->
            {ok, V * (1.0 / 6.0)};

        _ ->
            H1
    end,
    H3 = case H2 of
        {ok, V@1} when V@1 < +0.0 ->
            V@1 + 1.0;

        {ok, V@2} ->
            V@2;

        _ ->
            +0.0
    end,
    L = (Min_colour + Max_colour) / 2.0,
    S = case true of
        _ when Min_colour =:= Max_colour ->
            +0.0;

        _ when L < 0.5 ->
            case (Max_colour + Min_colour) of
                0.0 -> 0.0;
                Gleam@denominator -> (Max_colour - Min_colour) / Gleam@denominator
            end;

        _ ->
            case ((2.0 - Max_colour) - Min_colour) of
                0.0 -> 0.0;
                Gleam@denominator@1 -> (Max_colour - Min_colour) / Gleam@denominator@1
            end
    end,
    {H3, S, L, A}.

-spec from_rgb255(integer(), integer(), integer()) -> {ok, colour()} |
    {error, nil}.
from_rgb255(Red, Green, Blue) ->
    gleam@result:then(
        begin
            _pipe = Red,
            _pipe@1 = gleam@int:to_float(_pipe),
            _pipe@2 = gleam@float:divide(_pipe@1, 255.0),
            gleam@result:then(_pipe@2, fun valid_colour_value/1)
        end,
        fun(R) ->
            gleam@result:then(
                begin
                    _pipe@3 = Green,
                    _pipe@4 = gleam@int:to_float(_pipe@3),
                    _pipe@5 = gleam@float:divide(_pipe@4, 255.0),
                    gleam@result:then(_pipe@5, fun valid_colour_value/1)
                end,
                fun(G) ->
                    gleam@result:then(
                        begin
                            _pipe@6 = Blue,
                            _pipe@7 = gleam@int:to_float(_pipe@6),
                            _pipe@8 = gleam@float:divide(_pipe@7, 255.0),
                            gleam@result:then(_pipe@8, fun valid_colour_value/1)
                        end,
                        fun(B) -> {ok, {rgba, R, G, B, 1.0}} end
                    )
                end
            )
        end
    ).

-spec from_rgb(float(), float(), float()) -> {ok, colour()} | {error, nil}.
from_rgb(Red, Green, Blue) ->
    gleam@result:then(
        valid_colour_value(Red),
        fun(R) ->
            gleam@result:then(
                valid_colour_value(Green),
                fun(G) ->
                    gleam@result:then(
                        valid_colour_value(Blue),
                        fun(B) -> {ok, {rgba, R, G, B, 1.0}} end
                    )
                end
            )
        end
    ).

-spec from_rgba(float(), float(), float(), float()) -> {ok, colour()} |
    {error, nil}.
from_rgba(Red, Green, Blue, Alpha) ->
    gleam@result:then(
        valid_colour_value(Red),
        fun(R) ->
            gleam@result:then(
                valid_colour_value(Green),
                fun(G) ->
                    gleam@result:then(
                        valid_colour_value(Blue),
                        fun(B) ->
                            gleam@result:then(
                                valid_colour_value(Alpha),
                                fun(A) -> {ok, {rgba, R, G, B, A}} end
                            )
                        end
                    )
                end
            )
        end
    ).

-spec from_hsla(float(), float(), float(), float()) -> {ok, colour()} |
    {error, nil}.
from_hsla(Hue, Saturation, Lightness, Alpha) ->
    gleam@result:then(
        valid_colour_value(Hue),
        fun(H) ->
            gleam@result:then(
                valid_colour_value(Saturation),
                fun(S) ->
                    gleam@result:then(
                        valid_colour_value(Lightness),
                        fun(L) ->
                            gleam@result:then(
                                valid_colour_value(Alpha),
                                fun(A) -> {ok, {hsla, H, S, L, A}} end
                            )
                        end
                    )
                end
            )
        end
    ).

-spec from_hsl(float(), float(), float()) -> {ok, colour()} | {error, nil}.
from_hsl(Hue, Saturation, Lightness) ->
    from_hsla(Hue, Saturation, Lightness, 1.0).

-spec from_rgb_hex(integer()) -> {ok, colour()} | {error, nil}.
from_rgb_hex(Hex) ->
    case (Hex > 16#ffffff) orelse (Hex < 0) of
        true ->
            {error, nil};

        false ->
            R = begin
                _pipe = erlang:'bsr'(Hex, 16),
                erlang:'band'(_pipe, 16#ff)
            end,
            G = begin
                _pipe@1 = erlang:'bsr'(Hex, 8),
                erlang:'band'(_pipe@1, 16#ff)
            end,
            B = erlang:'band'(Hex, 16#ff),
            from_rgb255(R, G, B)
    end.

-spec from_rgb_hex_string(binary()) -> {ok, colour()} | {error, nil}.
from_rgb_hex_string(Hex_string) ->
    gleam@result:then(
        hex_string_to_int(Hex_string),
        fun(Hex_int) -> from_rgb_hex(Hex_int) end
    ).

-spec from_rgba_hex(integer()) -> {ok, colour()} | {error, nil}.
from_rgba_hex(Hex) ->
    case (Hex > 16#ffffffff) orelse (Hex < 0) of
        true ->
            {error, nil};

        false ->
            _assert_subject = begin
                _pipe = erlang:'bsr'(Hex, 24),
                _pipe@1 = erlang:'band'(_pipe, 16#ff),
                _pipe@2 = gleam@int:to_float(_pipe@1),
                gleam@float:divide(_pipe@2, 255.0)
            end,
            {ok, R} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/colour"/utf8>>,
                                function => <<"from_rgba_hex"/utf8>>,
                                line => 588})
            end,
            _assert_subject@1 = begin
                _pipe@3 = erlang:'bsr'(Hex, 16),
                _pipe@4 = erlang:'band'(_pipe@3, 16#ff),
                _pipe@5 = gleam@int:to_float(_pipe@4),
                gleam@float:divide(_pipe@5, 255.0)
            end,
            {ok, G} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"gleam_community/colour"/utf8>>,
                                function => <<"from_rgba_hex"/utf8>>,
                                line => 594})
            end,
            _assert_subject@2 = begin
                _pipe@6 = erlang:'bsr'(Hex, 8),
                _pipe@7 = erlang:'band'(_pipe@6, 16#ff),
                _pipe@8 = gleam@int:to_float(_pipe@7),
                gleam@float:divide(_pipe@8, 255.0)
            end,
            {ok, B} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"gleam_community/colour"/utf8>>,
                                function => <<"from_rgba_hex"/utf8>>,
                                line => 600})
            end,
            _assert_subject@3 = begin
                _pipe@9 = erlang:'band'(Hex, 16#ff),
                _pipe@10 = gleam@int:to_float(_pipe@9),
                gleam@float:divide(_pipe@10, 255.0)
            end,
            {ok, A} = case _assert_subject@3 of
                {ok, _} -> _assert_subject@3;
                _assert_fail@3 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@3,
                                module => <<"gleam_community/colour"/utf8>>,
                                function => <<"from_rgba_hex"/utf8>>,
                                line => 606})
            end,
            from_rgba(R, G, B, A)
    end.

-spec from_rgba_hex_string(binary()) -> {ok, colour()} | {error, nil}.
from_rgba_hex_string(Hex_string) ->
    gleam@result:then(
        hex_string_to_int(Hex_string),
        fun(Hex_int) -> from_rgba_hex(Hex_int) end
    ).

-spec to_rgba(colour()) -> {float(), float(), float(), float()}.
to_rgba(Colour) ->
    case Colour of
        {rgba, R, G, B, A} ->
            {R, G, B, A};

        {hsla, H, S, L, A@1} ->
            hsla_to_rgba(H, S, L, A@1)
    end.

-spec to_hsla(colour()) -> {float(), float(), float(), float()}.
to_hsla(Colour) ->
    case Colour of
        {hsla, H, S, L, A} ->
            {H, S, L, A};

        {rgba, R, G, B, A@1} ->
            rgba_to_hsla(R, G, B, A@1)
    end.

-spec to_css_rgba_string(colour()) -> binary().
to_css_rgba_string(Colour) ->
    {R, G, B, A} = to_rgba(Colour),
    Percent = fun(X) ->
        _assert_subject = begin
            _pipe = X,
            _pipe@1 = gleam@float:multiply(_pipe, 10000.0),
            _pipe@2 = gleam@float:round(_pipe@1),
            _pipe@3 = gleam@int:to_float(_pipe@2),
            gleam@float:divide(_pipe@3, 100.0)
        end,
        {ok, P} = case _assert_subject of
            {ok, _} -> _assert_subject;
            _assert_fail ->
                erlang:error(#{gleam_error => let_assert,
                            message => <<"Assertion pattern match failed"/utf8>>,
                            value => _assert_fail,
                            module => <<"gleam_community/colour"/utf8>>,
                            function => <<"to_css_rgba_string"/utf8>>,
                            line => 704})
        end,
        P
    end,
    Round_to = fun(X@1) ->
        _assert_subject@1 = begin
            _pipe@4 = X@1,
            _pipe@5 = gleam@float:multiply(_pipe@4, 1000.0),
            _pipe@6 = gleam@float:round(_pipe@5),
            _pipe@7 = gleam@int:to_float(_pipe@6),
            gleam@float:divide(_pipe@7, 1000.0)
        end,
        {ok, R@1} = case _assert_subject@1 of
            {ok, _} -> _assert_subject@1;
            _assert_fail@1 ->
                erlang:error(#{gleam_error => let_assert,
                            message => <<"Assertion pattern match failed"/utf8>>,
                            value => _assert_fail@1,
                            module => <<"gleam_community/colour"/utf8>>,
                            function => <<"to_css_rgba_string"/utf8>>,
                            line => 716})
        end,
        R@1
    end,
    gleam@string:join(
        [<<"rgba("/utf8>>,
            <<(gleam@float:to_string(Percent(R)))/binary, "%,"/utf8>>,
            <<(gleam@float:to_string(Percent(G)))/binary, "%,"/utf8>>,
            <<(gleam@float:to_string(Percent(B)))/binary, "%,"/utf8>>,
            gleam@float:to_string(Round_to(A)),
            <<")"/utf8>>],
        <<""/utf8>>
    ).

-spec to_rgba_hex(colour()) -> integer().
to_rgba_hex(Colour) ->
    {R, G, B, A} = to_rgba(Colour),
    Red = begin
        _pipe = R * 255.0,
        _pipe@1 = gleam@float:round(_pipe),
        erlang:'bsl'(_pipe@1, 24)
    end,
    Green = begin
        _pipe@2 = G * 255.0,
        _pipe@3 = gleam@float:round(_pipe@2),
        erlang:'bsl'(_pipe@3, 16)
    end,
    Blue = begin
        _pipe@4 = B * 255.0,
        _pipe@5 = gleam@float:round(_pipe@4),
        erlang:'bsl'(_pipe@5, 8)
    end,
    Alpha = begin
        _pipe@6 = A * 255.0,
        gleam@float:round(_pipe@6)
    end,
    ((Red + Green) + Blue) + Alpha.

-spec to_rgba_hex_string(colour()) -> binary().
to_rgba_hex_string(Colour) ->
    _pipe = to_rgba_hex(Colour),
    gleam@int:to_base16(_pipe).

-spec to_rgb_hex(colour()) -> integer().
to_rgb_hex(Colour) ->
    {R, G, B, _} = to_rgba(Colour),
    Red = begin
        _pipe = R * 255.0,
        _pipe@1 = gleam@float:round(_pipe),
        erlang:'bsl'(_pipe@1, 16)
    end,
    Green = begin
        _pipe@2 = G * 255.0,
        _pipe@3 = gleam@float:round(_pipe@2),
        erlang:'bsl'(_pipe@3, 8)
    end,
    Blue = begin
        _pipe@4 = B * 255.0,
        gleam@float:round(_pipe@4)
    end,
    (Red + Green) + Blue.

-spec to_rgb_hex_string(colour()) -> binary().
to_rgb_hex_string(Colour) ->
    _pipe = to_rgb_hex(Colour),
    gleam@int:to_base16(_pipe).
