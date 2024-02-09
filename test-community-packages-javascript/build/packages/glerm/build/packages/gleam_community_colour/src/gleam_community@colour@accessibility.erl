-module(gleam_community@colour@accessibility).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([luminance/1, contrast_ratio/2, maximum_contrast/2]).

-spec intensity(float()) -> float().
intensity(Colour_value) ->
    case true of
        _ when Colour_value =< 0.03928 ->
            Colour_value / 12.92;

        _ ->
            _assert_subject = gleam@float:power(
                (Colour_value + 0.055) / 1.055,
                2.4
            ),
            {ok, I} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"gleam_community/colour/accessibility"/utf8>>,
                                function => <<"intensity"/utf8>>,
                                line => 62})
            end,
            I
    end.

-spec luminance(gleam_community@colour:colour()) -> float().
luminance(Colour) ->
    {R, G, B, _} = gleam_community@colour:to_rgba(Colour),
    R_intensity = intensity(R),
    G_intensity = intensity(G),
    B_intensity = intensity(B),
    ((0.2126 * R_intensity) + (0.7152 * G_intensity)) + (0.0722 * B_intensity).

-spec contrast_ratio(
    gleam_community@colour:colour(),
    gleam_community@colour:colour()
) -> float().
contrast_ratio(Colour_a, Colour_b) ->
    Luminance_a = luminance(Colour_a) + 0.05,
    Luminance_b = luminance(Colour_b) + 0.05,
    case Luminance_a > Luminance_b of
        true ->
            case Luminance_b of
                0.0 -> 0.0;
                Gleam@denominator -> Luminance_a / Gleam@denominator
            end;

        false ->
            case Luminance_a of
                0.0 -> 0.0;
                Gleam@denominator@1 -> Luminance_b / Gleam@denominator@1
            end
    end.

-spec maximum_contrast(
    gleam_community@colour:colour(),
    list(gleam_community@colour:colour())
) -> {ok, gleam_community@colour:colour()} | {error, nil}.
maximum_contrast(Base, Colours) ->
    _pipe = Colours,
    _pipe@1 = gleam@list:sort(
        _pipe,
        fun(Colour_a, Colour_b) ->
            Contrast_a = contrast_ratio(Base, Colour_a),
            Contrast_b = contrast_ratio(Base, Colour_b),
            gleam@float:compare(Contrast_b, Contrast_a)
        end
    ),
    gleam@list:first(_pipe@1).
