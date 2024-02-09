-module(gleam_community@ansi).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([reset/1, bold/1, dim/1, italic/1, underline/1, inverse/1, hidden/1, strikethrough/1, black/1, red/1, green/1, yellow/1, blue/1, magenta/1, cyan/1, white/1, bright_black/1, grey/1, gray/1, bright_red/1, bright_green/1, bright_yellow/1, bright_blue/1, bright_magenta/1, bright_cyan/1, bright_white/1, hex/2, pink/1, colour/2, color/2, bg_black/1, bg_red/1, bg_green/1, bg_yellow/1, bg_blue/1, bg_magenta/1, bg_cyan/1, bg_white/1, bg_bright_black/1, bg_bright_red/1, bg_bright_green/1, bg_bright_yellow/1, bg_bright_blue/1, bg_bright_magenta/1, bg_bright_cyan/1, bg_bright_white/1, bg_hex/2, bg_pink/1, bg_colour/2, bg_color/2]).
-export_type([code/0]).

-type code() :: {code, binary(), binary(), binary()}.

-spec code(list(integer()), integer()) -> code().
code(Open, Close) ->
    Close_str = gleam@int:to_string(Close),
    Open_strs = gleam@list:map(Open, fun gleam@int:to_string/1),
    {code,
        <<<<<<""/utf8, "["/utf8>>/binary,
                (gleam@string:join(Open_strs, <<";"/utf8>>))/binary>>/binary,
            "m"/utf8>>,
        <<<<<<""/utf8, "["/utf8>>/binary, Close_str/binary>>/binary, "m"/utf8>>,
        <<<<<<""/utf8, "["/utf8>>/binary, Close_str/binary>>/binary, "m"/utf8>>}.

-spec run(binary(), code()) -> binary().
run(Text, Code) ->
    <<<<(erlang:element(2, Code))/binary,
            (gleam@string:replace(
                Text,
                erlang:element(4, Code),
                erlang:element(2, Code)
            ))/binary>>/binary,
        (erlang:element(3, Code))/binary>>.

-spec reset(binary()) -> binary().
reset(Text) ->
    run(Text, code([0], 0)).

-spec bold(binary()) -> binary().
bold(Text) ->
    run(Text, code([1], 22)).

-spec dim(binary()) -> binary().
dim(Text) ->
    run(Text, code([2], 22)).

-spec italic(binary()) -> binary().
italic(Text) ->
    run(Text, code([3], 23)).

-spec underline(binary()) -> binary().
underline(Text) ->
    run(Text, code([4], 24)).

-spec inverse(binary()) -> binary().
inverse(Text) ->
    run(Text, code([7], 27)).

-spec hidden(binary()) -> binary().
hidden(Text) ->
    run(Text, code([8], 28)).

-spec strikethrough(binary()) -> binary().
strikethrough(Text) ->
    run(Text, code([9], 29)).

-spec black(binary()) -> binary().
black(Text) ->
    run(Text, code([30], 39)).

-spec red(binary()) -> binary().
red(Text) ->
    run(Text, code([31], 39)).

-spec green(binary()) -> binary().
green(Text) ->
    run(Text, code([32], 39)).

-spec yellow(binary()) -> binary().
yellow(Text) ->
    run(Text, code([33], 39)).

-spec blue(binary()) -> binary().
blue(Text) ->
    run(Text, code([34], 39)).

-spec magenta(binary()) -> binary().
magenta(Text) ->
    run(Text, code([35], 39)).

-spec cyan(binary()) -> binary().
cyan(Text) ->
    run(Text, code([36], 39)).

-spec white(binary()) -> binary().
white(Text) ->
    run(Text, code([37], 39)).

-spec bright_black(binary()) -> binary().
bright_black(Text) ->
    run(Text, code([90], 39)).

-spec grey(binary()) -> binary().
grey(Text) ->
    bright_black(Text).

-spec gray(binary()) -> binary().
gray(Text) ->
    bright_black(Text).

-spec bright_red(binary()) -> binary().
bright_red(Text) ->
    run(Text, code([91], 39)).

-spec bright_green(binary()) -> binary().
bright_green(Text) ->
    run(Text, code([92], 39)).

-spec bright_yellow(binary()) -> binary().
bright_yellow(Text) ->
    run(Text, code([93], 39)).

-spec bright_blue(binary()) -> binary().
bright_blue(Text) ->
    run(Text, code([94], 39)).

-spec bright_magenta(binary()) -> binary().
bright_magenta(Text) ->
    run(Text, code([95], 39)).

-spec bright_cyan(binary()) -> binary().
bright_cyan(Text) ->
    run(Text, code([96], 39)).

-spec bright_white(binary()) -> binary().
bright_white(Text) ->
    run(Text, code([97], 39)).

-spec hex(binary(), integer()) -> binary().
hex(Text, Colour) ->
    Colour@1 = gleam@int:clamp(Colour, 16#0, 16#ffffff),
    run(
        Text,
        code(
            [38,
                2,
                begin
                    _pipe = erlang:'bsr'(Colour@1, 16),
                    erlang:'band'(_pipe, 16#ff)
                end,
                begin
                    _pipe@1 = erlang:'bsr'(Colour@1, 8),
                    erlang:'band'(_pipe@1, 16#ff)
                end,
                erlang:'band'(Colour@1, 16#ff)],
            39
        )
    ).

-spec pink(binary()) -> binary().
pink(Text) ->
    hex(Text, 16#ffaff3).

-spec colour(binary(), gleam_community@colour:colour()) -> binary().
colour(Text, Colour) ->
    Hex_colour = gleam_community@colour:to_rgb_hex(Colour),
    hex(Text, Hex_colour).

-spec color(binary(), gleam_community@colour:colour()) -> binary().
color(Text, Color) ->
    colour(Text, Color).

-spec bg_black(binary()) -> binary().
bg_black(Text) ->
    run(Text, code([40], 49)).

-spec bg_red(binary()) -> binary().
bg_red(Text) ->
    run(Text, code([41], 49)).

-spec bg_green(binary()) -> binary().
bg_green(Text) ->
    run(Text, code([42], 49)).

-spec bg_yellow(binary()) -> binary().
bg_yellow(Text) ->
    run(Text, code([43], 49)).

-spec bg_blue(binary()) -> binary().
bg_blue(Text) ->
    run(Text, code([44], 49)).

-spec bg_magenta(binary()) -> binary().
bg_magenta(Text) ->
    run(Text, code([45], 49)).

-spec bg_cyan(binary()) -> binary().
bg_cyan(Text) ->
    run(Text, code([46], 49)).

-spec bg_white(binary()) -> binary().
bg_white(Text) ->
    run(Text, code([47], 49)).

-spec bg_bright_black(binary()) -> binary().
bg_bright_black(Text) ->
    run(Text, code([100], 49)).

-spec bg_bright_red(binary()) -> binary().
bg_bright_red(Text) ->
    run(Text, code([101], 49)).

-spec bg_bright_green(binary()) -> binary().
bg_bright_green(Text) ->
    run(Text, code([102], 49)).

-spec bg_bright_yellow(binary()) -> binary().
bg_bright_yellow(Text) ->
    run(Text, code([103], 49)).

-spec bg_bright_blue(binary()) -> binary().
bg_bright_blue(Text) ->
    run(Text, code([104], 49)).

-spec bg_bright_magenta(binary()) -> binary().
bg_bright_magenta(Text) ->
    run(Text, code([105], 49)).

-spec bg_bright_cyan(binary()) -> binary().
bg_bright_cyan(Text) ->
    run(Text, code([106], 49)).

-spec bg_bright_white(binary()) -> binary().
bg_bright_white(Text) ->
    run(Text, code([107], 49)).

-spec bg_hex(binary(), integer()) -> binary().
bg_hex(Text, Colour) ->
    run(
        Text,
        code(
            [48,
                2,
                begin
                    _pipe = erlang:'bsr'(Colour, 16),
                    erlang:'band'(_pipe, 16#ff)
                end,
                begin
                    _pipe@1 = erlang:'bsr'(Colour, 8),
                    erlang:'band'(_pipe@1, 16#ff)
                end,
                erlang:'band'(Colour, 16#ff)],
            49
        )
    ).

-spec bg_pink(binary()) -> binary().
bg_pink(Text) ->
    bg_hex(Text, 16#ffaff3).

-spec bg_colour(binary(), gleam_community@colour:colour()) -> binary().
bg_colour(Text, Colour) ->
    Hex_colour = gleam_community@colour:to_rgb_hex(Colour),
    bg_hex(Text, Hex_colour).

-spec bg_color(binary(), gleam_community@colour:colour()) -> binary().
bg_color(Text, Colour) ->
    bg_colour(Text, Colour).
