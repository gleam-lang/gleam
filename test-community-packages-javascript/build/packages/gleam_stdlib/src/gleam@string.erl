-module(gleam@string).
-compile([no_auto_import, nowarn_unused_vars]).

-export([is_empty/1, reverse/1, replace/3, append/2, concat/1, repeat/2, join/2, to_option/1, length/1, lowercase/1, uppercase/1, compare/2, slice/3, drop_left/2, drop_right/2, pad_left/3, pad_right/3, crop/2, contains/2, starts_with/2, ends_with/2, split_once/2, trim/1, trim_left/1, trim_right/1, pop_grapheme/1, first/1, last/1, capitalise/1, utf_codepoint/1, utf_codepoint_to_int/1, inspect/1, byte_size/1, to_graphemes/1, split/2, to_utf_codepoints/1, from_utf_codepoints/1]).
-export_type([direction/0]).

-type direction() :: leading | trailing | both.

-spec is_empty(binary()) -> boolean().
is_empty(Str) ->
    Str =:= <<""/utf8>>.

-spec do_reverse(binary()) -> binary().
do_reverse(String) ->
    _pipe = String,
    _pipe@1 = gleam@string_builder:from_string(_pipe),
    _pipe@2 = gleam@string_builder:reverse(_pipe@1),
    gleam@string_builder:to_string(_pipe@2).

-spec reverse(binary()) -> binary().
reverse(String) ->
    do_reverse(String).

-spec replace(binary(), binary(), binary()) -> binary().
replace(String, Pattern, Substitute) ->
    _pipe = String,
    _pipe@1 = gleam@string_builder:from_string(_pipe),
    _pipe@2 = gleam@string_builder:replace(_pipe@1, Pattern, Substitute),
    gleam@string_builder:to_string(_pipe@2).

-spec append(binary(), binary()) -> binary().
append(First, Second) ->
    _pipe = First,
    _pipe@1 = gleam@string_builder:from_string(_pipe),
    _pipe@2 = gleam@string_builder:append(_pipe@1, Second),
    gleam@string_builder:to_string(_pipe@2).

-spec concat(list(binary())) -> binary().
concat(Strings) ->
    _pipe = Strings,
    _pipe@1 = gleam@string_builder:from_strings(_pipe),
    gleam@string_builder:to_string(_pipe@1).

-spec repeat(binary(), integer()) -> binary().
repeat(String, Times) ->
    _pipe = gleam@iterator:repeat(String),
    _pipe@1 = gleam@iterator:take(_pipe, Times),
    _pipe@2 = gleam@iterator:to_list(_pipe@1),
    concat(_pipe@2).

-spec join(list(binary()), binary()) -> binary().
join(Strings, Separator) ->
    _pipe = Strings,
    _pipe@1 = gleam@list:intersperse(_pipe, Separator),
    concat(_pipe@1).

-spec to_option(binary()) -> gleam@option:option(binary()).
to_option(S) ->
    case S of
        <<""/utf8>> ->
            none;

        _ ->
            {some, S}
    end.

-spec length(binary()) -> integer().
length(String) ->
    string:length(String).

-spec lowercase(binary()) -> binary().
lowercase(String) ->
    string:lowercase(String).

-spec uppercase(binary()) -> binary().
uppercase(String) ->
    string:uppercase(String).

-spec compare(binary(), binary()) -> gleam@order:order().
compare(A, B) ->
    case A =:= B of
        true ->
            eq;

        _ ->
            case gleam_stdlib:less_than(A, B) of
                true ->
                    lt;

                _ ->
                    gt
            end
    end.

-spec slice(binary(), integer(), integer()) -> binary().
slice(String, Idx, Len) ->
    case Len < 0 of
        true ->
            <<""/utf8>>;

        false ->
            case Idx < 0 of
                true ->
                    Translated_idx = length(String) + Idx,
                    case Translated_idx < 0 of
                        true ->
                            <<""/utf8>>;

                        false ->
                            string:slice(String, Translated_idx, Len)
                    end;

                false ->
                    string:slice(String, Idx, Len)
            end
    end.

-spec drop_left(binary(), integer()) -> binary().
drop_left(String, Num_graphemes) ->
    case Num_graphemes < 0 of
        true ->
            String;

        false ->
            slice(String, Num_graphemes, length(String) - Num_graphemes)
    end.

-spec drop_right(binary(), integer()) -> binary().
drop_right(String, Num_graphemes) ->
    case Num_graphemes < 0 of
        true ->
            String;

        false ->
            slice(String, 0, length(String) - Num_graphemes)
    end.

-spec padding(integer(), binary()) -> gleam@iterator:iterator(binary()).
padding(Size, Pad_string) ->
    Pad_length = length(Pad_string),
    Num_pads = case Pad_length of
        0 -> 0;
        Gleam@denominator -> Size div Gleam@denominator
    end,
    Extra = case Pad_length of
        0 -> 0;
        Gleam@denominator@1 -> Size rem Gleam@denominator@1
    end,
    _pipe = gleam@iterator:repeat(Pad_string),
    _pipe@1 = gleam@iterator:take(_pipe, Num_pads),
    gleam@iterator:append(
        _pipe@1,
        gleam@iterator:single(slice(Pad_string, 0, Extra))
    ).

-spec pad_left(binary(), integer(), binary()) -> binary().
pad_left(String, Desired_length, Pad_string) ->
    Current_length = length(String),
    To_pad_length = Desired_length - Current_length,
    _pipe = padding(To_pad_length, Pad_string),
    _pipe@1 = gleam@iterator:append(_pipe, gleam@iterator:single(String)),
    _pipe@2 = gleam@iterator:to_list(_pipe@1),
    concat(_pipe@2).

-spec pad_right(binary(), integer(), binary()) -> binary().
pad_right(String, Desired_length, Pad_string) ->
    Current_length = length(String),
    To_pad_length = Desired_length - Current_length,
    _pipe = gleam@iterator:single(String),
    _pipe@1 = gleam@iterator:append(_pipe, padding(To_pad_length, Pad_string)),
    _pipe@2 = gleam@iterator:to_list(_pipe@1),
    concat(_pipe@2).

-spec do_crop(binary(), binary()) -> binary().
do_crop(String, Substring) ->
    _pipe = String,
    _pipe@1 = string:find(_pipe, Substring),
    _pipe@2 = gleam@dynamic:string(_pipe@1),
    gleam@result:unwrap(_pipe@2, String).

-spec crop(binary(), binary()) -> binary().
crop(String, Substring) ->
    do_crop(String, Substring).

-spec do_contains(binary(), binary()) -> boolean().
do_contains(Haystack, Needle) ->
    _pipe = Haystack,
    _pipe@1 = string:find(_pipe, Needle),
    _pipe@2 = gleam@dynamic:bit_string(_pipe@1),
    gleam@result:is_ok(_pipe@2).

-spec contains(binary(), binary()) -> boolean().
contains(Haystack, Needle) ->
    do_contains(Haystack, Needle).

-spec starts_with(binary(), binary()) -> boolean().
starts_with(String, Prefix) ->
    gleam_stdlib:string_starts_with(String, Prefix).

-spec ends_with(binary(), binary()) -> boolean().
ends_with(String, Suffix) ->
    gleam_stdlib:string_ends_with(String, Suffix).

-spec do_split_once(binary(), binary()) -> {ok, {binary(), binary()}} |
    {error, nil}.
do_split_once(X, Substring) ->
    case string:split(X, Substring) of
        [First, Rest] ->
            {ok, {First, Rest}};

        _ ->
            {error, nil}
    end.

-spec split_once(binary(), binary()) -> {ok, {binary(), binary()}} |
    {error, nil}.
split_once(X, Substring) ->
    do_split_once(X, Substring).

-spec do_trim(binary()) -> binary().
do_trim(String) ->
    string:trim(String, both).

-spec trim(binary()) -> binary().
trim(String) ->
    do_trim(String).

-spec do_trim_left(binary()) -> binary().
do_trim_left(String) ->
    string:trim(String, leading).

-spec trim_left(binary()) -> binary().
trim_left(String) ->
    do_trim_left(String).

-spec do_trim_right(binary()) -> binary().
do_trim_right(String) ->
    string:trim(String, trailing).

-spec trim_right(binary()) -> binary().
trim_right(String) ->
    do_trim_right(String).

-spec pop_grapheme(binary()) -> {ok, {binary(), binary()}} | {error, nil}.
pop_grapheme(String) ->
    gleam_stdlib:string_pop_grapheme(String).

-spec first(binary()) -> {ok, binary()} | {error, nil}.
first(S) ->
    case pop_grapheme(S) of
        {ok, {First, _}} ->
            {ok, First};

        {error, E} ->
            {error, E}
    end.

-spec last(binary()) -> {ok, binary()} | {error, nil}.
last(S) ->
    case pop_grapheme(S) of
        {ok, {First, <<""/utf8>>}} ->
            {ok, First};

        {ok, {_, Rest}} ->
            {ok, slice(Rest, -1, 1)};

        {error, E} ->
            {error, E}
    end.

-spec capitalise(binary()) -> binary().
capitalise(S) ->
    case pop_grapheme(S) of
        {ok, {First, Rest}} ->
            append(uppercase(First), lowercase(Rest));

        _ ->
            <<""/utf8>>
    end.

-spec utf_codepoint(integer()) -> {ok, integer()} | {error, nil}.
utf_codepoint(Value) ->
    case Value of
        I when I > 1114111 ->
            {error, nil};

        65534 ->
            {error, nil};

        65535 ->
            {error, nil};

        I@1 when (I@1 >= 55296) andalso (I@1 =< 57343) ->
            {error, nil};

        I@2 ->
            {ok, gleam_stdlib:identity(I@2)}
    end.

-spec utf_codepoint_to_int(integer()) -> integer().
utf_codepoint_to_int(Cp) ->
    gleam_stdlib:identity(Cp).

-spec inspect(any()) -> binary().
inspect(Term) ->
    _pipe = gleam_stdlib:inspect(Term),
    gleam@string_builder:to_string(_pipe).

-spec byte_size(binary()) -> integer().
byte_size(String) ->
    erlang:byte_size(String).

-spec do_to_graphemes(binary(), list(binary())) -> list(binary()).
do_to_graphemes(String, Acc) ->
    case pop_grapheme(String) of
        {ok, {Grapheme, Rest}} ->
            do_to_graphemes(Rest, [Grapheme | Acc]);

        _ ->
            Acc
    end.

-spec to_graphemes(binary()) -> list(binary()).
to_graphemes(String) ->
    _pipe = do_to_graphemes(String, []),
    gleam@list:reverse(_pipe).

-spec split(binary(), binary()) -> list(binary()).
split(X, Substring) ->
    case Substring of
        <<""/utf8>> ->
            to_graphemes(X);

        _ ->
            _pipe = X,
            _pipe@1 = gleam@string_builder:from_string(_pipe),
            _pipe@2 = gleam@string_builder:split(_pipe@1, Substring),
            gleam@list:map(_pipe@2, fun gleam@string_builder:to_string/1)
    end.

-spec do_to_utf_codepoints_impl(bitstring(), list(integer())) -> list(integer()).
do_to_utf_codepoints_impl(Bit_string, Acc) ->
    case Bit_string of
        <<First/utf8, Rest/binary>> ->
            do_to_utf_codepoints_impl(Rest, [First | Acc]);

        <<>> ->
            Acc
    end.

-spec do_to_utf_codepoints(binary()) -> list(integer()).
do_to_utf_codepoints(String) ->
    _pipe = do_to_utf_codepoints_impl(gleam@bit_string:from_string(String), []),
    gleam@list:reverse(_pipe).

-spec to_utf_codepoints(binary()) -> list(integer()).
to_utf_codepoints(String) ->
    do_to_utf_codepoints(String).

-spec do_from_utf_codepoints_impl(list(integer()), bitstring()) -> bitstring().
do_from_utf_codepoints_impl(Utf_codepoints, Acc) ->
    case Utf_codepoints of
        [First | Rest] ->
            do_from_utf_codepoints_impl(Rest, <<Acc/bitstring, First/utf8>>);

        [] ->
            Acc
    end.

-spec do_from_utf_codepoints(list(integer())) -> binary().
do_from_utf_codepoints(Utf_codepoints) ->
    _assert_subject = begin
        _pipe = do_from_utf_codepoints_impl(
            Utf_codepoints,
            gleam@bit_string:from_string(<<""/utf8>>)
        ),
        gleam@bit_string:to_string(_pipe)
    end,
    {ok, String} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/string"/utf8>>,
                        function => <<"do_from_utf_codepoints"/utf8>>,
                        line => 860})
    end,
    String.

-spec from_utf_codepoints(list(integer())) -> binary().
from_utf_codepoints(Utf_codepoints) ->
    do_from_utf_codepoints(Utf_codepoints).
