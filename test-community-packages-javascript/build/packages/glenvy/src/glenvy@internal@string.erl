-module(glenvy@internal@string).
-compile([no_auto_import, nowarn_unused_vars]).

-export([trim_chars_left/2, trim_chars_right/2]).

-spec trim_chars_left(binary(), binary()) -> binary().
trim_chars_left(Value, Chars_to_trim) ->
    case gleam@string:is_empty(Chars_to_trim) orelse not gleam@string:starts_with(
        Value,
        Chars_to_trim
    ) of
        true ->
            Value;

        false ->
            _pipe = Value,
            gleam@string:slice(
                _pipe,
                gleam@string:length(Chars_to_trim),
                gleam@string:length(Value)
            )
    end.

-spec trim_chars_right(binary(), binary()) -> binary().
trim_chars_right(Value, Chars_to_trim) ->
    case gleam@string:is_empty(Chars_to_trim) orelse not gleam@string:ends_with(
        Value,
        Chars_to_trim
    ) of
        true ->
            Value;

        false ->
            _pipe = Value,
            gleam@string:slice(
                _pipe,
                0,
                gleam@string:length(Value) - gleam@string:length(Chars_to_trim)
            )
    end.
