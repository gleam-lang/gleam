-module(glenvy@internal@parser).
-compile([no_auto_import, nowarn_unused_vars]).

-export([parse_env_file/1]).

-spec skip_line_comment(binary()) -> {ok, binary()} | {error, nil}.
skip_line_comment(Line) ->
    case begin
        _pipe = Line,
        gleam@string:starts_with(_pipe, <<"#"/utf8>>)
    end of
        false ->
            {ok, Line};

        true ->
            {error, nil}
    end.

-spec strip_comments(binary()) -> {ok, binary()} | {error, nil}.
strip_comments(Line) ->
    case begin
        _pipe = Line,
        gleam@string:split_once(_pipe, <<"#"/utf8>>)
    end of
        {ok, {Value, _}} ->
            {ok, Value};

        {error, nil} ->
            {ok, Line}
    end.

-spec unquote(binary(), binary()) -> binary().
unquote(Line, Quote_char) ->
    _pipe = Line,
    _pipe@1 = glenvy@internal@string:trim_chars_left(_pipe, Quote_char),
    glenvy@internal@string:trim_chars_right(_pipe@1, Quote_char).

-spec parse_line(binary()) -> {ok, {binary(), binary()}} | {error, nil}.
parse_line(Line) ->
    gleam@result:'try'(
        skip_line_comment(Line),
        fun(Line@1) ->
            gleam@result:'try'(
                begin
                    _pipe = Line@1,
                    gleam@string:split_once(_pipe, <<"="/utf8>>)
                end,
                fun(_use0) ->
                    {Key, Value} = _use0,
                    gleam@result:'try'(
                        strip_comments(Value),
                        fun(Value@1) ->
                            Value@2 = begin
                                _pipe@1 = Value@1,
                                _pipe@2 = gleam@string:trim(_pipe@1),
                                _pipe@3 = unquote(_pipe@2, <<"'"/utf8>>),
                                unquote(_pipe@3, <<"\""/utf8>>)
                            end,
                            {ok, {Key, Value@2}}
                        end
                    )
                end
            )
        end
    ).

-spec parse_env_file(binary()) -> gleam@map:map_(binary(), binary()).
parse_env_file(Contents) ->
    Lines = glx@stringx:lines(Contents),
    Env_vars = begin
        _pipe = Lines,
        _pipe@1 = gleam@list:filter_map(_pipe, fun parse_line/1),
        gleam@map:from_list(_pipe@1)
    end,
    Env_vars.
