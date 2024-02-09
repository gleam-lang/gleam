-module(hug).
-compile(no_auto_import).

-export([error/6, warning/6, info/6]).
-export_type([location/0, output/0]).

-type location() :: {location, integer(), integer()}.

-type output() :: error | warning | info.

-spec error(
    binary(),
    binary(),
    {integer(), integer()},
    {integer(), integer()},
    binary(),
    binary()
) -> binary().
error(File_name, Source, Start, End, Msg, Hint) ->
    output(
        File_name,
        Source,
        {location, erlang:element(1, Start), erlang:element(2, Start)},
        {location, erlang:element(1, End), erlang:element(2, End)},
        Msg,
        Hint,
        error
    ).

-spec warning(
    binary(),
    binary(),
    {integer(), integer()},
    {integer(), integer()},
    binary(),
    binary()
) -> binary().
warning(File_name, Source, Start, End, Msg, Hint) ->
    output(
        File_name,
        Source,
        {location, erlang:element(1, Start), erlang:element(2, Start)},
        {location, erlang:element(1, End), erlang:element(2, End)},
        Msg,
        Hint,
        warning
    ).

-spec info(
    binary(),
    binary(),
    {integer(), integer()},
    {integer(), integer()},
    binary(),
    binary()
) -> binary().
info(File_name, Source, Start, End, Msg, Hint) ->
    output(
        File_name,
        Source,
        {location, erlang:element(1, Start), erlang:element(2, Start)},
        {location, erlang:element(1, End), erlang:element(2, End)},
        Msg,
        Hint,
        info
    ).

-spec output(
    binary(),
    binary(),
    location(),
    location(),
    binary(),
    binary(),
    output()
) -> binary().
output(File_name, Source, Start, End, Err, Hint, Output) ->
    Header = construct_header(Err, Output),
    Body = construct_body(File_name, Source, Start, End, Output),
    gleam@string:join([Header, Body, <<""/utf8>>, Hint], <<"\n"/utf8>>).

-spec get_relevant_lines(list(binary()), location(), location()) -> list(binary()).
get_relevant_lines(Source_lines, Start, End) ->
    gleam@list:index_fold(
        Source_lines,
        [],
        fun(Lines, Line, Index) ->
            case ((Index
            + 1)
            >= erlang:element(2, Start))
            andalso ((Index
            + 1)
            =< erlang:element(2, End)) of
                true ->
                    gleam@list:append(Lines, [Line]);

                false ->
                    Lines
            end
        end
    ).

-spec underline_source(list(binary()), location(), location(), output()) -> list(binary()).
underline_source(Source_lines, Start, End, Output) ->
    Colour = case Output of
        error ->
            fun gleam_community@ansi:red/1;

        warning ->
            fun gleam_community@ansi:yellow/1;

        info ->
            fun gleam_community@ansi:blue/1
    end,
    gleam@list:index_map(
        Source_lines,
        fun(Index, Line) ->
            case gleam@string:trim(Line) of
                <<""/utf8>> ->
                    <<""/utf8>>;

                _@1 ->
                    case Index =:= 0 of
                        true ->
                            White_space = gleam@string:repeat(
                                <<" "/utf8>>,
                                erlang:element(3, Start)
                                - 1
                            ),
                            Underline_end = case erlang:element(2, End)
                            =:= erlang:element(2, Start) of
                                true ->
                                    erlang:element(3, End) - erlang:element(
                                        3,
                                        Start
                                    );

                                false ->
                                    gleam@string:length(Line) - gleam@string:length(
                                        White_space
                                    )
                            end,
                            <<White_space/binary,
                                (Colour(
                                    gleam@string:repeat(
                                        <<"~"/utf8>>,
                                        Underline_end
                                    )
                                ))/binary>>;

                        false ->
                            Line_length = gleam@string:length(Line),
                            Line_length_post_trim = gleam@string:length(
                                gleam@string:trim_left(Line)
                            ),
                            Num_white_space = Line_length
                            - Line_length_post_trim,
                            White_space@1 = gleam@string:repeat(
                                <<" "/utf8>>,
                                Num_white_space
                            ),
                            <<White_space@1/binary,
                                (Colour(
                                    gleam@string:repeat(
                                        <<"~"/utf8>>,
                                        Line_length_post_trim
                                    )
                                ))/binary>>
                    end
            end
        end
    ).

-spec construct_header(binary(), output()) -> binary().
construct_header(Message, Output) ->
    case Output of
        error ->
            <<(gleam_community@ansi:red(<<"error: "/utf8>>))/binary,
                Message/binary>>;

        warning ->
            <<(gleam_community@ansi:yellow(<<"warning: "/utf8>>))/binary,
                Message/binary>>;

        info ->
            <<(gleam_community@ansi:blue(<<"info: "/utf8>>))/binary,
                Message/binary>>
    end.

-spec construct_body(binary(), binary(), location(), location(), output()) -> binary().
construct_body(File_name, Source, Start, End, Output) ->
    Left_padding = gleam@int:max(
        gleam@string:length(gleam@int:to_string(erlang:element(2, Start))),
        gleam@string:length(gleam@int:to_string(erlang:element(2, End)))
    )
    - 1,
    Body_start = <<<<<<<<<<<<(gleam@string:repeat(<<" "/utf8>>, Left_padding))/binary,
                            "  ┌─ "/utf8>>/binary,
                        File_name/binary>>/binary,
                    ":"/utf8>>/binary,
                (gleam@int:to_string(erlang:element(2, Start)))/binary>>/binary,
            ":"/utf8>>/binary,
        (gleam@int:to_string(erlang:element(3, Start)))/binary>>,
    Relevant_lines = get_relevant_lines(
        gleam@string:split(Source, <<"\n"/utf8>>),
        Start,
        End
    ),
    Underlines = underline_source(Relevant_lines, Start, End, Output),
    Trim_left_amount = get_trim_left_amount(Relevant_lines),
    Body = begin
        _pipe = gleam@list:zip(Relevant_lines, Underlines),
        _pipe@1 = gleam@list:index_map(
            _pipe,
            fun(Index, Input) ->
                construct_output_line(
                    Input,
                    Index
                    + erlang:element(2, Start),
                    Trim_left_amount,
                    Left_padding
                )
            end
        ),
        gleam@string:join(_pipe@1, <<"\n"/utf8>>)
    end,
    gleam@string:join(
        [Body_start,
            <<(gleam@string:repeat(<<" "/utf8>>, Left_padding))/binary,
                "  │"/utf8>>,
            Body,
            <<(gleam@string:repeat(<<" "/utf8>>, Left_padding))/binary,
                "  │"/utf8>>],
        <<"\n"/utf8>>
    ).

-spec construct_output_line(
    {binary(), binary()},
    integer(),
    integer(),
    integer()
) -> binary().
construct_output_line(Input, Row, Trim_left_amount, Left_padding) ->
    {Source_line, Underline} = Input,
    Line_number_padding = (Left_padding
    - gleam@string:length(gleam@int:to_string(Row)))
    + 1,
    Source_line@1 = <<<<<<(gleam_community@ansi:green(gleam@int:to_string(Row)))/binary,
                (gleam@string:repeat(<<" "/utf8>>, Line_number_padding))/binary>>/binary,
            " │ "/utf8>>/binary,
        (trim_left(Source_line, Trim_left_amount))/binary>>,
    case gleam@string:length(Underline) of
        0 ->
            Source_line@1;

        _@1 ->
            Underline_line = <<<<(gleam@string:repeat(
                        <<" "/utf8>>,
                        Left_padding
                    ))/binary,
                    "  │ "/utf8>>/binary,
                (trim_left(Underline, Trim_left_amount))/binary>>,
            gleam@string:join([Source_line@1, Underline_line], <<"\n"/utf8>>)
    end.

-spec get_trim_left_amount(list(binary())) -> integer().
get_trim_left_amount(Lines) ->
    Get_left_white_space = fun(Line) ->
        gleam@string:length(Line)
        - begin
            _pipe = Line,
            _pipe@1 = gleam@string:trim_left(_pipe),
            gleam@string:length(_pipe@1)
        end
    end,
    First_line = begin
        _pipe@2 = gleam@list:first(Lines),
        gleam@result:unwrap(_pipe@2, <<""/utf8>>)
    end,
    gleam@list:fold(
        Lines,
        Get_left_white_space(First_line),
        fun(Min_white_space, Line@1) ->
            case gleam@string:trim(Line@1) of
                <<""/utf8>> ->
                    Min_white_space;

                _@1 ->
                    White_space = gleam@string:length(Line@1)
                    - begin
                        _pipe@3 = Line@1,
                        _pipe@4 = gleam@string:trim_left(_pipe@3),
                        gleam@string:length(_pipe@4)
                    end,
                    gleam@int:min(Min_white_space, White_space)
            end
        end
    ).

-spec trim_left(binary(), integer()) -> binary().
trim_left(Str, Num_white_space) ->
    String_length = gleam@string:length(Str),
    gleam@string:slice(Str, Num_white_space, String_length).
