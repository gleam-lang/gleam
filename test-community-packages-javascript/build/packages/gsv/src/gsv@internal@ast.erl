-module(gsv@internal@ast).
-compile([no_auto_import, nowarn_unused_vars]).

-export([parse/1]).
-export_type([parse_state/0]).

-type parse_state() :: beginning |
    just_parsed_field |
    just_parsed_comma |
    just_parsed_newline |
    just_parsed_cr |
    inside_escaped_string.

-spec parse_p(
    list(gsv@internal@token:csv_token()),
    parse_state(),
    list(list(binary()))
) -> {ok, list(list(binary()))} | {error, nil}.
parse_p(Input, Parse_state, Llf) ->
    case {Input, Parse_state, Llf} of
        {[], beginning, _} ->
            {error, nil};

        {[], _, Llf@1} ->
            {ok, Llf@1};

        {[{textdata, Str} | Remaining_tokens], beginning, []} ->
            parse_p(Remaining_tokens, just_parsed_field, [[Str]]);

        {[doublequote | Remaining_tokens@1], beginning, []} ->
            parse_p(Remaining_tokens@1, inside_escaped_string, [[<<""/utf8>>]]);

        {_, beginning, _} ->
            {error, nil};

        {[comma | Remaining_tokens@2], just_parsed_field, Llf@2} ->
            parse_p(Remaining_tokens@2, just_parsed_comma, Llf@2);

        {[lf | Remaining_tokens@3], just_parsed_field, Llf@3} ->
            parse_p(Remaining_tokens@3, just_parsed_newline, Llf@3);

        {[cr | Remaining_tokens@4], just_parsed_field, Llf@4} ->
            parse_p(Remaining_tokens@4, just_parsed_cr, Llf@4);

        {_, just_parsed_field, _} ->
            {error, nil};

        {[lf | Remaining_tokens@5], just_parsed_cr, Llf@5} ->
            parse_p(Remaining_tokens@5, just_parsed_newline, Llf@5);

        {_, just_parsed_cr, _} ->
            {error, nil};

        {[{textdata, Str@1} | Remaining_tokens@6],
            just_parsed_comma,
            [Curr_line | Previously_parsed_lines]} ->
            parse_p(
                Remaining_tokens@6,
                just_parsed_field,
                [[Str@1 | Curr_line] | Previously_parsed_lines]
            );

        {[doublequote | Remaining_tokens@7],
            just_parsed_comma,
            [Curr_line@1 | Previously_parsed_lines@1]} ->
            parse_p(
                Remaining_tokens@7,
                inside_escaped_string,
                [[<<""/utf8>> | Curr_line@1] | Previously_parsed_lines@1]
            );

        {_, just_parsed_comma, _} ->
            {error, nil};

        {[{textdata, Str@2} | Remaining_tokens@8], just_parsed_newline, Llf@6} ->
            parse_p(Remaining_tokens@8, just_parsed_field, [[Str@2] | Llf@6]);

        {[doublequote | Remaining_tokens@9],
            just_parsed_newline,
            [Curr_line@2 | Previously_parsed_lines@2]} ->
            parse_p(
                Remaining_tokens@9,
                inside_escaped_string,
                [[<<""/utf8>> | Curr_line@2] | Previously_parsed_lines@2]
            );

        {_, just_parsed_newline, _} ->
            {error, nil};

        {[doublequote, doublequote | Remaining_tokens@10],
            inside_escaped_string,
            [[Str@3 | Rest_curr_line] | Previously_parsed_lines@3]} ->
            parse_p(
                Remaining_tokens@10,
                inside_escaped_string,
                [[<<Str@3/binary, "\""/utf8>> | Rest_curr_line] |
                    Previously_parsed_lines@3]
            );

        {[doublequote | Remaining_tokens@11], inside_escaped_string, Llf@7} ->
            parse_p(Remaining_tokens@11, just_parsed_field, Llf@7);

        {[Other_token | Remaining_tokens@12],
            inside_escaped_string,
            [[Str@4 | Rest_curr_line@1] | Previously_parsed_lines@4]} ->
            parse_p(
                Remaining_tokens@12,
                inside_escaped_string,
                [[<<Str@4/binary,
                            (gsv@internal@token:to_lexeme(Other_token))/binary>> |
                        Rest_curr_line@1] |
                    Previously_parsed_lines@4]
            )
    end.

-spec parse(list(gsv@internal@token:csv_token())) -> {ok, list(list(binary()))} |
    {error, nil}.
parse(Input) ->
    Inner_rev = (gleam@result:'try'(
        parse_p(Input, beginning, []),
        fun(Llf) ->
            gleam@list:try_map(Llf, fun(Lf) -> {ok, gleam@list:reverse(Lf)} end)
        end
    )),
    gleam@result:'try'(Inner_rev, fun(Ir) -> {ok, gleam@list:reverse(Ir)} end).
