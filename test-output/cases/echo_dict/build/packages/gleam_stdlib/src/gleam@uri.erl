-module(gleam@uri).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse/1, parse_query/1, percent_encode/1, query_to_string/1, percent_decode/1, path_segments/1, to_string/1, origin/1, merge/2]).
-export_type([uri/0]).

-type uri() :: {uri,
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(integer()),
        binary(),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 116).
-spec regex_submatches(binary(), binary()) -> list(gleam@option:option(binary())).
regex_submatches(Pattern, String) ->
    _pipe = Pattern,
    _pipe@1 = gleam@regex:compile(_pipe, {options, true, false}),
    _pipe@2 = gleam@result:nil_error(_pipe@1),
    _pipe@3 = gleam@result:map(
        _pipe@2,
        fun(_capture) -> gleam@regex:scan(_capture, String) end
    ),
    _pipe@4 = gleam@result:'try'(_pipe@3, fun gleam@list:first/1),
    _pipe@5 = gleam@result:map(_pipe@4, fun(M) -> erlang:element(3, M) end),
    gleam@result:unwrap(_pipe@5, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 126).
-spec noneify_query(gleam@option:option(binary())) -> gleam@option:option(binary()).
noneify_query(X) ->
    case X of
        none ->
            none;

        {some, X@1} ->
            case gleam@string:pop_grapheme(X@1) of
                {ok, {<<"?"/utf8>>, Query}} ->
                    {some, Query};

                _ ->
                    none
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 137).
-spec noneify_empty_string(gleam@option:option(binary())) -> gleam@option:option(binary()).
noneify_empty_string(X) ->
    case X of
        {some, <<""/utf8>>} ->
            none;

        none ->
            none;

        {some, _} ->
            X
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 178).
-spec extra_required(list(any()), integer()) -> integer().
extra_required(List, Remaining) ->
    case List of
        _ when Remaining =:= 0 ->
            0;

        [] ->
            Remaining;

        [_ | Rest] ->
            extra_required(Rest, Remaining - 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 173).
-spec pad_list(list(gleam@option:option(FJT)), integer()) -> list(gleam@option:option(FJT)).
pad_list(List, Size) ->
    _pipe = List,
    lists:append(_pipe, gleam@list:repeat(none, extra_required(List, Size))).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 145).
-spec split_authority(gleam@option:option(binary())) -> {gleam@option:option(binary()),
    gleam@option:option(binary()),
    gleam@option:option(integer())}.
split_authority(Authority) ->
    case gleam@option:unwrap(Authority, <<""/utf8>>) of
        <<""/utf8>> ->
            {none, none, none};

        <<"//"/utf8>> ->
            {none, {some, <<""/utf8>>}, none};

        Authority@1 ->
            Matches = begin
                _pipe = <<"^(//)?((.*)@)?(\\[[a-zA-Z0-9:.]*\\]|[^:]*)(:(\\d*))?"/utf8>>,
                _pipe@1 = regex_submatches(_pipe, Authority@1),
                pad_list(_pipe@1, 6)
            end,
            case Matches of
                [_, _, Userinfo, Host, _, Port] ->
                    Userinfo@1 = noneify_empty_string(Userinfo),
                    Host@1 = noneify_empty_string(Host),
                    Port@1 = begin
                        _pipe@2 = Port,
                        _pipe@3 = gleam@option:unwrap(_pipe@2, <<""/utf8>>),
                        _pipe@4 = gleam@int:parse(_pipe@3),
                        gleam@option:from_result(_pipe@4)
                    end,
                    {Userinfo@1, Host@1, Port@1};

                _ ->
                    {none, none, none}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 56).
-spec parse(binary()) -> {ok, uri()} | {error, nil}.
parse(Uri_string) ->
    gleam_stdlib:uri_parse(Uri_string).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 198).
-spec parse_query(binary()) -> {ok, list({binary(), binary()})} | {error, nil}.
parse_query(Query) ->
    gleam_stdlib:parse_query(Query).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 242).
-spec percent_encode(binary()) -> binary().
percent_encode(Value) ->
    gleam_stdlib:percent_encode(Value).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 225).
-spec query_pair({binary(), binary()}) -> gleam@string_builder:string_builder().
query_pair(Pair) ->
    gleam@string_builder:from_strings(
        [percent_encode(erlang:element(1, Pair)),
            <<"="/utf8>>,
            percent_encode(erlang:element(2, Pair))]
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 217).
-spec query_to_string(list({binary(), binary()})) -> binary().
query_to_string(Query) ->
    _pipe = Query,
    _pipe@1 = gleam@list:map(_pipe, fun query_pair/1),
    _pipe@2 = gleam@list:intersperse(
        _pipe@1,
        gleam@string_builder:from_string(<<"&"/utf8>>)
    ),
    _pipe@3 = gleam@string_builder:concat(_pipe@2),
    gleam@string_builder:to_string(_pipe@3).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 259).
-spec percent_decode(binary()) -> {ok, binary()} | {error, nil}.
percent_decode(Value) ->
    gleam_stdlib:percent_decode(Value).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 267).
-spec do_remove_dot_segments(list(binary()), list(binary())) -> list(binary()).
do_remove_dot_segments(Input, Accumulator) ->
    case Input of
        [] ->
            lists:reverse(Accumulator);

        [Segment | Rest] ->
            Accumulator@5 = case {Segment, Accumulator} of
                {<<""/utf8>>, Accumulator@1} ->
                    Accumulator@1;

                {<<"."/utf8>>, Accumulator@2} ->
                    Accumulator@2;

                {<<".."/utf8>>, []} ->
                    [];

                {<<".."/utf8>>, [_ | Accumulator@3]} ->
                    Accumulator@3;

                {Segment@1, Accumulator@4} ->
                    [Segment@1 | Accumulator@4]
            end,
            do_remove_dot_segments(Rest, Accumulator@5)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 286).
-spec remove_dot_segments(list(binary())) -> list(binary()).
remove_dot_segments(Input) ->
    do_remove_dot_segments(Input, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 302).
-spec path_segments(binary()) -> list(binary()).
path_segments(Path) ->
    remove_dot_segments(gleam@string:split(Path, <<"/"/utf8>>)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 318).
-spec to_string(uri()) -> binary().
to_string(Uri) ->
    Parts = case erlang:element(8, Uri) of
        {some, Fragment} ->
            [<<"#"/utf8>>, Fragment];

        _ ->
            []
    end,
    Parts@1 = case erlang:element(7, Uri) of
        {some, Query} ->
            [<<"?"/utf8>>, Query | Parts];

        _ ->
            Parts
    end,
    Parts@2 = [erlang:element(6, Uri) | Parts@1],
    Parts@3 = case {erlang:element(4, Uri),
        gleam@string:starts_with(erlang:element(6, Uri), <<"/"/utf8>>)} of
        {{some, Host}, false} when Host =/= <<""/utf8>> ->
            [<<"/"/utf8>> | Parts@2];

        {_, _} ->
            Parts@2
    end,
    Parts@4 = case {erlang:element(4, Uri), erlang:element(5, Uri)} of
        {{some, _}, {some, Port}} ->
            [<<":"/utf8>>, gleam@int:to_string(Port) | Parts@3];

        {_, _} ->
            Parts@3
    end,
    Parts@5 = case {erlang:element(2, Uri),
        erlang:element(3, Uri),
        erlang:element(4, Uri)} of
        {{some, S}, {some, U}, {some, H}} ->
            [S, <<"://"/utf8>>, U, <<"@"/utf8>>, H | Parts@4];

        {{some, S@1}, none, {some, H@1}} ->
            [S@1, <<"://"/utf8>>, H@1 | Parts@4];

        {{some, S@2}, {some, _}, none} ->
            [S@2, <<":"/utf8>> | Parts@4];

        {{some, S@2}, none, none} ->
            [S@2, <<":"/utf8>> | Parts@4];

        {none, none, {some, H@2}} ->
            [<<"//"/utf8>>, H@2 | Parts@4];

        {_, _, _} ->
            Parts@4
    end,
    gleam@string:concat(Parts@5).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 362).
-spec origin(uri()) -> {ok, binary()} | {error, nil}.
origin(Uri) ->
    {uri, Scheme, _, Host, Port, _, _, _} = Uri,
    case {Host, Scheme} of
        {{some, H}, {some, <<"https"/utf8>>}} when Port =:= {some, 443} ->
            {ok, gleam@string:concat([<<"https://"/utf8>>, H])};

        {{some, H@1}, {some, <<"http"/utf8>>}} when Port =:= {some, 80} ->
            {ok, gleam@string:concat([<<"http://"/utf8>>, H@1])};

        {{some, H@2}, {some, S}} when (S =:= <<"http"/utf8>>) orelse (S =:= <<"https"/utf8>>) ->
            case Port of
                {some, P} ->
                    {ok,
                        gleam@string:concat(
                            [S,
                                <<"://"/utf8>>,
                                H@2,
                                <<":"/utf8>>,
                                gleam@int:to_string(P)]
                        )};

                none ->
                    {ok, gleam@string:concat([S, <<"://"/utf8>>, H@2])}
            end;

        {_, _} ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 379).
-spec drop_last(list(FKT)) -> list(FKT).
drop_last(Elements) ->
    gleam@list:take(Elements, erlang:length(Elements) - 1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 383).
-spec join_segments(list(binary())) -> binary().
join_segments(Segments) ->
    gleam@string:join([<<""/utf8>> | Segments], <<"/"/utf8>>).

-file("/Users/louis/src/gleam/stdlib/src/gleam/uri.gleam", 393).
-spec merge(uri(), uri()) -> {ok, uri()} | {error, nil}.
merge(Base, Relative) ->
    case Base of
        {uri, {some, _}, _, {some, _}, _, _, _, _} ->
            case Relative of
                {uri, _, _, {some, _}, _, _, _, _} ->
                    Path = begin
                        _pipe = gleam@string:split(
                            erlang:element(6, Relative),
                            <<"/"/utf8>>
                        ),
                        _pipe@1 = remove_dot_segments(_pipe),
                        join_segments(_pipe@1)
                    end,
                    Resolved = {uri,
                        gleam@option:'or'(
                            erlang:element(2, Relative),
                            erlang:element(2, Base)
                        ),
                        none,
                        erlang:element(4, Relative),
                        gleam@option:'or'(
                            erlang:element(5, Relative),
                            erlang:element(5, Base)
                        ),
                        Path,
                        erlang:element(7, Relative),
                        erlang:element(8, Relative)},
                    {ok, Resolved};

                _ ->
                    {New_path, New_query} = case erlang:element(6, Relative) of
                        <<""/utf8>> ->
                            {erlang:element(6, Base),
                                gleam@option:'or'(
                                    erlang:element(7, Relative),
                                    erlang:element(7, Base)
                                )};

                        _ ->
                            Path_segments = case gleam@string:starts_with(
                                erlang:element(6, Relative),
                                <<"/"/utf8>>
                            ) of
                                true ->
                                    gleam@string:split(
                                        erlang:element(6, Relative),
                                        <<"/"/utf8>>
                                    );

                                false ->
                                    _pipe@2 = gleam@string:split(
                                        erlang:element(6, Base),
                                        <<"/"/utf8>>
                                    ),
                                    _pipe@3 = drop_last(_pipe@2),
                                    lists:append(
                                        _pipe@3,
                                        gleam@string:split(
                                            erlang:element(6, Relative),
                                            <<"/"/utf8>>
                                        )
                                    )
                            end,
                            Path@1 = begin
                                _pipe@4 = Path_segments,
                                _pipe@5 = remove_dot_segments(_pipe@4),
                                join_segments(_pipe@5)
                            end,
                            {Path@1, erlang:element(7, Relative)}
                    end,
                    Resolved@1 = {uri,
                        erlang:element(2, Base),
                        none,
                        erlang:element(4, Base),
                        erlang:element(5, Base),
                        New_path,
                        New_query,
                        erlang:element(8, Relative)},
                    {ok, Resolved@1}
            end;

        _ ->
            {error, nil}
    end.
