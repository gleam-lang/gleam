-module(gsv).
-compile([no_auto_import, nowarn_unused_vars]).

-export([to_lists/1, from_lists/3]).
-export_type([line_ending/0]).

-type line_ending() :: windows | unix.

-spec to_lists(binary()) -> {ok, list(list(binary()))} | {error, nil}.
to_lists(Input) ->
    _pipe = Input,
    _pipe@1 = gsv@internal@token:scan(_pipe),
    gsv@internal@ast:parse(_pipe@1).

-spec le_to_string(line_ending()) -> binary().
le_to_string(Le) ->
    case Le of
        windows ->
            <<"\r\n"/utf8>>;

        unix ->
            <<"\n"/utf8>>
    end.

-spec from_lists(list(list(binary())), binary(), line_ending()) -> binary().
from_lists(Input, Separator, Line_ending) ->
    _pipe = Input,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(Row) ->
            gleam@list:map(
                Row,
                fun(Entry) ->
                    Entry@1 = gleam@string:replace(
                        Entry,
                        <<"\""/utf8>>,
                        <<"\"\""/utf8>>
                    ),
                    case (gleam@string:contains(Entry@1, Separator) orelse gleam@string:contains(
                        Entry@1,
                        <<"\n"/utf8>>
                    ))
                    orelse gleam@string:contains(Entry@1, <<"\""/utf8>>) of
                        true ->
                            <<<<"\""/utf8, Entry@1/binary>>/binary, "\""/utf8>>;

                        false ->
                            Entry@1
                    end
                end
            )
        end
    ),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Row@1) -> gleam@string:join(Row@1, Separator) end
    ),
    gleam@string:join(_pipe@2, le_to_string(Line_ending)).
