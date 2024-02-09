-module(gsv@internal@token).
-compile([no_auto_import, nowarn_unused_vars]).

-export([to_lexeme/1, scan/1]).
-export_type([csv_token/0]).

-type csv_token() :: comma | lf | cr | doublequote | {textdata, binary()}.

-spec to_lexeme(csv_token()) -> binary().
to_lexeme(Token) ->
    case Token of
        comma ->
            <<","/utf8>>;

        lf ->
            <<"\n"/utf8>>;

        cr ->
            <<"\r"/utf8>>;

        doublequote ->
            <<"\""/utf8>>;

        {textdata, Str} ->
            Str
    end.

-spec scan(binary()) -> list(csv_token()).
scan(Input) ->
    _pipe = Input,
    _pipe@1 = gleam@string:to_utf_codepoints(_pipe),
    _pipe@2 = gleam@list:fold(
        _pipe@1,
        [],
        fun(Acc, X) -> case gleam@string:utf_codepoint_to_int(X) of
                16#2c ->
                    [comma | Acc];

                16#22 ->
                    [doublequote | Acc];

                16#0a ->
                    [lf | Acc];

                16#0D ->
                    [cr | Acc];

                _ ->
                    Cp = gleam@string:from_utf_codepoints([X]),
                    case Acc of
                        [{textdata, Str} | Rest] ->
                            [{textdata, <<Str/binary, Cp/binary>>} | Rest];

                        _ ->
                            [{textdata, Cp} | Acc]
                    end
            end end
    ),
    gleam@list:reverse(_pipe@2).
