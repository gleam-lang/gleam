-module(glenvy@env).
-compile([no_auto_import, nowarn_unused_vars]).

-export([get_string/1, get/2, get_int/1, get_float/1, get_bool/1]).

-spec get_string(binary()) -> {ok, binary()} | {error, nil}.
get_string(Name) ->
    glenvy@internal@os:get_env(Name).

-spec get(binary(), fun((binary()) -> {ok, FNV} | {error, nil})) -> {ok, FNV} |
    {error, nil}.
get(Name, Parse) ->
    gleam@result:'try'(get_string(Name), fun(Value) -> _pipe = Value,
            Parse(_pipe) end).

-spec get_int(binary()) -> {ok, integer()} | {error, nil}.
get_int(Name) ->
    _pipe = Name,
    get(_pipe, fun gleam@int:parse/1).

-spec get_float(binary()) -> {ok, float()} | {error, nil}.
get_float(Name) ->
    _pipe = Name,
    get(_pipe, fun gleam@float:parse/1).

-spec get_bool(binary()) -> {ok, boolean()} | {error, nil}.
get_bool(Name) ->
    Parse_bool = fun(Value) ->
        Value@1 = begin
            _pipe = Value,
            gleam@string:lowercase(_pipe)
        end,
        case Value@1 of
            <<"true"/utf8>> ->
                {ok, true};

            <<"t"/utf8>> ->
                {ok, true};

            <<"yes"/utf8>> ->
                {ok, true};

            <<"y"/utf8>> ->
                {ok, true};

            <<"1"/utf8>> ->
                {ok, true};

            <<"false"/utf8>> ->
                {ok, false};

            <<"f"/utf8>> ->
                {ok, false};

            <<"no"/utf8>> ->
                {ok, false};

            <<"n"/utf8>> ->
                {ok, false};

            <<"0"/utf8>> ->
                {ok, false};

            _ ->
                {error, nil}
        end
    end,
    _pipe@1 = Name,
    get(_pipe@1, Parse_bool).
