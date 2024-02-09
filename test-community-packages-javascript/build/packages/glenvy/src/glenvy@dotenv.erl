-module(glenvy@dotenv).
-compile([no_auto_import, nowarn_unused_vars]).

-export([load_from/1, load/0]).

-spec find(binary()) -> {ok, binary()} | {error, glenvy@error:error()}.
find(Filepath) ->
    gleam@result:'try'(
        begin
            _pipe = glenvy@internal@file:read(Filepath),
            gleam@result:map_error(_pipe, fun(Field@0) -> {io, Field@0} end)
        end,
        fun(Contents) -> {ok, Contents} end
    ).

-spec load_from(binary()) -> {ok, nil} | {error, glenvy@error:error()}.
load_from(Filepath) ->
    gleam@result:'try'(
        find(Filepath),
        fun(Env_file) ->
            Env_vars = begin
                _pipe = Env_file,
                glenvy@internal@parser:parse_env_file(_pipe)
            end,
            _pipe@1 = Env_vars,
            _pipe@2 = gleam@map:to_list(_pipe@1),
            gleam@list:each(
                _pipe@2,
                fun(Env_var) ->
                    {Key, Value} = Env_var,
                    glenvy@internal@os:set_env(Key, Value)
                end
            ),
            {ok, nil}
        end
    ).

-spec load() -> {ok, nil} | {error, glenvy@error:error()}.
load() ->
    load_from(<<".env"/utf8>>).
