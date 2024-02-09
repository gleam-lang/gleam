-module(gloml).
-compile([no_auto_import, nowarn_unused_vars]).

-export([decode/2, decode_dynamic/1]).
-export_type([decode_error/0, elx_invalid_toml_error/0]).

-type decode_error() :: {invalid_toml_error, binary()} |
    {unexpected_format, list(gleam@dynamic:decode_error())}.

-type elx_invalid_toml_error() :: any().

-spec decode_inner(binary()) -> {ok, gleam@dynamic:dynamic()} |
    {error, decode_error()}.
decode_inner(Toml_string) ->
    case 'Elixir.Toml':decode(Toml_string) of
        {ok, Value} ->
            {ok, Value};

        {error, Err} ->
            {error, {invalid_toml_error, 'Elixir.TomlFFI':get_reason(Err)}}
    end.

-spec decode(
    binary(),
    fun((gleam@dynamic:dynamic()) -> {ok, EUG} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, EUG} | {error, decode_error()}.
decode(Toml_string, Decoder) ->
    gleam@result:then(
        decode_inner(Toml_string),
        fun(Dyn) ->
            _pipe = Dyn,
            _pipe@1 = Decoder(_pipe),
            gleam@result:map_error(
                _pipe@1,
                fun(Field@0) -> {unexpected_format, Field@0} end
            )
        end
    ).

-spec decode_dynamic(binary()) -> {ok, gleam@dynamic:dynamic()} |
    {error, decode_error()}.
decode_dynamic(Toml_string) ->
    decode_inner(Toml_string).
