-module(gleam@erlang).
-compile([no_auto_import, nowarn_unused_vars]).

-export([format/1, term_to_binary/1, get_line/1, system_time/1, erlang_timestamp/0, rescue/1, binary_to_term/1, unsafe_binary_to_term/1, start_arguments/0, ensure_all_started/1, make_reference/0]).
-export_type([safe/0, get_line_error/0, time_unit/0, crash/0, ensure_all_started_error/0, reference_/0]).

-type safe() :: safe.

-type get_line_error() :: eof | no_data.

-type time_unit() :: second | millisecond | microsecond | nanosecond.

-type crash() :: {exited, gleam@dynamic:dynamic()} |
    {thrown, gleam@dynamic:dynamic()} |
    {errored, gleam@dynamic:dynamic()}.

-type ensure_all_started_error() :: {unknown_application,
        gleam@erlang@atom:atom_()} |
    {application_failed_to_start,
        gleam@erlang@atom:atom_(),
        gleam@dynamic:dynamic()}.

-type reference_() :: any().

-spec format(any()) -> binary().
format(Term) ->
    unicode:characters_to_binary(io_lib:format(<<"~p"/utf8>>, [Term])).

-spec term_to_binary(any()) -> bitstring().
term_to_binary(Field@0) ->
    erlang:term_to_binary(Field@0).

-spec get_line(binary()) -> {ok, binary()} | {error, get_line_error()}.
get_line(Field@0) ->
    gleam_erlang_ffi:get_line(Field@0).

-spec system_time(time_unit()) -> integer().
system_time(Field@0) ->
    os:system_time(Field@0).

-spec erlang_timestamp() -> {integer(), integer(), integer()}.
erlang_timestamp() ->
    os:timestamp().

-spec rescue(fun(() -> EYG)) -> {ok, EYG} | {error, crash()}.
rescue(Field@0) ->
    gleam_erlang_ffi:rescue(Field@0).

-spec binary_to_term(bitstring()) -> {ok, gleam@dynamic:dynamic()} |
    {error, nil}.
binary_to_term(Binary) ->
    case gleam_erlang_ffi:rescue(
        fun() -> erlang:binary_to_term(Binary, [safe]) end
    ) of
        {ok, Term} ->
            {ok, Term};

        {error, _} ->
            {error, nil}
    end.

-spec unsafe_binary_to_term(bitstring()) -> {ok, gleam@dynamic:dynamic()} |
    {error, nil}.
unsafe_binary_to_term(Binary) ->
    case gleam_erlang_ffi:rescue(fun() -> erlang:binary_to_term(Binary, []) end) of
        {ok, Term} ->
            {ok, Term};

        {error, _} ->
            {error, nil}
    end.

-spec start_arguments() -> list(binary()).
start_arguments() ->
    _pipe = init:get_plain_arguments(),
    gleam@list:map(_pipe, fun gleam@erlang@charlist:to_string/1).

-spec ensure_all_started(gleam@erlang@atom:atom_()) -> {ok,
        list(gleam@erlang@atom:atom_())} |
    {error, ensure_all_started_error()}.
ensure_all_started(Field@0) ->
    gleam_erlang_ffi:ensure_all_started(Field@0).

-spec make_reference() -> reference_().
make_reference() ->
    erlang:make_ref().
