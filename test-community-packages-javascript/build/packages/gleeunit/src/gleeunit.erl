-module(gleeunit).
-compile(no_auto_import).

-export([main/0]).
-export_type([atom_/0, encoding/0, report_module_name/0, gleeunit_progress_option/0, eunit_option/0]).

-type atom_() :: any().

-type encoding() :: utf8.

-type report_module_name() :: gleeunit_progress.

-type gleeunit_progress_option() :: {colored, boolean()}.

-type eunit_option() :: verbose |
    no_tty |
    {report, {report_module_name(), list(gleeunit_progress_option())}}.

-spec main() -> nil.
main() ->
    do_main().

-spec do_main() -> nil.
do_main() ->
    Options = [verbose,
        no_tty,
        {report, {gleeunit_progress, [{colored, true}]}}],
    Result = begin
        _pipe = gleeunit_ffi:find_files(
            <<"**/*.{erl,gleam}"/utf8>>,
            <<"test"/utf8>>
        ),
        _pipe@1 = gleam@list:map(_pipe, fun gleam_to_erlang_module_name/1),
        _pipe@2 = gleam@list:map(
            _pipe@1,
            fun(_capture) -> erlang:binary_to_atom(_capture, utf8) end
        ),
        _pipe@3 = eunit:test(_pipe@2, Options),
        _pipe@4 = (gleam@dynamic:result(
            fun gleam@dynamic:dynamic/1,
            fun gleam@dynamic:dynamic/1
        ))(_pipe@3),
        gleam@result:unwrap(_pipe@4, {error, gleam@dynamic:from(nil)})
    end,
    Code = case Result of
        {ok, _} ->
            0;

        {error, _} ->
            1
    end,
    erlang:halt(Code).

-spec gleam_to_erlang_module_name(binary()) -> binary().
gleam_to_erlang_module_name(Path) ->
    _pipe = Path,
    _pipe@1 = gleam@string:replace(_pipe, <<".gleam"/utf8>>, <<""/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<".erl"/utf8>>, <<""/utf8>>),
    gleam@string:replace(_pipe@2, <<"/"/utf8>>, <<"@"/utf8>>).
