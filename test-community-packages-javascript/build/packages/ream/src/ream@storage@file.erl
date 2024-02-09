-module(ream@storage@file).
-compile([no_auto_import, nowarn_unused_vars]).

-export([do_open/2, open/2, read/2, close/1, write/2, dirname/1, basename/1, join/1, position/2, recursive_make_directory/1]).
-export_type([endian/0, encoding_type/0, mode/0, location/0]).

-type endian() :: big | little.

-type encoding_type() :: unicode |
    utf8 |
    {utf16, endian()} |
    {utf32, endian()} |
    latin1.

-type mode() :: read |
    write |
    append |
    exclusive |
    raw |
    binary |
    {delayed_write, integer(), integer()} |
    {read_ahead, integer()} |
    compressed |
    compressed_one |
    {encoding, encoding_type()} |
    ram |
    sync |
    directory.

-type location() :: {bof, integer()} | {cur, integer()} | {eof, integer()}.

-spec do_open(binary(), list(mode())) -> {ok, gleam@erlang@process:pid_()} |
    {error, gleam@erlang@file:reason()}.
do_open(Field@0, Field@1) ->
    file:open(Field@0, Field@1).

-spec open(binary(), list(mode())) -> {ok, gleam@erlang@process:pid_()} |
    {error, gleam@erlang@file:reason()}.
open(Filename, Mode) ->
    file:open(Filename, [binary | Mode]).

-spec read(gleam@erlang@process:pid_(), integer()) -> ream@storage@file@read:result().
read(Field@0, Field@1) ->
    file:read(Field@0, Field@1).

-spec close(gleam@erlang@process:pid_()) -> {ok, boolean()} |
    {error, gleam@erlang@file:reason()}.
close(Io_device) ->
    case file:close(Io_device) of
        ok ->
            {ok, true};

        {error, Reason} ->
            {error, Reason}
    end.

-spec write(gleam@erlang@process:pid_(), bitstring()) -> {ok, boolean()} |
    {error, gleam@erlang@file:reason()}.
write(Io_device, Data) ->
    case file:write(Io_device, Data) of
        ok ->
            {ok, true};

        {error, Reason} ->
            {error, Reason}
    end.

-spec dirname(binary()) -> binary().
dirname(Field@0) ->
    filename:dirname(Field@0).

-spec basename(binary()) -> binary().
basename(Field@0) ->
    filename:basename(Field@0).

-spec join(list(binary())) -> binary().
join(Field@0) ->
    filename:join(Field@0).

-spec position(gleam@erlang@process:pid_(), location()) -> {ok, integer()} |
    {error, gleam@erlang@file:reason()}.
position(Field@0, Field@1) ->
    file:position(Field@0, Field@1).

-spec recursive_make_directory(binary()) -> {ok, boolean()} |
    {error, gleam@erlang@file:reason()}.
recursive_make_directory(Path) ->
    case gleam@erlang@file:is_directory(Path) of
        {error, enoent} ->
            Prev_dir = filename:dirname(Path),
            _assert_subject = recursive_make_directory(Prev_dir),
            {ok, true} = case _assert_subject of
                {ok, true} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/file"/utf8>>,
                                function => <<"recursive_make_directory"/utf8>>,
                                line => 95})
            end,
            _assert_subject@1 = gleam_erlang_ffi:make_directory(Path),
            {ok, _} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"ream/storage/file"/utf8>>,
                                function => <<"recursive_make_directory"/utf8>>,
                                line => 96})
            end,
            {ok, true};

        {error, eexist} ->
            {ok, true};

        {ok, true} ->
            {ok, true};

        _ ->
            {error, einval}
    end.
