-module(ream@storage@kv@value).
-compile([no_auto_import, nowarn_unused_vars]).

-export([create/2, open/3, close/1, write_value/2, write/2, read/2, delete/2, get_file_info/1]).
-export_type([reason/0, value/0, value_file/0, value_file_info/0]).

-type reason() :: capacity_exceeded.

-type value() :: {value,
        integer(),
        boolean(),
        gleam@option:option(bitstring()),
        integer()}.

-type value_file() :: {value_file,
        integer(),
        gleam@erlang@process:pid_(),
        integer(),
        integer(),
        binary()}.

-type value_file_info() :: {value_file_info,
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-spec get_file_name(binary(), integer()) -> binary().
get_file_name(Base_path, File_id) ->
    filename:join([Base_path | ream@uuid:parts(ream@uuid:from_int(File_id))]).

-spec create(binary(), integer()) -> {ok, value_file()} |
    {error, gleam@erlang@file:reason()}.
create(Base_path, Max_size) ->
    File_id = ream@uuid:to_int(ream@uuid:new()),
    File_name = get_file_name(Base_path, File_id),
    _assert_subject = ream@storage@file:recursive_make_directory(
        filename:dirname(File_name)
    ),
    {ok, true} = case _assert_subject of
        {ok, true} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"create"/utf8>>,
                        line => 78})
    end,
    gleam@result:'try'(
        ream@storage@file:open(File_name, [read, write]),
        fun(File_pid) ->
            {ok, {value_file, File_id, File_pid, 0, Max_size, File_name}}
        end
    ).

-spec open(binary(), integer(), integer()) -> {ok, value_file()} |
    {error, gleam@erlang@file:reason()}.
open(Path, File_id, Max_size) ->
    File_name = get_file_name(Path, File_id),
    _assert_subject = ream@storage@file:recursive_make_directory(
        filename:dirname(File_name)
    ),
    {ok, true} = case _assert_subject of
        {ok, true} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 97})
    end,
    _assert_subject@1 = ream@storage@file:open(File_name, [read, write]),
    {ok, File_pid} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 98})
    end,
    _assert_subject@2 = gleam_erlang_ffi:file_info(File_name),
    {ok, File_info} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 99})
    end,
    {ok,
        {value_file,
            File_id,
            File_pid,
            erlang:element(2, File_info),
            Max_size,
            File_name}}.

-spec close(value_file()) -> {ok, nil} | {error, gleam@erlang@file:reason()}.
close(Vfile) ->
    _assert_subject = ream@storage@file:close(erlang:element(3, Vfile)),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"close"/utf8>>,
                        line => 105})
    end,
    {ok, nil}.

-spec write_value(value_file(), value()) -> {ok, value_file()} |
    {error, reason()}.
write_value(Vfile, Value) ->
    Value_size_bits = 32,
    Value_size_bytes = Value_size_bits div 8,
    _assert_subject = erlang:element(4, Value),
    {some, Data} = case _assert_subject of
        {some, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"write_value"/utf8>>,
                        line => 120})
    end,
    Data_size = (gleam@bit_string:byte_size(Data) + Value_size_bytes) + 1,
    Deleted = case erlang:element(3, Value) of
        true ->
            1;

        false ->
            0
    end,
    Packed_data = <<Data_size:(lists:max([(Value_size_bits), 0])),
        Deleted:8,
        Data/bitstring>>,
    _assert_subject@1 = file:position(
        erlang:element(3, Vfile),
        {bof, erlang:element(2, Value)}
    ),
    {ok, Offset} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"write_value"/utf8>>,
                        line => 131})
    end,
    case (Offset + Data_size) > erlang:element(5, Vfile) of
        true ->
            {error, capacity_exceeded};

        false ->
            _assert_subject@2 = ream@storage@file:write(
                erlang:element(3, Vfile),
                Packed_data
            ),
            {ok, _} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"ream/storage/kv/value"/utf8>>,
                                function => <<"write_value"/utf8>>,
                                line => 135})
            end,
            case (Offset + Data_size) > erlang:element(4, Vfile) of
                true ->
                    {ok, erlang:setelement(4, Vfile, Offset + Data_size)};

                false ->
                    {ok, Vfile}
            end
    end.

-spec write(value_file(), bitstring()) -> {ok, {value_file(), value()}} |
    {error, reason()}.
write(Vfile, Value) ->
    Value@1 = {value,
        erlang:element(4, Vfile),
        false,
        {some, Value},
        erlang:element(2, Vfile)},
    gleam@result:'try'(
        write_value(Vfile, Value@1),
        fun(Vfile@1) -> {ok, {Vfile@1, erlang:setelement(4, Value@1, none)}} end
    ).

-spec read(value_file(), integer()) -> {ok, value()} | {error, reason()}.
read(Vfile, Offset) ->
    Value_size_bits = 32,
    Value_size_bytes = Value_size_bits div 8,
    _assert_subject = file:position(erlang:element(3, Vfile), {bof, Offset}),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"read"/utf8>>,
                        line => 170})
    end,
    _assert_subject@1 = file:read(
        erlang:element(3, Vfile),
        Value_size_bytes + 1
    ),
    {ok, <<Size:Value_size_bits, Deleted:8>>} = case _assert_subject@1 of
        {ok, <<_:Value_size_bits, _:8>>} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"read"/utf8>>,
                        line => 171})
    end,
    Content_size = (Size - Value_size_bytes) - 1,
    _assert_subject@2 = file:read(erlang:element(3, Vfile), Content_size),
    {ok, Data} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"read"/utf8>>,
                        line => 174})
    end,
    Deleted@1 = case Deleted of
        0 ->
            false;

        1 ->
            true
    end,
    {ok, {value, Offset, Deleted@1, {some, Data}, erlang:element(2, Vfile)}}.

-spec delete(value_file(), value()) -> {ok, value_file()} | {error, reason()}.
delete(Vfile, Value) ->
    gleam@result:'try'(
        read(Vfile, erlang:element(2, Value)),
        fun(Value@1) ->
            write_value(Vfile, erlang:setelement(3, Value@1, true))
        end
    ).

-spec get_entries_and_deleted(
    gleam@erlang@process:pid_(),
    {integer(), integer()}
) -> {ok, {integer(), integer()}} | {error, gleam@erlang@file:reason()}.
get_entries_and_deleted(Handler, Acc) ->
    Value_size_bits = 32,
    Value_size_bytes = Value_size_bits div 8,
    case file:read(Handler, Value_size_bytes + 1) of
        {ok, <<Size:Value_size_bits, Deleted:8>>} ->
            Payload_size = (Size - Value_size_bytes) - 1,
            _assert_subject = file:position(Handler, {cur, Payload_size}),
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/kv/value"/utf8>>,
                                function => <<"get_entries_and_deleted"/utf8>>,
                                line => 200})
            end,
            get_entries_and_deleted(
                Handler,
                {erlang:element(1, Acc) + 1, erlang:element(2, Acc) + Deleted}
            );

        eof ->
            {ok, Acc};

        {error, Reason} ->
            {error, Reason}
    end.

-spec get_file_info(value_file()) -> value_file_info().
get_file_info(Vfile) ->
    _assert_subject = file:position(erlang:element(3, Vfile), {bof, 0}),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"get_file_info"/utf8>>,
                        line => 183})
    end,
    _assert_subject@1 = get_entries_and_deleted(
        erlang:element(3, Vfile),
        {0, 0}
    ),
    {ok, {Entries, Deleted}} = case _assert_subject@1 of
        {ok, {_, _}} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv/value"/utf8>>,
                        function => <<"get_file_info"/utf8>>,
                        line => 184})
    end,
    {value_file_info,
        erlang:element(2, Vfile),
        erlang:element(4, Vfile),
        erlang:element(5, Vfile),
        Entries,
        Deleted}.
