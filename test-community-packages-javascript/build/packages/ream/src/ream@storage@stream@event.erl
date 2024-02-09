-module(ream@storage@stream@event).
-compile([no_auto_import, nowarn_unused_vars]).

-export([create/1, open/2, close/1, write/2, read/2]).
-export_type([event/0, event_file/0]).

-type event() :: {event, integer(), bitstring()}.

-type event_file() :: {event_file,
        integer(),
        gleam@erlang@process:pid_(),
        integer(),
        binary()}.

-spec get_file_name(binary(), integer()) -> binary().
get_file_name(Base_path, File_id) ->
    filename:join([Base_path | ream@uuid:parts(<<File_id:128>>)]).

-spec create(binary()) -> {ok, event_file()} |
    {error, gleam@erlang@file:reason()}.
create(Base_path) ->
    <<File_id:128>> = ream@uuid:new(),
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
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"create"/utf8>>,
                        line => 39})
    end,
    gleam@result:'try'(
        ream@storage@file:open(File_name, [read, append]),
        fun(File_pid) -> {ok, {event_file, File_id, File_pid, 0, File_name}} end
    ).

-spec open(binary(), integer()) -> {ok, event_file()} |
    {error, gleam@erlang@file:reason()}.
open(Path, File_id) ->
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
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 54})
    end,
    _assert_subject@1 = ream@storage@file:open(File_name, [read, append]),
    {ok, File_pid} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 55})
    end,
    _assert_subject@2 = gleam_erlang_ffi:file_info(File_name),
    {ok, File_info} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 56})
    end,
    {ok,
        {event_file, File_id, File_pid, erlang:element(2, File_info), File_name}}.

-spec close(event_file()) -> {ok, nil} | {error, gleam@erlang@file:reason()}.
close(Stream_file) ->
    _assert_subject = ream@storage@file:close(erlang:element(3, Stream_file)),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"close"/utf8>>,
                        line => 62})
    end,
    {ok, nil}.

-spec write(event_file(), event()) -> event_file().
write(Stream_file, Event) ->
    Event_size_bits = 24,
    Event_size_bytes = Event_size_bits div 8,
    Data_size = gleam@bit_string:byte_size(erlang:element(3, Event)) + Event_size_bytes,
    Data = <<Data_size:(lists:max([(Event_size_bits), 0])),
        (erlang:element(3, Event))/bitstring>>,
    _assert_subject = file:position(
        erlang:element(3, Stream_file),
        {bof, erlang:element(2, Event)}
    ),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"write"/utf8>>,
                        line => 76})
    end,
    _assert_subject@1 = ream@storage@file:write(
        erlang:element(3, Stream_file),
        Data
    ),
    {ok, _} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"write"/utf8>>,
                        line => 77})
    end,
    Data_size@1 = gleam@bit_string:byte_size(Data),
    erlang:setelement(
        4,
        Stream_file,
        erlang:element(4, Stream_file) + Data_size@1
    ).

-spec read(event_file(), integer()) -> {ok, event()} |
    {error, gleam@erlang@file:reason()}.
read(Stream_file, Offset) ->
    Event_size_bits = 24,
    Event_size_bytes = Event_size_bits div 8,
    _assert_subject = file:position(
        erlang:element(3, Stream_file),
        {bof, Offset}
    ),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"read"/utf8>>,
                        line => 89})
    end,
    _assert_subject@1 = file:read(
        erlang:element(3, Stream_file),
        Event_size_bytes
    ),
    {ok, <<Size:Event_size_bits>>} = case _assert_subject@1 of
        {ok, <<_:Event_size_bits>>} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"read"/utf8>>,
                        line => 90})
    end,
    Content_size = Size - Event_size_bytes,
    _assert_subject@2 = file:read(erlang:element(3, Stream_file), Content_size),
    {ok, Data} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/stream/event"/utf8>>,
                        function => <<"read"/utf8>>,
                        line => 93})
    end,
    {ok, {event, Offset, Data}}.
