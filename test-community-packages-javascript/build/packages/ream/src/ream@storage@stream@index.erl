-module(ream@storage@stream@index).
-compile([no_auto_import, nowarn_unused_vars]).

-export([open/1, close/1, count/1, set_pos/2, get_next/1, get/2, add/3]).
-export_type([index/0, index_file/0]).

-type index() :: {index, integer(), integer(), integer()}.

-type index_file() :: {index_file,
        gleam@erlang@process:pid_(),
        integer(),
        binary()}.

-spec open(binary()) -> {ok, index_file()} | {error, gleam@erlang@file:reason()}.
open(Path) ->
    _assert_subject = ream@storage@file:recursive_make_directory(Path),
    {ok, true} = case _assert_subject of
        {ok, true} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/stream/index"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 34})
    end,
    Index = filename:join([Path, <<"index"/utf8>>]),
    gleam@result:'try'(
        ream@storage@file:open(Index, [read, append]),
        fun(Index_pid) ->
            gleam@result:'try'(
                gleam_erlang_ffi:file_info(Index),
                fun(Index_info) ->
                    {ok,
                        {index_file,
                            Index_pid,
                            erlang:element(2, Index_info),
                            Index}}
                end
            )
        end
    ).

-spec close(index_file()) -> {ok, nil} | {error, gleam@erlang@file:reason()}.
close(Index_file) ->
    _assert_subject = ream@storage@file:close(erlang:element(2, Index_file)),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/stream/index"/utf8>>,
                        function => <<"close"/utf8>>,
                        line => 42})
    end,
    {ok, nil}.

-spec index_binary(index()) -> bitstring().
index_binary(Index) ->
    Offset_size_bits = 48,
    Event_size_bits = 24,
    File_id_size_bits = 128,
    <<(erlang:element(2, Index)):(lists:max([(Offset_size_bits), 0])),
        (erlang:element(3, Index)):(lists:max([(Event_size_bits), 0])),
        (erlang:element(4, Index)):(lists:max([(File_id_size_bits), 0]))>>.

-spec count(index_file()) -> integer().
count(Index_file) ->
    _assert_subject = gleam@int:divide(erlang:element(3, Index_file), 25),
    {ok, Result} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/stream/index"/utf8>>,
                        function => <<"count"/utf8>>,
                        line => 120})
    end,
    Result.

-spec set_pos(index_file(), integer()) -> {ok, integer()} |
    {error, gleam@erlang@file:reason()}.
set_pos(Index_file, Index) ->
    file:position(erlang:element(2, Index_file), {bof, Index * 25}).

-spec get_next(index_file()) -> {ok, index()} |
    {error, gleam@erlang@file:reason()}.
get_next(Index_file) ->
    Offset_size_bits = 48,
    Event_size_bits = 24,
    File_id_size_bits = 128,
    case file:read(erlang:element(2, Index_file), 25) of
        {ok,
            <<Offset:Offset_size_bits,
                Size:Event_size_bits,
                File_id:File_id_size_bits>>} ->
            {ok, {index, Offset, Size, File_id}};

        eof ->
            {error, espipe};

        _ ->
            {error, einval}
    end.

-spec get(index_file(), integer()) -> {ok, index()} |
    {error, gleam@erlang@file:reason()}.
get(Index_file, Index) ->
    case count(Index_file) > Index of
        true ->
            _assert_subject = set_pos(Index_file, Index),
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/stream/index"/utf8>>,
                                function => <<"get"/utf8>>,
                                line => 148})
            end,
            get_next(Index_file);

        false ->
            {error, einval}
    end.

-spec last_entry_for_file(gleam@erlang@process:pid_(), integer()) -> {ok,
        bitstring()} |
    {error, gleam@erlang@file:reason()}.
last_entry_for_file(Index_file, File_id) ->
    Offset_size_bits = 48,
    Event_size_bits = 24,
    File_id_size_bits = 128,
    _assert_subject = file:read(Index_file, 25),
    {ok,
        <<Offset:Offset_size_bits,
            Size:Event_size_bits,
            New_file_id:File_id_size_bits>>} = case _assert_subject of
        {ok, <<_:Offset_size_bits, _:Event_size_bits, _:File_id_size_bits>>} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/stream/index"/utf8>>,
                        function => <<"last_entry_for_file"/utf8>>,
                        line => 103})
    end,
    case New_file_id =:= File_id of
        true ->
            {ok, index_binary({index, Offset, Size, File_id})};

        false ->
            case file:position(Index_file, {cur, -2 * 25}) of
                {ok, _} ->
                    last_entry_for_file(Index_file, File_id);

                {error, einval} ->
                    {ok, index_binary({index, 0, 0, File_id})}
            end
    end.

-spec add(index_file(), integer(), integer()) -> {index(), index_file()}.
add(Index_file, Event_size, File_id) ->
    Event_size_bytes = 24 div 8,
    {Index_content, Index@2} = case erlang:element(3, Index_file) of
        0 ->
            Index = {index, 0, Event_size + Event_size_bytes, File_id},
            {index_binary(Index), Index};

        _ ->
            _assert_subject = file:position(
                erlang:element(2, Index_file),
                {eof, - 25}
            ),
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/stream/index"/utf8>>,
                                function => <<"add"/utf8>>,
                                line => 58})
            end,
            Offset_size_bits = 48,
            Event_size_bits = 24,
            File_id_size_bits = 128,
            _assert_subject@1 = last_entry_for_file(
                erlang:element(2, Index_file),
                File_id
            ),
            {ok,
                <<Offset:Offset_size_bits,
                    Prev_size:Event_size_bits,
                    _:File_id_size_bits>>} = case _assert_subject@1 of
                {ok,
                    <<_:Offset_size_bits,
                        _:Event_size_bits,
                        _:File_id_size_bits>>} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"ream/storage/stream/index"/utf8>>,
                                function => <<"add"/utf8>>,
                                line => 65})
            end,
            Offset@1 = Offset + Prev_size,
            Index@1 = {index, Offset@1, Event_size + Event_size_bytes, File_id},
            {index_binary(Index@1), Index@1}
    end,
    _assert_subject@2 = ream@storage@file:write(
        erlang:element(2, Index_file),
        Index_content
    ),
    {ok, _} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/stream/index"/utf8>>,
                        function => <<"add"/utf8>>,
                        line => 75})
    end,
    Index_file@1 = erlang:setelement(
        3,
        Index_file,
        erlang:element(3, Index_file) + 25
    ),
    {Index@2, Index_file@1}.
