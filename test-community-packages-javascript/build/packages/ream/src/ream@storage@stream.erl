-module(ream@storage@stream).
-compile([no_auto_import, nowarn_unused_vars]).

-export([get_base_path/2, close/1, open/2, add_event/2, get_event/2]).
-export_type([stream/0]).

-type stream() :: {stream,
        binary(),
        ream@storage@stream@index:index_file(),
        gleam@option:option(ream@storage@stream@event:event_file()),
        gleam@map:map_(integer(), ream@storage@stream@event:event_file()),
        binary()}.

-spec get_base_path(binary(), binary()) -> binary().
get_base_path(Path, Name) ->
    filename:join([Path, <<"stream"/utf8>>, Name]).

-spec less_populated_file(list(ream@storage@stream@event:event_file())) -> gleam@option:option(ream@storage@stream@event:event_file()).
less_populated_file(Files) ->
    _pipe = Files,
    gleam@list:fold(
        _pipe,
        none,
        fun(Acc, File) ->
            File_size = erlang:element(4, File),
            case Acc of
                {some, {event_file, _, _, Size, _}} when File_size >= Size ->
                    Acc;

                _ ->
                    {some, File}
            end
        end
    ).

-spec close(stream()) -> {ok, nil} | {error, gleam@erlang@file:reason()}.
close(Stream) ->
    _assert_subject = ream@storage@stream@index:close(erlang:element(3, Stream)),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/stream"/utf8>>,
                        function => <<"close"/utf8>>,
                        line => 50})
    end,
    _pipe = erlang:element(5, Stream),
    _pipe@1 = gleam@map:values(_pipe),
    gleam@list:each(
        _pipe@1,
        fun(File) -> ream@storage@file:close(erlang:element(3, File)) end
    ),
    {ok, nil}.

-spec do_open_files(
    ream@storage@stream@index:index_file(),
    binary(),
    gleam@map:map_(integer(), ream@storage@stream@event:event_file())
) -> {ok, gleam@map:map_(integer(), ream@storage@stream@event:event_file())} |
    {error, gleam@erlang@file:reason()}.
do_open_files(Index_file, Path, Acc) ->
    case ream@storage@stream@index:count(Index_file) of
        0 ->
            {ok, Acc};

        Num_of_events ->
            _ = ream@storage@stream@index:set_pos(Index_file, 0),
            Files = begin
                _pipe = Num_of_events - 1,
                _pipe@1 = gleam@iterator:range(0, _pipe),
                gleam@iterator:fold(
                    _pipe@1,
                    Acc,
                    fun(Acc@1, _) ->
                        _assert_subject = ream@storage@stream@index:get_next(
                            Index_file
                        ),
                        {ok, {index, _, _, File_id}} = case _assert_subject of
                            {ok, {index, _, _, _}} -> _assert_subject;
                            _assert_fail ->
                                erlang:error(#{gleam_error => assert,
                                            message => <<"Assertion pattern match failed"/utf8>>,
                                            value => _assert_fail,
                                            module => <<"ream/storage/stream"/utf8>>,
                                            function => <<"do_open_files"/utf8>>,
                                            line => 75})
                        end,
                        case gleam@map:has_key(Acc@1, File_id) of
                            true ->
                                Acc@1;

                            false ->
                                _assert_subject@1 = ream@storage@stream@event:open(
                                    Path,
                                    File_id
                                ),
                                {ok, File} = case _assert_subject@1 of
                                    {ok, _} -> _assert_subject@1;
                                    _assert_fail@1 ->
                                        erlang:error(#{gleam_error => assert,
                                                    message => <<"Assertion pattern match failed"/utf8>>,
                                                    value => _assert_fail@1,
                                                    module => <<"ream/storage/stream"/utf8>>,
                                                    function => <<"do_open_files"/utf8>>,
                                                    line => 80})
                                end,
                                gleam@map:insert(Acc@1, File_id, File)
                        end
                    end
                )
            end,
            {ok, Files}
    end.

-spec open(binary(), binary()) -> {ok, stream()} |
    {error, gleam@erlang@file:reason()}.
open(Name, Path) ->
    Base_path = filename:join([Path, <<"stream"/utf8>>, Name]),
    _assert_subject = ream@storage@stream@index:open(Base_path),
    {ok, Index_file} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/stream"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 28})
    end,
    gleam@result:'try'(
        do_open_files(Index_file, Base_path, gleam@map:new()),
        fun(Files) ->
            Active_file = less_populated_file(gleam@map:values(Files)),
            {ok, {stream, Name, Index_file, Active_file, Files, Base_path}}
        end
    ).

-spec add_event(stream(), bitstring()) -> {ok, stream()} |
    {error, gleam@erlang@file:reason()}.
add_event(Stream, Event_content) ->
    Event_size = gleam@bit_string:byte_size(Event_content),
    {Stream@3, Index@2} = case ream@storage@stream@index:count(
        erlang:element(3, Stream)
    ) of
        0 ->
            _assert_subject = ream@storage@stream@event:create(
                erlang:element(6, Stream)
            ),
            {ok, Stream_file} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/stream"/utf8>>,
                                function => <<"add_event"/utf8>>,
                                line => 99})
            end,
            {Index, Index_file} = ream@storage@stream@index:add(
                erlang:element(3, Stream),
                Event_size,
                erlang:element(2, Stream_file)
            ),
            Stream@1 = erlang:setelement(
                5,
                erlang:setelement(
                    4,
                    erlang:setelement(3, Stream, Index_file),
                    {some, Stream_file}
                ),
                gleam@map:insert(
                    erlang:element(5, Stream),
                    erlang:element(2, Stream_file),
                    Stream_file
                )
            ),
            {Stream@1, Index};

        _ ->
            _assert_subject@1 = erlang:element(4, Stream),
            {some, Active_file} = case _assert_subject@1 of
                {some, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"ream/storage/stream"/utf8>>,
                                function => <<"add_event"/utf8>>,
                                line => 112})
            end,
            {Index@1, Index_file@1} = ream@storage@stream@index:add(
                erlang:element(3, Stream),
                Event_size,
                erlang:element(2, Active_file)
            ),
            Stream@2 = erlang:setelement(3, Stream, Index_file@1),
            {Stream@2, Index@1}
    end,
    _assert_subject@2 = gleam@map:get(
        erlang:element(5, Stream@3),
        erlang:element(4, Index@2)
    ),
    {ok, File} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/stream"/utf8>>,
                        function => <<"add_event"/utf8>>,
                        line => 120})
    end,
    Event = {event, erlang:element(2, Index@2), Event_content},
    Stream_file@1 = ream@storage@stream@event:write(File, Event),
    Files = gleam@map:insert(
        erlang:element(5, Stream@3),
        erlang:element(2, Stream_file@1),
        Stream_file@1
    ),
    {ok, erlang:setelement(5, Stream@3, Files)}.

-spec get_event(stream(), integer()) -> {ok, bitstring()} |
    {error, gleam@erlang@file:reason()}.
get_event(Stream, Index) ->
    case ream@storage@stream@index:count(erlang:element(3, Stream)) > Index of
        true ->
            _assert_subject = ream@storage@stream@index:get(
                erlang:element(3, Stream),
                Index
            ),
            {ok, {index, Offset, _, File_id}} = case _assert_subject of
                {ok, {index, _, _, _}} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/stream"/utf8>>,
                                function => <<"get_event"/utf8>>,
                                line => 131})
            end,
            _assert_subject@1 = gleam@map:get(
                erlang:element(5, Stream),
                File_id
            ),
            {ok, File} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"ream/storage/stream"/utf8>>,
                                function => <<"get_event"/utf8>>,
                                line => 133})
            end,
            _assert_subject@2 = ream@storage@stream@event:read(File, Offset),
            {ok, {event, _, Data}} = case _assert_subject@2 of
                {ok, {event, _, _}} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"ream/storage/stream"/utf8>>,
                                function => <<"get_event"/utf8>>,
                                line => 134})
            end,
            {ok, Data};

        false ->
            {error, einval}
    end.
