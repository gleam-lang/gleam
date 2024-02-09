-module(ream@storage@kv).
-compile([no_auto_import, nowarn_unused_vars]).

-export([get/2, info/1, open/5, flush/1, close/1, set/3]).
-export_type([mem_table_range/0, kv/0, kv_info/0]).

-type mem_table_range() :: {mem_table_range,
        integer(),
        integer(),
        gleam@option:option(ream@storage@kv@memtable:mem_table())}.

-type kv() :: {kv,
        binary(),
        binary(),
        gleam@map:map_(integer(), mem_table_range()),
        gleam@option:option(integer()),
        gleam@map:map_(integer(), ream@storage@kv@value:value_file()),
        integer(),
        integer(),
        integer(),
        integer()}.

-type kv_info() :: {kv_info,
        binary(),
        binary(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-spec new_id() -> integer().
new_id() ->
    ream@uuid:to_int(ream@uuid:new()).

-spec sstable_path(binary(), integer()) -> binary().
sstable_path(Path, Id) ->
    Parts = ream@uuid:parts(ream@uuid:from_int(Id)),
    filename:join([Path, <<"key"/utf8>> | Parts]).

-spec find_range(kv(), integer(), integer()) -> {integer(), kv()}.
find_range(Kv, Key_hash, Max_size) ->
    Max_memtables_loaded = erlang:element(8, Kv),
    _assert_subject@2 = begin
        _pipe = erlang:element(4, Kv),
        _pipe@1 = gleam@map:to_list(_pipe),
        gleam@list:map_fold(
            _pipe@1,
            {erlang:element(7, Kv), none},
            fun(Acc, Entry) ->
                {Id, Range} = Entry,
                {Loaded, Range_id} = Acc,
                case {(Key_hash >= erlang:element(2, Range)) andalso (Key_hash
                    =< erlang:element(3, Range)),
                    erlang:element(4, Range)} of
                    {true, {some, _}} ->
                        {{Loaded, {some, Id}}, {Id, Range}};

                    {true, none} ->
                        _assert_subject = ream@storage@kv@sstable:load(
                            sstable_path(erlang:element(2, Kv), Id),
                            Max_size
                        ),
                        {ok, Memtable} = case _assert_subject of
                            {ok, _} -> _assert_subject;
                            _assert_fail ->
                                erlang:error(#{gleam_error => assert,
                                            message => <<"Assertion pattern match failed"/utf8>>,
                                            value => _assert_fail,
                                            module => <<"ream/storage/kv"/utf8>>,
                                            function => <<"find_range"/utf8>>,
                                            line => 174})
                        end,
                        {{Loaded + 1, {some, Id}},
                            {Id, erlang:setelement(4, Range, {some, Memtable})}};

                    {false, {some, Memtable@1}} when Loaded >= Max_memtables_loaded ->
                        _assert_subject@1 = ream@storage@kv@sstable:flush(
                            Memtable@1,
                            sstable_path(erlang:element(2, Kv), Id)
                        ),
                        {ok, _} = case _assert_subject@1 of
                            {ok, _} -> _assert_subject@1;
                            _assert_fail@1 ->
                                erlang:error(#{gleam_error => assert,
                                            message => <<"Assertion pattern match failed"/utf8>>,
                                            value => _assert_fail@1,
                                            module => <<"ream/storage/kv"/utf8>>,
                                            function => <<"find_range"/utf8>>,
                                            line => 182})
                        end,
                        {{Loaded - 1, Range_id},
                            {Id, erlang:setelement(4, Range, none)}};

                    {false, _} ->
                        {Acc, {Id, Range}}
                end
            end
        )
    end,
    {{Loaded@1, {some, Range_id@1}}, Range_list} = case _assert_subject@2 of
        {{_, {some, _}}, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"find_range"/utf8>>,
                        line => 160})
    end,
    {Range_id@1,
        erlang:setelement(
            7,
            erlang:setelement(4, Kv, gleam@map:from_list(Range_list)),
            Loaded@1
        )}.

-spec get(kv(), binary()) -> {{ok, bitstring()} | {error, nil}, kv()}.
get(Kv, Key) ->
    Key_hash = erlang:phash2(Key),
    {Range_id, Kv@1} = find_range(Kv, Key_hash, erlang:element(9, Kv)),
    _assert_subject = gleam@map:get(erlang:element(4, Kv@1), Range_id),
    {ok, Range} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"get"/utf8>>,
                        line => 264})
    end,
    _assert_subject@1 = erlang:element(4, Range),
    {some, Memtable} = case _assert_subject@1 of
        {some, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"get"/utf8>>,
                        line => 265})
    end,
    case ream@storage@kv@memtable:get(Memtable, Key) of
        {ok, Value} ->
            _assert_subject@2 = gleam@map:get(
                erlang:element(6, Kv@1),
                erlang:element(5, Value)
            ),
            {ok, Vfile} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"get"/utf8>>,
                                line => 268})
            end,
            case ream@storage@kv@value:read(Vfile, erlang:element(2, Value)) of
                {ok, {value, _, false, {some, Data}, _}} ->
                    {{ok, Data}, Kv@1};

                _ ->
                    {{error, nil}, Kv@1}
            end;

        {error, _} ->
            {{error, nil}, Kv@1}
    end.

-spec split(
    kv(),
    integer(),
    integer(),
    mem_table_range(),
    ream@storage@kv@memtable:mem_table()
) -> kv().
split(Kv, Key_hash, Range_id, Range, Memtable) ->
    {Memtable_low, Memtable_high, Pivot} = ream@storage@kv@memtable:split(
        Memtable,
        Key_hash
    ),
    {mem_table_range, Lower, Upper, _} = case Range of
        {mem_table_range, _, _, _} -> Range;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"split"/utf8>>,
                        line => 332})
    end,
    {Memtable_range_low, Memtable_range_high} = case erlang:element(
        3,
        Memtable_low
    )
    >= erlang:element(3, Memtable_high) of
        false ->
            {{mem_table_range, Lower, Pivot - 1, {some, Memtable_low}},
                {mem_table_range, Pivot, Upper, none}};

        true ->
            {{mem_table_range, Lower, Pivot - 1, none},
                {mem_table_range, Pivot, Upper, {some, Memtable_high}}}
    end,
    Memtable_high_id = new_id(),
    Memtable_ranges = begin
        _pipe = erlang:element(4, Kv),
        _pipe@1 = gleam@map:insert(_pipe, Range_id, Memtable_range_low),
        gleam@map:insert(_pipe@1, Memtable_high_id, Memtable_range_high)
    end,
    _assert_subject = ream@storage@kv@sstable:flush(
        Memtable_high,
        sstable_path(erlang:element(2, Kv), Memtable_high_id)
    ),
    {ok, true} = case _assert_subject of
        {ok, true} -> _assert_subject;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"split"/utf8>>,
                        line => 351})
    end,
    _assert_subject@1 = ream@storage@kv@sstable:flush(
        Memtable_low,
        sstable_path(erlang:element(2, Kv), Range_id)
    ),
    {ok, true} = case _assert_subject@1 of
        {ok, true} -> _assert_subject@1;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"split"/utf8>>,
                        line => 354})
    end,
    erlang:setelement(4, Kv, Memtable_ranges).

-spec delete_value(kv(), ream@storage@kv@value:value()) -> {ok, kv()} |
    {error, nil}.
delete_value(Kv, Value) ->
    File_id = erlang:element(5, Value),
    _assert_subject = gleam@map:get(erlang:element(6, Kv), File_id),
    {ok, Vfile} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"delete_value"/utf8>>,
                        line => 398})
    end,
    _assert_subject@1 = ream@storage@kv@value:delete(Vfile, Value),
    {ok, Vfile@1} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"delete_value"/utf8>>,
                        line => 399})
    end,
    Values = gleam@map:insert(erlang:element(6, Kv), File_id, Vfile@1),
    {ok, erlang:setelement(6, Kv, Values)}.

-spec info(kv()) -> kv_info().
info(Kv) ->
    {kv_info,
        erlang:element(2, Kv),
        erlang:element(3, Kv),
        gleam@map:size(erlang:element(6, Kv)),
        gleam@map:fold(
            erlang:element(6, Kv),
            0,
            fun(Acc, _, Value_file) -> Acc + erlang:element(4, Value_file) end
        ),
        gleam@map:size(erlang:element(4, Kv)),
        erlang:element(7, Kv),
        gleam@map:fold(
            erlang:element(4, Kv),
            0,
            fun(Acc@1, _, Memtable_range) ->
                case erlang:element(4, Memtable_range) of
                    {some, Memtable} ->
                        Acc@1 + erlang:element(3, Memtable);

                    none ->
                        Acc@1
                end
            end
        ),
        erlang:element(8, Kv),
        erlang:element(9, Kv),
        erlang:element(10, Kv)}.

-spec read_memtable_ranges(
    gleam@erlang@process:pid_(),
    binary(),
    integer(),
    gleam@map:map_(integer(), mem_table_range())
) -> gleam@map:map_(integer(), mem_table_range()).
read_memtable_ranges(Kv, Path, Max_size, Acc) ->
    case file:read(Kv, 48) of
        {ok, <<Lower:128, Upper:128, Id:128>>} ->
            Range = {mem_table_range, Lower, Upper, none},
            read_memtable_ranges(
                Kv,
                Path,
                Max_size,
                gleam@map:insert(Acc, Id, Range)
            );

        eof ->
            Acc;

        {error, _} ->
            _assert_subject = ream@storage@file:close(Kv),
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"read_memtable_ranges"/utf8>>,
                                line => 119})
            end,
            erlang:error(#{gleam_error => panic,
                    message => <<"panic expression evaluated"/utf8>>,
                    module => <<"ream/storage/kv"/utf8>>,
                    function => <<"read_memtable_ranges"/utf8>>,
                    line => 121})
    end.

-spec read_values(
    gleam@erlang@process:pid_(),
    binary(),
    gleam@option:option(integer()),
    integer(),
    gleam@map:map_(integer(), ream@storage@kv@value:value_file())
) -> {gleam@option:option(integer()),
    gleam@map:map_(integer(), ream@storage@kv@value:value_file())}.
read_values(Kv, Path, Last_file_id, Max_value_size, Acc) ->
    case file:read(Kv, 16) of
        {ok, <<File_id:128>>} ->
            _assert_subject = ream@storage@kv@value:open(
                Path,
                File_id,
                Max_value_size
            ),
            {ok, Value_file} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"read_values"/utf8>>,
                                line => 135})
            end,
            read_values(
                Kv,
                Path,
                {some, File_id},
                Max_value_size,
                gleam@map:insert(Acc, File_id, Value_file)
            );

        eof ->
            {Last_file_id, Acc};

        {error, _} ->
            _assert_subject@1 = ream@storage@file:close(Kv),
            {ok, _} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"read_values"/utf8>>,
                                line => 146})
            end,
            erlang:error(#{gleam_error => panic,
                    message => <<"panic expression evaluated"/utf8>>,
                    module => <<"ream/storage/kv"/utf8>>,
                    function => <<"read_values"/utf8>>,
                    line => 148})
    end.

-spec open(binary(), binary(), integer(), integer(), integer()) -> kv().
open(Path, Name, Max_memtables_loaded, Max_memtable_size, Max_value_size) ->
    Path@1 = filename:join([Path, <<"kv"/utf8>>, Name]),
    Key_index_file = filename:join([Path@1, <<"key"/utf8>>, <<"index"/utf8>>]),
    _assert_subject = ream@storage@file:recursive_make_directory(
        filename:dirname(Key_index_file)
    ),
    {ok, true} = case _assert_subject of
        {ok, true} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 60})
    end,
    _assert_subject@1 = ream@storage@file:open(Key_index_file, [read, write]),
    {ok, Kv} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 61})
    end,
    Ranges = read_memtable_ranges(
        Kv,
        filename:join([Path@1, <<"key"/utf8>>]),
        Max_memtable_size,
        gleam@map:new()
    ),
    {Memtables_loaded, Ranges@2} = case gleam@map:size(Ranges) =:= 0 of
        true ->
            Memtable = ream@storage@kv@memtable:new(Max_memtable_size),
            Ranges@1 = begin
                _pipe = [{new_id(),
                        {mem_table_range,
                            0,
                            340282366920938463463374607431768211455,
                            {some, Memtable}}}],
                gleam@map:from_list(_pipe)
            end,
            {1, Ranges@1};

        false ->
            {0, Ranges}
    end,
    _assert_subject@2 = ream@storage@file:close(Kv),
    {ok, _} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 79})
    end,
    Value_index_file = filename:join(
        [Path@1, <<"value"/utf8>>, <<"index"/utf8>>]
    ),
    _assert_subject@3 = ream@storage@file:recursive_make_directory(
        filename:dirname(Value_index_file)
    ),
    {ok, true} = case _assert_subject@3 of
        {ok, true} -> _assert_subject@3;
        _assert_fail@3 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@3,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 82})
    end,
    _assert_subject@4 = ream@storage@file:open(Value_index_file, [read, write]),
    {ok, Kv@1} = case _assert_subject@4 of
        {ok, _} -> _assert_subject@4;
        _assert_fail@4 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@4,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 84})
    end,
    {Active_value_file, Values} = read_values(
        Kv@1,
        filename:join([Path@1, <<"value"/utf8>>]),
        none,
        Max_value_size,
        gleam@map:new()
    ),
    _assert_subject@5 = ream@storage@file:close(Kv@1),
    {ok, _} = case _assert_subject@5 of
        {ok, _} -> _assert_subject@5;
        _assert_fail@5 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@5,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"open"/utf8>>,
                        line => 87})
    end,
    {kv,
        Path@1,
        Name,
        Ranges@2,
        Active_value_file,
        Values,
        Memtables_loaded,
        Max_memtables_loaded,
        Max_memtable_size,
        Max_value_size}.

-spec write_memtable_ranges(
    gleam@erlang@process:pid_(),
    binary(),
    list({integer(), mem_table_range()})
) -> {ok, nil} | {error, nil}.
write_memtable_ranges(Kv_file, Base_path, Memtable_ranges) ->
    case Memtable_ranges of
        [{Id, {mem_table_range, Lower, Upper, {some, Memtable}}} | Rest] ->
            _assert_subject = ream@storage@kv@sstable:flush(
                Memtable,
                sstable_path(Base_path, Id)
            ),
            {ok, true} = case _assert_subject of
                {ok, true} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"write_memtable_ranges"/utf8>>,
                                line => 239})
            end,
            _assert_subject@1 = ream@storage@file:write(
                Kv_file,
                <<Lower:128, Upper:128, Id:128>>
            ),
            {ok, _} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"write_memtable_ranges"/utf8>>,
                                line => 240})
            end,
            write_memtable_ranges(Kv_file, Base_path, Rest);

        [{Id@1, {mem_table_range, Lower@1, Upper@1, none}} | Rest@1] ->
            _assert_subject@2 = ream@storage@file:write(
                Kv_file,
                <<Lower@1:128, Upper@1:128, Id@1:128>>
            ),
            {ok, _} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"write_memtable_ranges"/utf8>>,
                                line => 244})
            end,
            write_memtable_ranges(Kv_file, Base_path, Rest@1);

        [] ->
            {ok, nil}
    end.

-spec write_values(gleam@erlang@process:pid_(), list(integer())) -> {ok, nil} |
    {error, nil}.
write_values(Kv, Values) ->
    case Values of
        [Id | Rest] ->
            _assert_subject = ream@storage@file:write(Kv, <<Id:128>>),
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"write_values"/utf8>>,
                                line => 254})
            end,
            write_values(Kv, Rest);

        [] ->
            {ok, nil}
    end.

-spec flush(kv()) -> {ok, nil} | {error, nil}.
flush(Kv) ->
    Key_index_file = filename:join(
        [erlang:element(2, Kv), <<"key"/utf8>>, <<"index"/utf8>>]
    ),
    _assert_subject = ream@storage@file:recursive_make_directory(
        filename:dirname(Key_index_file)
    ),
    {ok, true} = case _assert_subject of
        {ok, true} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 215})
    end,
    _assert_subject@1 = ream@storage@file:open(Key_index_file, [write]),
    {ok, Kv_file} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 216})
    end,
    Memtable_ranges = gleam@map:to_list(erlang:element(4, Kv)),
    _assert_subject@2 = write_memtable_ranges(
        Kv_file,
        erlang:element(2, Kv),
        Memtable_ranges
    ),
    {ok, _} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 218})
    end,
    _assert_subject@3 = ream@storage@file:close(Kv_file),
    {ok, _} = case _assert_subject@3 of
        {ok, _} -> _assert_subject@3;
        _assert_fail@3 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@3,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 220})
    end,
    Value_index_file = filename:join(
        [erlang:element(2, Kv), <<"value"/utf8>>, <<"index"/utf8>>]
    ),
    _assert_subject@4 = ream@storage@file:recursive_make_directory(
        filename:dirname(Value_index_file)
    ),
    {ok, true} = case _assert_subject@4 of
        {ok, true} -> _assert_subject@4;
        _assert_fail@4 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@4,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 223})
    end,
    _assert_subject@5 = ream@storage@file:open(Value_index_file, [write]),
    {ok, Kv_file@1} = case _assert_subject@5 of
        {ok, _} -> _assert_subject@5;
        _assert_fail@5 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@5,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 225})
    end,
    _assert_subject@6 = write_values(
        Kv_file@1,
        gleam@map:keys(erlang:element(6, Kv))
    ),
    {ok, _} = case _assert_subject@6 of
        {ok, _} -> _assert_subject@6;
        _assert_fail@6 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@6,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 226})
    end,
    _assert_subject@7 = ream@storage@file:close(Kv_file@1),
    {ok, _} = case _assert_subject@7 of
        {ok, _} -> _assert_subject@7;
        _assert_fail@7 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@7,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 227})
    end,
    {ok, nil}.

-spec close(kv()) -> {ok, nil} | {error, nil}.
close(Kv) ->
    _assert_subject = flush(Kv),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"close"/utf8>>,
                        line => 205})
    end,
    _pipe = gleam@map:values(erlang:element(6, Kv)),
    gleam@list:each(_pipe, fun(V) -> ream@storage@kv@value:close(V) end),
    {ok, nil}.

-spec set(kv(), binary(), bitstring()) -> kv().
set(Kv, Key, Value) ->
    Key_hash = erlang:phash2(Key),
    {Range_id, Kv@1} = find_range(Kv, Key_hash, erlang:element(9, Kv)),
    _assert_subject = gleam@map:get(erlang:element(4, Kv@1), Range_id),
    {ok, Range} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"set"/utf8>>,
                        line => 284})
    end,
    _assert_subject@1 = erlang:element(4, Range),
    {some, Memtable} = case _assert_subject@1 of
        {some, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"set"/utf8>>,
                        line => 285})
    end,
    Kv@2 = case erlang:element(5, Kv@1) of
        {some, _} ->
            Kv@1;

        none ->
            _assert_subject@2 = ream@storage@kv@value:create(
                filename:join([erlang:element(2, Kv@1), <<"value"/utf8>>]),
                erlang:element(10, Kv@1)
            ),
            {ok, Vfile} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"set"/utf8>>,
                                line => 289})
            end,
            erlang:setelement(
                6,
                erlang:setelement(5, Kv@1, {some, erlang:element(2, Vfile)}),
                gleam@map:insert(
                    erlang:element(6, Kv@1),
                    erlang:element(2, Vfile),
                    Vfile
                )
            )
    end,
    case ream@storage@kv@memtable:get(Memtable, Key) of
        {ok, Old_value} ->
            case store_value(Kv@2, Key, Range_id, Range, Memtable, Value) of
                {ok, Kv@3} ->
                    Kv@3;

                {error, capacity_exceeded} ->
                    Kv@4 = split(Kv@2, Key_hash, Range_id, Range, Memtable),
                    set(Kv@4, Key, Value)
            end,
            _assert_subject@3 = delete_value(Kv@2, Old_value),
            {ok, Kv@5} = case _assert_subject@3 of
                {ok, _} -> _assert_subject@3;
                _assert_fail@3 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@3,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"set"/utf8>>,
                                line => 308})
            end,
            Kv@5;

        {error, nil} ->
            case store_value(Kv@2, Key, Range_id, Range, Memtable, Value) of
                {ok, Kv@6} ->
                    Kv@6;

                {error, capacity_exceeded} ->
                    Kv@7 = split(Kv@2, Key_hash, Range_id, Range, Memtable),
                    set(Kv@7, Key, Value)
            end
    end.

-spec store_value(
    kv(),
    binary(),
    integer(),
    mem_table_range(),
    ream@storage@kv@memtable:mem_table(),
    bitstring()
) -> {ok, kv()} | {error, ream@storage@kv@memtable:reason()}.
store_value(Kv, Key, Range_id, Range, Memtable, Value_data) ->
    _assert_subject = erlang:element(5, Kv),
    {some, File_id} = case _assert_subject of
        {some, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"store_value"/utf8>>,
                        line => 368})
    end,
    _assert_subject@1 = gleam@map:get(erlang:element(6, Kv), File_id),
    {ok, Vfile} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv"/utf8>>,
                        function => <<"store_value"/utf8>>,
                        line => 369})
    end,
    case ream@storage@kv@value:write(Vfile, Value_data) of
        {ok, {Vfile@1, Value}} ->
            gleam@result:'try'(
                ream@storage@kv@memtable:set(Memtable, Key, Value),
                fun(Memtable@1) ->
                    Range@1 = erlang:setelement(4, Range, {some, Memtable@1}),
                    {ok,
                        erlang:setelement(
                            4,
                            erlang:setelement(
                                6,
                                erlang:setelement(
                                    5,
                                    Kv,
                                    {some, erlang:element(2, Vfile@1)}
                                ),
                                gleam@map:insert(
                                    erlang:element(6, Kv),
                                    erlang:element(2, Vfile@1),
                                    Vfile@1
                                )
                            ),
                            gleam@map:insert(
                                erlang:element(4, Kv),
                                Range_id,
                                Range@1
                            )
                        )}
                end
            );

        {error, capacity_exceeded} ->
            _assert_subject@2 = ream@storage@kv@value:create(
                filename:join([erlang:element(2, Kv), <<"value"/utf8>>]),
                erlang:element(10, Kv)
            ),
            {ok, Vfile@2} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"ream/storage/kv"/utf8>>,
                                function => <<"store_value"/utf8>>,
                                line => 384})
            end,
            _pipe = erlang:setelement(
                6,
                erlang:setelement(5, Kv, {some, erlang:element(2, Vfile@2)}),
                gleam@map:insert(
                    erlang:element(6, Kv),
                    erlang:element(2, Vfile@2),
                    Vfile@2
                )
            ),
            store_value(_pipe, Key, Range_id, Range, Memtable, Value_data)
    end.
