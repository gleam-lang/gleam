-module(ream@storage@kv@memtable).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new/1, bitstring_to_entry/1, search_pivot/1, get_bounds/1, hash/1, entry_to_bitstring/1, contains/2, get/2, set/3, delete/2, from_entries/2, split/2]).
-export_type([mem_table/0, mem_table_entry/0, reason/0]).

-type mem_table() :: {mem_table,
        gleam@map:map_(integer(), mem_table_entry()),
        integer(),
        integer()}.

-type mem_table_entry() :: {mem_table_entry,
        binary(),
        ream@storage@kv@value:value()}.

-type reason() :: capacity_exceeded.

-spec new(integer()) -> mem_table().
new(Max_size) ->
    {mem_table, gleam@map:new(), 0, Max_size}.

-spec bitstring_to_entry(bitstring()) -> {integer(), mem_table_entry()}.
bitstring_to_entry(Bitstring) ->
    <<Key_hash:128, _:16, File_id:128, File_offset:32, Key_string/bitstring>> = Bitstring,
    _assert_subject = gleam@bit_string:to_string(Key_string),
    {ok, Key} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv/memtable"/utf8>>,
                        function => <<"bitstring_to_entry"/utf8>>,
                        line => 96})
    end,
    Value = {value, File_offset, false, none, File_id},
    {Key_hash, {mem_table_entry, Key, Value}}.

-spec search_pivot(mem_table()) -> integer().
search_pivot(Mem_table) ->
    Entries = erlang:element(2, Mem_table),
    Keys = gleam@map:keys(Entries),
    Entries_count = gleam@map:size(Entries),
    {_, [Pivot | _]} = gleam@list:split(Keys, Entries_count div 2),
    Pivot.

-spec get_lower(integer(), integer()) -> integer().
get_lower(Lower_bound, Key) ->
    case Lower_bound of
        0 ->
            Key;

        Lower_bound@1 when Key < Lower_bound@1 ->
            Key;

        Lower_bound@2 ->
            Lower_bound@2
    end.

-spec get_higher(integer(), integer()) -> integer().
get_higher(Higher_bound, Key) ->
    case Higher_bound of
        0 ->
            Key;

        Higher_bound@1 when Key > Higher_bound@1 ->
            Key;

        Higher_bound@2 ->
            Higher_bound@2
    end.

-spec get_bounds(mem_table()) -> {integer(), integer()}.
get_bounds(Mem_table) ->
    case gleam@map:to_list(erlang:element(2, Mem_table)) of
        [] ->
            {0, 0};

        [{K, _} | Entries] ->
            gleam@list:fold(
                Entries,
                {K, K},
                fun(Acc, Entry) ->
                    {get_lower(erlang:element(1, Acc), erlang:element(1, Entry)),
                        get_higher(
                            erlang:element(2, Acc),
                            erlang:element(1, Entry)
                        )}
                end
            )
    end.

-spec hash(binary()) -> integer().
hash(Field@0) ->
    erlang:phash2(Field@0).

-spec entry_to_bitstring(mem_table_entry()) -> bitstring().
entry_to_bitstring(Entry) ->
    Key_hash = erlang:phash2(erlang:element(2, Entry)),
    Key_string = gleam@bit_string:from_string(erlang:element(2, Entry)),
    Key_size = gleam@bit_string:byte_size(Key_string),
    File_id = erlang:element(5, erlang:element(3, Entry)),
    File_offset = erlang:element(2, erlang:element(3, Entry)),
    <<Key_hash:128,
        Key_size:16,
        File_id:128,
        File_offset:32,
        Key_string/bitstring>>.

-spec contains(mem_table(), binary()) -> boolean().
contains(Mem_table, Key) ->
    gleam@map:has_key(erlang:element(2, Mem_table), erlang:phash2(Key)).

-spec get(mem_table(), binary()) -> {ok, ream@storage@kv@value:value()} |
    {error, nil}.
get(Mem_table, Key) ->
    case gleam@map:get(erlang:element(2, Mem_table), erlang:phash2(Key)) of
        {ok, {mem_table_entry, Stored_key, Value}} when Key =:= Stored_key ->
            {ok, Value};

        {ok, {mem_table_entry, _, _}} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"panic expression evaluated"/utf8>>,
                    module => <<"ream/storage/kv/memtable"/utf8>>,
                    function => <<"get"/utf8>>,
                    line => 183});

        {error, nil} ->
            {error, nil}
    end.

-spec calculate_size(mem_table_entry()) -> integer().
calculate_size(Entry) ->
    gleam@bit_string:byte_size(entry_to_bitstring(Entry)).

-spec set(mem_table(), binary(), ream@storage@kv@value:value()) -> {ok,
        mem_table()} |
    {error, reason()}.
set(Mem_table, Key, Value) ->
    Key_hash = erlang:phash2(Key),
    case gleam@map:get(erlang:element(2, Mem_table), Key_hash) of
        {error, nil} ->
            Entry = {mem_table_entry, Key, Value},
            Entry_size = calculate_size(Entry),
            Current_size = erlang:element(3, Mem_table) + Entry_size,
            case Current_size > erlang:element(4, Mem_table) of
                true ->
                    {error, capacity_exceeded};

                false ->
                    {ok,
                        erlang:setelement(
                            3,
                            erlang:setelement(
                                2,
                                Mem_table,
                                gleam@map:insert(
                                    erlang:element(2, Mem_table),
                                    Key_hash,
                                    Entry
                                )
                            ),
                            Current_size
                        )}
            end;

        {ok, Old_entry} ->
            Old_entry_size = calculate_size(Old_entry),
            Entry@1 = erlang:setelement(3, Old_entry, Value),
            Current_entry_size = calculate_size(Entry@1),
            Mem_table_size = (erlang:element(3, Mem_table) + Current_entry_size)
            - Old_entry_size,
            case Mem_table_size > erlang:element(4, Mem_table) of
                true ->
                    {error, capacity_exceeded};

                false ->
                    {ok,
                        erlang:setelement(
                            3,
                            erlang:setelement(
                                2,
                                Mem_table,
                                gleam@map:insert(
                                    erlang:element(2, Mem_table),
                                    Key_hash,
                                    Entry@1
                                )
                            ),
                            Mem_table_size
                        )}
            end
    end.

-spec delete(mem_table(), binary()) -> mem_table().
delete(Mem_table, Key) ->
    Key_hash = erlang:phash2(Key),
    case gleam@map:get(erlang:element(2, Mem_table), Key_hash) of
        {error, nil} ->
            Mem_table;

        {ok, Entry} ->
            erlang:setelement(
                3,
                erlang:setelement(
                    2,
                    Mem_table,
                    gleam@map:delete(erlang:element(2, Mem_table), Key_hash)
                ),
                erlang:element(3, Mem_table) - calculate_size(Entry)
            )
    end.

-spec calculate_entries_size(gleam@map:map_(integer(), mem_table_entry())) -> integer().
calculate_entries_size(Entries) ->
    gleam@map:fold(
        Entries,
        0,
        fun(Acc, _, Entry) -> Acc + calculate_size(Entry) end
    ).

-spec from_entries(gleam@map:map_(integer(), mem_table_entry()), integer()) -> mem_table().
from_entries(Entries, Max_size) ->
    Size = calculate_entries_size(Entries),
    {mem_table, Entries, Size, Max_size}.

-spec split(mem_table(), integer()) -> {mem_table(), mem_table(), integer()}.
split(Mem_table, Pivot) ->
    {Low_entries, High_entries} = begin
        _pipe = erlang:element(2, Mem_table),
        _pipe@1 = gleam@map:to_list(_pipe),
        gleam@list:partition(
            _pipe@1,
            fun(Entry) -> erlang:element(1, Entry) < Pivot end
        )
    end,
    Low_entries@1 = gleam@map:from_list(Low_entries),
    Low = erlang:setelement(
        3,
        erlang:setelement(2, Mem_table, Low_entries@1),
        calculate_entries_size(Low_entries@1)
    ),
    High_entries@1 = gleam@map:from_list(High_entries),
    High = erlang:setelement(
        3,
        erlang:setelement(2, Mem_table, High_entries@1),
        calculate_entries_size(High_entries@1)
    ),
    case {erlang:element(3, Low) >= erlang:element(3, High),
        gleam@map:get(erlang:element(2, High), Pivot)} of
        {true, _} ->
            {Low, High, Pivot};

        {false, {error, nil}} ->
            {Low, High, Pivot + 1};

        {false, {ok, Entry@1}} ->
            High@1 = erlang:setelement(
                3,
                erlang:setelement(
                    2,
                    High,
                    gleam@map:delete(erlang:element(2, High), Pivot)
                ),
                erlang:element(3, High) - calculate_size(Entry@1)
            ),
            Low@1 = erlang:setelement(
                3,
                erlang:setelement(
                    2,
                    Low,
                    gleam@map:insert(erlang:element(2, Low), Pivot, Entry@1)
                ),
                erlang:element(3, Low) + calculate_size(Entry@1)
            ),
            {Low@1, High@1, Pivot + 1}
    end.
