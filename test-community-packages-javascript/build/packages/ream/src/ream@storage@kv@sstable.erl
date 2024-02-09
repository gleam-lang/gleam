-module(ream@storage@kv@sstable).
-compile([no_auto_import, nowarn_unused_vars]).

-export([flush/2, load/2]).

-spec flush(ream@storage@kv@memtable:mem_table(), binary()) -> {ok, boolean()} |
    {error, gleam@erlang@file:reason()}.
flush(Mem_table, Path) ->
    _assert_subject = ream@storage@file:recursive_make_directory(
        filename:dirname(Path)
    ),
    {ok, true} = case _assert_subject of
        {ok, true} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv/sstable"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 11})
    end,
    _assert_subject@1 = ream@storage@file:open(Path, [write]),
    {ok, File} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv/sstable"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 12})
    end,
    gleam@map:filter(
        erlang:element(2, Mem_table),
        fun(_, Entry) ->
            Content = ream@storage@kv@memtable:entry_to_bitstring(Entry),
            _assert_subject@2 = ream@storage@file:write(File, Content),
            {ok, _} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"ream/storage/kv/sstable"/utf8>>,
                                function => <<"flush"/utf8>>,
                                line => 18})
            end,
            false
        end
    ),
    _assert_subject@3 = ream@storage@file:close(File),
    {ok, _} = case _assert_subject@3 of
        {ok, _} -> _assert_subject@3;
        _assert_fail@3 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@3,
                        module => <<"ream/storage/kv/sstable"/utf8>>,
                        function => <<"flush"/utf8>>,
                        line => 22})
    end,
    {ok, true}.

-spec read_entries(
    gleam@erlang@process:pid_(),
    gleam@map:map_(integer(), ream@storage@kv@memtable:mem_table_entry())
) -> {ok, gleam@map:map_(integer(), ream@storage@kv@memtable:mem_table_entry())} |
    {error, gleam@erlang@file:reason()}.
read_entries(File, Entries) ->
    case file:read(File, 38) of
        {ok, <<Key_hash:128, Key_size:16, File_id:128, Offset:32>>} ->
            _assert_subject = file:read(File, Key_size),
            {ok, Key_string} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"ream/storage/kv/sstable"/utf8>>,
                                function => <<"read_entries"/utf8>>,
                                line => 39})
            end,
            {_, Entry} = ream@storage@kv@memtable:bitstring_to_entry(
                <<Key_hash:128,
                    Key_size:16,
                    File_id:128,
                    Offset:32,
                    Key_string/bitstring>>
            ),
            Entries@1 = gleam@map:insert(Entries, Key_hash, Entry),
            read_entries(File, Entries@1);

        eof ->
            {ok, Entries};

        {error, Reason} ->
            {error, Reason}
    end.

-spec load(binary(), integer()) -> {ok, ream@storage@kv@memtable:mem_table()} |
    {error, gleam@erlang@file:reason()}.
load(Path, Max_size) ->
    _assert_subject = ream@storage@file:open(Path, [read]),
    {ok, File} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/storage/kv/sstable"/utf8>>,
                        function => <<"load"/utf8>>,
                        line => 27})
    end,
    _assert_subject@1 = read_entries(File, gleam@map:new()),
    {ok, Entries} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"ream/storage/kv/sstable"/utf8>>,
                        function => <<"load"/utf8>>,
                        line => 28})
    end,
    _assert_subject@2 = ream@storage@file:close(File),
    {ok, _} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"ream/storage/kv/sstable"/utf8>>,
                        function => <<"load"/utf8>>,
                        line => 29})
    end,
    {ok, ream@storage@kv@memtable:from_entries(Entries, Max_size)}.
