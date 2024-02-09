-record(mem_table, {
    entries :: gleam@map:map_(integer(), ream@storage@kv@memtable:mem_table_entry()),
    size :: integer(),
    max_size :: integer()
}).
