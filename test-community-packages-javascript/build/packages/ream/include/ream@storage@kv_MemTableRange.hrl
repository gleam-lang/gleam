-record(mem_table_range, {
    lower :: integer(),
    upper :: integer(),
    memtable :: gleam@option:option(ream@storage@kv@memtable:mem_table())
}).
