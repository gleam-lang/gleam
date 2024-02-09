-record(kv, {
    base_path :: binary(),
    name :: binary(),
    memtable_ranges :: gleam@map:map_(integer(), ream@storage@kv:mem_table_range()),
    active_value_file :: gleam@option:option(integer()),
    values :: gleam@map:map_(integer(), ream@storage@kv@value:value_file()),
    memtables_loaded :: integer(),
    max_memtables_loaded :: integer(),
    max_memtable_size :: integer(),
    max_value_size :: integer()
}).
