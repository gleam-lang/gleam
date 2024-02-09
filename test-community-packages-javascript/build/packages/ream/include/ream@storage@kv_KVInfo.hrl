-record(kv_info, {
    base_path :: binary(),
    name :: binary(),
    values :: integer(),
    values_size_bytes :: integer(),
    memtables_total :: integer(),
    memtables_loaded :: integer(),
    memtables_loaded_size_bytes :: integer(),
    max_memtables_loaded :: integer(),
    max_memtable_size :: integer(),
    max_value_size :: integer()
}).
