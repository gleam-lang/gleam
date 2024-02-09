-record(value, {
    offset :: integer(),
    deleted :: boolean(),
    data :: gleam@option:option(bitstring()),
    file_id :: integer()
}).
