-record(response, {
    status :: integer(),
    headers :: list({binary(), binary()}),
    body :: gleam@option:option(binary())
}).
