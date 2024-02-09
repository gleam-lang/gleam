-record(response, {
    status :: integer(),
    headers :: list({binary(), binary()}),
    body :: any()
}).
