-record(request, {
    method :: gleam@http:method(),
    headers :: list({binary(), binary()}),
    body :: any(),
    scheme :: gleam@http:scheme(),
    host :: binary(),
    port :: gleam@option:option(integer()),
    path :: binary(),
    'query' :: gleam@option:option(binary())
}).
