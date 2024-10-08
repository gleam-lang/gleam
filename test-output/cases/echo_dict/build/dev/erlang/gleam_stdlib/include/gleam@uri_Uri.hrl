-record(uri, {
    scheme :: gleam@option:option(binary()),
    userinfo :: gleam@option:option(binary()),
    host :: gleam@option:option(binary()),
    port :: gleam@option:option(integer()),
    path :: binary(),
    'query' :: gleam@option:option(binary()),
    fragment :: gleam@option:option(binary())
}).
