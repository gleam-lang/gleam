-record(attributes, {
    max_age :: gleam@option:option(integer()),
    domain :: gleam@option:option(binary()),
    path :: gleam@option:option(binary()),
    secure :: boolean(),
    http_only :: boolean(),
    same_site :: gleam@option:option(gleam@http@cookie:same_site_policy())
}).
