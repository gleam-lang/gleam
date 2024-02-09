-record(function, {
    linkage :: glove:linkage(),
    name :: binary(),
    arguments :: list({glove:type(), glove:value()}),
    return_ty :: gleam@option:option(glove:type()),
    blocks :: list(glove:block())
}).
