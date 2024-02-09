-record(data_def, {
    linkage :: glove:linkage(),
    name :: binary(),
    align :: gleam@option:option(integer()),
    items :: list({glove:type(), glove:data_item()})
}).
