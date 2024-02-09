-record(type_def, {
    name :: binary(),
    align :: gleam@option:option(integer()),
    items :: list({glove:type(), integer()})
}).
