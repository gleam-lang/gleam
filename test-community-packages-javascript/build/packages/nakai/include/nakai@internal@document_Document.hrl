-record(document, {
    doctype :: gleam@option:option(binary()),
    html_attrs :: gleam@string_builder:string_builder(),
    body_attrs :: gleam@string_builder:string_builder(),
    head :: gleam@string_builder:string_builder(),
    body :: gleam@string_builder:string_builder(),
    scripts :: list(binary())
}).
