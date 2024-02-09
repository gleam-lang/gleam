-record(stream, {
    name :: binary(),
    index :: ream@storage@stream@index:index_file(),
    active_file :: gleam@option:option(ream@storage@stream@event:event_file()),
    files :: gleam@map:map_(integer(), ream@storage@stream@event:event_file()),
    base_path :: binary()
}).
