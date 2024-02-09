-record(server, {
    port :: integer(),
    layout :: fun((nakai@html:node_(gliew@internal@event:event())) -> nakai@html:node_(gliew@internal@event:event())),
    handler :: fun((gleam@http@request:request(mist@internal@http:body())) -> gliew:response())
}).
