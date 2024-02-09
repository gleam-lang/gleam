-record(file_body, {
    file_descriptor :: mist@internal@file:file_descriptor(),
    content_type :: binary(),
    offset :: integer(),
    length :: integer()
}).
