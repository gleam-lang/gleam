# Stack overflow test

Windows has a smaller stack than other operating systems, meaning recursively
analysing many nested AST nodes can cause a stack overflow on Windows.

See: https://github.com/gleam-lang/gleam/issues/4287 for details.
