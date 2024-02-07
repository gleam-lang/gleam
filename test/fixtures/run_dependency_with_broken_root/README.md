# run_dependency_with_broken_root

This package has a module with broken code.
It is used to test that running `gleam run -m <module>` with a module belonging
to an external dependency - gleeunit in this case - still works.
