# root_package_not_compiled_when_running_dep

This package is used to check that the compiler can compile and run a dependency
even if the root package has compilation errors.

So one can do `gleam run -m <dependency-module>` even if their own code doesn't
compile.
