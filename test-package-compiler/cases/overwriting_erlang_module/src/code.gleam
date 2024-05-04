// This module is named "code".
//
// If you were to compile this and load it into the Erlang virtual machine it
// would replace the built-in "code" module. That would be _bad_. You'd get
// super cryptic error messages and likely not know what is happening. This did
// happen fairly often with new folks, so now the build tool ensures you don't
// use one of these Erlang names.
//
// This test case ensures this is true.
