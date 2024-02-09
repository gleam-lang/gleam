//// Access to the shell's environment variables

import gleam/map.{Map}

/// Returns the list of all available environment variables as a list of key,
/// tuples.
///
/// ## Examples
///
///    > get_all_env()
///    map.from_list([
///      #("SHELL", "/bin/bash"),
///      #("PWD", "/home/j3rn"),
///      ...
///    ])
///
pub external fn get_all_env() -> Map(String, String) =
  "gleam_erlang_ffi" "get_all_env"

/// Returns the value associated with the given environment variable name.
///
/// ## Examples
///
///    > get_env("SHELL")
///    "/bin/bash"
///
///    > get_env(name: "PWD")
///    "/home/j3rn"
///
pub external fn get_env(name: String) -> Result(String, Nil) =
  "gleam_erlang_ffi" "get_env"

/// Associates the given value with the given environment variable name.
///
/// ## Examples
///
///    > set_env("MYVAR", "MYVALUE")
///    Nil
///    > get_env("MYVAR")
///    "MYVALUE"
///
///    > set_env(value: "MYVALUE", name: "MYVAR")
///    Nil
///
pub external fn set_env(name: String, value: String) -> Nil =
  "gleam_erlang_ffi" "set_env"

/// Removes the environment variable with the given name.
///
/// Returns Nil regardless of whether the variable ever existed.
///
/// ## Examples
///
///    > get_env("MYVAR")
///    Ok("MYVALUE")
///    > unset_env("MYVAR")
///    Nil
///    > get_env("MYVAR")
///    Error(Nil)
///
///    > unset_env(name: "MYVAR")
///    Nil
///
pub external fn unset_env(name: String) -> Nil =
  "gleam_erlang_ffi" "unset_env"

/// Represents operating system kernels
pub type OsFamily {
  // The family which includes modern versions of the Windows operating system.
  WindowsNt
  // The family of operating systems based on the open source Linux kernel.
  Linux
  // The family of Apple operating systems such as macOS and iOS.
  Darwin
  // The family of operating systems based on the FreeBSD kernel.
  FreeBsd
  // An operating system kernel other than Linux, Darwin, FreeBSD, or NT.
  Other(String)
}

/// Returns the kernel of the host operating system.
///
/// Unknown kernels are reported as `Other(String)`; e.g. `Other("sunos")`.
///
/// ## Examples
///
///    > family()
///    Linux
///    > family()
///    Darwin
///    > family()
///    Other("sunos")
///
pub external fn family() -> OsFamily =
  "gleam_erlang_ffi" "os_family"
