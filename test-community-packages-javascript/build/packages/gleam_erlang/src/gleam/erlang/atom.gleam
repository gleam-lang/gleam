import gleam/dynamic.{DecodeErrors, Dynamic}

/// Atom is a special string-like data-type that is most commonly used for
/// interfacing with code written in other BEAM languages such as Erlang and
/// Elixir. It is preferable to define your own custom types to use instead of
/// atoms where possible.
///
/// Atoms are not used much in typical Gleam code!
///
/// ## Creating atoms
///
/// We can create atoms with the the [`create_from_string`](#create_from_string)
/// function, though we must be careful when doing so as atoms are never
/// garbage collected. If we create too many atoms (for example, if we convert
/// user input into atoms) we may hit the max limit of atoms and cause the
/// virtual machine to crash.
///
pub external type Atom

/// An error returned when no atom is found in the virtual machine's atom table
/// for a given string when calling the [`from_string`](#from_string) function.
pub type FromStringError {
  AtomNotLoaded
}

/// Finds an existing Atom for the given String.
///
/// If no atom is found in the virtual machine's atom table for the String then
/// an error is returned.
///
/// ## Examples
///
///    > from_string("ok")
///    Ok(create_from_string("ok"))
///
///    > from_string("some_new_atom")
///    Error(AtomNotLoaded)
///
pub external fn from_string(String) -> Result(Atom, FromStringError) =
  "gleam_erlang_ffi" "atom_from_string"

/// Creates an atom from a string, inserting a new value into the virtual
/// machine's atom table if an atom does not already exist for the given
/// string.
///
/// We must be careful when using this function as there is a limit to the
/// number of atom that can fit in the virtual machine's atom table. Never
/// convert user input into atoms as filling the atom table will cause the
/// virtual machine to crash!
///
pub external fn create_from_string(String) -> Atom =
  "erlang" "binary_to_atom"

/// Retuns a `String` corresponding to the text representation of the given
/// `Atom`.
///
/// ## Examples
///
///    > let ok_atom = create_from_string("ok")
///    > to_string(ok_atom)
///    "ok"
///
pub external fn to_string(Atom) -> String =
  "erlang" "atom_to_binary"

/// Checks to see whether a `Dynamic` value is an atom, and return the atom if
/// it is.
///
/// ## Examples
///
///    > import gleam/dynamic
///    > from_dynamic(dynamic.from(create_from_string("hello")))
///    Ok(create_from_string("hello"))
///
///    > from_dynamic(dynamic.from(123))
///    Error([DecodeError(expected: "Atom", found: "Int", path: [])])
///
pub external fn from_dynamic(from: Dynamic) -> Result(Atom, DecodeErrors) =
  "gleam_erlang_ffi" "atom_from_dynamic"
