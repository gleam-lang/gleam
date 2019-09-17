pub external type Atom;

pub enum AtomNotLoaded =
  | AtomNotLoaded

pub external fn from_string(String) -> Result(Atom, AtomNotLoaded) =
  "gleam_stdlib" "atom_from_string";

// This function can create a new atom if one does not already exist for
// the given string. Atoms are not garbage collected so this can result
// in a memory leak if called over time on new values
//
pub external fn create_from_string(String) -> Atom =
  "gleam_stdlib" "atom_create_from_string";

pub external fn to_string(Atom) -> String =
  "gleam_stdlib" "atom_to_string";
