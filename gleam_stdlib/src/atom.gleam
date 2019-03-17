import expect

pub external type Atom;

pub enum AtomNotLoaded =
  | AtomNotLoaded

pub external fn from_string(String) -> Result(Atom, AtomNotLoaded) =
  "gleam__stdlib" "atom_from_string";

test from_string {
  "ok"
  |> from_string
  |> expect:is_ok

  "expect"
  |> from_string
  |> expect:is_ok

  "this is not an atom we have seen before"
  |> from_string
  |> expect:equal(_, Error(AtomNotLoaded))
}

// This function can create a new atom if one does not already exist for
// the given string. Atoms are not garbage collected so this can result
// in a memory leak if called over time on new values
//
pub external fn create_from_string(String) -> Atom =
  "gleam__stdlib" "atom_create_from_string";

test create_from_string {
  let ok = fn(x) { Ok(x) }

  "ok"
  |> create_from_string
  |> ok
  |> expect:equal(_, from_string("ok"))

  "expect"
  |> create_from_string
  |> ok
  |> expect:equal(_, from_string("expect"))

  "this is another atom we have not seen before"
  |> create_from_string
  |> ok
  |> expect:equal(_, from_string("this is another atom we have not seen before"))
}

pub external fn to_string(Atom) -> String =
  "gleam__stdlib" "atom_to_string";

test to_string {
  "ok"
  |> create_from_string
  |> to_string
  |> expect:equal(_, "ok")

  "expect"
  |> create_from_string
  |> to_string
  |> expect:equal(_, "expect")
}
