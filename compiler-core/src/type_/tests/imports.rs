use crate::{assert_infer_with_module, assert_with_module_error};

// https://github.com/gleam-lang/gleam/issues/1760
#[test]
fn import_value_with_same_name_as_imported_module() {
    assert_infer_with_module!(
        ("other", "pub const other = 1"),
        "
import other.{other}
pub const a = other
",
        vec![("a", "Int")],
    );
}

#[test]
fn imported_constant_record() {
    assert_infer_with_module!(
        ("one/two", "pub type Thing { Thing(Int) }"),
        "
import one/two

pub const a = two.Thing(1)
",
        vec![("a", "Thing")],
    );
}

#[test]
fn using_private_constructo() {
    assert_with_module_error!(
        ("one", "type Two { Two }"),
        "import one

pub fn main() {
  one.Two
}",
    );
}

#[test]
fn using_private_constructor_pattern() {
    assert_with_module_error!(
        ("one", "type Two { Two }"),
        "import one

pub fn main(x) {
  let one.Two = x
}",
    );
}

#[test]
fn using_opaque_constructo() {
    assert_with_module_error!(
        ("one", "pub opaque type Two { Two }"),
        "import one

pub fn main() {
  one.Two
}",
    );
}

#[test]
fn using_private_function() {
    assert_with_module_error!(
        ("one", "fn two() { 2 }"),
        "import one

pub fn main() {
  one.two
}",
    );
}

#[test]
fn using_private_type_alias() {
    assert_with_module_error!(
        ("one", "type X = Int"),
        "import one

pub fn main() {
  one.X
}",
    );
}

#[test]
fn using_private_unqualified_type_alias() {
    assert_with_module_error!(
        ("one", "type X = Int"),
        "import one.{X}

pub fn main() {
  X
}",
    );
}

#[test]
fn using_private_external_type() {
    assert_with_module_error!(
        ("one", "external type X"),
        "import one

pub fn main() {
  one.X
}",
    );
}

#[test]
fn using_private_unqualified_external_type() {
    assert_with_module_error!(
        ("one", "external type X"),
        "import one.{X}

pub fn main() {
  X
}",
    );
}

#[test]
fn using_private_custom_type() {
    assert_with_module_error!(
        ("one", "type X { Y }"),
        "import one

pub fn main() {
  one.X
}",
    );
}

#[test]
fn using_private_unqualified_custom_type() {
    assert_with_module_error!(
        ("one", "type X { Y }"),
        "import one.{X}

pub fn main() {
  X
}",
    );
}

#[test]
fn unqualified_using_private_constructo() {
    assert_with_module_error!(
        ("one", "type Two { Two }"),
        "import one.{Two}

pub fn main() {
  Two
}",
    );
}

#[test]
fn unqualified_using_private_constructor_pattern() {
    assert_with_module_error!(
        ("one", "type Two { Two }"),
        "import one.{Two}

pub fn main(x) {
  let Two = x
}",
    );
}

#[test]
fn unqualified_using_opaque_constructo() {
    assert_with_module_error!(
        ("one", "pub opaque type Two { Two }"),
        "import one.{Two}

pub fn main() {
  Two
}",
    );
}

#[test]
fn unqualified_using_private_function() {
    assert_with_module_error!(
        ("one", "fn two() { 2 }"),
        "import one.{two}

pub fn main() {
  two
}",
    );
}
