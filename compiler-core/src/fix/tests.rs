use super::*;

use camino::Utf8Path;

fn fix(src: &str) -> String {
    parse_fix_and_format(&src.into(), Utf8Path::new("test")).unwrap()
}

#[test]
fn empty() {
    assert_eq!(fix(""), "\n")
}

#[test]
fn import() {
    assert_eq!(fix("import gleam.{BitString}"), "import gleam.{BitArray}\n");
}

#[test]
fn import_aliased() {
    assert_eq!(
        fix("import gleam.{BitString as B}"),
        "import gleam.{BitArray as B}\n"
    );
}

#[test]
fn alias() {
    assert_eq!(
        fix("pub type X =
  BitString
"),
        "pub type X =
  BitArray
"
    );
}

#[test]
fn alias_qualified() {
    assert_eq!(
        fix("import gleam

pub type X =
  gleam.BitString
"),
        "import gleam

pub type X =
  gleam.BitArray
"
    );
}

#[test]
fn alias_qualified_aliased() {
    assert_eq!(
        fix("import gleam as g

pub type X =
  g.BitString
"),
        "import gleam as g

pub type X =
  g.BitArray
"
    );
}

#[test]
fn custom_type() {
    assert_eq!(
        fix("pub type X {
  X(BitString)
}
"),
        "pub type X {
  X(BitArray)
}
"
    );
}

#[test]
fn custom_type_qualified() {
    assert_eq!(
        fix("import gleam

pub type X {
  X(gleam.BitString)
}
"),
        "import gleam

pub type X {
  X(gleam.BitArray)
}
"
    );
}

#[test]
fn custom_type_qualified_aliased() {
    assert_eq!(
        fix("import gleam as g

pub type X {
  X(g.BitString)
}
"),
        "import gleam as g

pub type X {
  X(g.BitArray)
}
"
    );
}

#[test]
fn shadowed_by_alias() {
    assert_eq!(
        fix("pub type X =
  BitString

pub type BitString =
  Int
"),
        "pub type X =
  BitString

pub type BitString =
  Int
"
    );
}

#[test]
fn shadowed_by_custom_type() {
    assert_eq!(
        fix("pub type X =
  BitString

pub type BitString {
  B
}
"),
        "pub type X =
  BitString

pub type BitString {
  B
}
"
    );
}

#[test]
fn shadowed_by_import() {
    assert_eq!(
        fix("import x.{BitString}

pub type X =
  BitString
"),
        "import x.{BitString}

pub type X =
  BitString
"
    );
}

#[test]
fn return_type() {
    assert_eq!(
        fix("pub fn main() -> BitString {
  todo
}
"),
        "pub fn main() -> BitArray {
  todo
}
"
    );
}

#[test]
fn arguments() {
    assert_eq!(
        fix("pub fn main(
  a: BitString,
  b b: List(BitString),
  z,
  _: BitString,
  _: BitString,
) -> Nil {
  todo
}
"),
        "pub fn main(
  a: BitArray,
  b b: List(BitArray),
  z,
  _: BitArray,
  _: BitArray,
) -> Nil {
  todo
}
"
    );
}

#[test]
fn let_annotation() {
    assert_eq!(
        fix("pub fn main() {
  let x: List(BitString) = todo
}
"),
        "pub fn main() {
  let x: List(BitArray) = todo
}
"
    );
}

#[test]
fn use_annotation() {
    assert_eq!(
        fix("pub fn main() {
  use x: List(BitString), y: BitString <- todo
}
"),
        "pub fn main() {
  use x: List(BitArray), y: BitArray <- todo
}
"
    );
}

#[test]
fn constant() {
    assert_eq!(
        fix("pub const x: BitString = <<>>"),
        "pub const x: BitArray = <<>>\n"
    );
}
