use super::*;

fn fix(src: &str) -> String {
    parse_fix_and_format(&src.into(), &Path::new("test")).unwrap()
}

#[test]
fn empty() {
    assert_eq!(fix(""), "\n")
}

#[test]
fn import_needed() {
    assert_eq!(
        fix("
pub fn main(y) {
  try x = y
  y
}
"),
        "import gleam/result

pub fn main(y) {
  use x <- result.then(y)
  y
}
"
    )
}

#[test]
fn import_not_needed() {
    assert_eq!(
        fix("import gleam/result

pub fn main(y) {
  try x = y
  y
}
"),
        "import gleam/result

pub fn main(y) {
  use x <- result.then(y)
  y
}
"
    )
}

#[test]
fn discarding_the_value() {
    assert_eq!(
        fix("import gleam/result

pub fn main(y) {
  try _ = y
  y
}
"),
        "import gleam/result

pub fn main(y) {
  use _ <- result.then(y)
  y
}
"
    )
}

#[test]
fn module_name_already_taken() {
    assert_eq!(
        fix("import one/result

pub fn main(y) {
  try _ = y
  y
}
"),
        "import gleam/result as gleam_result

import one/result

pub fn main(y) {
  use _ <- gleam_result.then(y)
  y
}
"
    )
}

#[test]
fn both_module_names_already_taken() {
    assert_eq!(
        fix("import two/result as gleam_result
import one/result

pub fn main(y) {
  try _ = y
  y
}
"),
        "import gleam/result as gleam_gleam_result

import two/result as gleam_result
import one/result

pub fn main(y) {
  use _ <- gleam_gleam_result.then(y)
  y
}
"
    )
}

#[test]
fn patterns() {
    assert_eq!(
        fix("
pub fn main(y) {
  try Box(x) = y
  x
}
"),
        "import gleam/result

pub fn main(y) {
  use Box(x) <- result.then(y)
  x
}
"
    )
}
