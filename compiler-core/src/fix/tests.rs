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
