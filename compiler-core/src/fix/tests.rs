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

// TODO: shadowed
// TODO: imported unqualified
// TODO: imported unqualified but used qualified
// TODO: imported qualified but used unqualified
