use super::*;

use camino::Utf8Path;

fn fix(src: &str) -> String {
    parse_fix_and_format(&src.into(), Utf8Path::new("test")).unwrap()
}

#[test]
fn empty() {
    assert_eq!(fix(""), "\n")
}
