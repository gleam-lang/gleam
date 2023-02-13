use super::*;

fn fix(src: &str) -> String {
    parse_fix_and_format(&src.into(), &Path::new("test")).unwrap()
}

#[test]
fn empty() {
    assert_eq!("", fix(""))
}
