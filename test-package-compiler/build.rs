use std::path::PathBuf;

pub fn main() {
    let mut module = String::new();

    let cases = PathBuf::from("./cases")
        .canonicalize()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
        .join("cases");

    for entry in std::fs::read_dir(&cases).unwrap() {
        let name = entry.unwrap().file_name().into_string().unwrap();
        let path = cases.join(&name);
        let path = path.to_str().unwrap();
        module.push_str(&format!(
            r#"#[test]
fn {name}() {{
  let output = crate::prepare("{path}");
  insta::assert_snapshot!(insta::internals::AutoName, output, "{path}");
}}

"#
        ));
    }

    let out = PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("tests.rs");
    std::fs::write(out, module).unwrap();
}
