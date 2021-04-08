use super::*;
use crate::{config::PackageConfig, fs::OutputFile, project::Input};
use std::path::PathBuf;

#[test]
fn module_docs_test() {
    let src = r#"
//// module comment

/// A constant value
pub const hello = "test"

/// doc comment
// regular comment
pub fn public_fun(x: Int) -> Int {
  x
}

pub fn implicit_return() {
  "testing"
}

fn private_fun() {
  1
}

pub fn complicated_fun(
  over thing: a,
  from initial: b,
  with fun: fn(a, b) -> b,
) -> b {
  fun(thing, initial)
}
  "#;

    let input = Input {
        origin: ModuleOrigin::Src,
        path: PathBuf::from("/src/test.gleam"),
        source_base_path: PathBuf::from("/src"),
        src: src.to_string(),
    };

    let config = PackageConfig {
        name: "test".to_string(),
        docs: Default::default(),
        tool: Default::default(),
        version: Default::default(),
        repository: Default::default(),
        description: Default::default(),
        dependencies: Default::default(),
        otp_start_module: None,
    };

    let mut analysed = crate::project::analysed(vec![input]).expect("Compilation failed");
    analysed
        .iter_mut()
        .for_each(|a| a.attach_doc_and_module_comments());

    let output_files = generate_html(
        PathBuf::from("."),
        &config,
        &analysed,
        &[],
        &PathBuf::from("/docs"),
    );
    let module_page = output_files
        .iter()
        .find(|page| page.path == PathBuf::from("/docs/test/index.html"))
        .expect("Missing docs page");

    // Comments
    module_page.should_contain("module comment");
    module_page.should_contain("doc comment");
    module_page.should_not_contain("regular comment");

    // Constants
    module_page.should_contain("pub const hello: String = &quot;test&quot;");
    module_page.should_contain("A constant value");

    // Functions
    module_page.should_contain("pub fn public_fun(x: Int) -&gt; Int");
    module_page.should_contain("pub fn implicit_return() -&gt; String");
    module_page.should_not_contain("private_fun()");

    module_page.should_contain(
        "pub fn complicated_fun(
  over thing: a,
  from initial: b,
  with fun: fn(a, b) -&gt; b,
) -&gt; b",
    );
}

impl OutputFile {
    fn should_contain(&self, text: &str) {
        assert!(
            self.text.contains(&text.to_string()),
            "Generated docs page did not contain: `{}`",
            text
        );
    }

    fn should_not_contain(&self, text: &str) {
        assert!(
            !self.text.contains(&text.to_string()),
            "Generated docs page was not supposed to contain: `{}`",
            text
        );
    }
}
