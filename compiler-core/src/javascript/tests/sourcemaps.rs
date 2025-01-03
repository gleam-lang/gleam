use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::Config,
};
use itertools::Itertools;
use termcolor::NoColor;

use crate::javascript::tests::LineNumbers;

use super::compile_js_and_sourcemap;

#[macro_export]
macro_rules! assert_sourcemap {
    ($code:literal) => {
        let src = $code;
        let (compiled, sourcemap) =
            compile_js_and_sourcemap(src, vec![]).expect("Failed to compile js");
        let mut files = SimpleFiles::new();
        let original = files.add("original.gleam", src);
        let original_line_numbers = LineNumbers::new(src);
        let generated = files.add("generated.js", &compiled);
        let generated_line_numbers = LineNumbers::new(&compiled);
        let codespan_reporting_config = Config::default();
        let sourcemap_viz = sourcemap.tokens().map(|token| {
            let (src_line, src_col) = token.get_src();
            let src_index = original_line_numbers
                .byte_index(src_line, src_col)
                .try_into()
                .expect("to transform index to usize");
            let (dst_line, dst_col) = token.get_dst();
            let dst_index = generated_line_numbers
                .byte_index(dst_line, dst_col)
                .try_into()
                .expect("to transform index to usize");
            let mut buffer = Vec::new();
            let diag = Diagnostic::note().with_labels(vec![
                Label::primary(original, src_index..src_index).with_message("This code"),
                Label::primary(generated, dst_index..dst_index).with_message("Gets mapped to this"),
            ]);
            let mut writer = NoColor::new(&mut buffer);
            codespan_reporting::term::emit(&mut writer, &codespan_reporting_config, &files, &diag)
                .expect("Failed to write diagnostic to buffer");
            String::from_utf8(buffer).expect("Invalid UTF-8")
        });
        let sourcemap_viz: String =
            Itertools::intersperse(sourcemap_viz, "---\n".to_owned()).collect();
        insta::assert_snapshot!(insta::internals::AutoName, sourcemap_viz);
    };
}

#[test]
fn sourcemap_function_definition() {
    assert_sourcemap!(
        "fn add_2(x) {
  x + 2
}"
    );
}
