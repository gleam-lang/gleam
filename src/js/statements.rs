use crate::{ast::*, line_numbers::LineNumbers, pretty::*, error::Error::UnsupportedFeature, Target, Result};

pub fn statement<'a>(
    _current_module: &'a [String],
    statement: &'a TypedStatement,
    _module: &'a [String],
    _line_numbers: &'a LineNumbers,
) -> Option<Result<Document<'a>>> {
    match statement {
        Statement::TypeAlias { .. } => None,
        Statement::CustomType { .. } => None,
        Statement::Import {..} =>     Some(Err(UnsupportedFeature {
            target: Target::JavaScript,
            feature: "Importing modules ".to_string(),
        })),
        _ => unimplemented!("SSS")
    }
}
