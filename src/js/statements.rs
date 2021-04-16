use crate::{
    ast::*, error::Error::UnsupportedFeature, line_numbers::LineNumbers, pretty::*, Result, Target,
};

pub fn statement<'a>(
    _current_module: &'a [String],
    statement: &'a TypedStatement,
    _module: &'a [String],
    _line_numbers: &'a LineNumbers,
) -> Option<Result<Document<'a>>> {
    match statement {
        Statement::TypeAlias { .. } => None,
        Statement::CustomType { .. } => None,
        Statement::Import { .. } => Some(Err(UnsupportedFeature {
            target: Target::JavaScript,
            feature: "Importing modules ".to_string(),
        })),
        Statement::ExternalType { .. } => None,
        Statement::ModuleConstant { .. } => Some(Err(UnsupportedFeature {
            target: Target::JavaScript,
            feature: "Adding a module constant ".to_string(),
        })),
        Statement::Fn { .. } => unimplemented!("TODO this PR"),
        Statement::ExternalFn { .. } => Some(Err(UnsupportedFeature {
            target: Target::JavaScript,
            feature: "Using an external function ".to_string(),
        })),
    }
}
