use crate::{
    format::{Formatter, Intermediate},
    Error, Result,
};
use camino::Utf8Path;
use ecow::EcoString;

pub fn parse_fix_and_format(src: &EcoString, path: &Utf8Path) -> Result<String> {
    // Parse
    let parsed = crate::parse::parse_module(src).map_err(|error| Error::Parse {
        path: path.to_path_buf(),
        src: src.clone(),
        error,
    })?;
    let intermediate = Intermediate::from_extra(&parsed.extra, src);
    let module = parsed.module;

    // Fix
    // let module = some_fixer_module::Fixer::fix(module);

    // Format
    let mut buffer = String::new();
    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, &mut buffer)?;

    Ok(buffer)
}
