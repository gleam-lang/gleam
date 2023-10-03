mod bit_string_rename;
mod import_type;
#[cfg(test)]
mod tests;

use crate::{
    format::{Formatter, Intermediate},
    Error, Result,
};
use camino::Utf8Path;
use smol_str::SmolStr;

pub fn parse_fix_and_format(src: &SmolStr, path: &Utf8Path) -> Result<String> {
    // Parse
    let parsed = crate::parse::parse_module(src).map_err(|error| Error::Parse {
        path: path.to_path_buf(),
        src: src.clone(),
        error,
    })?;
    let intermediate = Intermediate::from_extra(&parsed.extra, src);
    let module = parsed.module;

    // Fix
    let module = bit_string_rename::Fixer::fix(module);
    let module = import_type::Fixer::fix(module);

    // Format
    let mut buffer = String::new();
    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, &mut buffer)?;

    Ok(buffer)
}
