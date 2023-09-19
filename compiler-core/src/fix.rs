#[cfg(test)]
mod tests;

use smol_str::SmolStr;

use camino::Utf8Path;

use crate::{
    ast::UntypedModule,
    format::{Formatter, Intermediate},
    Error, Result,
};

pub fn parse_fix_and_format(src: &SmolStr, path: &Utf8Path) -> Result<String> {
    // Parse
    let parsed = crate::parse::parse_module(src).map_err(|error| Error::Parse {
        path: path.to_path_buf(),
        src: src.clone(),
        error,
    })?;
    let intermediate = Intermediate::from_extra(&parsed.extra, src);

    // Fix
    let module = Fixer::fix(parsed.module);

    // Format
    let mut buffer = String::new();
    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, &mut buffer)?;

    Ok(buffer)
}

#[derive(Debug, Default)]
pub struct Fixer {
    prelude_module_import_alias: Option<SmolStr>,
}

impl Fixer {
    pub fn fix(module: UntypedModule) -> UntypedModule {
        let mut fixer = Self {
            prelude_module_import_alias: None,
        };
        fixer.fix_module(module)
    }

    fn fix_module(&mut self, module: UntypedModule) -> UntypedModule {
        module
    }
}
