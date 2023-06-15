#[cfg(test)]
mod tests;

use std::path::Path;

use smol_str::SmolStr;

use crate::{
    ast::{Definition, TargettedDefinition, UntypedDefinition, UntypedModule},
    format::{Formatter, Intermediate},
    Error, Result,
};

pub fn parse_fix_and_format(src: &SmolStr, path: &Path) -> Result<String> {
    // Parse
    let parsed = crate::parse::parse_module(src).map_err(|error| Error::Parse {
        path: path.to_path_buf(),
        src: src.clone(),
        error,
    })?;
    let mut module = parsed.module;
    let intermediate = Intermediate::from_extra(&parsed.extra, src);

    // Fix
    Fixer::fix(&mut module);

    // Format
    let mut buffer = String::new();
    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, &mut buffer)?;

    Ok(buffer)
}

#[derive(Debug, Clone, Copy)]
pub struct Fixer {}

impl Fixer {
    pub fn fix(module: &mut UntypedModule) {
        Self {}.fix_module(module)
    }

    fn fix_module(&mut self, module: &mut UntypedModule) {
        for group in module.definitions.iter_mut() {
            match group {
                TargettedDefinition::Any(definition) | TargettedDefinition::Only(_, definition) => {
                    self.fix_definition(definition);
                }
            }
        }
    }

    fn fix_definition(&mut self, definition: &mut UntypedDefinition) {
        match definition {
            Definition::Function(_)
            | Definition::TypeAlias(_)
            | Definition::CustomType(_)
            | Definition::ExternalFunction(_)
            | Definition::Import(_)
            | Definition::ModuleConstant(_) => (),
        }
    }
}
