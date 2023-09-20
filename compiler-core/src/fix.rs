#[cfg(test)]
mod tests;

use crate::{
    ast::{
        Definition, TargetedDefinition, TypeAlias, TypeAst, TypeAstConstructor, UntypedDefinition,
        UntypedModule,
    },
    ast_folder::TypeAstFolder,
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
    bit_string_name: SmolStr,
    bit_array_name: SmolStr,
}

impl Fixer {
    pub fn fix(module: UntypedModule) -> UntypedModule {
        let mut fixer = Self {
            prelude_module_import_alias: None,
            bit_string_name: "BitString".into(),
            bit_array_name: "BitArray".into(),
        };
        fixer.fix_module(module)
    }

    fn fix_module(&mut self, mut module: UntypedModule) -> UntypedModule {
        // Determine what name the prelude has been imported under, if any
        for d in module.definitions.iter() {
            if let Definition::Import(i) = &d.definition {
                if i.module == "gleam" {
                    self.prelude_module_import_alias = Some(i.used_name());
                    break;
                }
            }
        }

        module.definitions = module
            .definitions
            .into_iter()
            .map(|d| self.fix_targetted_definition(d))
            .collect();

        module
    }

    fn fix_targetted_definition(&mut self, mut d: TargetedDefinition) -> TargetedDefinition {
        d.definition = self.fix_definition(d.definition);
        d
    }

    fn fix_definition(&mut self, definition: UntypedDefinition) -> UntypedDefinition {
        match definition {
            Definition::Import(_) => todo!(),
            Definition::Function(_) => todo!(),
            Definition::CustomType(_) => todo!(),
            Definition::TypeAlias(a) => Definition::TypeAlias(self.fix_alias(a)),
            Definition::ModuleConstant(_) => todo!(),
        }
    }

    fn fix_alias(&mut self, mut alias: TypeAlias<()>) -> TypeAlias<()> {
        alias.type_ast = self.fold(alias.type_ast);
        alias
    }
}

impl TypeAstFolder for Fixer {
    fn fold_constructor(&mut self, mut constructor: TypeAstConstructor) -> TypeAst {
        if constructor.name == self.bit_string_name {
            constructor.name = self.bit_array_name.clone();
        }

        TypeAst::Constructor(constructor)
    }
}
