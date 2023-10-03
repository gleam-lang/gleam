#[cfg(test)]
mod tests;

use crate::{
    ast::{
        CustomType, Definition, Import, TypeAlias, TypeAst, TypeAstConstructor, UntypedDefinition,
        UntypedImport, UntypedModule,
    },
    ast_folder::{TypeAstFolder, UntypedExprFolder, UntypedModuleFolder},
    build::Target,
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

    fn fix_module(&mut self, module: UntypedModule) -> UntypedModule {
        // Work out what BitString is called in this module
        for d in module.definitions.iter() {
            self.determine_names(&d.definition);
        }

        // Fix the module
        self.fold_module(module)
    }

    fn determine_names(&mut self, d: &UntypedDefinition) {
        match d {
            Definition::Function(_) | Definition::ModuleConstant(_) => (),

            Definition::CustomType(CustomType { name, .. })
            | Definition::TypeAlias(TypeAlias { alias: name, .. }) => {
                if name == "BitString" && self.bit_string_name == "BitString" {
                    self.bit_string_name = "".into();
                }
            }

            Definition::Import(i) => {
                if i.module == "gleam" {
                    self.prelude_module_import_alias = Some(i.used_name());
                }

                for i in &i.unqualified_values {
                    if i.variable_name() == "BitString" && self.bit_string_name == "BitString" {
                        self.bit_string_name = "".into();
                    }
                }
            }
        }
    }
}

impl UntypedModuleFolder for Fixer {
    fn fold_import(&mut self, mut i: UntypedImport, _target: Option<Target>) -> Import<()> {
        if i.module == "gleam" {
            i.unqualified_values = i
                .unqualified_values
                .into_iter()
                .map(|mut i| {
                    if i.name == "BitString" {
                        i.name = "BitArray".into();
                    }
                    i
                })
                .collect();
        }
        i
    }
}

impl UntypedExprFolder for Fixer {}

impl TypeAstFolder for Fixer {
    fn fold_type_constructor(&mut self, mut constructor: TypeAstConstructor) -> TypeAst {
        if constructor.name == self.bit_string_name {
            constructor.name = self.bit_array_name.clone();
        }

        TypeAst::Constructor(constructor)
    }
}
