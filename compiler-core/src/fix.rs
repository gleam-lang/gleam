#[cfg(test)]
mod tests;

use crate::{
    ast::{
        CustomType, Definition, Import, TargetedDefinition, TypeAlias, TypeAst, TypeAstConstructor,
        UntypedDefinition, UntypedFunction, UntypedModule, UntypedModuleConstant,
    },
    ast_folder::{TypeAstFolder, UntypedExprFolder},
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
        // Work out what BitString is called in this module
        for d in module.definitions.iter() {
            self.determine_names(&d.definition);
        }

        // Fix the module
        module.definitions = module
            .definitions
            .into_iter()
            .map(|d| self.fix_targeted_definition(d))
            .collect();

        module
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

                for i in &i.unqualified {
                    if i.variable_name() == "BitString" && self.bit_string_name == "BitString" {
                        self.bit_string_name = "".into();
                    }
                }
            }
        }
    }

    fn fix_targeted_definition(&mut self, mut d: TargetedDefinition) -> TargetedDefinition {
        d.definition = self.fix_definition(d.definition);
        d
    }

    fn fix_definition(&mut self, definition: UntypedDefinition) -> UntypedDefinition {
        match definition {
            Definition::Import(i) => Definition::Import(self.fix_import(i)),
            Definition::Function(f) => Definition::Function(self.fix_function(f)),
            Definition::CustomType(c) => Definition::CustomType(self.fix_custom_type(c)),
            Definition::TypeAlias(a) => Definition::TypeAlias(self.fix_alias(a)),
            Definition::ModuleConstant(c) => {
                Definition::ModuleConstant(self.fix_module_constant(c))
            }
        }
    }

    fn fix_alias(&mut self, mut alias: TypeAlias<()>) -> TypeAlias<()> {
        alias.type_ast = self.fold_type(alias.type_ast);
        alias
    }

    fn fix_custom_type(&mut self, mut c: CustomType<()>) -> CustomType<()> {
        c.constructors = c
            .constructors
            .into_iter()
            .map(|mut c| {
                c.arguments = c
                    .arguments
                    .into_iter()
                    .map(|mut a| {
                        a.ast = self.fold_type(a.ast);
                        a
                    })
                    .collect();
                c
            })
            .collect();
        c
    }

    fn fix_import(&self, mut i: Import<()>) -> Import<()> {
        if i.module != "gleam" {
            return i;
        }

        i.unqualified = i
            .unqualified
            .into_iter()
            .map(|mut i| {
                if i.name == "BitString" {
                    i.name = "BitArray".into();
                }
                i
            })
            .collect();

        i
    }

    fn fix_function(&mut self, mut f: UntypedFunction) -> UntypedFunction {
        f.return_annotation = f.return_annotation.map(|t| self.fold_type(t));
        f.arguments = f
            .arguments
            .into_iter()
            .map(|mut a| {
                a.annotation = a.annotation.map(|t| self.fold_type(t));
                a
            })
            .collect();
        f.body = f.body.mapped(|e| self.fold_statement(e));
        f
    }

    fn fix_module_constant(&mut self, mut c: UntypedModuleConstant) -> UntypedModuleConstant {
        c.annotation = c.annotation.map(|t| self.fold_type(t));
        c
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
