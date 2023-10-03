use std::collections::HashMap;

use crate::{
    ast::{
        Definition, SrcSpan, TypeAst, TypeAstConstructor, UntypedExpr, UntypedImport, UntypedModule,
    },
    ast_folder::{TypeAstFolder, UntypedExprFolder, UntypedModuleFolder},
};
use smol_str::SmolStr;

#[derive(Debug, Default)]
pub struct Fixer {
    imports: HashMap<SmolStr, Imported>,
}

impl Fixer {
    pub fn fix(module: UntypedModule) -> UntypedModule {
        Self::default().fix_module(module)
    }

    fn fix_module(&mut self, module: UntypedModule) -> UntypedModule {
        for d in module.definitions.iter() {
            if let Definition::Import(i) = &d.definition {
                self.register_unqualified_imported_constructors(&i);
            }
        }

        // Determine which imported constructors are used as types and which are
        // used as values.
        let mut module = self.fold_module(module);

        for d in module.definitions.iter_mut() {
            if let Definition::Import(ref mut i) = d.definition {
                self.fix_import(i);
            }
        }

        module
    }

    fn register_unqualified_imported_constructors(&mut self, i: &UntypedImport) {
        for unqualified in &i.unqualified_values {
            let first_char = unqualified.name.chars().next().unwrap_or('a');
            if first_char.is_uppercase() {
                let data = Imported {
                    module: i.module.clone(),
                    used_as_type: false,
                    used_as_value: false,
                };
                let _ = self
                    .imports
                    .insert(unqualified.variable_name().into(), data);
            }
        }
    }

    fn fix_import(&mut self, import: &mut UntypedImport) {
        let mut types = vec![];
        let mut values = vec![];

        for unqualified in import.unqualified_values.drain(..) {
            match self.imports.get_mut(&unqualified.name) {
                Some(i) if i.module == import.module => {
                    if i.used_as_type {
                        types.push(unqualified.clone());
                    }
                    if i.used_as_value {
                        values.push(unqualified.clone());
                    }
                }
                _ => values.push(unqualified),
            }
        }

        import.unqualified_values = values;
        for t in types {
            import.unqualified_types.push(t);
        }
    }
}

impl UntypedModuleFolder for Fixer {}

impl UntypedExprFolder for Fixer {
    fn fold_var(&mut self, location: SrcSpan, name: SmolStr) -> UntypedExpr {
        if let Some(import) = self.imports.get_mut(&name) {
            import.used_as_value = true;
        }
        UntypedExpr::Var { location, name }
    }
}

impl TypeAstFolder for Fixer {
    fn fold_type_constructor(&mut self, constructor: TypeAstConstructor) -> TypeAst {
        if constructor.module.is_none() {
            if let Some(import) = self.imports.get_mut(&constructor.name) {
                import.used_as_type = true;
            }
        }

        TypeAst::Constructor(constructor)
    }
}

#[derive(Debug, Default)]
struct Imported {
    module: SmolStr,
    used_as_type: bool,
    used_as_value: bool,
}
