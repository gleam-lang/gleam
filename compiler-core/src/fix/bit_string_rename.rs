use crate::{
    ast::{
        CallArg, Constant, CustomType, Definition, Import, SrcSpan, TypeAlias, TypeAst,
        TypeAstConstructor, UntypedConstant, UntypedDefinition, UntypedImport, UntypedModule,
    },
    ast_folder::{
        PatternFolder, TypeAstFolder, UntypedConstantFolder, UntypedExprFolder, UntypedModuleFolder,
    },
    build::Target,
};
use ecow::EcoString;

#[derive(Debug, Default)]
pub struct Fixer {
    prelude_module_import_alias: Option<EcoString>,
    bit_string_name: EcoString,
    bit_array_name: EcoString,
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
                    self.prelude_module_import_alias = i.used_name();
                } else {
                    for i in i.unqualified_values.iter().chain(&i.unqualified_types) {
                        if i.used_name() == "BitString" && self.bit_string_name == "BitString" {
                            self.bit_string_name = "".into();
                        }
                    }
                }
            }
        }
    }
}

impl UntypedModuleFolder for Fixer {
    fn fold_import(&mut self, mut i: UntypedImport, _target: Option<Target>) -> Import<()> {
        if i.module == "gleam" {
            i.unqualified_values
                .iter_mut()
                .chain(&mut i.unqualified_types)
                .filter(|i| i.name == "BitString")
                .for_each(|i| i.name = "BitArray".into());
        }
        i
    }
}

impl UntypedConstantFolder for Fixer {
    fn fold_constant_record(
        &mut self,
        location: SrcSpan,
        module: Option<EcoString>,
        name: EcoString,
        args: Vec<CallArg<UntypedConstant>>,
    ) -> UntypedConstant {
        let name = if name == self.bit_string_name {
            self.bit_array_name.clone()
        } else {
            name
        };
        Constant::Record {
            location,
            module,
            name,
            args,
            tag: (),
            typ: (),
            field_map: None,
        }
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

impl PatternFolder for Fixer {}
