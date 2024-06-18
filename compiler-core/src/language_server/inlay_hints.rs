use crate::{
    ast::{Definition, Statement, TypedDefinition, TypedExpr, TypedModule, TypedStatement},
    type_::pretty::Printer,
};

#[derive(Debug, Eq, PartialEq)]
pub struct InlayHint {
    pub label: String,
    pub offset: u32,
}

#[derive(Default)]
struct Buf {
    hints: Vec<InlayHint>,
}

impl Buf {
    pub fn get_inlay_hints_definition(&mut self, typed_def: TypedDefinition) {
        match typed_def {
            Definition::Function(f) => {
                for st in f.body {
                    self.get_inlay_hints_statement(st)
                }
            }
            _ => (),
        }
    }

    fn get_inlay_hints_statement(&mut self, typed_st: TypedStatement) {
        match typed_st {
            Statement::Expression(e) => self.get_inlay_hints_expr(e),
            Statement::Assignment(_) => todo!(),
            Statement::Use(_) => todo!(),
        }
    }

    fn get_inlay_hints_expr(&mut self, typed_st: TypedExpr) {
        match typed_st {
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Var { .. } => (),

            TypedExpr::Pipeline {
                location: _,
                assignments,
                finally,
            } => {
                for assign in assignments {
                    let str_type = Printer::new().pretty_print(assign.type_().as_ref(), 0);

                    self.hints.push(InlayHint {
                        label: str_type,
                        offset: assign.location.end,
                    })
                }

                let str_type_finally = Printer::new().pretty_print(finally.type_().as_ref(), 0);
                self.hints.push(InlayHint {
                    label: str_type_finally,
                    offset: finally.location().end,
                })
            }

            TypedExpr::Case { clauses, .. } => {
                // TODO iterate on clauses?
                for clause in clauses {
                    self.get_inlay_hints_expr(clause.then)
                }
            }

            TypedExpr::Block { statements, .. } => {
                for st in statements {
                    self.get_inlay_hints_statement(st);
                }
            }
            // TODO
            _ => (),
        }
    }
}

pub fn get_inlay_hints(typed_module: TypedModule) -> Vec<InlayHint> {
    let mut buf = Buf::default();
    for def in typed_module.definitions {
        buf.get_inlay_hints_definition(def);
    }
    buf.hints
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analyse::TargetSupport,
        ast::TypedModule,
        build::Target,
        config::PackageConfig,
        line_numbers::LineNumbers,
        type_::{build_prelude, PRELUDE_MODULE_NAME},
        uid::UniqueIdGenerator,
        warning::TypeWarningEmitter,
    };

    #[ignore]
    #[test]
    fn no_hints_when_same_line() {
        let src = r#"
      fn identity(x) {
        x
      }

      fn ret_str(_x) {
        "abc"
      }

      pub fn example_pipe() {
        0 |> ret_str() |> identity()
      }
  "#;

        let typed_module = compile_module(src);
        let inlay_hints = get_inlay_hints(typed_module);

        assert_eq!(inlay_hints, vec![]);
    }

    #[test]
    fn show_many_hints() {
        let src = r#"
            fn identity(x) {
              x
            }

            fn ret_str(_x) {
              "abc"
            }

            pub fn example_pipe() {
              0
              |> ret_str()
              |> identity()
            }
        "#;

        let typed_module = compile_module(src);
        let inlay_hints = get_inlay_hints(typed_module);

        assert_eq!(inlay_hints.len(), 3);
        assert_eq!(
            inlay_hints,
            vec![
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "0"),
                },
                InlayHint {
                    label: "String".to_string(),
                    offset: index_of_end(src, "|> ret_str()"),
                },
                InlayHint {
                    label: "String".to_string(),
                    offset: index_of_end(src, "|> identity()"),
                }
            ]
        );
    }

    #[test]
    fn hints_nested_in_case_block() {
        let src = r#"
            fn identity(x) {
              x
            }

            fn main(a) {
              case a {
                _ -> {
                    0
                    |> identity()
                }
              }
            }
        "#;

        let typed_module = compile_module(src);
        let inlay_hints = get_inlay_hints(typed_module);

        assert_eq!(inlay_hints.len(), 2);
        assert_eq!(
            inlay_hints,
            vec![
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "0"),
                },
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "|> identity()"),
                },
            ]
        );
    }

    #[test]
    fn test_index_of() {
        let src = r#"a[Z]c"#;
        assert_eq!(index_of_end(src, "[Z]"), 4);
    }

    fn index_of_end(src: &str, search_for: &str) -> u32 {
        let lookup = src.find(search_for).expect("Expected to find lookup");
        (lookup + search_for.len()) as u32
    }

    fn compile_module(src: &str) -> TypedModule {
        let parsed = crate::parse::parse_module(src).expect("syntax error");
        let ast = parsed.module;
        let ids = UniqueIdGenerator::new();
        let mut config = PackageConfig::default();
        config.name = "thepackage".into();
        let mut modules = im::HashMap::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert(PRELUDE_MODULE_NAME.into(), build_prelude(&ids));
        let line_numbers = LineNumbers::new(src);
        let mut config = PackageConfig::default();
        config.name = "thepackage".into();

        crate::analyse::ModuleAnalyzerConstructor::<()> {
            target: Target::Erlang,
            ids: &ids,
            origin: crate::build::Origin::Src,
            importable_modules: &modules,
            warnings: &TypeWarningEmitter::null(),
            direct_dependencies: &std::collections::HashMap::new(),
            target_support: TargetSupport::Enforced,
            package_config: &config,
        }
        .infer_module(ast, line_numbers, "".into())
        .expect("should successfully infer")
    }
}
