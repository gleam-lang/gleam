use crate::{
    ast::{
        Definition, SrcSpan, Statement, TypedDefinition, TypedExpr, TypedModule, TypedStatement,
    },
    line_numbers::LineNumbers,
    type_::pretty::Printer,
};

#[derive(Debug, Eq, PartialEq)]
pub struct InlayHint {
    pub label: String,
    pub offset: u32,
}

struct Buf<'a> {
    hints: Vec<InlayHint>,
    line_numbers: &'a LineNumbers,
}

impl<'a> Buf<'a> {
    fn new(line_numbers: &'a LineNumbers) -> Buf<'a> {
        Buf {
            hints: vec![],
            line_numbers,
        }
    }
}

impl<'a> Buf<'a> {
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
            Statement::Assignment(assig) => self.get_inlay_hints_expr(*assig.value),
            // TODO
            Statement::Use(_) => (),
        }
    }

    fn get_inlay_hints_expr(&mut self, typed_st: TypedExpr) {
        match typed_st {
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::ModuleSelect { .. } => (),

            TypedExpr::Pipeline {
                location: _,
                assignments,
                finally,
            } => {
                fn get_this_line(this: &Buf<'_>, span: &SrcSpan) -> u32 {
                    this.line_numbers.line_and_column_number(span.end).line
                }

                let mut prev_hint = None;

                for assign in assignments {
                    let str_type = Printer::new().pretty_print(assign.type_().as_ref(), 0);

                    let this_line = get_this_line(self, &assign.location);

                    if let Some((prev_line, prev_hint)) = prev_hint {
                        if prev_line != this_line {
                            self.hints.push(prev_hint);
                        }
                    };

                    let this_hint = InlayHint {
                        label: str_type,
                        offset: assign.location.end,
                    };
                    prev_hint = Some((this_line, this_hint));
                    self.get_inlay_hints_expr(*assign.value);
                }

                let (prev_line, prev_hint) = prev_hint.expect("Expected a non-empty arr");
                let this_line = get_this_line(self, &finally.location());
                if this_line != prev_line {
                    let str_type_finally = Printer::new().pretty_print(finally.type_().as_ref(), 0);
                    self.hints.push(prev_hint);
                    self.hints.push(InlayHint {
                        label: str_type_finally,
                        offset: finally.location().end,
                    })
                }

                self.get_inlay_hints_expr(*finally);
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

            TypedExpr::Call { fun: _, args, .. } => {
                // TODO should we iterate the caller?
                for arg in args {
                    self.get_inlay_hints_expr(arg.value);
                }
            }

            TypedExpr::Fn { body, .. } => {
                for st in body {
                    self.get_inlay_hints_statement(st);
                }
            }

            TypedExpr::List { elements, tail, .. } => {
                for el in elements {
                    self.get_inlay_hints_expr(el);
                }
                if let Some(tail) = tail {
                    self.get_inlay_hints_expr(*tail);
                }
            }

            TypedExpr::BinOp { left, right, .. } => {
                self.get_inlay_hints_expr(*left);
                self.get_inlay_hints_expr(*right);
            }

            TypedExpr::RecordAccess { record, .. } => {
                self.get_inlay_hints_expr(*record);
            }

            TypedExpr::Tuple { elems, .. } => {
                for el in elems {
                    self.get_inlay_hints_expr(el);
                }
            }

            TypedExpr::TupleIndex { tuple, .. } => {
                self.get_inlay_hints_expr(*tuple);
            }

            TypedExpr::RecordUpdate { spread, args, .. } => {
                for arg in args {
                    self.get_inlay_hints_expr(arg.value);
                }
                self.get_inlay_hints_expr(*spread);
            }

            TypedExpr::NegateBool { value, .. } => {
                self.get_inlay_hints_expr(*value);
            }

            TypedExpr::NegateInt { value, .. } => {
                self.get_inlay_hints_expr(*value);
            }
        }
    }
}

pub fn get_inlay_hints(typed_module: TypedModule, line_numbers: &LineNumbers) -> Vec<InlayHint> {
    let mut buf = Buf::new(line_numbers);
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

        assert_inlay_hints(src, vec![]);
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

        assert_inlay_hints(
            src,
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
                },
            ],
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

        assert_inlay_hints(
            src,
            vec![
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "0"),
                },
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "|> identity()"),
                },
            ],
        );
    }

    #[test]
    fn hints_nested_for_apply_fn_let() {
        let src = r#"
            fn identity(x) {
              x
            }

            fn main() {
              let f = identity(fn() {
                0
                |> identity()
              })
            }
        "#;

        assert_inlay_hints(
            src,
            vec![
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "0"),
                },
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "|> identity()"),
                },
            ],
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

    // TODO dedup this function
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

    fn assert_inlay_hints(src: &str, expected_inlay_hints: Vec<InlayHint>) {
        let typed_module = compile_module(src);
        let line_numbers = LineNumbers::new(src);
        let inlay_hints = get_inlay_hints(typed_module, &line_numbers);

        assert_eq!(inlay_hints, expected_inlay_hints);
    }
}
