use crate::{
    ast::{
        visit::{self, Visit},
        SrcSpan, TypedAssignment, TypedExpr, TypedModule,
    },
    line_numbers::LineNumbers,
    type_::pretty::Printer,
};

#[derive(Debug, Eq, PartialEq)]
pub struct InlayHint {
    pub label: String,
    pub offset: u32,
}

fn is_simple_lit(expr: &TypedExpr) -> bool {
    matches!(
        expr,
        TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::BitArray { .. }
    )
}

struct InlayHintsVisitor<'a> {
    hints: Vec<InlayHint>,
    line_numbers: &'a LineNumbers,
}

impl<'a> InlayHintsVisitor<'a> {
    fn new(line_numbers: &'a LineNumbers) -> InlayHintsVisitor<'a> {
        InlayHintsVisitor {
            hints: vec![],
            line_numbers,
        }
    }
}

impl<'a, 'ast> Visit<'ast> for InlayHintsVisitor<'a> {
    fn visit_typed_expr_pipeline(
        &mut self,
        _location: &'ast SrcSpan,
        assignments: &'ast [TypedAssignment],
        finally: &'ast TypedExpr,
    ) {
        fn get_this_line(this: &InlayHintsVisitor<'_>, span: &SrcSpan) -> u32 {
            this.line_numbers.line_and_column_number(span.end).line
        }

        let mut prev_hint: Option<(u32, Option<InlayHint>)> = None;
        for assign in assignments {
            let this_line = get_this_line(self, &assign.location);

            if let Some((prev_line, prev_hint)) = prev_hint {
                if prev_line != this_line {
                    if let Some(prev_hint) = prev_hint {
                        self.hints.push(prev_hint);
                    }
                }
            };

            let this_hint = InlayHint {
                label: Printer::new().pretty_print(assign.type_().as_ref(), 0),
                offset: assign.location.end,
            };
            prev_hint = Some((
                this_line,
                if is_simple_lit(&assign.value) {
                    None
                } else {
                    Some(this_hint)
                },
            ));

            visit::visit_typed_expr(self, &assign.value);
        }

        if let Some((prev_line, prev_hint)) = prev_hint {
            let this_line = get_this_line(self, &finally.location());
            if this_line != prev_line {
                if let Some(prev_hint) = prev_hint {
                    self.hints.push(prev_hint);
                }
                self.hints.push(InlayHint {
                    label: Printer::new().pretty_print(finally.type_().as_ref(), 0),
                    offset: finally.location().end,
                });
            }
        }
    }
}

pub fn get_inlay_hints(typed_module: TypedModule, line_numbers: &LineNumbers) -> Vec<InlayHint> {
    let mut visitor = InlayHintsVisitor::new(line_numbers);
    visitor.visit_typed_module(&typed_module);
    visitor.hints
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
    fn no_hints_when_value_is_literal() {
        let src = r#"
      pub fn ret_str(f1) {
        "abc"
        |> f1()
      }

      pub fn ret_int(f2) {
        42
        |> f2()
      }

      pub fn ret_float(f3) {
        42.2
        |> f3()
      }

      pub fn ret_bit_array(f4) {
        <<1, 2>>
        |> f4()
      }
  "#;

        assert_inlay_hints(
            src,
            vec![
                InlayHint {
                    label: "a".to_string(),
                    offset: index_of_end(src, "|> f1()"),
                },
                InlayHint {
                    label: "a".to_string(),
                    offset: index_of_end(src, "|> f2()"),
                },
                InlayHint {
                    label: "a".to_string(),
                    offset: index_of_end(src, "|> f3()"),
                },
                InlayHint {
                    label: "a".to_string(),
                    offset: index_of_end(src, "|> f4()"),
                },
            ],
        );
    }

    #[test]
    fn show_many_hints() {
        let src = r#"
            const int_val = 0

            fn identity(x) {
              x
            }

            fn ret_str(_x) {
              "abc"
            }

            pub fn example_pipe() {
              int_val
              |> ret_str()
              |> identity()
            }
        "#;

        assert_inlay_hints(
            src,
            vec![
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "  int_val"),
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
            const int_val = 0

            fn identity(x) {
              x
            }

            fn main(a) {
              case a {
                _ -> {
                    int_val
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
                    offset: index_of_end(src, "  int_val"),
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
            const int_val = 0

            fn identity(x) {
              x
            }

            fn main() {
              let f = identity(fn() {
                int_val
                |> identity()
              })
            }
        "#;

        assert_inlay_hints(
            src,
            vec![
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "   int_val"),
                },
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "|> identity()"),
                },
            ],
        );
    }

    #[test]
    fn hints_in_use() {
        let src = r#"
            const int_val = 0

            fn identity(x) {
              x
            }

            fn main(f) {
              use a <- f()
              int_val
              |> identity()
            }
        "#;

        assert_inlay_hints(
            src,
            vec![
                InlayHint {
                    label: "Int".to_string(),
                    offset: index_of_end(src, "  int_val"),
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
