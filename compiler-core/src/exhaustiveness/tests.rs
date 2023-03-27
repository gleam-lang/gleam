use super::*;

fn new_type(compiler: &mut Compiler, typ: Type) -> TypeId {
    let id = compiler.types.len();

    compiler.types.push(typ);
    TypeId(id)
}

fn tt() -> Pattern {
    Pattern::Constructor(Constructor::True, Vec::new())
}

fn ff() -> Pattern {
    Pattern::Constructor(Constructor::False, Vec::new())
}

fn bind(name: &str) -> Pattern {
    Pattern::Variable(name.into())
}

fn discard() -> Pattern {
    Pattern::Discard
}

fn variant(typ: TypeId, index: usize, args: Vec<Pattern>) -> Pattern {
    Pattern::Constructor(Constructor::Variant(typ, index), args)
}

fn pair(typ1: TypeId, typ2: TypeId, pat1: Pattern, pat2: Pattern) -> Pattern {
    Pattern::Constructor(Constructor::Tuple(vec![typ1, typ2]), vec![pat1, pat2])
}

fn tuple(args: Vec<(Pattern, TypeId)>) -> Pattern {
    let types = args.iter().map(|(_, t)| *t).collect();
    let patterns = args.into_iter().map(|(p, _)| p).collect();
    Pattern::Constructor(Constructor::Tuple(types), patterns)
}

fn int(val: &str) -> Pattern {
    Pattern::Int(val.into())
}

fn rhs(value: u16) -> Body {
    Body {
        bindings: Vec::new(),
        clause_index: value,
    }
}

fn var(id: usize, type_id: TypeId) -> Variable {
    Variable { id, type_id }
}

fn compile(compiler: Compiler, input: Variable, rules: Vec<(Pattern, Body)>) -> Match {
    let rows = rules
        .into_iter()
        .map(|(pat, body)| Row::new(vec![Column::new(input, pat)], None, body))
        .collect();

    compiler.compile(rows)
}

fn failure() -> Decision {
    Decision::Failure
}

fn success(value: u16) -> Decision {
    Decision::Success(Body {
        bindings: Vec::new(),
        clause_index: value,
    })
}

fn success_with_bindings(bindings: Vec<(&str, Variable)>, value: u16) -> Decision {
    Decision::Success(Body {
        bindings: bindings.into_iter().map(|(n, v)| (n.into(), v)).collect(),
        clause_index: value,
    })
}

#[test]
fn test_move_variable_patterns() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Boolean);
    let var1 = compiler.new_variable(typ);
    let var2 = compiler.new_variable(typ);
    let cons = Constructor::True;
    let case = compiler.move_unconditional_patterns(Row {
        columns: vec![
            Column::new(var2, bind("a")),
            Column::new(var1, Pattern::Constructor(cons.clone(), Vec::new())),
        ],
        guard: None,
        body: Body {
            bindings: Vec::new(),
            clause_index: 42,
        },
    });

    assert_eq!(
        case,
        Row {
            columns: vec![Column::new(var1, Pattern::Constructor(cons, Vec::new()))],
            guard: None,
            body: Body {
                bindings: vec![("a".into(), var2)],
                clause_index: 42
            }
        }
    );
}

#[test]
fn test_move_variable_patterns_without_constructor_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Boolean);
    let var1 = compiler.new_variable(typ);
    let case = compiler.move_unconditional_patterns(Row {
        columns: vec![Column::new(var1, bind("a"))],
        guard: None,
        body: Body {
            bindings: Vec::new(),
            clause_index: 42,
        },
    });

    assert_eq!(
        case,
        Row {
            columns: Vec::new(),
            guard: None,
            body: Body {
                bindings: vec![("a".into(), var1)],
                clause_index: 42
            }
        }
    );
}

#[test]
fn test_branch_variable() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Boolean);
    let var1 = compiler.new_variable(typ);
    let var2 = compiler.new_variable(typ);
    let rows = vec![
        Row::new(
            vec![
                Column::new(var1, Pattern::Int("42".into())),
                Column::new(var2, Pattern::Int("50".into())),
            ],
            None,
            rhs(1),
        ),
        Row::new(
            vec![Column::new(var2, Pattern::Int("42".into()))],
            None,
            rhs(2),
        ),
    ];

    let branch = compiler.branch_variable(&rows[0], &rows);

    assert_eq!(branch, var2);
}

#[test]
fn test_compile_simple_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Boolean);
    let input = compiler.new_variable(typ);
    let result = compile(compiler, input, vec![(tt(), rhs(1)), (ff(), rhs(2))]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::False, Vec::new(), success(2)),
                Case::new(Constructor::True, Vec::new(), success(1)),
            ],
            None
        )
    );
}

#[test]
fn test_compile_nonexhaustive_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Boolean);
    let input = compiler.new_variable(typ);
    let result = compile(compiler, input, vec![(tt(), rhs(1))]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::False, Vec::new(), failure()),
                Case::new(Constructor::True, Vec::new(), success(1)),
            ],
            None
        )
    );
    assert!(result.diagnostics.missing);
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("false")]);
}

#[test]
fn test_compile_redundant_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Boolean);
    let input = compiler.new_variable(typ);
    let result = compile(
        compiler,
        input,
        vec![(tt(), rhs(1)), (tt(), rhs(2)), (ff(), rhs(3))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::False, Vec::new(), success(3)),
                Case::new(Constructor::True, Vec::new(), success(1)),
            ],
            None
        )
    );
    assert_eq!(result.diagnostics.reachable, vec![3, 1]);
}

#[test]
fn test_compile_redundant_int() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(typ);
    let result = compile(
        compiler,
        input,
        vec![
            (int("1"), rhs(1)),
            (int("1"), rhs(2)),
            (int("2"), rhs(3)),
            (bind("a"), rhs(4)),
        ],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::Int("1".into()), Vec::new(), success(1)),
                Case::new(Constructor::Int("2".into()), Vec::new(), success(3)),
            ],
            Some(Box::new(success_with_bindings(vec![("a", input)], 4)))
        )
    );
    assert_eq!(result.diagnostics.reachable, vec![1, 3, 4]);
}

#[test]
fn test_compile_variable_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Boolean);
    let input = compiler.new_variable(typ);
    let result = compile(compiler, input, vec![(tt(), rhs(1)), (bind("a"), rhs(2))]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::False,
                    Vec::new(),
                    success_with_bindings(vec![("a", input)], 2)
                ),
                Case::new(Constructor::True, Vec::new(), success(1)),
            ],
            None
        )
    );
}

#[test]
fn test_compile_nonexhaustive_int_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(int_type);
    let result = compile(
        compiler,
        input,
        vec![(int("4"), rhs(1)), (int("5"), rhs(2))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::Int("4".into()), Vec::new(), success(1)),
                Case::new(Constructor::Int("5".into()), Vec::new(), success(2)),
            ],
            Some(Box::new(failure()))
        )
    );
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("_")]);
}

#[test]
fn test_compile_exhaustive_int_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(int_type);
    let result = compile(
        compiler,
        input,
        vec![(int("4"), rhs(1)), (int("5"), rhs(2)), (bind("a"), rhs(3))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::Int("4".into()), Vec::new(), success(1)),
                Case::new(Constructor::Int("5".into()), Vec::new(), success(2)),
            ],
            Some(Box::new(success_with_bindings(vec![("a", input)], 3)))
        )
    );
}

#[test]
fn test_compile_nonexhaustive_nested_int_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let tup_type = new_type(&mut compiler, Type::Tuple(vec![int_type, int_type]));
    let input = compiler.new_variable(tup_type);
    let result = compile(
        compiler,
        input,
        vec![(pair(int_type, int_type, int("4"), bind("a")), rhs(1))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Tuple(vec![int_type, int_type]),
                vec![var(1, int_type), var(2, int_type),],
                Decision::Switch(
                    var(1, int_type),
                    vec![Case::new(
                        Constructor::Int("4".into()),
                        Vec::new(),
                        success_with_bindings(vec![("a", var(2, int_type))], 1)
                    )],
                    Some(Box::new(failure()))
                )
            )],
            None
        )
    );
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("#(_, _)")]);
}

#[test]
fn test_compile_exhaustive_empty_tuple_pattern() {
    let mut compiler = Compiler::new();
    let tup_type = new_type(&mut compiler, Type::Tuple(vec![]));
    let input = compiler.new_variable(tup_type);
    let result = compile(compiler, input, vec![(tuple(vec![]), rhs(1))]);
    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(Constructor::Tuple(vec![]), vec![], success(1))],
            None
        )
    );
}

#[test]
fn test_compile_exhaustive_three_tuple_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let tup_type = new_type(
        &mut compiler,
        Type::Tuple(vec![int_type, int_type, int_type]),
    );
    let input = compiler.new_variable(tup_type);
    let result = compile(
        compiler,
        input,
        vec![(
            tuple(vec![
                (discard(), int_type),
                (discard(), int_type),
                (discard(), int_type),
            ]),
            rhs(1),
        )],
    );
    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Tuple(vec![int_type, int_type, int_type]),
                vec![var(1, int_type), var(2, int_type), var(3, int_type),],
                success(1)
            )],
            None
        )
    );
}

#[test]
fn test_compile_exhaustive_nested_int_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let tup_type = new_type(&mut compiler, Type::Tuple(vec![int_type, int_type]));
    let input = compiler.new_variable(tup_type);
    let result = compile(
        compiler,
        input,
        vec![
            (pair(int_type, int_type, int("4"), int("5")), rhs(1)),
            (pair(int_type, int_type, bind("a"), bind("b")), rhs(2)),
        ],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Tuple(vec![int_type, int_type]),
                vec![var(1, int_type), var(2, int_type)],
                Decision::Switch(
                    var(2, int_type),
                    vec![Case::new(
                        Constructor::Int("5".into()),
                        Vec::new(),
                        Decision::Switch(
                            var(1, int_type),
                            vec![Case::new(
                                Constructor::Int("4".into()),
                                Vec::new(),
                                success(1)
                            )],
                            Some(Box::new(success_with_bindings(
                                vec![("a", var(1, int_type)), ("b", var(2, int_type))],
                                2
                            )))
                        )
                    )],
                    Some(Box::new(success_with_bindings(
                        vec![("a", var(1, int_type)), ("b", var(2, int_type))],
                        2
                    )))
                )
            )],
            None
        )
    );
}

#[test]
fn test_compile_nonexhaustive_option_type() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let option_type = new_type(
        &mut compiler,
        Type::Enum(vec![
            ("Some".into(), vec![int_type]),
            ("None".into(), Vec::new()),
        ]),
    );
    let input = compiler.new_variable(option_type);
    let result = compile(
        compiler,
        input,
        vec![(
            variant(option_type, 0, vec![Pattern::Int("4".into())]),
            rhs(1),
        )],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var(1, int_type)],
                    Decision::Switch(
                        var(1, int_type),
                        vec![Case::new(
                            Constructor::Int("4".into()),
                            Vec::new(),
                            success(1)
                        )],
                        Some(Box::new(failure()))
                    )
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), failure())
            ],
            None,
        )
    );
    assert_eq!(
        result.missing_patterns(),
        vec![SmolStr::new("None"), SmolStr::new("Some(_)")]
    );
}

#[test]
fn test_compile_nonexhaustive_option_type_with_multiple_arguments() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let option_type = new_type(
        &mut compiler,
        Type::Enum(vec![
            ("Some".into(), vec![int_type, int_type]),
            ("None".into(), Vec::new()),
        ]),
    );
    let input = compiler.new_variable(option_type);
    let result = compile(
        compiler,
        input,
        vec![(
            variant(
                option_type,
                0,
                vec![Pattern::Int("4".into()), Pattern::Int("5".into())],
            ),
            rhs(1),
        )],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var(1, int_type), var(2, int_type)],
                    Decision::Switch(
                        var(2, int_type),
                        vec![Case::new(
                            Constructor::Int("5".into()),
                            Vec::new(),
                            Decision::Switch(
                                var(1, int_type),
                                vec![Case::new(
                                    Constructor::Int("4".into()),
                                    Vec::new(),
                                    success(1)
                                )],
                                Some(Box::new(failure()))
                            )
                        )],
                        Some(Box::new(failure()))
                    )
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), failure())
            ],
            None
        )
    );
    assert_eq!(
        result.missing_patterns(),
        vec![SmolStr::new("None"), SmolStr::new("Some(_, _)")]
    );
}

#[test]
fn test_compile_exhaustive_option_type() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let option_type = new_type(
        &mut compiler,
        Type::Enum(vec![
            ("Some".into(), vec![int_type]),
            ("None".into(), Vec::new()),
        ]),
    );
    let input = compiler.new_variable(option_type);
    let result = compile(
        compiler,
        input,
        vec![
            (
                variant(option_type, 0, vec![Pattern::Int("4".into())]),
                rhs(1),
            ),
            (variant(option_type, 0, vec![bind("a")]), rhs(2)),
            (variant(option_type, 1, Vec::new()), rhs(3)),
        ],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var(1, int_type)],
                    Decision::Switch(
                        var(1, int_type),
                        vec![Case::new(
                            Constructor::Int("4".into()),
                            Vec::new(),
                            success(1)
                        )],
                        Some(Box::new(success_with_bindings(
                            vec![("a", var(1, int_type))],
                            2
                        )))
                    )
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), success(3))
            ],
            None
        )
    );
}

#[test]
fn test_compile_redundant_option_type_with_bool() {
    let mut compiler = Compiler::new();
    let bool_type = new_type(&mut compiler, Type::Boolean);
    let option_type = new_type(
        &mut compiler,
        Type::Enum(vec![
            ("Some".into(), vec![bool_type]),
            ("None".into(), Vec::new()),
        ]),
    );
    let input = compiler.new_variable(option_type);
    let result = compile(
        compiler,
        input,
        vec![
            (variant(option_type, 0, vec![tt()]), rhs(1)),
            (variant(option_type, 0, vec![tt()]), rhs(10)),
            (variant(option_type, 0, vec![bind("a")]), rhs(2)),
            (variant(option_type, 1, Vec::new()), rhs(3)),
        ],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var(1, bool_type)],
                    Decision::Switch(
                        var(1, bool_type),
                        vec![
                            Case::new(
                                Constructor::False,
                                Vec::new(),
                                success_with_bindings(vec![("a", var(1, bool_type))], 2)
                            ),
                            Case::new(Constructor::True, Vec::new(), success(1))
                        ],
                        None
                    )
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), success(3))
            ],
            None
        )
    );

    assert_eq!(result.diagnostics.reachable, vec![2, 1, 3]);
}

#[test]
fn test_compile_redundant_option_type_with_int() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let option_type = new_type(
        &mut compiler,
        Type::Enum(vec![
            ("Some".into(), vec![int_type]),
            ("None".into(), Vec::new()),
        ]),
    );
    let input = compiler.new_variable(option_type);
    let result = compile(
        compiler,
        input,
        vec![
            (
                variant(option_type, 0, vec![Pattern::Int("4".into())]),
                rhs(1),
            ),
            (
                variant(option_type, 0, vec![Pattern::Int("4".into())]),
                rhs(10),
            ),
            (variant(option_type, 0, vec![bind("a")]), rhs(2)),
            (variant(option_type, 1, Vec::new()), rhs(3)),
        ],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var(1, int_type)],
                    Decision::Switch(
                        var(1, int_type),
                        vec![Case::new(
                            Constructor::Int("4".into()),
                            Vec::new(),
                            success(1)
                        ),],
                        Some(Box::new(success_with_bindings(
                            vec![("a", var(1, int_type))],
                            2
                        )))
                    )
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), success(3))
            ],
            None
        )
    );

    assert_eq!(result.diagnostics.reachable, vec![1, 2, 3]);
}

#[test]
fn test_compile_exhaustive_option_type_with_binding() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let option_type = new_type(
        &mut compiler,
        Type::Enum(vec![
            ("Some".into(), vec![int_type]),
            ("None".into(), Vec::new()),
        ]),
    );
    let input = compiler.new_variable(option_type);
    let result = compile(
        compiler,
        input,
        vec![
            (
                variant(option_type, 0, vec![Pattern::Int("4".into())]),
                rhs(1),
            ),
            (bind("a"), rhs(2)),
        ],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var(1, int_type)],
                    Decision::Switch(
                        var(1, int_type),
                        vec![Case::new(
                            Constructor::Int("4".into()),
                            Vec::new(),
                            success(1)
                        )],
                        Some(Box::new(success_with_bindings(vec![("a", input)], 2)))
                    )
                ),
                Case::new(
                    Constructor::Variant(option_type, 1),
                    Vec::new(),
                    success_with_bindings(vec![("a", input)], 2)
                )
            ],
            None,
        )
    );
}

#[test]
fn test_compile_nonexhaustive_pair_in_option_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let tup_type = new_type(&mut compiler, Type::Tuple(vec![int_type, int_type]));
    let option_type = new_type(
        &mut compiler,
        Type::Enum(vec![
            ("Some".into(), vec![tup_type]),
            ("None".into(), Vec::new()),
        ]),
    );
    let input = compiler.new_variable(option_type);
    let result = compile(
        compiler,
        input,
        vec![(
            variant(
                option_type,
                0,
                vec![pair(int_type, int_type, int("4"), bind("a"))],
            ),
            rhs(1),
        )],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var(1, tup_type)],
                    Decision::Switch(
                        var(1, tup_type),
                        vec![Case::new(
                            Constructor::Tuple(vec![int_type, int_type]),
                            vec![var(2, int_type), var(3, int_type),],
                            Decision::Switch(
                                var(2, int_type),
                                vec![Case::new(
                                    Constructor::Int("4".into()),
                                    Vec::new(),
                                    success_with_bindings(vec![("a", var(3, int_type))], 1)
                                )],
                                Some(Box::new(failure()))
                            )
                        )],
                        None,
                    )
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), failure())
            ],
            None
        )
    );
    assert_eq!(
        result.missing_patterns(),
        vec![SmolStr::new("None"), SmolStr::new("Some(#(_, _))")]
    );
}

#[test]
fn test_compile_or_bool_pattern() {
    let mut compiler = Compiler::new();
    let bool_type = new_type(&mut compiler, Type::Boolean);
    let input = compiler.new_variable(bool_type);
    let result = compile(
        compiler,
        input,
        vec![(Pattern::Or(vec![tt(), ff()]), rhs(1))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::False, Vec::new(), success(1)),
                Case::new(Constructor::True, Vec::new(), success(1)),
            ],
            None
        )
    );
}

#[test]
fn test_compile_or_int_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(int_type);
    let result = compile(
        compiler,
        input,
        vec![(Pattern::Or(vec![int("4"), int("5")]), rhs(1))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::Int("4".into()), Vec::new(), success(1)),
                Case::new(Constructor::Int("5".into()), Vec::new(), success(1)),
            ],
            Some(Box::new(failure()))
        )
    );
}

#[test]
fn test_nonexhaustive_guard() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(int_type);

    let result = compiler.compile(vec![Row::new(
        vec![Column::new(input, int("4"))],
        Some(42),
        rhs(1),
    )]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("4".into()),
                Vec::new(),
                Decision::Guard(42, rhs(1), Box::new(failure()))
            )],
            Some(Box::new(failure()))
        )
    );

    assert_eq!(result.missing_patterns(), vec![SmolStr::new("_")]);
}

#[test]
fn test_nonexhaustive_option_with_two_rows_and_guard() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let option_type = new_type(
        &mut compiler,
        Type::Enum(vec![
            ("Some".into(), vec![int_type]),
            ("None".into(), Vec::new()),
        ]),
    );
    let input = compiler.new_variable(option_type);
    let result = compiler.compile(vec![
        Row::new(
            vec![Column::new(input, variant(option_type, 0, vec![int("4")]))],
            Some(42),
            rhs(1),
        ),
        Row::new(
            vec![Column::new(input, variant(option_type, 0, vec![bind("a")]))],
            None,
            rhs(2),
        ),
    ]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var(1, int_type)],
                    Decision::Switch(
                        var(1, int_type),
                        vec![Case::new(
                            Constructor::Int("4".into()),
                            Vec::new(),
                            Decision::Guard(
                                42,
                                rhs(1),
                                Box::new(success_with_bindings(vec![("a", var(1, int_type))], 2)),
                            )
                        )],
                        Some(Box::new(success_with_bindings(
                            vec![("a", var(1, int_type))],
                            2
                        )))
                    ),
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), failure())
            ],
            None
        )
    );

    assert_eq!(result.missing_patterns(), vec![SmolStr::new("None")]);
}

#[test]
fn test_exhaustive_guard() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(int_type);
    let result = compiler.compile(vec![
        Row::new(vec![Column::new(input, int("4"))], Some(42), rhs(1)),
        Row::new(vec![Column::new(input, bind("a"))], None, rhs(2)),
    ]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("4".into()),
                Vec::new(),
                Decision::Guard(
                    42,
                    rhs(1),
                    Box::new(success_with_bindings(vec![("a", input)], 2))
                )
            )],
            Some(Box::new(success_with_bindings(vec![("a", input)], 2)))
        )
    );
}

#[test]
fn test_exhaustive_guard_with_bool() {
    let mut compiler = Compiler::new();
    let bool_type = new_type(&mut compiler, Type::Boolean);
    let input = compiler.new_variable(bool_type);
    let result = compiler.compile(vec![
        Row::new(vec![Column::new(input, tt())], Some(42), rhs(1)),
        Row::new(vec![Column::new(input, bind("a"))], None, rhs(2)),
    ]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::False,
                    Vec::new(),
                    success_with_bindings(vec![("a", input)], 2)
                ),
                Case::new(
                    Constructor::True,
                    Vec::new(),
                    Decision::Guard(
                        42,
                        rhs(1),
                        Box::new(success_with_bindings(vec![("a", input)], 2))
                    )
                )
            ],
            None
        )
    );
}

#[test]
fn test_exhaustive_guard_with_int() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(int_type);
    let result = compiler.compile(vec![
        Row::new(vec![Column::new(input, int("1"))], Some(42), rhs(1)),
        Row::new(vec![Column::new(input, int("2"))], None, rhs(2)),
        Row::new(vec![Column::new(input, bind("b"))], None, rhs(3)),
    ]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Int("1".into()),
                    Vec::new(),
                    Decision::Guard(
                        42,
                        rhs(1),
                        Box::new(success_with_bindings(vec![("b", input)], 3))
                    )
                ),
                Case::new(Constructor::Int("2".into()), Vec::new(), success(2))
            ],
            Some(Box::new(success_with_bindings(vec![("b", input)], 3)))
        )
    );
}

#[test]
fn test_exhaustive_guard_with_same_int() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(int_type);
    let result = compiler.compile(vec![
        Row::new(vec![Column::new(input, int("1"))], Some(10), rhs(1)),
        Row::new(vec![Column::new(input, int("1"))], Some(20), rhs(2)),
        Row::new(vec![Column::new(input, int("1"))], None, rhs(3)),
        Row::new(vec![Column::new(input, bind("b"))], None, rhs(4)),
    ]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("1".into()),
                Vec::new(),
                Decision::Guard(
                    10,
                    rhs(1),
                    Box::new(Decision::Guard(20, rhs(2), Box::new(success(3))))
                )
            )],
            Some(Box::new(success_with_bindings(vec![("b", input)], 4)))
        )
    );
}

#[test]
fn test_exhaustive_option_with_guard() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let option_type = new_type(
        &mut compiler,
        Type::Enum(vec![
            ("Some".into(), vec![int_type]),
            ("None".into(), Vec::new()),
        ]),
    );
    let input = compiler.new_variable(option_type);
    let result = compiler.compile(vec![
        Row::new(
            vec![Column::new(input, variant(option_type, 1, Vec::new()))],
            None,
            rhs(1),
        ),
        Row::new(
            vec![Column::new(input, variant(option_type, 0, vec![bind("a")]))],
            Some(42),
            rhs(2),
        ),
        Row::new(
            vec![Column::new(input, variant(option_type, 0, vec![bind("a")]))],
            None,
            rhs(3),
        ),
    ]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var(1, int_type)],
                    Decision::Guard(
                        42,
                        Body {
                            bindings: vec![("a".into(), var(1, int_type))],
                            clause_index: 2
                        },
                        Box::new(success_with_bindings(vec![("a", var(1, int_type))], 3))
                    )
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), success(1)),
            ],
            None
        )
    );
}

#[test]
fn test_compile_exhaustive_nested_int_with_guard() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let tup_type = new_type(&mut compiler, Type::Tuple(vec![int_type, int_type]));
    let input = compiler.new_variable(tup_type);
    let result = compiler.compile(vec![
        Row::new(
            vec![Column::new(
                input,
                pair(int_type, int_type, int("4"), int("5")),
            )],
            Some(42),
            rhs(1),
        ),
        Row::new(
            vec![Column::new(
                input,
                pair(int_type, int_type, int("4"), int("5")),
            )],
            None,
            rhs(2),
        ),
        Row::new(
            vec![Column::new(
                input,
                pair(int_type, int_type, bind("a"), bind("b")),
            )],
            None,
            rhs(3),
        ),
    ]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Tuple(vec![int_type, int_type]),
                vec![var(1, int_type), var(2, int_type)],
                Decision::Switch(
                    var(2, int_type),
                    vec![Case::new(
                        Constructor::Int("5".into()),
                        Vec::new(),
                        Decision::Switch(
                            var(1, int_type),
                            vec![Case::new(
                                Constructor::Int("4".into()),
                                Vec::new(),
                                Decision::Guard(42, rhs(1), Box::new(success(2)),)
                            )],
                            Some(Box::new(success_with_bindings(
                                vec![("a", var(1, int_type)), ("b", var(2, int_type))],
                                3
                            )))
                        )
                    )],
                    Some(Box::new(success_with_bindings(
                        vec![("a", var(1, int_type)), ("b", var(2, int_type))],
                        3
                    )))
                )
            )],
            None
        )
    );
}
