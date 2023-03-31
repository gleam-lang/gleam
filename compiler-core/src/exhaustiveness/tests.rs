use crate::ast::AssignName;

use super::*;

fn new_type(compiler: &mut Compiler, typ: Type) -> TypeId {
    let id = compiler.types.len();

    compiler.types.push(typ);
    TypeId(id)
}

fn bind(name: &str) -> Pattern {
    Pattern::Variable { value: name.into() }
}

fn assign(name: &str, pattern: Pattern) -> Pattern {
    Pattern::Assign {
        name: name.into(),
        pattern: Box::new(pattern),
    }
}

fn discard() -> Pattern {
    Pattern::Discard
}

fn list(elements: Vec<Pattern>, tail: Option<AssignName>) -> Pattern {
    Pattern::List { elements, tail }
}

fn variant(typ: TypeId, index: usize, args: Vec<Pattern>) -> Pattern {
    Pattern::Constructor {
        constructor: Constructor::Variant(typ, index),
        arguments: args,
    }
}

fn pair(typ1: TypeId, typ2: TypeId, pat1: Pattern, pat2: Pattern) -> Pattern {
    Pattern::Constructor {
        constructor: Constructor::Tuple(vec![typ1, typ2]),
        arguments: vec![pat1, pat2],
    }
}

fn tuple(args: Vec<(Pattern, TypeId)>) -> Pattern {
    let types = args.iter().map(|(_, t)| *t).collect();
    let patterns = args.into_iter().map(|(p, _)| p).collect();
    Pattern::Constructor {
        constructor: Constructor::Tuple(types),
        arguments: patterns,
    }
}

fn string(val: &str) -> Pattern {
    Pattern::String { value: val.into() }
}

fn int(val: &str) -> Pattern {
    Pattern::Int { value: val.into() }
}

fn float(val: &str) -> Pattern {
    Pattern::Float { value: val.into() }
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
fn move_variable_patterns() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Int);
    let var1 = compiler.new_variable(typ);
    let var2 = compiler.new_variable(typ);
    let cons = Constructor::Int("42".into());
    let case = compiler.move_unconditional_patterns(Row {
        columns: vec![
            Column::new(var2, bind("a")),
            Column::new(
                var1,
                Pattern::Constructor {
                    constructor: cons.clone(),
                    arguments: Vec::new(),
                },
            ),
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
            columns: vec![Column::new(
                var1,
                Pattern::Constructor {
                    constructor: cons,
                    arguments: Vec::new()
                }
            )],
            guard: None,
            body: Body {
                bindings: vec![("a".into(), var2)],
                clause_index: 42
            }
        }
    );
}

#[test]
fn move_variable_patterns_without_constructor_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Int);
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
fn branch_variable() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Int);
    let var1 = compiler.new_variable(typ);
    let var2 = compiler.new_variable(typ);
    let rows = vec![
        Row::new(
            vec![
                Column::new(var1, Pattern::Int { value: "42".into() }),
                Column::new(var2, Pattern::Int { value: "50".into() }),
            ],
            None,
            rhs(1),
        ),
        Row::new(
            vec![Column::new(var2, Pattern::Int { value: "42".into() })],
            None,
            rhs(2),
        ),
    ];

    let branch = compiler.branch_variable(&rows[0], &rows);

    assert_eq!(branch, var2);
}

#[test]
fn compile_simple_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(typ);
    let result = compile(
        compiler,
        input,
        vec![(int("1"), rhs(1)), (discard(), rhs(2))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("1".into()),
                Vec::new(),
                success(1)
            )],
            Some(Box::new(success(2)))
        )
    );
}

#[test]
fn compile_nonexhaustive_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(typ);
    let result = compile(compiler, input, vec![(int("1"), rhs(1))]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("1".into()),
                Vec::new(),
                success(1)
            )],
            Some(Box::new(failure()))
        )
    );
    assert!(result.diagnostics.missing);
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("_")]);
}

#[test]
fn compile_redundant_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(typ);
    let result = compile(
        compiler,
        input,
        vec![(int("1"), rhs(1)), (bind("a"), rhs(2)), (int("1"), rhs(3))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("1".into()),
                Vec::new(),
                success(1)
            ),],
            Some(Box::new(success_with_bindings(vec![("a", var(0, typ))], 2)))
        )
    );
    assert_eq!(result.diagnostics.reachable, vec![1, 2]);
}

#[test]
fn compile_redundant_int() {
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
fn compile_variable_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(typ);
    let result = compile(
        compiler,
        input,
        vec![(int("1"), rhs(1)), (bind("a"), rhs(2))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("1".into()),
                Vec::new(),
                success(1)
            )],
            Some(Box::new(success_with_bindings(vec![("a", input)], 2)))
        )
    );
}

#[test]
fn compile_nonexhaustive_float_pattern() {
    let mut compiler = Compiler::new();
    let float_type = new_type(&mut compiler, Type::Float);
    let input = compiler.new_variable(float_type);
    let result = compile(
        compiler,
        input,
        vec![(float("4.0"), rhs(1)), (float("5.3"), rhs(2))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::Float("4.0".into()), Vec::new(), success(1)),
                Case::new(Constructor::Float("5.3".into()), Vec::new(), success(2)),
            ],
            Some(Box::new(failure()))
        )
    );
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("_")]);
}

#[test]
fn compile_exhaustive_float_pattern() {
    let mut compiler = Compiler::new();
    let float_type = new_type(&mut compiler, Type::Float);
    let input = compiler.new_variable(float_type);
    let result = compile(
        compiler,
        input,
        vec![
            (float("4.0"), rhs(1)),
            (float("5.1"), rhs(2)),
            (bind("a"), rhs(3)),
        ],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::Float("4.0".into()), Vec::new(), success(1)),
                Case::new(Constructor::Float("5.1".into()), Vec::new(), success(2)),
            ],
            Some(Box::new(success_with_bindings(vec![("a", input)], 3)))
        )
    );
}

#[test]
fn compile_nonexhaustive_int_pattern() {
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
fn compile_exhaustive_int_pattern() {
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
fn compile_nonexhaustive_nested_int_pattern() {
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
fn compile_exhaustive_empty_tuple_pattern() {
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
fn compile_exhaustive_three_tuple_pattern() {
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
fn compile_exhaustive_nested_int_pattern() {
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
fn compile_nonexhaustive_option_type() {
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
            variant(option_type, 0, vec![Pattern::Int { value: "4".into() }]),
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
fn compile_nonexhaustive_option_type_with_multiple_arguments() {
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
                vec![
                    Pattern::Int { value: "4".into() },
                    Pattern::Int { value: "5".into() },
                ],
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

// #[test]
// fn compile_exhaustive_option_type() {
//     let mut compiler = Compiler::new();
//     let int_type = new_type(&mut compiler, Type::Int);
//     let option_type = new_type(
//         &mut compiler,
//         Type::Enum(vec![
//             ("Some".into(), vec![int_type]),
//             ("None".into(), Vec::new()),
//         ]),
//     );
//     let input = compiler.new_variable(option_type);
//     let result = compile(
//         compiler,
//         input,
//         vec![
//             (
//                 variant(option_type, 0, vec![Pattern::Int("4".into())]),
//                 rhs(1),
//             ),
//             (variant(option_type, 0, vec![bind("a")]), rhs(2)),
//             (variant(option_type, 1, Vec::new()), rhs(3)),
//         ],
//     );

//     assert_eq!(
//         result.tree,
//         Decision::Switch(
//             input,
//             vec![
//                 Case::new(
//                     Constructor::Variant(option_type, 0),
//                     vec![var(1, int_type)],
//                     Decision::Switch(
//                         var(1, int_type),
//                         vec![Case::new(
//                             Constructor::Int("4".into()),
//                             Vec::new(),
//                             success(1)
//                         )],
//                         Some(Box::new(success_with_bindings(
//                             vec![("a", var(1, int_type))],
//                             2
//                         )))
//                     )
//                 ),
//                 Case::new(Constructor::Variant(option_type, 1), Vec::new(), success(3))
//             ],
//             None
//         )
//     );
// }

#[test]
fn compile_redundant_option_type_with_bool() {
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
            (variant(option_type, 0, vec![int("1".into())]), rhs(1)),
            (variant(option_type, 0, vec![int("1".into())]), rhs(10)),
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
                            Constructor::Int("1".into()),
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

    assert_eq!(result.diagnostics.reachable, vec![1, 2, 3]);
}

#[test]
fn compile_redundant_option_type_with_int() {
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
                variant(option_type, 0, vec![Pattern::Int { value: "4".into() }]),
                rhs(1),
            ),
            (
                variant(option_type, 0, vec![Pattern::Int { value: "4".into() }]),
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
fn compile_exhaustive_option_type_with_binding() {
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
                variant(option_type, 0, vec![Pattern::Int { value: "4".into() }]),
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
fn compile_nonexhaustive_pair_in_option_pattern() {
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
fn compile_or_pattern() {
    let mut compiler = Compiler::new();
    let bool_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(bool_type);
    let result = compile(
        compiler,
        input,
        vec![(
            Pattern::Or {
                left: Box::new(int("1")),
                right: Box::new(int("2")),
            },
            rhs(1),
        )],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::Int("1".into()), Vec::new(), success(1)),
                Case::new(Constructor::Int("2".into()), Vec::new(), success(1))
            ],
            Some(Box::new(failure()))
        )
    );
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("_")]);
}

#[test]
fn compile_or_int_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(int_type);
    let result = compile(
        compiler,
        input,
        vec![(
            Pattern::Or {
                left: Box::new(int("4")),
                right: Box::new(int("5")),
            },
            rhs(1),
        )],
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
fn nonexhaustive_guard() {
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
fn nonexhaustive_option_with_two_rows_and_guard() {
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
fn exhaustive_guard() {
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
fn exhaustive_guard_with_bool() {
    let mut compiler = Compiler::new();
    let bool_type = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(bool_type);
    let result = compiler.compile(vec![
        Row::new(vec![Column::new(input, int("10"))], Some(42), rhs(1)),
        Row::new(vec![Column::new(input, bind("a"))], None, rhs(2)),
    ]);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("10".into()),
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
fn exhaustive_guard_with_int() {
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
fn exhaustive_guard_with_same_int() {
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
fn exhaustive_option_with_guard() {
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
fn compile_exhaustive_nested_int_with_guard() {
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

#[test]
fn assign_pattern() {
    let mut compiler = Compiler::new();
    let typ = new_type(&mut compiler, Type::Int);
    let input = compiler.new_variable(typ);
    let result = compile(
        compiler,
        input,
        vec![(int("1"), rhs(1)), (assign("it", discard()), rhs(2))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("1".into()),
                Vec::new(),
                success(1)
            )],
            Some(Box::new(success_with_bindings(
                vec![("it", var(0, typ))],
                2
            )))
        )
    );
}

#[test]
fn compile_nonexhaustive_string_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::String);
    let input = compiler.new_variable(int_type);
    let result = compile(
        compiler,
        input,
        vec![(string("Hello"), rhs(1)), (string("Goodbye"), rhs(2))],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::String("Hello".into()), Vec::new(), success(1)),
                Case::new(
                    Constructor::String("Goodbye".into()),
                    Vec::new(),
                    success(2)
                ),
            ],
            Some(Box::new(failure()))
        )
    );
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("_")]);
}

#[test]
fn compile_exhaustive_string_pattern() {
    let mut compiler = Compiler::new();
    let int_type = new_type(&mut compiler, Type::String);
    let input = compiler.new_variable(int_type);
    let result = compile(
        compiler,
        input,
        vec![
            (string("Hello"), rhs(1)),
            (string("Goodbye"), rhs(2)),
            (bind("a"), rhs(3)),
        ],
    );

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::String("Hello".into()), Vec::new(), success(1)),
                Case::new(
                    Constructor::String("Goodbye".into()),
                    Vec::new(),
                    success(2)
                ),
            ],
            Some(Box::new(success_with_bindings(vec![("a", input)], 3)))
        )
    );
}
