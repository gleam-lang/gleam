use super::*;
use crate::ast::AssignName;
use id_arena::Arena;

#[derive(Debug)]
struct Setup {
    compiler: Compiler,
}

impl Setup {
    fn new() -> Self {
        Self {
            compiler: Compiler::new(Arena::new()),
        }
    }
}

impl Setup {
    fn new_type(&mut self, typ: Type) -> TypeId {
        let id = self.compiler.types.len();
        self.compiler.types.push(typ);
        TypeId(id)
    }

    fn new_variable(&mut self, type_id: TypeId) -> Variable {
        self.compiler.new_variable(type_id)
    }

    fn bind(&mut self, name: &str) -> PatternId {
        self.compiler
            .patterns
            .alloc(Pattern::Variable { name: name.into() })
    }

    fn assign(&mut self, name: &str, pattern: PatternId) -> PatternId {
        self.compiler.patterns.alloc(Pattern::Assign {
            name: name.into(),
            pattern,
        })
    }

    fn discard(&mut self) -> PatternId {
        self.compiler.patterns.alloc(Pattern::Discard)
    }

    fn empty_list(&mut self) -> PatternId {
        self.compiler.patterns.alloc(Pattern::EmptyList)
    }

    fn list(&mut self, first: PatternId, rest: PatternId) -> PatternId {
        self.compiler.patterns.alloc(Pattern::List { first, rest })
    }

    fn variant(&mut self, typ: TypeId, index: usize, args: Vec<PatternId>) -> PatternId {
        self.compiler.patterns.alloc(Pattern::Constructor {
            constructor: Constructor::Variant(typ, index),
            arguments: args,
        })
    }

    fn or(&mut self, left: PatternId, right: PatternId) -> PatternId {
        self.compiler.patterns.alloc(Pattern::Or { left, right })
    }

    fn pair(&mut self, typ1: TypeId, typ2: TypeId, pat1: PatternId, pat2: PatternId) -> PatternId {
        self.compiler.patterns.alloc(Pattern::Constructor {
            constructor: Constructor::Tuple(vec![typ1, typ2]),
            arguments: vec![pat1, pat2],
        })
    }

    fn tuple(&mut self, args: Vec<(PatternId, TypeId)>) -> PatternId {
        let types = args.iter().map(|(_, t)| *t).collect();
        let patterns = args.into_iter().map(|(p, _)| p).collect();
        self.compiler.patterns.alloc(Pattern::Constructor {
            constructor: Constructor::Tuple(types),
            arguments: patterns,
        })
    }

    fn string(&mut self, value: &str) -> PatternId {
        self.compiler.patterns.alloc(Pattern::String {
            value: value.into(),
        })
    }

    fn string_prefix_discard(&mut self, prefix: &str) -> PatternId {
        self.compiler.patterns.alloc(Pattern::StringPrefix {
            prefix: prefix.into(),
            rest: AssignName::Discard("_".into()),
        })
    }

    fn string_prefix_bind(&mut self, prefix: &str, rest: &str) -> PatternId {
        self.compiler.patterns.alloc(Pattern::StringPrefix {
            prefix: prefix.into(),
            rest: AssignName::Variable(rest.into()),
        })
    }

    fn bit_string(&mut self, value: &str) -> PatternId {
        self.compiler.patterns.alloc(Pattern::BitString {
            value: value.into(),
        })
    }

    fn int(&mut self, value: &str) -> PatternId {
        self.compiler.patterns.alloc(Pattern::Int {
            value: value.into(),
        })
    }

    fn float(&mut self, value: &str) -> PatternId {
        self.compiler.patterns.alloc(Pattern::Float {
            value: value.into(),
        })
    }

    fn var(&mut self, id: usize, type_id: TypeId) -> Variable {
        Variable { id, type_id }
    }

    fn compile(self, input: Variable, rules: Vec<(PatternId, Body)>) -> Match {
        let rows = rules
            .into_iter()
            .map(|(pat, body)| Row::new(vec![Column::new(input, pat)], None, body))
            .collect();

        self.compiler.compile(rows)
    }
}

fn rhs(value: u16) -> Body {
    Body {
        bindings: Vec::new(),
        clause_index: value,
    }
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
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Int);
    let var1 = setup.new_variable(typ);
    let var2 = setup.new_variable(typ);
    let cons = Constructor::Int("42".into());
    let constructor_pattern = setup.compiler.patterns.alloc(Pattern::Constructor {
        constructor: cons.clone(),
        arguments: Vec::new(),
    });
    let row = Row {
        columns: vec![
            Column::new(var2, setup.bind("a")),
            Column::new(var1, constructor_pattern),
        ],
        guard: None,
        body: Body {
            bindings: Vec::new(),
            clause_index: 42,
        },
    };
    let case = setup.compiler.move_unconditional_patterns(row);

    assert_eq!(
        case,
        Row {
            columns: vec![Column::new(var1, constructor_pattern)],
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
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Int);
    let var1 = setup.new_variable(typ);
    let row = Row {
        columns: vec![Column::new(var1, setup.bind("a"))],
        guard: None,
        body: Body {
            bindings: Vec::new(),
            clause_index: 42,
        },
    };
    let case = setup.compiler.move_unconditional_patterns(row);

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
fn branch_mode_int() {
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Int);
    let var1 = setup.new_variable(typ);
    let var2 = setup.new_variable(typ);
    let rows = vec![
        Row::new(
            vec![
                Column::new(var1, setup.int("42")),
                Column::new(var2, setup.int("51")),
            ],
            None,
            rhs(1),
        ),
        Row::new(vec![Column::new(var2, setup.int("42"))], None, rhs(2)),
    ];
    let branch = setup.compiler.branch_mode(&rows);
    assert_eq!(branch, BranchMode::Infinite { variable: var2 });
}

#[test]
fn branch_mode_string() {
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::String);
    let var1 = setup.new_variable(typ);
    let var2 = setup.new_variable(typ);
    let rows = vec![
        Row::new(vec![Column::new(var2, setup.string("42"))], None, rhs(2)),
        Row::new(
            vec![
                Column::new(var1, setup.string("42")),
                Column::new(var2, setup.string("51")),
            ],
            None,
            rhs(1),
        ),
    ];
    let branch = setup.compiler.branch_mode(&rows);
    assert_eq!(branch, BranchMode::Infinite { variable: var2 });
}

#[test]
fn branch_mode_float() {
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Float);
    let var1 = setup.new_variable(typ);
    let var2 = setup.new_variable(typ);
    let rows = vec![
        Row::new(
            vec![
                Column::new(var1, setup.float("42")),
                Column::new(var2, setup.float("51")),
            ],
            None,
            rhs(1),
        ),
        Row::new(vec![Column::new(var2, setup.float("42"))], None, rhs(2)),
    ];
    let branch = setup.compiler.branch_mode(&rows);
    assert_eq!(branch, BranchMode::Infinite { variable: var2 });
}

#[test]
fn branch_mode_bitstring() {
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::BitString);
    let var1 = setup.new_variable(typ);
    let var2 = setup.new_variable(typ);
    let rows = vec![
        Row::new(
            vec![
                Column::new(var1, setup.bit_string("42")),
                Column::new(var2, setup.bit_string("51")),
            ],
            None,
            rhs(1),
        ),
        Row::new(
            vec![Column::new(var2, setup.bit_string("42"))],
            None,
            rhs(2),
        ),
    ];
    let branch = setup.compiler.branch_mode(&rows);
    assert_eq!(branch, BranchMode::Infinite { variable: var2 });
}

#[test]
fn branch_mode_tuple() {
    let mut setup = Setup::new();
    let int = setup.new_type(Type::Int);
    let type_ = setup.new_type(Type::Tuple(vec![int, int]));
    let var1 = setup.new_variable(type_);
    let var2 = setup.new_variable(type_);
    let int_0 = setup.int("0");
    let int_1 = setup.int("1");
    let int_2 = setup.int("2");
    let rows = vec![
        Row::new(
            vec![
                Column::new(var1, setup.tuple(vec![(int_0, int), (int_1, int)])),
                Column::new(var1, setup.tuple(vec![(int_1, int), (int_2, int)])),
            ],
            None,
            rhs(1),
        ),
        Row::new(
            vec![Column::new(
                var2,
                setup.tuple(vec![(int_0, int), (int_2, int)]),
            )],
            None,
            rhs(2),
        ),
    ];
    let branch = setup.compiler.branch_mode(&rows);
    assert_eq!(
        branch,
        BranchMode::Tuple {
            variable: var1,
            types: vec![int, int]
        }
    );
}

#[test]
fn simple_pattern() {
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Int);
    let input = setup.new_variable(typ);
    let rules = vec![(setup.int("1"), rhs(1)), (setup.discard(), rhs(2))];
    let result = setup.compile(input, rules);

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
fn nonexhaustive_pattern() {
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Int);
    let input = setup.new_variable(typ);
    let rules = vec![(setup.int("1"), rhs(1))];
    let result = setup.compile(input, rules);

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
fn redundant_pattern() {
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Int);
    let input = setup.new_variable(typ);
    let rules = vec![
        (setup.int("1"), rhs(1)),
        (setup.bind("a"), rhs(2)),
        (setup.int("1"), rhs(3)),
    ];
    let var = setup.var(0, typ);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("1".into()),
                Vec::new(),
                success(1)
            ),],
            Some(Box::new(success_with_bindings(vec![("a", var)], 2)))
        )
    );
    assert_eq!(result.diagnostics.reachable, vec![1, 2]);
}

#[test]
fn redundant_int() {
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Int);
    let input = setup.new_variable(typ);
    let rules = vec![
        (setup.int("1"), rhs(1)),
        (setup.int("1"), rhs(2)),
        (setup.int("2"), rhs(3)),
        (setup.bind("a"), rhs(4)),
    ];
    let result = setup.compile(input, rules);

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
fn variable_pattern() {
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Int);
    let input = setup.new_variable(typ);
    let rules = vec![(setup.int("1"), rhs(1)), (setup.bind("a"), rhs(2))];
    let result = setup.compile(input, rules);

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
fn nonexhaustive_float_pattern() {
    let mut setup = Setup::new();
    let float_type = setup.new_type(Type::Float);
    let input = setup.new_variable(float_type);
    let rules = vec![(setup.float("4.0"), rhs(1)), (setup.float("5.3"), rhs(2))];
    let result = setup.compile(input, rules);

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
fn exhaustive_float_pattern() {
    let mut setup = Setup::new();
    let float_type = setup.new_type(Type::Float);
    let input = setup.new_variable(float_type);
    let rules = vec![
        (setup.float("4.0"), rhs(1)),
        (setup.float("5.1"), rhs(2)),
        (setup.bind("a"), rhs(3)),
    ];
    let result = setup.compile(input, rules);

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
fn nonexhaustive_int_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let input = setup.new_variable(int_type);
    let rules = vec![(setup.int("4"), rhs(1)), (setup.int("5"), rhs(2))];
    let result = setup.compile(input, rules);

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
fn exhaustive_int_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let input = setup.new_variable(int_type);
    let rules = vec![
        (setup.int("4"), rhs(1)),
        (setup.int("5"), rhs(2)),
        (setup.bind("a"), rhs(3)),
    ];
    let result = setup.compile(input, rules);

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
fn nonexhaustive_nested_int_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let tup_type = setup.new_type(Type::Tuple(vec![int_type, int_type]));
    let input = setup.new_variable(tup_type);
    let int_4 = setup.int("4");
    let var_a = setup.bind("a");
    let rules = vec![(setup.pair(int_type, int_type, int_4, var_a), rhs(1))];
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, int_type);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Tuple(vec![int_type, int_type]),
                vec![var_1, var_2],
                Decision::Switch(
                    var_1,
                    vec![Case::new(
                        Constructor::Int("4".into()),
                        Vec::new(),
                        success_with_bindings(vec![("a", var_2)], 1)
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
fn exhaustive_empty_tuple_pattern() {
    let mut setup = Setup::new();
    let tup_type = setup.new_type(Type::Tuple(vec![]));
    let input = setup.new_variable(tup_type);
    let rules = vec![(setup.tuple(vec![]), rhs(1))];
    let result = setup.compile(input, rules);
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
fn exhaustive_three_tuple_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let tup_type = setup.new_type(Type::Tuple(vec![int_type, int_type, int_type]));
    let input = setup.new_variable(tup_type);
    let discard = setup.discard();
    let var1 = setup.var(1, int_type);
    let var2 = setup.var(2, int_type);
    let var3 = setup.var(3, int_type);
    let rules = vec![(
        setup.tuple(vec![
            (discard, int_type),
            (discard, int_type),
            (discard, int_type),
        ]),
        rhs(1),
    )];
    let result = setup.compile(input, rules);
    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Tuple(vec![int_type, int_type, int_type]),
                vec![var1, var2, var3],
                success(1)
            )],
            None
        )
    );
}

#[test]
fn exhaustive_nested_int_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let tup_type = setup.new_type(Type::Tuple(vec![int_type, int_type]));
    let input = setup.new_variable(tup_type);
    let int4 = setup.int("4");
    let int5 = setup.int("5");
    let var_a = setup.bind("a");
    let var_b = setup.bind("b");
    let var1 = setup.var(1, int_type);
    let var2 = setup.var(2, int_type);
    let rules = vec![
        (setup.pair(int_type, int_type, int4, int5), rhs(1)),
        (setup.pair(int_type, int_type, var_a, var_b), rhs(2)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Tuple(vec![int_type, int_type]),
                vec![var1, var2],
                Decision::Switch(
                    var2,
                    vec![Case::new(
                        Constructor::Int("5".into()),
                        Vec::new(),
                        Decision::Switch(
                            var1,
                            vec![Case::new(
                                Constructor::Int("4".into()),
                                Vec::new(),
                                success(1)
                            )],
                            Some(Box::new(success_with_bindings(
                                vec![("a", var1), ("b", var2)],
                                2
                            )))
                        )
                    )],
                    Some(Box::new(success_with_bindings(
                        vec![("a", var1), ("b", var2)],
                        2
                    )))
                )
            )],
            None
        )
    );
}

#[test]
fn nonexhaustive_option_type() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let option_type = setup.new_type(Type::Variants(vec![
        ("Some".into(), vec![int_type]),
        ("None".into(), Vec::new()),
    ]));
    let input = setup.new_variable(option_type);
    let int = setup.int("4");
    let rules = vec![(setup.variant(option_type, 0, vec![int]), rhs(1))];
    let var1 = setup.var(1, int_type);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var1],
                    Decision::Switch(
                        var1,
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
fn nonexhaustive_option_type_with_multiple_arguments() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let option_type = setup.new_type(Type::Variants(vec![
        ("Some".into(), vec![int_type, int_type]),
        ("None".into(), Vec::new()),
    ]));
    let int4 = setup.int("4");
    let int5 = setup.int("5");
    let var1 = setup.var(1, int_type);
    let var2 = setup.var(2, int_type);
    let input = setup.new_variable(option_type);
    let rules = vec![(setup.variant(option_type, 0, vec![int4, int5]), rhs(1))];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var1, var2],
                    Decision::Switch(
                        var2,
                        vec![Case::new(
                            Constructor::Int("5".into()),
                            Vec::new(),
                            Decision::Switch(
                                var1,
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
fn exhaustive_option_type() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let int_4 = setup.int("4");
    let bind_a = setup.bind("a");
    let option_type = setup.new_type(Type::Variants(vec![
        ("Some".into(), vec![int_type]),
        ("None".into(), Vec::new()),
    ]));
    let var_1 = setup.var(1, int_type);
    let input = setup.new_variable(option_type);
    let rules = vec![
        (setup.variant(option_type, 0, vec![int_4]), rhs(1)),
        (setup.variant(option_type, 0, vec![bind_a]), rhs(2)),
        (setup.variant(option_type, 1, Vec::new()), rhs(3)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var_1],
                    Decision::Switch(
                        var_1,
                        vec![Case::new(
                            Constructor::Int("4".into()),
                            Vec::new(),
                            success(1)
                        )],
                        Some(Box::new(success_with_bindings(vec![("a", var_1)], 2)))
                    )
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), success(3))
            ],
            None
        )
    );
}

#[test]
fn redundant_option_type_with_bool() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let option_type = setup.new_type(Type::Variants(vec![
        ("Some".into(), vec![int_type]),
        ("None".into(), Vec::new()),
    ]));
    let int1 = setup.int("1");
    let var_a = setup.bind("a");
    let var1 = setup.var(1, int_type);
    let input = setup.new_variable(option_type);
    let rules = vec![
        (setup.variant(option_type, 0, vec![int1]), rhs(1)),
        (setup.variant(option_type, 0, vec![int1]), rhs(10)),
        (setup.variant(option_type, 0, vec![var_a]), rhs(2)),
        (setup.variant(option_type, 1, Vec::new()), rhs(3)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var1],
                    Decision::Switch(
                        var1,
                        vec![Case::new(
                            Constructor::Int("1".into()),
                            Vec::new(),
                            success(1)
                        )],
                        Some(Box::new(success_with_bindings(vec![("a", var1)], 2)))
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
fn redundant_option_type_with_int() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let option_type = setup.new_type(Type::Variants(vec![
        ("Some".into(), vec![int_type]),
        ("None".into(), Vec::new()),
    ]));
    let input = setup.new_variable(option_type);
    let int4 = setup.int("4");
    let bind_a = setup.bind("a");
    let var1 = setup.var(1, int_type);
    let rules = vec![
        (setup.variant(option_type, 0, vec![int4]), rhs(1)),
        (setup.variant(option_type, 0, vec![int4]), rhs(10)),
        (setup.variant(option_type, 0, vec![bind_a]), rhs(2)),
        (setup.variant(option_type, 1, Vec::new()), rhs(3)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var1],
                    Decision::Switch(
                        var1,
                        vec![Case::new(
                            Constructor::Int("4".into()),
                            Vec::new(),
                            success(1)
                        ),],
                        Some(Box::new(success_with_bindings(vec![("a", var1)], 2)))
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
fn exhaustive_option_type_with_binding() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let option_type = setup.new_type(Type::Variants(vec![
        ("Some".into(), vec![int_type]),
        ("None".into(), Vec::new()),
    ]));
    let input = setup.new_variable(option_type);
    let int4 = setup.int("4");
    let bind_a = setup.bind("a");
    let var1 = setup.var(1, int_type);
    let rules = vec![
        (setup.variant(option_type, 0, vec![int4]), rhs(1)),
        (bind_a, rhs(2)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var1],
                    Decision::Switch(
                        var1,
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
fn nonexhaustive_pair_in_option_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let tup_type = setup.new_type(Type::Tuple(vec![int_type, int_type]));
    let option_type = setup.new_type(Type::Variants(vec![
        ("Some".into(), vec![tup_type]),
        ("None".into(), Vec::new()),
    ]));
    let input = setup.new_variable(option_type);
    let int_4 = setup.int("4");
    let bind_a = setup.bind("a");
    let pair = setup.pair(int_type, int_type, int_4, bind_a);
    let rules = vec![(setup.variant(option_type, 0, vec![pair]), rhs(1))];
    let var_1 = setup.var(1, tup_type);
    let var_2 = setup.var(2, int_type);
    let var_3 = setup.var(3, int_type);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var_1],
                    Decision::Switch(
                        var_1,
                        vec![Case::new(
                            Constructor::Tuple(vec![int_type, int_type]),
                            vec![var_2, var_3],
                            Decision::Switch(
                                var_2,
                                vec![Case::new(
                                    Constructor::Int("4".into()),
                                    Vec::new(),
                                    success_with_bindings(vec![("a", var_3)], 1)
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
fn or_pattern() {
    let mut setup = Setup::new();
    let bool_type = setup.new_type(Type::Int);
    let input = setup.new_variable(bool_type);
    let int_1 = setup.int("1");
    let int_2 = setup.int("2");
    let rules = vec![(setup.or(int_1, int_2), rhs(1))];
    let result = setup.compile(input, rules);

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
fn or_int_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let input = setup.new_variable(int_type);
    let int_4 = setup.int("4");
    let int_5 = setup.int("5");
    let rules = vec![(setup.or(int_4, int_5), rhs(1))];
    let result = setup.compile(input, rules);

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
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let input = setup.new_variable(int_type);

    let rows = vec![Row::new(
        vec![Column::new(input, setup.int("4"))],
        Some(42),
        rhs(1),
    )];
    let result = setup.compiler.compile(rows);

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
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let option_type = setup.new_type(Type::Variants(vec![
        ("Some".into(), vec![int_type]),
        ("None".into(), Vec::new()),
    ]));
    let int_4 = setup.int("4");
    let bind_a = setup.bind("a");
    let var_1 = setup.var(1, int_type);
    let input = setup.new_variable(option_type);
    let rows = vec![
        Row::new(
            vec![Column::new(
                input,
                setup.variant(option_type, 0, vec![int_4]),
            )],
            Some(42),
            rhs(1),
        ),
        Row::new(
            vec![Column::new(
                input,
                setup.variant(option_type, 0, vec![bind_a]),
            )],
            None,
            rhs(2),
        ),
    ];
    let result = setup.compiler.compile(rows);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var_1],
                    Decision::Switch(
                        var_1,
                        vec![Case::new(
                            Constructor::Int("4".into()),
                            Vec::new(),
                            Decision::Guard(
                                42,
                                rhs(1),
                                Box::new(success_with_bindings(vec![("a", var_1)], 2)),
                            )
                        )],
                        Some(Box::new(success_with_bindings(vec![("a", var_1)], 2)))
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
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let input = setup.new_variable(int_type);
    let rows = vec![
        Row::new(vec![Column::new(input, setup.int("4"))], Some(42), rhs(1)),
        Row::new(vec![Column::new(input, setup.bind("a"))], None, rhs(2)),
    ];
    let result = setup.compiler.compile(rows);

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
    let mut setup = Setup::new();
    let bool_type = setup.new_type(Type::Int);
    let input = setup.new_variable(bool_type);
    let rows = vec![
        Row::new(vec![Column::new(input, setup.int("10"))], Some(42), rhs(1)),
        Row::new(vec![Column::new(input, setup.bind("a"))], None, rhs(2)),
    ];
    let result = setup.compiler.compile(rows);

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
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let input = setup.new_variable(int_type);
    let rows = vec![
        Row::new(vec![Column::new(input, setup.int("1"))], Some(42), rhs(1)),
        Row::new(vec![Column::new(input, setup.int("2"))], None, rhs(2)),
        Row::new(vec![Column::new(input, setup.bind("b"))], None, rhs(3)),
    ];
    let result = setup.compiler.compile(rows);

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
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let input = setup.new_variable(int_type);
    let rows = vec![
        Row::new(vec![Column::new(input, setup.int("1"))], Some(10), rhs(1)),
        Row::new(vec![Column::new(input, setup.int("1"))], Some(20), rhs(2)),
        Row::new(vec![Column::new(input, setup.int("1"))], None, rhs(3)),
        Row::new(vec![Column::new(input, setup.bind("b"))], None, rhs(4)),
    ];
    let result = setup.compiler.compile(rows);

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
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let option_type = setup.new_type(Type::Variants(vec![
        ("Some".into(), vec![int_type]),
        ("None".into(), Vec::new()),
    ]));
    let bind_a = setup.bind("a");
    let input = setup.new_variable(option_type);
    let var_1 = setup.var(1, int_type);
    let rows = vec![
        Row::new(
            vec![Column::new(
                input,
                setup.variant(option_type, 1, Vec::new()),
            )],
            None,
            rhs(1),
        ),
        Row::new(
            vec![Column::new(
                input,
                setup.variant(option_type, 0, vec![bind_a]),
            )],
            Some(42),
            rhs(2),
        ),
        Row::new(
            vec![Column::new(
                input,
                setup.variant(option_type, 0, vec![bind_a]),
            )],
            None,
            rhs(3),
        ),
    ];
    let result = setup.compiler.compile(rows);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::Variant(option_type, 0),
                    vec![var_1],
                    Decision::Guard(
                        42,
                        Body {
                            bindings: vec![("a".into(), var_1)],
                            clause_index: 2
                        },
                        Box::new(success_with_bindings(vec![("a", var_1)], 3))
                    )
                ),
                Case::new(Constructor::Variant(option_type, 1), Vec::new(), success(1)),
            ],
            None
        )
    );
}

#[test]
fn exhaustive_nested_int_with_guard() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let tup_type = setup.new_type(Type::Tuple(vec![int_type, int_type]));
    let input = setup.new_variable(tup_type);
    let int_4 = setup.int("4");
    let int_5 = setup.int("5");
    let bind_a = setup.bind("a");
    let bind_b = setup.bind("b");
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, int_type);
    let rows = vec![
        Row::new(
            vec![Column::new(
                input,
                setup.pair(int_type, int_type, int_4, int_5),
            )],
            Some(42),
            rhs(1),
        ),
        Row::new(
            vec![Column::new(
                input,
                setup.pair(int_type, int_type, int_4, int_5),
            )],
            None,
            rhs(2),
        ),
        Row::new(
            vec![Column::new(
                input,
                setup.pair(int_type, int_type, bind_a, bind_b),
            )],
            None,
            rhs(3),
        ),
    ];
    let result = setup.compiler.compile(rows);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Tuple(vec![int_type, int_type]),
                vec![var_1, var_2],
                Decision::Switch(
                    var_2,
                    vec![Case::new(
                        Constructor::Int("5".into()),
                        Vec::new(),
                        Decision::Switch(
                            var_1,
                            vec![Case::new(
                                Constructor::Int("4".into()),
                                Vec::new(),
                                Decision::Guard(42, rhs(1), Box::new(success(2)),)
                            )],
                            Some(Box::new(success_with_bindings(
                                vec![("a", var_1), ("b", var_2)],
                                3
                            )))
                        )
                    )],
                    Some(Box::new(success_with_bindings(
                        vec![("a", var_1), ("b", var_2)],
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
    let mut setup = Setup::new();
    let typ = setup.new_type(Type::Int);
    let input = setup.new_variable(typ);
    let discard = setup.discard();
    let rules = vec![
        (setup.int("1"), rhs(1)),
        (setup.assign("it", discard), rhs(2)),
    ];
    let var_0 = setup.var(0, typ);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![Case::new(
                Constructor::Int("1".into()),
                Vec::new(),
                success(1)
            )],
            Some(Box::new(success_with_bindings(vec![("it", var_0)], 2)))
        )
    );
}

#[test]
fn nonexhaustive_string_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::String);
    let input = setup.new_variable(int_type);
    let rules = vec![
        (setup.string("Hello"), rhs(1)),
        (setup.string("Goodbye"), rhs(2)),
    ];
    let result = setup.compile(input, rules);

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
fn exhaustive_string_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::String);
    let input = setup.new_variable(int_type);
    let rules = vec![
        (setup.string("Hello"), rhs(1)),
        (setup.string("Goodbye"), rhs(2)),
        (setup.bind("a"), rhs(3)),
    ];
    let result = setup.compile(input, rules);

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

#[test]
fn nonexhaustive_bit_string_pattern() {
    let mut setup = Setup::new();
    let bit_string_type = setup.new_type(Type::BitString);
    let input = setup.new_variable(bit_string_type);
    let rules = vec![
        (setup.bit_string("1"), rhs(1)),
        (setup.bit_string("2"), rhs(2)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::BitString, Vec::new(), success(1)),
                Case::new(Constructor::BitString, Vec::new(), success(2)),
            ],
            Some(Box::new(failure()))
        )
    );
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("_")]);
}

#[test]
fn exhaustive_bit_string_pattern() {
    let mut setup = Setup::new();
    let bit_string_type = setup.new_type(Type::BitString);
    let input = setup.new_variable(bit_string_type);
    let rules = vec![
        (setup.bit_string("1"), rhs(1)),
        (setup.bit_string("2"), rhs(2)),
        (setup.bind("a"), rhs(3)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::BitString, Vec::new(), success(1)),
                Case::new(Constructor::BitString, Vec::new(), success(2)),
            ],
            Some(Box::new(success_with_bindings(vec![("a", input)], 3)))
        )
    );
}

#[test]
fn exhaustive_list_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let list_type = setup.new_type(Type::List(int_type));
    let input = setup.new_variable(list_type);
    let rules = vec![(setup.empty_list(), rhs(1)), (setup.bind("a"), rhs(2))];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::List {
            variable: input,
            empty: Box::new(success(1)),
            non_empty: Box::new(NonEmptyListDecision {
                first: Variable {
                    id: 1,
                    type_id: int_type
                },
                rest: Variable {
                    id: 2,
                    type_id: list_type
                },
                decision: success_with_bindings(vec![("a", input)], 2)
            })
        }
    );
}

#[test]
fn exhaustive_custom_list_empty() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let list_type = setup.new_type(Type::Int);
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, list_type);
    setup.compiler.types[list_type.0] = Type::Variants(vec![
        ("Empty".into(), Vec::new()),
        ("NonEmpty".into(), vec![int_type, list_type]),
    ]);
    let input = setup.new_variable(list_type);
    let rules = vec![
        (setup.variant(list_type, 0, vec![]), rhs(1)),
        (setup.discard(), rhs(2)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::Variant(list_type, 0), vec![], success(1)),
                Case::new(
                    Constructor::Variant(list_type, 1),
                    vec![var_1, var_2],
                    success_with_bindings(vec![], 2),
                ),
            ],
            None
        )
    );
}
#[test]
fn exhaustive_list_just_empty_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let list_type = setup.new_type(Type::List(int_type));
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, list_type);
    let input = setup.new_variable(list_type);
    let rules = vec![(setup.empty_list(), rhs(1)), (setup.discard(), rhs(2))];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::List {
            variable: input,
            empty: Box::new(success(1)),
            non_empty: Box::new(NonEmptyListDecision {
                first: Variable {
                    id: 1,
                    type_id: int_type
                },
                rest: Variable {
                    id: 2,
                    type_id: list_type
                },
                decision: success(2)
            })
        }
    );
}

#[test]
fn exhaustive_custom_list_non_empty() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let list_type = setup.new_type(Type::Int);
    let discard = setup.discard();
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, list_type);
    setup.compiler.types[list_type.0] = Type::Variants(vec![
        ("Empty".into(), Vec::new()),
        ("NonEmpty".into(), vec![int_type, list_type]),
    ]);
    let rules = vec![
        (setup.variant(list_type, 1, vec![discard, discard]), rhs(1)),
        (discard, rhs(2)),
    ];
    let input = setup.new_variable(list_type);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::Variant(list_type, 0), vec![], success(2)),
                Case::new(
                    Constructor::Variant(list_type, 1),
                    vec![var_1, var_2],
                    success(1),
                ),
            ],
            None
        )
    );
}

#[test]
fn exhaustive_list_non_empty() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let list_type = setup.new_type(Type::List(int_type));
    let discard = setup.discard();
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, list_type);
    let rules = vec![(setup.list(discard, discard), rhs(1)), (discard, rhs(2))];
    let input = setup.new_variable(list_type);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::List {
            variable: input,
            empty: Box::new(success(2)),
            non_empty: Box::new(NonEmptyListDecision {
                first: Variable {
                    id: 1,
                    type_id: int_type
                },
                rest: Variable {
                    id: 2,
                    type_id: list_type
                },
                decision: success(1)
            })
        }
    );
}

#[test]
fn nonexhaustive_list_empty() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let list_type = setup.new_type(Type::List(int_type));
    let discard = setup.discard();
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, list_type);
    let rules = vec![(setup.list(discard, discard), rhs(1))];
    let input = setup.new_variable(list_type);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::List {
            variable: input,
            empty: Box::new(failure()),
            non_empty: Box::new(NonEmptyListDecision {
                first: Variable {
                    id: 1,
                    type_id: int_type
                },
                rest: Variable {
                    id: 2,
                    type_id: list_type
                },
                decision: success(1)
            })
        }
    );
    assert!(result.diagnostics.missing);
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("[]")]);
}

#[test]
fn nonexhaustive_list_at_least_one() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let list_type = setup.new_type(Type::List(int_type));
    let discard = setup.discard();
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, list_type);
    let rules = vec![(setup.empty_list(), rhs(1))];
    let input = setup.new_variable(list_type);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::List {
            variable: input,
            empty: Box::new(success(1)),
            non_empty: Box::new(NonEmptyListDecision {
                first: Variable {
                    id: 1,
                    type_id: int_type
                },
                rest: Variable {
                    id: 2,
                    type_id: list_type
                },
                decision: failure()
            })
        }
    );
    assert!(result.diagnostics.missing);
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("[_, ..]")]);
}

#[test]
fn nonexhaustive_list_exactly_one() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let list_type = setup.new_type(Type::List(int_type));
    let discard = setup.discard();
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, list_type);
    let empty_pattern = setup.empty_list();
    let at_least_one_pattern = setup.list(discard, discard);
    let rules = vec![
        (empty_pattern, rhs(1)),
        (setup.list(discard, at_least_one_pattern), rhs(2)),
    ];
    let input = setup.new_variable(list_type);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::List {
            variable: input,
            empty: Box::new(success(1)),
            non_empty: Box::new(NonEmptyListDecision {
                first: Variable {
                    id: 1,
                    type_id: int_type
                },
                rest: Variable {
                    id: 2,
                    type_id: list_type
                },
                decision: Decision::List {
                    variable: Variable {
                        id: 2,
                        type_id: list_type
                    },
                    empty: Box::new(failure()),
                    non_empty: Box::new(NonEmptyListDecision {
                        first: Variable {
                            id: 3,
                            type_id: int_type
                        },
                        rest: Variable {
                            id: 4,
                            type_id: list_type
                        },
                        decision: success(2)
                    })
                }
            })
        }
    );
    assert!(result.diagnostics.missing);
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("[_]")]);
}

#[test]
fn nonexhaustive_list_at_least_two() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::Int);
    let list_type = setup.new_type(Type::List(int_type));
    let discard = setup.discard();
    let var_1 = setup.var(1, int_type);
    let var_2 = setup.var(2, list_type);
    let empty_pattern = setup.empty_list();
    let one_pattern = setup.list(discard, empty_pattern);
    let two_pattern = setup.list(discard, one_pattern);
    let rules = vec![
        (empty_pattern, rhs(1)),
        (one_pattern, rhs(2)),
        (two_pattern, rhs(3)),
    ];
    let input = setup.new_variable(list_type);
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::List {
            variable: input,
            empty: Box::new(success(1)),
            non_empty: Box::new(NonEmptyListDecision {
                first: Variable {
                    id: 1,
                    type_id: int_type
                },
                rest: Variable {
                    id: 2,
                    type_id: list_type
                },
                decision: Decision::List {
                    variable: Variable {
                        id: 2,
                        type_id: list_type
                    },
                    empty: Box::new(success(2)),
                    non_empty: Box::new(NonEmptyListDecision {
                        first: Variable {
                            id: 3,
                            type_id: int_type
                        },
                        rest: Variable {
                            id: 4,
                            type_id: list_type
                        },
                        decision: Decision::List {
                            variable: Variable {
                                id: 4,
                                type_id: list_type
                            },
                            empty: Box::new(success(3)),
                            non_empty: Box::new(NonEmptyListDecision {
                                first: Variable {
                                    id: 5,
                                    type_id: int_type
                                },
                                rest: Variable {
                                    id: 6,
                                    type_id: list_type
                                },
                                decision: failure()
                            })
                        }
                    })
                }
            })
        }
    );
    assert!(result.diagnostics.missing);
    assert_eq!(
        result.missing_patterns(),
        vec![SmolStr::new("[_, _, _, ..]")]
    );
}

#[test]
fn nonexhaustive_string_prefix_discard_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::String);
    let input = setup.new_variable(int_type);
    let rules = vec![
        (setup.string_prefix_discard("Hello"), rhs(1)),
        (setup.string_prefix_discard("Goodbye"), rhs(2)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::StringPrefix, Vec::new(), success(1)),
                Case::new(Constructor::StringPrefix, Vec::new(), success(2)),
            ],
            Some(Box::new(failure()))
        )
    );
    assert_eq!(result.missing_patterns(), vec![SmolStr::new("_")]);
}

#[test]
fn exhaustive_string_prefix_discard_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::String);
    let input = setup.new_variable(int_type);
    let rules = vec![
        (setup.string_prefix_discard("Hello"), rhs(1)),
        (setup.string("Goodbye"), rhs(2)),
        (setup.bind("a"), rhs(3)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(Constructor::StringPrefix, Vec::new(), success(1)),
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

#[test]
fn exhaustive_string_prefix_bind_pattern() {
    let mut setup = Setup::new();
    let int_type = setup.new_type(Type::String);
    let input = setup.new_variable(int_type);
    let var_1 = setup.var(0, int_type);
    let rules = vec![
        (setup.string_prefix_bind("Hello", "a"), rhs(1)),
        (setup.string("Goodbye"), rhs(2)),
        (setup.bind("a"), rhs(3)),
    ];
    let result = setup.compile(input, rules);

    assert_eq!(
        result.tree,
        Decision::Switch(
            input,
            vec![
                Case::new(
                    Constructor::StringPrefix,
                    Vec::new(),
                    success_with_bindings(vec![("a", var_1)], 1)
                ),
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
