use super::*;
use crate::{
    ast::*,
    line_numbers::LineNumbers,
    pretty::*,
    type_::{
        FieldMap, ModuleValueConstructor, PatternConstructor, ValueConstructor,
        ValueConstructorVariant,
    },
};

static RECORD_KEY: &str = "type";
static TMP_VAR: &str = "gleam$tmp";

#[derive(Debug)]
pub struct Generator<'module> {
    module_name: &'module [String],
    line_numbers: &'module LineNumbers,
    function_name: &'module str,
    current_scope_vars: im::HashMap<String, usize>,
    tail_position: bool,
    // We register whether float division is used within an expression so that
    // the module generator can output a suitable function if it is needed.
    float_division_used: &'module mut bool,
    object_equality_used: &'module mut bool,
}

impl<'module> Generator<'module> {
    pub fn new(
        module_name: &'module [String],
        line_numbers: &'module LineNumbers,
        function_name: &'module str,
        float_division_used: &'module mut bool,
        object_equality_used: &'module mut bool,
    ) -> Self {
        Self {
            module_name,
            line_numbers,
            function_name,
            current_scope_vars: Default::default(),
            tail_position: true,
            float_division_used,
            object_equality_used,
        }
    }

    fn local_var_name<'a>(&mut self, name: &'a str) -> Document<'a> {
        match self.current_scope_vars.get(name) {
            None => {
                let _ = self.current_scope_vars.insert(name.to_string(), 0);
                name.to_doc()
            }
            Some(0) => name.to_doc(),
            Some(n) => {
                Document::String(format!("{}${}", name, n))
            }
        }
    }

    fn next_local_var_name<'a>(&mut self, name: &'a str) -> Document<'a> {
        let next = self.current_scope_vars.get(name).map_or(0, |i| i + 1);
        let _ = self.current_scope_vars.insert(name.to_string(), next);
        self.local_var_name(name)
    }

    pub fn expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        let document = match expression {
            TypedExpr::String { value, .. } => Ok(string(value)),

            TypedExpr::Int { value, .. } => Ok(int(value)),
            TypedExpr::Float { value, .. } => Ok(float(value)),

            TypedExpr::List { elements, tail, .. } => {
                let inner = tail
                    .as_ref()
                    .map(|t| self.not_in_tail_position(|gen| gen.wrap_expression(t)))
                    .unwrap_or(Ok("[]".to_doc()))?;
                let renderer =
                    |element| self.not_in_tail_position(|gen| gen.wrap_expression(element));
                wrap_list(elements, inner, renderer)
            }

            TypedExpr::Tuple { elems, .. } => self.tuple(elems),
            TypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            TypedExpr::Case { .. } => unsupported("Case"),

            TypedExpr::Call { fun, args, .. } => self.call(fun, args),
            TypedExpr::Fn { args, body, .. } => self.fun(args, body),

            TypedExpr::RecordAccess { record, label, .. } => self.record_access(record, label),
            TypedExpr::RecordUpdate { spread, args, .. } => self.record_update(spread, args),

            TypedExpr::Var {
                name, constructor, ..
            } => self.variable(name, constructor),

            TypedExpr::Sequence { expressions, .. } => self.sequence(expressions),

            TypedExpr::Assignment { value, pattern, .. } => self.let_(value, pattern),

            TypedExpr::Try { .. } => unsupported("Try"),

            TypedExpr::BinOp {
                name, left, right, ..
            } => self.bin_op(name, left, right),

            TypedExpr::Todo {
                label, location, ..
            } => Ok(self.todo(label, location)),

            TypedExpr::BitString { .. } => unsupported("Bitstring"),

            TypedExpr::Pipe { .. } => unsupported("Pipe"),

            TypedExpr::ModuleSelect {
                module_name,
                label,
                constructor,
                ..
            } => self.module_select(module_name, label, constructor),
        }?;
        Ok(match expression {
            TypedExpr::Sequence { .. } | TypedExpr::Assignment { .. } => document,
            _ => match self.tail_position {
                true => docvec!["return ", document, ";"],
                _ => document,
            },
        })
    }

    fn not_in_tail_position<'a, CompileFn>(&mut self, compile: CompileFn) -> Output<'a>
    where
        CompileFn: Fn(&mut Self) -> Output<'a>,
    {
        let tail = self.tail_position;
        self.tail_position = false;
        let result = compile(self);
        self.tail_position = tail;
        result
    }

    /// Wrap an expression in an immediately involked function expression if
    /// required due to being a JS statement
    fn wrap_expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::Sequence { .. } | TypedExpr::Assignment { .. } => {
                self.immediately_involked_function_expression(expression)
            }
            _ => self.expression(expression),
        }
    }

    /// Wrap an expression in an immediately involked function expression
    fn immediately_involked_function_expression<'a>(
        &mut self,
        expression: &'a TypedExpr,
    ) -> Output<'a> {
        let tail = self.tail_position;
        self.tail_position = true;
        let current_scope_vars = self.current_scope_vars.clone();
        let result = self.expression(expression);
        self.tail_position = tail;
        self.current_scope_vars = current_scope_vars;
        Ok(docvec!(
            docvec!("(() => {", break_("", " "), result?)
                .nest(INDENT)
                .group(),
            break_("", " "),
            "})()",
        ))
    }

    fn variable<'a>(&mut self, name: &'a str, constructor: &'a ValueConstructor) -> Output<'a> {
        match &constructor.variant {
            ValueConstructorVariant::Record { name, .. }
                if constructor.type_.is_bool() && name == "True" =>
            {
                Ok("true".to_doc())
            }
            ValueConstructorVariant::Record { name, .. }
                if constructor.type_.is_bool() && name == "False" =>
            {
                Ok("false".to_doc())
            }
            ValueConstructorVariant::Record { .. } if constructor.type_.is_nil() => {
                Ok("undefined".to_doc())
            }
            ValueConstructorVariant::Record { name, arity: 0, .. } => {
                let record_type = name.to_doc().surround("\"", "\"");
                let record_head = (RECORD_KEY.to_doc(), Some(record_type));
                Ok(wrap_object(std::iter::once(record_head)))
            }
            ValueConstructorVariant::Record {
                name,
                arity,
                field_map,
                ..
            } => {
                let vars = (0..*arity)
                    .into_iter()
                    .map(|i| Document::String(format!("var{}", i)));

                let body = docvec![
                    "return ",
                    construct_record(name, *arity, field_map, vars.clone().into_iter()),
                    ";"
                ];

                Ok(docvec!(
                    docvec!(wrap_args(vars), " => {", break_("", " "), body,)
                        .nest(INDENT)
                        .append(break_("", " "))
                        .group(),
                    "}",
                ))
            }
            ValueConstructorVariant::LocalVariable => Ok(self.local_var_name(name)),
            ValueConstructorVariant::ModuleFn { .. } => Ok(name.to_doc()),
            _ => unsupported("Referencing variables"),
        }
    }

    fn sequence<'a>(&mut self, expressions: &'a [TypedExpr]) -> Output<'a> {
        let count = expressions.len();
        let mut documents = Vec::with_capacity(count * 3);
        documents.push(force_break());
        for (i, expression) in expressions.iter().enumerate() {
            if i + 1 < count {
                documents.push(self.not_in_tail_position(|gen| gen.expression(expression))?);
                match expression {
                    // Assignment generates multiple lines of JS and so handles it's own ;
                    TypedExpr::Assignment { .. } => (),
                    _ => documents.push(";".to_doc()),
                }
                documents.push(line());
            } else {
                documents.push(self.expression(expression)?);
            }
        }
        Ok(documents.to_doc())
    }

    fn let_<'a>(&mut self, value: &'a TypedExpr, pattern: &'a TypedPattern) -> Output<'a> {
        // Render the value before printing patterns as patterns might update local variable state.
        let value = self.not_in_tail_position(|gen| gen.wrap_expression(value))?;
        match pattern {
            // Return early don't use gleam$temp if single assignment
            Pattern::Var { name, .. } => Ok(docvec![
                "let ",
                self.next_local_var_name(name),
                " = ",
                value,
                ";"
            ]),
            _ => {
                let mut checks = vec![];
                let mut path = vec![];
                let mut assignments = vec![];

                let tmp_var = self.next_local_var_name(TMP_VAR);

                let () = self.traverse_pattern(
                    pattern,
                    tmp_var.clone(),
                    &mut path,
                    &mut checks,
                    &mut assignments,
                )?;

                let check_line = match checks.is_empty() {
                    true => "".to_doc(),
                    false => docvec![
                        "if (!(",
                        docvec![
                            break_("", ""),
                            Itertools::intersperse(checks.into_iter(), break_(" &&", " && "))
                                .collect::<Vec<_>>()
                                .to_doc(),
                        ]
                        .nest(INDENT),
                        break_("", ""),
                        ")) throw new Error(\"Bad match\");",
                        line()
                    ]
                    .group(),
                };

                Ok(docvec![
                    "let ",
                    tmp_var,
                    " = ",
                    value,
                    ";",
                    line(),
                    check_line,
                    assignments
                ])
            }
        }
    }

    fn traverse_pattern<'a>(
        &mut self,
        pattern: &'a TypedPattern,
        tmp_var: Document<'a>,
        path: &mut Vec<Document<'a>>,
        checks: &mut Vec<Document<'a>>,
        assignments: &mut Vec<Document<'a>>,
    ) -> Result<(), Error> {
        match pattern {
            Pattern::String { value, .. } => {
                checks.push(equality(tmp_var, &path, string(value)));
                Ok(())
            }
            Pattern::Int { value, .. } => {
                checks.push(equality(tmp_var, &path, int(value)));
                Ok(())
            }
            Pattern::Float { value, .. } => {
                checks.push(equality(tmp_var, &path, float(value)));
                Ok(())
            }
            Pattern::Var { name, .. } => {
                assignments.push(docvec![
                    "let ",
                    self.next_local_var_name(name),
                    " = ",
                    tmp_var,
                    concat(path.into_iter().map(|i| i.clone().surround("[", "]"))),
                    ";",
                    line()
                ]);
                Ok(())
            }
            Pattern::Discard { .. } => Ok(()),
            Pattern::Assign { name, pattern, .. } => {
                assignments.push(docvec![
                    "let ",
                    self.next_local_var_name(name),
                    " = ",
                    tmp_var.clone(),
                    concat(
                        path.clone()
                            .into_iter()
                            .map(|i| i.clone().surround("[", "]"))
                    ),
                    ";",
                    line()
                ]);
                self.traverse_pattern(pattern, tmp_var, path, checks, assignments)
            }

            Pattern::List { elements, tail, .. } => {
                let path_string = concat(path.into_iter().map(|i| i.clone().surround("[", "]")));
                let length_check = match tail {
                    Some(_) => "?.length !== undefined".to_doc(),
                    None => "?.length === 0".to_doc(),
                };
                checks.push(docvec![
                    tmp_var.clone(),
                    path_string,
                    Document::String("?.[1]".repeat(elements.len())),
                    length_check,
                ]);
                let _ = elements
                    .into_iter()
                    .enumerate()
                    .map(|x| {
                        let (index, pattern) = x;
                        let mut path = path.clone();
                        for _ in 0..index {
                            path.push("1".to_doc());
                        }
                        path.push("0".to_doc());

                        self.traverse_pattern(
                            pattern,
                            tmp_var.clone(),
                            &mut path,
                            checks,
                            assignments,
                        )
                    })
                    .collect::<Result<Vec<()>, Error>>()?;
                if let Some(pattern) = tail {
                    let mut path = path.clone();
                    for _ in 0..elements.len() {
                        path.push("1".to_doc());
                    }
                    self.traverse_pattern(pattern, tmp_var, &mut path, checks, assignments)?
                }
                Ok(())
            }
            Pattern::Tuple { elems, .. } => {
                // We don't check the length, because type system means it's a tuple
                let _ = elems
                    .into_iter()
                    .enumerate()
                    .map(|x| {
                        let (index, pattern) = x;
                        let mut path = path.clone();
                        path.push(Document::String(format!("{}", index)));
                        self.traverse_pattern(
                            pattern,
                            tmp_var.clone(),
                            &mut path,
                            checks,
                            assignments,
                        )
                    })
                    .collect::<Result<Vec<()>, Error>>()?;
                Ok(())
            }
            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if name == "True" => {
                checks.push(equality(tmp_var, &path, "true".to_doc()));
                Ok(())
            }
            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if name == "False" => {
                checks.push(equality(tmp_var, &path, "false".to_doc()));
                Ok(())
            }
            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if name == "Nil" => {
                checks.push(equality(tmp_var, &path, "undefined".to_doc()));
                Ok(())
            }
            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, field_map },
                arguments,
                ..
            } => {
                let mut type_path = path.clone();
                type_path.push(string("type"));
                checks.push(equality(tmp_var.clone(), &type_path, string(name)));

                let _ = arguments
                    .iter()
                    .enumerate()
                    .map(|x| {
                        let (index, arg) = x;
                        let mut path = path.clone();
                        match field_map {
                            None => path.push(Document::String(format!("{}", index))),
                            Some(FieldMap { fields, .. }) => {
                                let label = fields.iter().find_map(|(key, &val)| {
                                    if val == index {
                                        Some(key)
                                    } else {
                                        None
                                    }
                                });
                                path.push(string(label.unwrap()))
                            }
                        }
                        self.traverse_pattern(
                            &arg.value,
                            tmp_var.clone(),
                            &mut path,
                            checks,
                            assignments,
                        )
                    })
                    .collect::<Result<Vec<()>, Error>>()?;
                Ok(())
            }

            Pattern::VarUsage { .. } | Pattern::BitString { .. } => {
                unimplemented!("BitString matching not supported in JS backend")
            }
        }?;
        Ok(())
    }

    fn tuple<'a>(&mut self, elements: &'a [TypedExpr]) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            array(elements.iter().map(|element| gen.wrap_expression(element)))
        })
    }

    fn call<'a>(&mut self, fun: &'a TypedExpr, arguments: &'a [CallArg<TypedExpr>]) -> Output<'a> {
        match fun {
            // Short circuit creation of a record so the rendered JS creates the record inline.
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant:
                            ValueConstructorVariant::Record {
                                name,
                                arity,
                                field_map,
                                ..
                            },
                        ..
                    },
                ..
            } => {
                let tail = self.tail_position;
                self.tail_position = false;
                let arguments = arguments
                    .iter()
                    .map(|element| self.wrap_expression(&element.value))
                    .collect::<Result<Vec<_>, _>>()?;
                self.tail_position = tail;

                Ok(construct_record(
                    name,
                    *arity,
                    field_map,
                    arguments.into_iter(),
                ))
            }
            _ => {
                let fun = self.not_in_tail_position(|gen| gen.expression(fun))?;
                let arguments = self.not_in_tail_position(|gen| {
                    call_arguments(
                        arguments
                            .iter()
                            .map(|element| gen.wrap_expression(&element.value)),
                    )
                })?;
                Ok(docvec![fun, arguments])
            }
        }
    }

    fn fun<'a>(&mut self, arguments: &'a [TypedArg], body: &'a TypedExpr) -> Output<'a> {
        let tail = self.tail_position;
        self.tail_position = true;
        let result = self.expression(body);
        self.tail_position = tail;
        Ok(docvec!(
            docvec!(fun_args(arguments), " => {", break_("", " "), result?,)
                .nest(INDENT)
                .append(break_("", " "))
                .group(),
            "}",
        ))
    }

    fn record_access<'a>(&mut self, record: &'a TypedExpr, label: &'a str) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            let record = gen.wrap_expression(record)?;
            Ok(docvec![record, ".", label])
        })
    }

    fn record_update<'a>(
        &mut self,
        spread: &'a TypedExpr,
        updates: &'a [TypedRecordUpdateArg],
    ) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            let spread = gen.wrap_expression(spread)?;
            let updates: Vec<(Document<'a>, Option<Document<'a>>)> = updates
                .iter()
                .map(|TypedRecordUpdateArg { label, value, .. }| {
                    Ok((label.to_doc(), Some(gen.wrap_expression(value)?)))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let assign_args = vec!["{}".to_doc(), spread, wrap_object(updates.into_iter())];
            Ok(docvec!["Object.assign", wrap_args(assign_args.into_iter())])
        })
    }

    fn tuple_index<'a>(&mut self, tuple: &'a TypedExpr, index: u64) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            let tuple = gen.wrap_expression(tuple)?;
            Ok(docvec![tuple, Document::String(format!("[{}]", index))])
        })
    }

    // TODO: handle precedence rules
    fn bin_op<'a>(
        &mut self,
        name: &'a BinOp,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;
        match name {
            BinOp::And => self.print_bin_op(left, right, "&&"),
            BinOp::Or => self.print_bin_op(left, right, "||"),
            BinOp::LtInt | BinOp::LtFloat => self.print_bin_op(left, right, "<"),
            BinOp::LtEqInt | BinOp::LtEqFloat => self.print_bin_op(left, right, "<="),
            BinOp::Eq => {
                use std::iter::once;
                *self.object_equality_used = true;
                Ok(docvec!(
                    "$deepEqual",
                    wrap_args(once(left).chain(once(right)))
                ))
            }
            BinOp::NotEq => {
                use std::iter::once;
                *self.object_equality_used = true;
                Ok(docvec!(
                    "!$deepEqual",
                    wrap_args(once(left).chain(once(right)))
                ))
            }
            BinOp::GtInt | BinOp::GtFloat => self.print_bin_op(left, right, ">"),
            BinOp::GtEqInt | BinOp::GtEqFloat => self.print_bin_op(left, right, ">="),
            BinOp::AddInt | BinOp::AddFloat => self.print_bin_op(left, right, "+"),
            BinOp::SubInt | BinOp::SubFloat => self.print_bin_op(left, right, "-"),
            BinOp::MultInt | BinOp::MultFloat => self.print_bin_op(left, right, "*"),
            BinOp::DivInt => Ok(self.print_bin_op(left, right, "/")?.append(" | 0")),
            BinOp::ModuloInt => self.print_bin_op(left, right, "%"),
            BinOp::DivFloat => {
                use std::iter::once;
                *self.float_division_used = true;
                Ok(docvec!("$divide", wrap_args(once(left).chain(once(right)))))
            }
        }
    }

    fn print_bin_op<'a>(
        &mut self,
        left: Document<'a>,
        right: Document<'a>,
        op: &'a str,
    ) -> Output<'a> {
        Ok(left
            .append(" ")
            .append(op.to_doc())
            .append(" ")
            .append(right))
    }

    fn todo<'a>(&mut self, message: &'a Option<String>, location: &'a SrcSpan) -> Document<'a> {
        self.tail_position = false;
        let gleam_error = "todo";
        let message = message
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or_else(|| "This has not yet been implemented");
        let module_name = Document::String(self.module_name.join("_"));
        let line = self.line_numbers.line_number(location.start);

        docvec![
            "throw Object.assign",
            wrap_args(
                vec![
                    docvec!["new Error", wrap_args(std::iter::once(string(message)))],
                    wrap_object(
                        vec![
                            ("gleam_error".to_doc(), Some(string(gleam_error))),
                            ("module".to_doc(), Some(module_name.surround("\"", "\""))),
                            (
                                "function".to_doc(),
                                Some(
                                    // TODO switch to use `string(self.function_name)`
                                    // This will require resolving the difference in lifetimes 'module and 'a.
                                    Document::String(self.function_name.to_string())
                                        .surround("\"", "\"")
                                )
                            ),
                            ("line".to_doc(), Some(line.to_doc())),
                        ]
                        .into_iter()
                    )
                ]
                .into_iter()
            )
        ]
    }
    fn module_select<'a>(
        &mut self,
        module_name: &'a Vec<String>,
        label: &'a String,
        constructor: &'a ModuleValueConstructor,
    ) -> Output<'a> {
        match constructor {
            ModuleValueConstructor::Fn | ModuleValueConstructor::Constant { .. } => {
                Ok(docvec![Document::String(module_name.join("_")), ".", label,])
            }
            _ => unsupported("Module function call"),
        }
    }
}

fn equality<'a>(
    tmp_var: Document<'a>,
    path: &Vec<Document<'a>>,
    to_match: Document<'a>,
) -> Document<'a> {
    let path_string = concat(path.into_iter().map(|i| i.clone().surround("[", "]")));
    docvec![tmp_var, path_string, " === ", to_match]
}

fn int(value: &str) -> Document<'_> {
    value.to_doc()
}

fn float(value: &str) -> Document<'_> {
    value.to_doc()
}
pub fn constant_expression<'a>(expression: &'a TypedConstant) -> Output<'a> {
    match expression {
        Constant::Int { value, .. } => Ok(int(value)),
        Constant::Float { value, .. } => Ok(float(value)),
        Constant::String { value, .. } => Ok(string(&value.as_str())),
        Constant::Tuple { elements, .. } => array(elements.iter().map(|e| constant_expression(&e))),
        Constant::List { elements, .. } => wrap_list(elements, "[]".to_doc(), constant_expression),
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "True" => {
            Ok("true".to_doc())
        }
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "False" => {
            Ok("false".to_doc())
        }
        Constant::Record { typ, .. } if typ.is_nil() => Ok("undefined".to_doc()),
        Constant::Record {
            tag,
            args,
            field_map,
            ..
        } => {
            let field_values: Result<Vec<_>, _> = args
                .iter()
                .map(|arg| constant_expression(&arg.value))
                .collect();
            Ok(construct_record(
                tag,
                args.len(),
                &field_map,
                field_values?.into_iter(),
            ))
        }
        Constant::BitString { .. } => unsupported("BitString as constant"),
    }
}

fn string(value: &str) -> Document<'_> {
    value.to_doc().surround("\"", "\"")
}

fn array<'a, Elements: Iterator<Item = Output<'a>>>(elements: Elements) -> Output<'a> {
    let elements = Itertools::intersperse(elements, Ok(break_(",", ", ")))
        .collect::<Result<Vec<_>, _>>()?
        .to_doc();
    Ok(docvec![
        "[",
        docvec![break_("", ""), elements].nest(INDENT),
        break_(",", ""),
        "]"
    ]
    .group())
}

fn wrap_list<'a, E>(
    elements: &'a [E],
    inner: Document<'a>,
    mut renderer: impl FnMut(&'a E) -> Output<'a>,
) -> Output<'a> {
    match elements.split_last() {
        Some((element, rest)) => {
            let inner = docvec![
                "[",
                docvec![renderer(element)?, break_(",", ", "), inner,]
                    .nest(INDENT)
                    .group(),
                "]",
            ];
            wrap_list(rest, inner, renderer)
        }
        None => Ok(inner),
    }
}

fn call_arguments<'a, Elements: Iterator<Item = Output<'a>>>(elements: Elements) -> Output<'a> {
    let elements = Itertools::intersperse(elements, Ok(break_(",", ", ")))
        .collect::<Result<Vec<_>, _>>()?
        .to_doc();
    Ok(docvec![
        "(",
        docvec![break_("", ""), elements].nest(INDENT),
        break_(",", ""),
        ")"
    ]
    .group())
}

fn construct_record<'a>(
    name: &'a str,
    arity: usize,
    field_map: &'a Option<FieldMap>,
    values: impl Iterator<Item = Document<'a>>,
) -> Document<'a> {
    let field_names: Vec<Document<'_>> = match field_map {
        Some(FieldMap { fields, .. }) => fields
            .iter()
            .sorted_by_key(|(_, &v)| v)
            .map(|x| x.0.as_str().to_doc())
            .collect(),
        None => (0..arity)
            .into_iter()
            .map(|i| Document::String(format!("{}", i)))
            .collect(),
    };

    let record_head = (
        RECORD_KEY.to_doc(),
        Some(name.to_doc().surround("\"", "\"")),
    );

    let record_values = field_names
        .into_iter()
        .zip(values)
        .map(|(name, value)| (name, Some(value)));

    wrap_object(std::iter::once(record_head).chain(record_values))
}
