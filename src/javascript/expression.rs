use super::*;
use crate::{
    ast::*,
    line_numbers::LineNumbers,
    pretty::*,
    type_::{FieldMap, ModuleValueConstructor, ValueConstructor, ValueConstructorVariant},
};

static RECORD_KEY: &str = "type";

#[derive(Debug)]
pub struct Generator<'module> {
    module_name: &'module [String],
    line_numbers: &'module LineNumbers,
    function_name: &'module str,
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
            tail_position: true,
            float_division_used,
            object_equality_used,
        }
    }

    pub fn expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        let document = match expression {
            TypedExpr::String { value, .. } => Ok(string(value)),

            TypedExpr::Int { value, .. } => Ok(int(value)),
            TypedExpr::Float { value, .. } => Ok(float(value)),

            TypedExpr::List { .. } => unsupported("List"),

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

            TypedExpr::Seq { first, then, .. } => self.sequence(first, then),

            TypedExpr::Assignment { .. } => unsupported("Assigning variables"),

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
            TypedExpr::Seq { .. } | TypedExpr::Assignment { .. } => document,
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
            TypedExpr::Seq { .. } | TypedExpr::Assignment { .. } => {
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
        let result = self.expression(expression);
        self.tail_position = tail;
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
            ValueConstructorVariant::LocalVariable => Ok(name.to_doc()),
            ValueConstructorVariant::ModuleFn { .. } => Ok(name.to_doc()),
            _ => unsupported("Referencing variables"),
        }
    }

    fn sequence<'a>(&mut self, first: &'a TypedExpr, then: &'a TypedExpr) -> Output<'a> {
        let first = self.not_in_tail_position(|gen| gen.expression(first))?;
        let then = self.expression(then)?;
        Ok(docvec![force_break(), first, ";", line(), then])
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
        Constant::List { .. } => unsupported("List as constant"),
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "True" => {
            Ok("true".to_doc())
        }
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "False" => {
            Ok("false".to_doc())
        }
        Constant::Record { typ, .. } if typ.is_nil() => Ok("undefined".to_doc()),
        Constant::Record { tag, args, .. } => {
            let field_values: Result<Vec<_>, _> = args
                .iter()
                .map(|arg| constant_expression(&arg.value))
                .collect();
            Ok(construct_record(
                tag,
                args.len(),
                &None,
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
