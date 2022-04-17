#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    docvec,
    io::Utf8Writer,
    parse::extra::Comment,
    pretty::*,
    type_::{self, Type},
    Error, Result,
};
use itertools::Itertools;
use std::{path::Path, sync::Arc};
use vec1::Vec1;

const INDENT: isize = 2;

pub fn pretty(writer: &mut impl Utf8Writer, src: &str, path: &Path) -> Result<()> {
    let (module, extra) = crate::parse::parse_module(src).map_err(|error| Error::Parse {
        path: path.to_path_buf(),
        src: src.to_string(),
        error,
    })?;
    let intermediate = Intermediate {
        comments: extra
            .comments
            .iter()
            .map(|span| Comment::from((span, src)))
            .collect(),
        doc_comments: extra
            .doc_comments
            .iter()
            .map(|span| Comment::from((span, src)))
            .collect(),
        empty_lines: &extra.empty_lines,
        module_comments: extra
            .module_comments
            .iter()
            .map(|span| Comment::from((span, src)))
            .collect(),
    };

    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, writer)
}

struct Intermediate<'a> {
    comments: Vec<Comment<'a>>,
    doc_comments: Vec<Comment<'a>>,
    module_comments: Vec<Comment<'a>>,
    empty_lines: &'a [usize],
}

/// Hayleigh's bane
#[derive(Debug, Clone, Default)]
pub struct Formatter<'a> {
    comments: &'a [Comment<'a>],
    doc_comments: &'a [Comment<'a>],
    module_comments: &'a [Comment<'a>],
    empty_lines: &'a [usize],
}

impl<'comments> Formatter<'comments> {
    pub fn new() -> Self {
        Default::default()
    }

    fn with_comments(extra: &'comments Intermediate<'comments>) -> Self {
        Self {
            comments: &extra.comments,
            doc_comments: &extra.doc_comments,
            module_comments: &extra.module_comments,
            empty_lines: extra.empty_lines,
        }
    }

    // Pop comments that occur before a byte-index in the source
    fn pop_comments(&mut self, limit: usize) -> impl Iterator<Item = &'comments str> {
        let (popped, rest) = comments_before(self.comments, limit);
        self.comments = rest;
        popped
    }

    // Pop doc comments that occur before a byte-index in the source
    fn pop_doc_comments(&mut self, limit: usize) -> impl Iterator<Item = &'comments str> {
        let (popped, rest) = comments_before(self.doc_comments, limit);
        self.doc_comments = rest;
        popped
    }

    fn pop_empty_lines(&mut self, limit: usize) -> bool {
        let mut end = 0;
        for (i, &postition) in self.empty_lines.iter().enumerate() {
            if postition > limit {
                break;
            }
            end = i + 1;
        }

        self.empty_lines = self
            .empty_lines
            .get(end..)
            .expect("Pop empty lines slicing");
        end != 0
    }

    fn target_group<'a>(&mut self, target_group: &'a TargetGroup) -> Document<'a> {
        let mut has_imports = false;
        let mut has_declarations = false;
        let mut imports = Vec::new();
        let mut declarations = Vec::with_capacity(target_group.len());

        for statement in target_group.statements_ref() {
            let start = statement.location().start;
            match statement {
                Statement::Import { .. } => {
                    has_imports = true;
                    let comments = self.pop_comments(start);
                    let statement = self.statement(statement);
                    imports.push(commented(statement, comments))
                }

                _other => {
                    has_declarations = true;
                    let comments = self.pop_comments(start);
                    let declaration = self.documented_statement(statement);
                    declarations.push(commented(declaration, comments))
                }
            }
        }

        let imports = concat(Itertools::intersperse(imports.into_iter(), line()));
        let declarations = concat(Itertools::intersperse(declarations.into_iter(), lines(2)));

        let sep = if has_imports && has_declarations {
            lines(2)
        } else {
            nil()
        };

        match target_group {
            TargetGroup::Any(_) => docvec![imports, sep, declarations],
            TargetGroup::Only(target, _) => docvec![
                "if ",
                Document::String(target.to_string()),
                " {",
                docvec![line(), imports, sep, declarations].nest(INDENT),
                line(),
                "}"
            ],
        }
    }

    fn module<'a>(&mut self, module: &'a UntypedModule) -> Document<'a> {
        let groups = concat(Itertools::intersperse(
            module.statements.iter().map(|t| self.target_group(t)),
            lines(2),
        ));

        let doc_comments = concat(self.doc_comments.iter().map(|comment| {
            line()
                .append("///")
                .append(Document::String(comment.content.to_string()))
        }));
        let comments = concat(self.comments.iter().map(|comment| {
            line()
                .append("//")
                .append(Document::String(comment.content.to_string()))
        }));

        let module_comments = if !self.module_comments.is_empty() {
            let comments = self.module_comments.iter().map(|s| {
                "////"
                    .to_doc()
                    .append(Document::String(s.content.to_string()))
                    .append(line())
            });
            concat(comments).append(line())
        } else {
            nil()
        };

        module_comments
            .append(groups)
            .append(doc_comments)
            .append(comments)
            .append(line())
    }

    fn statement<'a>(&mut self, statement: &'a UntypedStatement) -> Document<'a> {
        match statement {
            Statement::Fn {
                name,
                arguments: args,
                body,
                public,
                return_annotation,
                end_position,
                ..
            } => self.statement_fn(public, name, args, return_annotation, body, *end_position),

            Statement::TypeAlias {
                alias,
                parameters: args,
                type_ast: resolved_type,
                public,
                ..
            } => self.type_alias(*public, alias, args, resolved_type),

            Statement::CustomType {
                name,
                parameters,
                public,
                constructors,
                location,
                opaque,
                ..
            } => self.custom_type(*public, *opaque, name, parameters, constructors, location),

            Statement::ExternalFn {
                public,
                arguments: args,
                name,
                return_: retrn,
                module,
                fun,
                ..
            } => self
                .external_fn_signature(*public, name, args, retrn)
                .append(" =")
                .append(line())
                .append("  \"")
                .append(module.as_str())
                .append("\" \"")
                .append(fun.as_str())
                .append("\""),

            Statement::ExternalType {
                public,
                name,
                arguments: args,
                ..
            } => self.external_type(*public, name, args),

            Statement::Import {
                module,
                as_name,
                unqualified,
                ..
            } => "import "
                .to_doc()
                .append(Document::String(module.join("/")))
                .append(if unqualified.is_empty() {
                    nil()
                } else {
                    let unqualified = Itertools::intersperse(
                        unqualified
                            .iter()
                            .sorted_by(|a, b| a.name.cmp(&b.name))
                            .map(|e| e.to_doc()),
                        break_(",", ", ").flex_break(),
                    );
                    let unqualified = break_("", "")
                        .append(concat(unqualified))
                        .nest(INDENT)
                        .append(break_(",", ""))
                        .group();
                    ".{".to_doc().append(unqualified).append("}")
                })
                .append(if let Some(name) = as_name {
                    docvec![" as ", name]
                } else {
                    nil()
                }),

            Statement::ModuleConstant {
                public,
                name,
                annotation,
                value,
                ..
            } => {
                let head = pub_(*public).append("const ").append(name.as_str());
                let head = match annotation {
                    None => head,
                    Some(t) => head.append(": ").append(self.type_ast(t)),
                };
                head.append(" = ").append(self.const_expr(value))
            }
        }
    }

    fn const_expr<'a, A, B>(&mut self, value: &'a Constant<A, B>) -> Document<'a> {
        match value {
            Constant::Int { value, .. } | Constant::Float { value, .. } => value.to_doc(),

            Constant::String { value, .. } => value.to_doc().surround("\"", "\""),

            Constant::List { elements, .. } => {
                let comma: fn() -> Document<'a> = if elements.iter().all(Constant::is_simple) {
                    || break_(",", ", ").flex_break()
                } else {
                    || break_(",", ", ")
                };
                let elements =
                    Itertools::intersperse(elements.iter().map(|e| self.const_expr(e)), comma());
                list(concat(elements), None)
            }

            Constant::Tuple { elements, .. } => "#"
                .to_doc()
                .append(wrap_args(elements.iter().map(|e| self.const_expr(e))))
                .group(),

            Constant::BitString { segments, .. } => bit_string(
                segments
                    .iter()
                    .map(|s| bit_string_segment(s, |e| self.const_expr(e))),
                segments.iter().all(|s| s.value.is_simple()),
            ),

            Constant::Record {
                name,
                args,
                module: None,
                ..
            } if args.is_empty() => name.to_doc(),

            Constant::Record {
                name,
                args,
                module: Some(m),
                ..
            } if args.is_empty() => m.to_doc().append(".").append(name.as_str()),

            Constant::Record {
                name,
                args,
                module: None,
                ..
            } => name
                .to_doc()
                .append(wrap_args(args.iter().map(|a| self.constant_call_arg(a))))
                .group(),

            Constant::Record {
                name,
                args,
                module: Some(m),
                ..
            } => m
                .to_doc()
                .append(".")
                .append(name.as_str())
                .append(wrap_args(args.iter().map(|a| self.constant_call_arg(a))))
                .group(),
        }
    }

    pub fn docs_const_expr<'a>(
        &mut self,
        public: bool,
        name: &'a str,
        value: &'a TypedConstant,
    ) -> Document<'a> {
        let mut printer = type_::pretty::Printer::new();

        pub_(public)
            .append("const ")
            .append(name)
            .append(": ")
            .append(printer.print(&value.type_()))
            .append(" = ")
            .append(self.const_expr(value))
    }

    fn documented_statement<'a>(&mut self, s: &'a UntypedStatement) -> Document<'a> {
        let comments = self.doc_comments(s.location().start);
        comments.append(self.statement(s)).group()
    }

    fn doc_comments<'a>(&mut self, limit: usize) -> Document<'a> {
        let mut comments = self.pop_doc_comments(limit).peekable();
        match comments.peek() {
            None => nil(),
            Some(_) => concat(Itertools::intersperse(
                comments.map(|c| "///".to_doc().append(Document::String(c.to_string()))),
                line(),
            ))
            .append(force_break())
            .append(line()),
        }
    }

    fn type_ast_constructor<'a>(
        &mut self,
        module: &'a Option<String>,
        name: &'a str,
        args: &'a [TypeAst],
    ) -> Document<'a> {
        let head = module
            .as_ref()
            .map(|qualifier| qualifier.to_doc().append(".").append(name))
            .unwrap_or_else(|| name.to_doc());

        if args.is_empty() {
            head
        } else {
            head.append(self.type_arguments(args))
        }
    }

    fn type_ast<'a>(&mut self, t: &'a TypeAst) -> Document<'a> {
        match t {
            TypeAst::Hole { name, .. } => name.to_doc(),

            TypeAst::Constructor {
                name,
                arguments: args,
                module,
                ..
            } => self.type_ast_constructor(module, name, args),

            TypeAst::Fn {
                arguments: args,
                return_: retrn,
                ..
            } => "fn"
                .to_doc()
                .append(self.type_arguments(args))
                .group()
                .append(" ->")
                .append(break_("", " ").append(self.type_ast(retrn)).nest(INDENT)),

            TypeAst::Var { name, .. } => name.to_doc(),

            TypeAst::Tuple { elems, .. } => "#".to_doc().append(self.type_arguments(elems)),
        }
        .group()
    }

    fn type_arguments<'a>(&mut self, args: &'a [TypeAst]) -> Document<'a> {
        wrap_args(args.iter().map(|t| self.type_ast(t)))
    }

    pub fn type_alias<'a>(
        &mut self,
        public: bool,
        name: &'a str,
        args: &'a [String],
        typ: &'a TypeAst,
    ) -> Document<'a> {
        let head = pub_(public).append("type ").append(name);

        let head = if args.is_empty() {
            head
        } else {
            head.append(wrap_args(args.iter().map(|e| e.to_doc())).group())
        };

        head.append(" =")
            .append(line().append(self.type_ast(typ)).group().nest(INDENT))
    }

    fn fn_arg<'a, A>(&mut self, arg: &'a Arg<A>) -> Document<'a> {
        let comments = self.pop_comments(arg.location.start);
        let doc = match &arg.annotation {
            None => arg.names.to_doc(),
            Some(a) => arg.names.to_doc().append(": ").append(self.type_ast(a)),
        }
        .group();
        commented(doc, comments)
    }

    fn statement_fn<'a>(
        &mut self,
        public: &'a bool,
        name: &'a str,
        args: &'a [UntypedArg],
        return_annotation: &'a Option<TypeAst>,
        body: &'a UntypedExpr,
        end_location: usize,
    ) -> Document<'a> {
        // Fn name and args
        let head = pub_(*public)
            .append("fn ")
            .append(name)
            .append(wrap_args(args.iter().map(|e| self.fn_arg(e))));

        // Add return annotation
        let head = match return_annotation {
            Some(anno) => head.append(" -> ").append(self.type_ast(anno)),
            None => head,
        }
        .group();

        // Format body
        let body = self.expr(body);

        // Add any trailing comments
        let body = match printed_comments(self.pop_comments(end_location)) {
            Some(comments) => body.append(line()).append(comments),
            None => body,
        };

        // Stick it all together
        head.append(" {")
            .append(line().append(body).nest(INDENT).group())
            .append(line())
            .append("}")
    }

    pub fn external_fn_signature<'a, A>(
        &mut self,
        public: bool,
        name: &'a str,
        args: &'a [ExternalFnArg<A>],
        retrn: &'a TypeAst,
    ) -> Document<'a> {
        pub_(public)
            .to_doc()
            .append("external fn ")
            .append(name)
            .append(self.external_fn_args(args))
            .append(" -> ".to_doc().append(self.type_ast(retrn)))
            .group()
    }

    fn expr_fn<'a>(
        &mut self,
        args: &'a [UntypedArg],
        return_annotation: Option<&'a TypeAst>,
        body: &'a UntypedExpr,
    ) -> Document<'a> {
        let args = wrap_args(args.iter().map(|e| self.fn_arg(e))).group();
        let body = match body {
            UntypedExpr::Case { .. } => force_break().append(self.expr(body)),
            _ => self.expr(body),
        };

        let header = "fn".to_doc().append(args);

        let header = match return_annotation {
            None => header,
            Some(t) => header.append(" -> ").append(self.type_ast(t)),
        };

        header
            .append(
                break_(" {", " { ")
                    .append(body)
                    .nest(INDENT)
                    .append(break_("", " "))
                    .append("}"),
            )
            .group()
    }

    fn sequence<'a>(&mut self, expressions: &'a [UntypedExpr]) -> Document<'a> {
        let count = expressions.len();
        let mut documents = Vec::with_capacity(count * 2);
        documents.push(force_break());
        for (i, expression) in expressions.iter().enumerate() {
            let preceeding_newline = self.pop_empty_lines(expression.start_byte_index());
            if i != 0 && preceeding_newline {
                documents.push(lines(2));
            } else if i != 0 {
                documents.push(lines(1));
            }
            documents.push(self.expr(expression).group());
        }
        documents.to_doc()
    }

    fn assignment<'a>(
        &mut self,
        pattern: &'a UntypedPattern,
        value: &'a UntypedExpr,
        then: Option<&'a UntypedExpr>,
        kind: Option<AssignmentKind>,
        annotation: &'a Option<TypeAst>,
    ) -> Document<'a> {
        let _ = self.pop_empty_lines(pattern.location().end);

        let keyword = match kind {
            Some(AssignmentKind::Let) => "let ",
            Some(AssignmentKind::Assert) => "assert ",
            None => "try ",
        };

        let pattern = self.pattern(pattern);

        let annotation = annotation
            .as_ref()
            .map(|a| ": ".to_doc().append(self.type_ast(a)));

        let doc = if then.is_some() {
            force_break().append(keyword)
        } else {
            keyword.to_doc()
        }
        .append(pattern.append(annotation).group())
        .append(" =")
        .append(self.assigned_value(value));

        if let Some(then) = then {
            doc.append(if self.pop_empty_lines(then.start_byte_index()) {
                lines(2)
            } else {
                line()
            })
            .append(self.expr(then))
        } else {
            doc
        }
    }

    fn expr<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        let comments = self.pop_comments(expr.start_byte_index());

        let document = match expr {
            UntypedExpr::Todo { label: None, .. } => "todo".to_doc(),

            UntypedExpr::Todo { label: Some(l), .. } => docvec!["todo(\"", l, "\")"],

            UntypedExpr::PipeLine { expressions, .. } => self.pipeline(expressions),

            UntypedExpr::Int { value, .. } => value.to_doc(),

            UntypedExpr::Float { value, .. } => value.to_doc(),

            UntypedExpr::String { value, .. } => value.to_doc().surround("\"", "\""),

            UntypedExpr::Sequence { expressions, .. } => self.sequence(expressions),

            UntypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => "_".to_doc(),

            UntypedExpr::Var { name, .. } => name.to_doc(),

            UntypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            UntypedExpr::Negate { value, .. } => self.negate(value),

            UntypedExpr::Fn {
                is_capture: true,
                body,
                ..
            } => self.fn_capture(body),

            UntypedExpr::Fn {
                return_annotation,
                arguments: args,
                body,
                ..
            } => self.expr_fn(args, return_annotation.as_ref(), body),

            UntypedExpr::List { elements, tail, .. } => self.list(elements, tail.as_deref()),

            UntypedExpr::Call {
                fun,
                arguments: args,
                ..
            } => self.call(fun, args),

            UntypedExpr::BinOp {
                name, left, right, ..
            } => self.bin_op(name, left, right),

            UntypedExpr::Assignment {
                value,
                pattern,
                annotation,
                kind,
                ..
            } => self.assignment(pattern, value, None, Some(*kind), annotation),

            UntypedExpr::Try {
                value,
                pattern,
                annotation,
                then,
                ..
            } => self.assignment(pattern, value, Some(then), None, annotation),

            UntypedExpr::Case {
                subjects, clauses, ..
            } => self.case(subjects, clauses),

            UntypedExpr::FieldAccess {
                label, container, ..
            } => self.expr(container).append(".").append(label.as_str()),

            UntypedExpr::Tuple { elems, .. } => "#"
                .to_doc()
                .append(wrap_args(elems.iter().map(|e| self.wrap_expr(e))))
                .group(),

            UntypedExpr::BitString { segments, .. } => bit_string(
                segments
                    .iter()
                    .map(|s| bit_string_segment(s, |e| self.expr(e))),
                segments.iter().all(|s| s.value.is_simple_constant()),
            ),
            UntypedExpr::RecordUpdate {
                constructor,
                spread,
                arguments: args,
                ..
            } => self.record_update(constructor, spread, args),
        };
        commented(document, comments)
    }

    fn pattern_constructor<'a>(
        &mut self,
        name: &'a str,
        args: &'a [CallArg<UntypedPattern>],
        module: &'a Option<String>,
        with_spread: bool,
    ) -> Document<'a> {
        fn is_breakable(expr: &UntypedPattern) -> bool {
            match expr {
                Pattern::Tuple { .. } | Pattern::List { .. } | Pattern::BitString { .. } => true,
                Pattern::Constructor {
                    arguments: args, ..
                } => !args.is_empty(),
                _ => false,
            }
        }

        let name = match module {
            Some(m) => m.to_doc().append(".").append(name),
            None => name.to_doc(),
        };

        if args.is_empty() && with_spread {
            name.append("(..)")
        } else if args.is_empty() {
            name
        } else if with_spread {
            name.append(wrap_args_with_spread(
                args.iter().map(|a| self.pattern_call_arg(a)),
            ))
        } else {
            match args {
                [arg] if is_breakable(&arg.value) => name
                    .append("(")
                    .append(self.pattern_call_arg(arg))
                    .append(")")
                    .group(),

                _ => name
                    .append(wrap_args(args.iter().map(|a| self.pattern_call_arg(a))))
                    .group(),
            }
        }
    }

    fn call<'a>(&mut self, fun: &'a UntypedExpr, args: &'a [CallArg<UntypedExpr>]) -> Document<'a> {
        match args {
            [arg] if is_breakable_expr(&arg.value) => self
                .expr(fun)
                .append("(")
                .append(self.call_arg(arg))
                .append(")")
                .group(),

            _ => self
                .expr(fun)
                .append(wrap_args(args.iter().map(|a| self.call_arg(a))))
                .group(),
        }
    }

    pub fn case<'a>(
        &mut self,
        subjects: &'a [UntypedExpr],
        clauses: &'a [UntypedClause],
    ) -> Document<'a> {
        let subjects_doc = concat(Itertools::intersperse(
            subjects.iter().map(|s| self.expr(s)),
            ", ".to_doc(),
        ));

        let clauses_doc = concat(clauses.iter().enumerate().map(|(i, c)| self.clause(c, i)));

        force_break()
            .append("case ")
            .append(subjects_doc)
            .append(" {")
            .append(
                line()
                    .append(force_break())
                    .append(clauses_doc)
                    .nest(INDENT),
            )
            .append(line())
            .append("}")
    }

    pub fn record_update<'a>(
        &mut self,
        constructor: &'a UntypedExpr,
        spread: &'a RecordUpdateSpread,
        args: &'a [UntypedRecordUpdateArg],
    ) -> Document<'a> {
        use std::iter::once;
        let constructor_doc = self.expr(constructor);
        let spread_doc = "..".to_doc().append(self.expr(&spread.base));
        let arg_docs = args.iter().map(|a| self.record_update_arg(a));
        let all_arg_docs = once(spread_doc).chain(arg_docs);
        constructor_doc.append(wrap_args(all_arg_docs)).group()
    }

    pub fn bin_op<'a>(
        &mut self,
        name: &'a BinOp,
        left: &'a UntypedExpr,
        right: &'a UntypedExpr,
    ) -> Document<'a> {
        let precedence = name.precedence();
        let left_precedence = left.binop_precedence();
        let right_precedence = right.binop_precedence();
        let left = self.expr(left);
        let right = self.expr(right);
        self.operator_side(left, precedence, left_precedence)
            .append(name)
            .append(self.operator_side(right, precedence, right_precedence - 1))
    }

    pub fn operator_side<'a>(&mut self, doc: Document<'a>, op: u8, side: u8) -> Document<'a> {
        if op > side {
            break_("{", "{ ")
                .append(doc)
                .nest(INDENT)
                .append(break_("", " "))
                .append("}")
                .group()
        } else {
            doc
        }
    }

    fn pipeline<'a>(&mut self, expressions: &'a Vec1<UntypedExpr>) -> Document<'a> {
        let mut docs = vec![force_break(); expressions.len() * 3];
        let first = expressions.first();
        let first_precedence = first.binop_precedence();
        let first = self.wrap_expr(first);
        docs.push(self.operator_side(first, 5, first_precedence));

        for expr in expressions.iter().skip(1) {
            let comments = self.pop_comments(expr.location().start);
            let doc = match expr {
                UntypedExpr::Fn {
                    is_capture: true,
                    body,
                    ..
                } => self.pipe_capture_right_hand_side(body),

                _ => self.wrap_expr(expr),
            };
            docs.push(line());
            docs.push(commented("|> ".to_doc(), comments));
            docs.push(self.operator_side(doc, 4, expr.binop_precedence()));
        }

        docs.to_doc()
    }

    fn pipe_capture_right_hand_side<'a>(&mut self, fun: &'a UntypedExpr) -> Document<'a> {
        let (fun, args) = match fun {
            UntypedExpr::Call {
                fun,
                arguments: args,
                ..
            } => (fun, args),
            _ => panic!("Function capture found not to have a function call body when formatting"),
        };

        let hole_in_first_position = matches!(
            args.first(),
            Some(CallArg {
                value: UntypedExpr::Var { name, .. },
                ..
            }) if name == CAPTURE_VARIABLE
        );

        if hole_in_first_position && args.len() == 1 {
            // x |> fun(_)
            self.expr(fun)
        } else if hole_in_first_position {
            // x |> fun(_, 2, 3)
            self.expr(fun)
                .append(wrap_args(args.iter().skip(1).map(|a| self.call_arg(a))).group())
        } else {
            // x |> fun(1, _, 3)
            self.expr(fun)
                .append(wrap_args(args.iter().map(|a| self.call_arg(a))).group())
        }
    }

    fn fn_capture<'a>(&mut self, call: &'a UntypedExpr) -> Document<'a> {
        match call {
            UntypedExpr::Call {
                fun,
                arguments: args,
                ..
            } => match args.as_slice() {
                [first, second] if is_breakable_expr(&second.value) && first.is_capture_hole() => {
                    self.expr(fun)
                        .append("(_, ")
                        .append(self.call_arg(second))
                        .append(")")
                        .group()
                }

                _ => self
                    .expr(fun)
                    .append(wrap_args(args.iter().map(|a| self.call_arg(a))).group()),
            },

            // The body of a capture being not a fn shouldn't be possible...
            _ => panic!("Function capture body found not to be a call in the formatter",),
        }
    }

    pub fn record_constructor<'a, A>(
        &mut self,
        constructor: &'a RecordConstructor<A>,
    ) -> Document<'a> {
        let comments = self.pop_comments(constructor.location.start);
        let doc_comments = self.doc_comments(constructor.location.start);

        let doc = if constructor.arguments.is_empty() {
            constructor.name.to_doc()
        } else {
            constructor
                .name
                .to_doc()
                .append(wrap_args(constructor.arguments.iter().map(
                    |RecordConstructorArg {
                         label,
                         ast,
                         location,
                         ..
                     }| {
                        let arg_comments = self.pop_comments(location.start);
                        let arg = match label {
                            Some(l) => l.to_doc().append(": ").append(self.type_ast(ast)),
                            None => self.type_ast(ast),
                        };

                        commented(
                            self.doc_comments(location.start).append(arg).group(),
                            arg_comments,
                        )
                    },
                )))
                .group()
        };

        commented(doc_comments.append(doc).group(), comments)
    }

    pub fn custom_type<'a, A>(
        &mut self,
        public: bool,
        opaque: bool,
        name: &'a str,
        args: &'a [String],
        constructors: &'a [RecordConstructor<A>],
        location: &'a SrcSpan,
    ) -> Document<'a> {
        let _ = self.pop_empty_lines(location.start);
        pub_(public)
            .to_doc()
            .append(if opaque { "opaque type " } else { "type " })
            .append(if args.is_empty() {
                name.to_doc()
            } else {
                name.to_doc()
                    .append(wrap_args(args.iter().map(|e| e.to_doc())))
                    .group()
            })
            .append(" {")
            .append(concat(constructors.iter().map(|c| {
                if self.pop_empty_lines(c.location.start) {
                    lines(2)
                } else {
                    line()
                }
                .append(self.record_constructor(c))
                .nest(INDENT)
                .group()
            })))
            .append(line())
            .append("}")
    }

    pub fn docs_opaque_custom_type<'a>(
        &mut self,
        public: bool,
        name: &'a str,
        args: &'a [String],
        location: &'a SrcSpan,
    ) -> Document<'a> {
        let _ = self.pop_empty_lines(location.start);
        pub_(public)
            .to_doc()
            .append("opaque type ")
            .append(if args.is_empty() {
                name.to_doc()
            } else {
                name.to_doc()
                    .append(wrap_args(args.iter().map(|e| e.to_doc())))
            })
    }

    pub fn docs_fn_signature<'a>(
        &mut self,
        public: bool,
        name: &'a str,
        args: &'a [TypedArg],
        return_type: Arc<Type>,
    ) -> Document<'a> {
        let mut printer = type_::pretty::Printer::new();

        pub_(public)
            .append("fn ")
            .append(name)
            .append(self.docs_fn_args(args, &mut printer))
            .append(" -> ".to_doc())
            .append(printer.print(&return_type))
    }

    // Will always print the types, even if they were implicit in the original source
    pub fn docs_fn_args<'a>(
        &mut self,
        args: &'a [TypedArg],
        printer: &mut type_::pretty::Printer,
    ) -> Document<'a> {
        wrap_args(args.iter().map(|arg| {
            arg.names
                .to_doc()
                .append(": ".to_doc().append(printer.print(&arg.type_)))
                .group()
        }))
    }

    fn external_fn_arg<'a, A>(&mut self, arg: &'a ExternalFnArg<A>) -> Document<'a> {
        let comments = self.pop_comments(arg.location.start);
        let doc = label(&arg.label).append(self.type_ast(&arg.annotation));
        commented(doc.group(), comments)
    }

    fn external_fn_args<'a, A>(&mut self, args: &'a [ExternalFnArg<A>]) -> Document<'a> {
        wrap_args(args.iter().map(|e| self.external_fn_arg(e)))
    }

    fn wrap_expr<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::Sequence { .. }
            | UntypedExpr::Assignment { .. }
            | UntypedExpr::Try { .. } => "{"
                .to_doc()
                .append(force_break())
                .append(line().append(self.expr(expr)).nest(INDENT))
                .append(line())
                .append("}"),

            _ => self.expr(expr),
        }
    }

    fn call_arg<'a>(&mut self, arg: &'a CallArg<UntypedExpr>) -> Document<'a> {
        match &arg.label {
            Some(s) => commented(
                s.to_doc().append(": "),
                self.pop_comments(arg.location.start),
            ),
            None => nil(),
        }
        .append(self.wrap_expr(&arg.value))
    }

    fn record_update_arg<'a>(&mut self, arg: &'a UntypedRecordUpdateArg) -> Document<'a> {
        arg.label
            .to_doc()
            .append(": ")
            .append(self.wrap_expr(&arg.value))
    }

    fn tuple_index<'a>(&mut self, tuple: &'a UntypedExpr, index: u64) -> Document<'a> {
        match tuple {
            UntypedExpr::TupleIndex { .. } => self.expr(tuple).surround("{", "}"),
            _ => self.expr(tuple),
        }
        .append(".")
        .append(index)
    }

    fn case_clause_value<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::Try { .. }
            | UntypedExpr::Sequence { .. }
            | UntypedExpr::Assignment { .. } => " {"
                .to_doc()
                .append(line().append(self.expr(expr)).nest(INDENT).group())
                .append(line())
                .append(force_break())
                .append("}"),

            UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::BitString { .. } => " ".to_doc().append(self.expr(expr)).group(),

            UntypedExpr::Case { .. } => line().append(self.expr(expr)).nest(INDENT).group(),

            _ => break_("", " ").append(self.expr(expr)).nest(INDENT).group(),
        }
    }

    fn assigned_value<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::Case { .. } => " ".to_doc().append(self.expr(expr)).group(),
            _ => self.case_clause_value(expr),
        }
    }

    fn clause<'a>(&mut self, clause: &'a UntypedClause, index: usize) -> Document<'a> {
        let space_before = self.pop_empty_lines(clause.location.start);
        let after_position = clause.location.end;
        let clause_doc = concat(Itertools::intersperse(
            std::iter::once(&clause.pattern)
                .chain(&clause.alternative_patterns)
                .map(|p| {
                    concat(Itertools::intersperse(
                        p.iter().map(|p| self.pattern(p)),
                        ", ".to_doc(),
                    ))
                }),
            " | ".to_doc(),
        ));
        let clause_doc = match &clause.guard {
            None => clause_doc,
            Some(guard) => clause_doc.append(" if ").append(self.clause_guard(guard)),
        };

        // Remove any unused empty lines within the clause
        let _ = self.pop_empty_lines(after_position);

        if index == 0 {
            clause_doc
        } else if space_before {
            lines(2).append(clause_doc)
        } else {
            lines(1).append(clause_doc)
        }
        .append(" ->")
        .append(self.case_clause_value(&clause.then))
    }

    pub fn external_type<'a>(
        &mut self,
        public: bool,
        name: &'a str,
        args: &'a [String],
    ) -> Document<'a> {
        pub_(public)
            .append("external type ")
            .append(name)
            .append(if args.is_empty() {
                nil()
            } else {
                wrap_args(args.iter().map(|e| e.to_doc()))
            })
    }

    fn list<'a>(
        &mut self,
        elements: &'a [UntypedExpr],
        tail: Option<&'a UntypedExpr>,
    ) -> Document<'a> {
        let comma: fn() -> Document<'a> =
            if tail.is_none() && elements.iter().all(UntypedExpr::is_simple_constant) {
                || break_(",", ", ").flex_break()
            } else {
                || break_(",", ", ")
            };
        let elements = concat(Itertools::intersperse(
            elements.iter().map(|e| self.wrap_expr(e)),
            comma(),
        ));
        let tail = tail.map(|e| self.expr(e));
        list(elements, tail)
    }

    fn pattern<'a>(&mut self, pattern: &'a UntypedPattern) -> Document<'a> {
        let comments = self.pop_comments(pattern.location().start);
        let doc = match pattern {
            Pattern::Int { value, .. } => value.to_doc(),

            Pattern::Float { value, .. } => value.to_doc(),

            Pattern::String { value, .. } => value.to_doc().surround("\"", "\""),

            Pattern::Var { name, .. } => name.to_doc(),

            Pattern::VarUsage { name, .. } => name.to_doc(),

            Pattern::Assign { name, pattern, .. } => {
                self.pattern(pattern).append(" as ").append(name.as_str())
            }

            Pattern::Discard { name, .. } => name.to_doc(),

            Pattern::List { elements, tail, .. } => {
                let elements = concat(Itertools::intersperse(
                    elements.iter().map(|e| self.pattern(e)),
                    break_(",", ", "),
                ));
                let tail = tail.as_ref().map(|e| {
                    if e.is_discard() {
                        nil()
                    } else {
                        self.pattern(e)
                    }
                });
                list(elements, tail)
            }

            Pattern::Constructor {
                name,
                arguments: args,
                module,
                with_spread,
                ..
            } => self.pattern_constructor(name, args, module, *with_spread),

            Pattern::Tuple { elems, .. } => "#"
                .to_doc()
                .append(wrap_args(elems.iter().map(|e| self.pattern(e))))
                .group(),

            Pattern::BitString { segments, .. } => bit_string(
                segments
                    .iter()
                    .map(|s| bit_string_segment(s, |e| self.pattern(e))),
                false,
            ),
        };
        commented(doc, comments)
    }

    fn pattern_call_arg<'a>(&mut self, arg: &'a CallArg<UntypedPattern>) -> Document<'a> {
        arg.label
            .as_ref()
            .map(|s| s.to_doc().append(": "))
            .unwrap_or_else(nil)
            .append(self.pattern(&arg.value))
    }

    fn clause_guard<'a>(&mut self, clause_guard: &'a UntypedClauseGuard) -> Document<'a> {
        match clause_guard {
            ClauseGuard::And { left, right, .. } => self
                .clause_guard(left)
                .append(" && ")
                .append(self.clause_guard(right)),

            ClauseGuard::Or { left, right, .. } => self
                .clause_guard(left)
                .append(" || ")
                .append(self.clause_guard(right)),

            ClauseGuard::Equals { left, right, .. } => self
                .clause_guard(left)
                .append(" == ")
                .append(self.clause_guard(right)),

            ClauseGuard::NotEquals { left, right, .. } => self
                .clause_guard(left)
                .append(" != ")
                .append(self.clause_guard(right)),

            ClauseGuard::GtInt { left, right, .. } => self
                .clause_guard(left)
                .append(" > ")
                .append(self.clause_guard(right)),

            ClauseGuard::GtEqInt { left, right, .. } => self
                .clause_guard(left)
                .append(" >= ")
                .append(self.clause_guard(right)),

            ClauseGuard::LtInt { left, right, .. } => self
                .clause_guard(left)
                .append(" < ")
                .append(self.clause_guard(right)),

            ClauseGuard::LtEqInt { left, right, .. } => self
                .clause_guard(left)
                .append(" <= ")
                .append(self.clause_guard(right)),

            ClauseGuard::GtFloat { left, right, .. } => self
                .clause_guard(left)
                .append(" >. ")
                .append(self.clause_guard(right)),

            ClauseGuard::GtEqFloat { left, right, .. } => self
                .clause_guard(left)
                .append(" >=. ")
                .append(self.clause_guard(right)),

            ClauseGuard::LtFloat { left, right, .. } => self
                .clause_guard(left)
                .append(" <. ")
                .append(self.clause_guard(right)),

            ClauseGuard::LtEqFloat { left, right, .. } => self
                .clause_guard(left)
                .append(" <=. ")
                .append(self.clause_guard(right)),

            ClauseGuard::Var { name, .. } => name.to_doc(),

            ClauseGuard::TupleIndex { tuple, index, .. } => {
                self.clause_guard(tuple).append(".").append(*index).to_doc()
            }

            ClauseGuard::Constant(constant) => self.const_expr(constant),
        }
    }

    fn constant_call_arg<'a, A, B>(&mut self, arg: &'a CallArg<Constant<A, B>>) -> Document<'a> {
        match &arg.label {
            None => self.const_expr(&arg.value),
            Some(s) => s.to_doc().append(": ").append(self.const_expr(&arg.value)),
        }
    }

    fn negate<'a>(&mut self, value: &'a UntypedExpr) -> Document<'a> {
        docvec!["!", self.wrap_expr(value)]
    }
}

impl<'a> Documentable<'a> for &'a ArgNames {
    fn to_doc(self) -> Document<'a> {
        match self {
            ArgNames::Named { name } | ArgNames::Discard { name } => name.to_doc(),
            ArgNames::LabelledDiscard { label, name } | ArgNames::NamedLabelled { label, name } => {
                docvec![label, " ", name]
            }
        }
    }
}

fn pub_(public: bool) -> Document<'static> {
    if public {
        "pub ".to_doc()
    } else {
        nil()
    }
}

impl<'a> Documentable<'a> for &'a UnqualifiedImport {
    fn to_doc(self) -> Document<'a> {
        self.name.to_doc().append(match &self.as_name {
            None => nil(),
            Some(s) => " as ".to_doc().append(s.as_str()),
        })
    }
}

fn label(label: &Option<String>) -> Document<'_> {
    match label {
        Some(s) => Document::Str(s).append(": "),
        None => nil(),
    }
}

impl<'a> Documentable<'a> for &'a BinOp {
    fn to_doc(self) -> Document<'a> {
        match self {
            BinOp::And => " && ",
            BinOp::Or => " || ",
            BinOp::LtInt => " < ",
            BinOp::LtEqInt => " <= ",
            BinOp::LtFloat => " <. ",
            BinOp::LtEqFloat => " <=. ",
            BinOp::Eq => " == ",
            BinOp::NotEq => " != ",
            BinOp::GtEqInt => " >= ",
            BinOp::GtInt => " > ",
            BinOp::GtEqFloat => " >=. ",
            BinOp::GtFloat => " >. ",
            BinOp::AddInt => " + ",
            BinOp::AddFloat => " +. ",
            BinOp::SubInt => " - ",
            BinOp::SubFloat => " -. ",
            BinOp::MultInt => " * ",
            BinOp::MultFloat => " *. ",
            BinOp::DivInt => " / ",
            BinOp::DivFloat => " /. ",
            BinOp::ModuloInt => " % ",
        }
        .to_doc()
    }
}

pub fn wrap_args<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    let mut args = args.into_iter().peekable();
    if args.peek().is_none() {
        return "()".to_doc();
    }
    break_("(", "(")
        .append(concat(Itertools::intersperse(args, break_(",", ", "))))
        .nest(INDENT)
        .append(break_(",", ""))
        .append(")")
}

pub fn wrap_args_with_spread<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    let mut args = args.into_iter().peekable();
    if args.peek().is_none() {
        return "()".to_doc();
    }

    break_("(", "(")
        .append(concat(Itertools::intersperse(args, break_(",", ", "))))
        .append(break_(",", ", "))
        .append("..")
        .nest(INDENT)
        .append(break_(",", ""))
        .append(")")
        .group()
}

fn bit_string<'a>(
    segments: impl IntoIterator<Item = Document<'a>>,
    is_simple: bool,
) -> Document<'a> {
    let comma = if is_simple {
        break_(",", ", ").flex_break()
    } else {
        break_(",", ", ")
    };
    break_("<<", "<<")
        .append(concat(Itertools::intersperse(segments.into_iter(), comma)))
        .nest(INDENT)
        .append(break_(",", ""))
        .append(">>")
        .group()
}

fn list<'a>(elems: Document<'a>, tail: Option<Document<'a>>) -> Document<'a> {
    let doc = break_("[", "[").append(elems);

    match tail {
        None => doc.nest(INDENT).append(break_(",", "")),

        // Don't print tail if it is a discard
        Some(Document::String(t)) if t == *"_" => doc
            .append(break_(",", ", "))
            .append("..")
            .nest(INDENT)
            .append(break_("", "")),

        Some(final_tail) => doc
            .append(break_(",", ", "))
            .append("..")
            .append(final_tail)
            .nest(INDENT)
            .append(break_("", "")),
    }
    .append("]")
    .group()
}

fn printed_comments<'a, 'comments>(
    comments: impl IntoIterator<Item = &'comments str>,
) -> Option<Document<'a>> {
    let mut comments = comments.into_iter().peekable();
    let _ = comments.peek()?;
    Some(concat(Itertools::intersperse(
        comments.map(|c| "//".to_doc().append(Document::String(c.to_string()))),
        line(),
    )))
}

fn commented<'a, 'comments>(
    doc: Document<'a>,
    comments: impl IntoIterator<Item = &'comments str>,
) -> Document<'a> {
    match printed_comments(comments) {
        Some(comments) => comments.append(force_break()).append(line()).append(doc),
        None => doc,
    }
}

fn bit_string_segment<Value, Type, ToDoc>(
    segment: &BitStringSegment<Value, Type>,
    mut to_doc: ToDoc,
) -> Document<'_>
where
    ToDoc: FnMut(&Value) -> Document<'_>,
{
    match segment {
        BitStringSegment { value, options, .. } if options.is_empty() => to_doc(value),

        BitStringSegment { value, options, .. } => {
            to_doc(value)
                .append(":")
                .append(concat(Itertools::intersperse(
                    options.iter().map(|o| segment_option(o, |e| to_doc(e))),
                    "-".to_doc(),
                )))
        }
    }
}

fn segment_option<ToDoc, Value>(
    option: &BitStringSegmentOption<Value>,
    mut to_doc: ToDoc,
) -> Document<'_>
where
    ToDoc: FnMut(&Value) -> Document<'_>,
{
    match option {
        BitStringSegmentOption::Binary { .. } => "binary".to_doc(),
        BitStringSegmentOption::Int { .. } => "int".to_doc(),
        BitStringSegmentOption::Float { .. } => "float".to_doc(),
        BitStringSegmentOption::BitString { .. } => "bit_string".to_doc(),
        BitStringSegmentOption::Utf8 { .. } => "utf8".to_doc(),
        BitStringSegmentOption::Utf16 { .. } => "utf16".to_doc(),
        BitStringSegmentOption::Utf32 { .. } => "utf32".to_doc(),
        BitStringSegmentOption::Utf8Codepoint { .. } => "utf8_codepoint".to_doc(),
        BitStringSegmentOption::Utf16Codepoint { .. } => "utf16_codepoint".to_doc(),
        BitStringSegmentOption::Utf32Codepoint { .. } => "utf32_codepoint".to_doc(),
        BitStringSegmentOption::Signed { .. } => "signed".to_doc(),
        BitStringSegmentOption::Unsigned { .. } => "unsigned".to_doc(),
        BitStringSegmentOption::Big { .. } => "big".to_doc(),
        BitStringSegmentOption::Little { .. } => "little".to_doc(),
        BitStringSegmentOption::Native { .. } => "native".to_doc(),

        BitStringSegmentOption::Size {
            value,
            short_form: false,
            ..
        } => "size"
            .to_doc()
            .append("(")
            .append(to_doc(value))
            .append(")"),

        BitStringSegmentOption::Size {
            value,
            short_form: true,
            ..
        } => to_doc(value),

        BitStringSegmentOption::Unit { value, .. } => "unit"
            .to_doc()
            .append("(")
            .append(Document::String(format!("{}", value)))
            .append(")"),
    }
}

pub fn comments_before<'a>(
    comments: &'a [Comment<'a>],
    limit: usize,
) -> (impl Iterator<Item = &'a str>, &'a [Comment<'a>]) {
    let end = comments
        .iter()
        .position(|c| c.start > limit)
        .unwrap_or(comments.len());
    let popped = comments
        .get(0..end)
        .expect("Comments before slicing popped")
        .iter()
        .map(|c| c.content);
    (
        popped,
        comments.get(end..).expect("Comments before slicing"),
    )
}

fn is_breakable_expr(expr: &UntypedExpr) -> bool {
    matches!(
        expr,
        UntypedExpr::Fn { .. }
            | UntypedExpr::Sequence { .. }
            | UntypedExpr::Assignment { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::BitString { .. }
    )
}
