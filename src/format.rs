pub(crate) mod command;
#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    parser::{Comment, ModuleComments},
    pretty::*,
    typ::{self, Type},
};
use itertools::Itertools;
use std::sync::Arc;

const INDENT: isize = 2;

pub fn pretty(src: &str) -> Result<String, crate::parser::LalrpopError> {
    let (stripped_src, comments) = crate::parser::strip_extra(src.as_ref());
    let ast = crate::grammar::ModuleParser::new()
        .parse(&stripped_src)
        .map_err(|e| e.map_token(|crate::grammar::Token(a, b)| (a, b.to_string())))?;
    let mut formatter = Formatter::with_comments(&comments);
    Ok(pretty_module(&ast, &mut formatter))
}

#[derive(Debug, Clone)]
pub struct Formatter<'a> {
    comments: &'a [Comment<'a>],
    doc_comments: &'a [Comment<'a>],
    module_comments: &'a [&'a str],
    empty_lines: &'a [usize],
}

impl<'a> Formatter<'a> {
    pub fn new() -> Self {
        Self {
            comments: &[],
            doc_comments: &[],
            module_comments: &[],
            empty_lines: &[],
        }
    }

    pub fn with_comments(comments: &'a ModuleComments) -> Self {
        Self {
            comments: comments.comments.as_slice(),
            doc_comments: comments.doc_comments.as_slice(),
            empty_lines: comments.empty_lines.as_slice(),
            module_comments: comments.module_comments.as_slice(),
        }
    }

    // Pop comments that occur before a byte-index in the source
    fn pop_comments(&mut self, limit: usize) -> impl Iterator<Item = &'a str> {
        let (popped, rest) = crate::parser::take_before(self.comments, limit);
        self.comments = rest;
        popped
    }

    // Pop doc comments that occur before a byte-index in the source
    fn pop_doc_comments(&mut self, limit: usize) -> impl Iterator<Item = &'a str> {
        let (popped, rest) = crate::parser::take_before(self.doc_comments, limit);
        self.doc_comments = rest;
        popped
    }

    fn pop_empty_lines(&mut self, limit: usize) -> bool {
        let mut end = 0;
        for (i, postition) in self.empty_lines.iter().enumerate() {
            if *postition > limit {
                break;
            }
            end = i + 1;
        }

        self.empty_lines = &self.empty_lines[end..];
        end != 0
    }

    fn module(&mut self, module: &UntypedModule) -> Document {
        let mut has_imports = false;
        let mut has_declarations = false;
        let mut imports = Vec::new();
        let mut declarations = Vec::with_capacity(module.statements.len());

        for statement in module.statements.iter() {
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

        let imports = concat(imports.into_iter().intersperse(line()));
        let declarations = concat(declarations.into_iter().intersperse(lines(2)));

        let sep = if has_imports && has_declarations {
            lines(2)
        } else {
            nil()
        };

        let doc_comments = concat(
            self.doc_comments
                .into_iter()
                .map(|comment| line().append("///").append(comment.content)),
        );
        let comments = concat(
            self.comments
                .into_iter()
                .map(|comment| line().append("//").append(comment.content)),
        );

        let module_comments = if !self.module_comments.is_empty() {
            let comments = self
                .module_comments
                .iter()
                .map(|s| "////".to_doc().append(s.to_string()).append(line()));
            concat(comments).append(line())
        } else {
            nil()
        };

        module_comments
            .append(imports)
            .append(sep)
            .append(declarations)
            .append(doc_comments)
            .append(comments)
            .append(line())
    }

    fn statement(&mut self, statement: &UntypedStatement) -> Document {
        match statement {
            Statement::Fn {
                name,
                args,
                body,
                public,
                return_annotation,
                end_location,
                ..
            } => self.fn_(public, name, args, return_annotation, body, *end_location),

            Statement::TypeAlias {
                alias,
                args,
                resolved_type,
                public,
                ..
            } => self.type_alias(*public, alias, args, resolved_type),

            Statement::CustomType {
                name,
                args,
                public,
                constructors,
                location,
                opaque,
                ..
            } => self.custom_type(
                *public,
                *opaque,
                name,
                args.as_slice(),
                constructors,
                location,
            ),

            Statement::ExternalFn {
                public,
                args,
                name,
                retrn,
                module,
                fun,
                ..
            } => self
                .external_fn_signature(*public, name, args, retrn)
                .append(" =")
                .append(line())
                .append(format!("  \"{}\" ", module))
                .append(format!("\"{}\"", fun)),

            Statement::ExternalType {
                public, name, args, ..
            } => self.external_type(*public, name, args),

            Statement::Import {
                module,
                as_name,
                unqualified,
                ..
            } => nil()
                .append("import ")
                .append(module.join("/"))
                .append(if unqualified.is_empty() {
                    nil()
                } else {
                    let unqualified = unqualified
                        .iter()
                        .sorted_by(|a, b| a.name.cmp(&b.name))
                        .map(|e| e.to_doc().flex_break())
                        .intersperse(delim(",").flex_break());
                    ".{".to_doc()
                        .append(concat(unqualified).flex_group(INDENT))
                        .append("}")
                })
                .append(if let Some(name) = as_name {
                    format!(" as {}", name).to_doc()
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
                let head = pub_(*public).append("const ").append(name.to_string());
                let head = match annotation {
                    None => head,
                    Some(t) => head.append(": ").append(self.type_ast(t)),
                };
                head.append(" = ").append(self.const_expr(value))
            }
        }
    }

    fn const_expr<A, B>(&mut self, value: &Constant<A, B>) -> Document {
        match value {
            Constant::Int { value, .. } | Constant::Float { value, .. } => value.clone().to_doc(),

            Constant::String { value, .. } => value.clone().to_doc().surround("\"", "\""),

            Constant::List { elements, .. } => {
                let elements = elements
                    .iter()
                    .map(|e| self.const_expr(e))
                    .intersperse(delim(","));
                list(concat(elements), None)
            }

            Constant::Tuple { elements, .. } => "tuple"
                .to_doc()
                .append(wrap_args(elements.iter().map(|e| self.const_expr(e)))),

            Constant::BitString { segments, .. } => bit_string(
                segments
                    .iter()
                    .map(|s| bit_string_segment(s, |e| self.const_expr(e))),
            ),

            Constant::Record {
                name,
                args,
                module: None,
                ..
            } if args.is_empty() => name.to_string().to_doc(),

            Constant::Record {
                name,
                args,
                module: Some(m),
                ..
            } if args.is_empty() => m.to_string().to_doc().append(".").append(name.to_string()),

            Constant::Record {
                name,
                args,
                module: None,
                ..
            } => name
                .to_string()
                .to_doc()
                .append(wrap_args(args.iter().map(|a| self.constant_call_arg(a)))),

            Constant::Record {
                name,
                args,
                module: Some(m),
                ..
            } => m
                .to_string()
                .to_doc()
                .append(".")
                .append(name.to_string())
                .append(wrap_args(args.iter().map(|a| self.constant_call_arg(a)))),
        }
    }

    pub fn docs_const_expr(&mut self, public: bool, name: &str, value: &TypedConstant) -> Document {
        let mut printer = typ::pretty::Printer::new();

        pub_(public)
            .append("const ")
            .append(name.to_string())
            .append(": ")
            .append(printer.print(value.typ().as_ref()))
            .append(" = ")
            .append(self.const_expr(value))
    }

    fn documented_statement(&mut self, s: &UntypedStatement) -> Document {
        let comments = self.doc_comments(s.location().start);
        comments.append(self.statement(s)).group()
    }

    fn doc_comments(&mut self, limit: usize) -> Document {
        let mut comments = self.pop_doc_comments(limit).peekable();
        match comments.peek() {
            None => nil(),
            Some(_) => concat(
                comments
                    .map(|c| "///".to_doc().append(c))
                    .intersperse(line()),
            )
            .append(force_break())
            .append(line()),
        }
    }

    fn type_ast_constructor(
        &mut self,
        module: &Option<String>,
        name: &str,
        args: &[TypeAst],
    ) -> Document {
        let head = match module {
            None => name.to_doc(),
            Some(qualifier) => qualifier.to_string().to_doc().append(".").append(name),
        };

        if args.is_empty() {
            head
        } else {
            head.append(self.type_arguments(args))
        }
    }

    fn type_ast(&mut self, t: &TypeAst) -> Document {
        match t {
            TypeAst::Constructor {
                name, args, module, ..
            } => self.type_ast_constructor(module, name, args),

            TypeAst::Fn { args, retrn, .. } => "fn"
                .to_string()
                .to_doc()
                .append(self.type_arguments(args))
                .append(" -> ")
                .append(self.type_ast(retrn)),

            TypeAst::Var { name, .. } => name.clone().to_doc(),

            TypeAst::Tuple { elems, .. } => "tuple".to_doc().append(self.type_arguments(elems)),
        }
        .group()
    }

    fn type_arguments(&mut self, args: &[TypeAst]) -> Document {
        wrap_args(args.iter().map(|t| self.type_ast(t)))
    }

    pub fn type_alias(
        &mut self,
        public: bool,
        name: &str,
        args: &[String],
        typ: &TypeAst,
    ) -> Document {
        pub_(public)
            .append("type ")
            .append(name.to_string())
            .append(if args.is_empty() {
                nil()
            } else {
                wrap_args(args.iter().map(|e| e.clone().to_doc()))
            })
            .append(" =")
            .append(line().append(self.type_ast(typ)).group().nest(INDENT))
    }

    fn fn_args<A>(&mut self, args: &[Arg<A>]) -> Document {
        wrap_args(args.iter().map(|e| self.fn_arg(e)))
    }

    fn fn_arg<A>(&mut self, arg: &Arg<A>) -> Document {
        let comments = self.pop_comments(arg.location.start);
        let doc = match &arg.annotation {
            None => arg.names.to_doc(),
            Some(a) => arg.names.to_doc().append(": ").append(self.type_ast(a)),
        }
        .group();
        commented(doc, comments)
    }

    fn fn_(
        &mut self,
        public: &bool,
        name: &str,
        args: &Vec<UntypedArg>,
        return_annotation: &Option<TypeAst>,
        body: &UntypedExpr,
        end_location: usize,
    ) -> Document {
        // Fn name and args
        let head = pub_(*public)
            .append("fn ")
            .append(name)
            .append(self.fn_args(args));

        // Add return annotation
        let head = match return_annotation {
            Some(anno) => head.append(" -> ").append(self.type_ast(anno)),
            None => head,
        };

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

    pub fn external_fn_signature(
        &mut self,
        public: bool,
        name: &str,
        args: &[ExternalFnArg],
        retrn: &TypeAst,
    ) -> Document {
        pub_(public)
            .to_doc()
            .append("external fn ")
            .group()
            .append(name.to_string())
            .append(self.external_fn_args(args))
            .append(" -> ".to_doc())
            .append(self.type_ast(retrn))
    }

    fn expr_fn(
        &mut self,
        args: &[UntypedArg],
        return_annotation: Option<&TypeAst>,
        body: &UntypedExpr,
    ) -> Document {
        let args = self.fn_args(args);
        let body = match body {
            UntypedExpr::Case { .. } => force_break().append(self.expr(body)),
            _ => self.expr(body),
        };

        let header = "fn".to_doc().append(args);

        let header = match return_annotation {
            None => header,
            Some(t) => header.append(" -> ").append(self.type_ast(&t)),
        };

        header
            .append(
                break_(" {", " { ")
                    .append(body)
                    .nest(INDENT)
                    .append(delim(""))
                    .append("}"),
            )
            .group()
    }

    fn seq(&mut self, first: &UntypedExpr, then: &UntypedExpr) -> Document {
        force_break()
            .append({
                let doc = self.expr(first).group();
                self.pop_empty_lines(first.location().end);
                doc
            })
            .append(if self.pop_empty_lines(then.start_byte_index()) {
                lines(2)
            } else {
                line()
            })
            .append(self.expr(then))
    }

    fn let_(
        &mut self,
        pattern: &UntypedPattern,
        value: &UntypedExpr,
        then: &UntypedExpr,
        kind: BindingKind,
        annotation: &Option<TypeAst>,
    ) -> Document {
        self.pop_empty_lines(pattern.location().end);

        let keyword = match kind {
            BindingKind::Let => "let ",
            BindingKind::Try => "try ",
            BindingKind::Assert => "assert ",
        };

        let line = if self.pop_empty_lines(then.start_byte_index()) {
            lines(2)
        } else {
            line()
        };
        force_break()
            .append(keyword)
            .append(self.pattern(pattern))
            .append(
                annotation
                    .as_ref()
                    .map(|a| ": ".to_doc().append(self.type_ast(a))),
            )
            .append(" = ")
            .append(self.hanging_expr(value))
            .append(line)
            .append(self.expr(then))
    }

    fn expr(&mut self, expr: &UntypedExpr) -> Document {
        let comments = self.pop_comments(expr.start_byte_index());

        let document = match expr {
            UntypedExpr::Todo { label: None, .. } => "todo".to_doc(),

            UntypedExpr::Todo { label: Some(l), .. } => ["todo", "(\"", l, "\")"].join("").to_doc(),

            UntypedExpr::Pipe {
                left,
                right,
                location,
                ..
            } => self.pipe(left, right, location.start),

            UntypedExpr::Int { value, .. } => value.clone().to_doc(),

            UntypedExpr::Float { value, .. } => value.clone().to_doc(),

            UntypedExpr::String { value, .. } => value.clone().to_doc().surround("\"", "\""),

            UntypedExpr::Seq { first, then, .. } => self.seq(first, then),

            UntypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => "_".to_doc(),

            UntypedExpr::Var { name, .. } => name.clone().to_doc(),

            UntypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            UntypedExpr::Fn {
                is_capture: true,
                body,
                ..
            } => self.fn_capture(body.as_ref()),

            UntypedExpr::Fn {
                return_annotation,
                args,
                body,
                ..
            } => self.expr_fn(args.as_slice(), return_annotation.as_ref(), body.as_ref()),

            UntypedExpr::ListNil { .. } => "[]".to_doc(),

            UntypedExpr::ListCons { head, tail, .. } => self.list_cons(head, tail),

            UntypedExpr::Call { fun, args, .. } => self
                .expr(fun)
                .append(wrap_args(args.iter().map(|a| self.call_arg(a)))),

            UntypedExpr::BinOp {
                name, left, right, ..
            } => self.bin_op(name, left, right),

            UntypedExpr::Let {
                value,
                pattern,
                annotation,
                then,
                kind,
                ..
            } => self.let_(pattern, value, then, *kind, annotation),

            UntypedExpr::Case {
                subjects, clauses, ..
            } => self.case(subjects, clauses),

            UntypedExpr::FieldAccess {
                label, container, ..
            } => self.expr(container).append(".").append(label.clone()),

            UntypedExpr::Tuple { elems, .. } => "tuple"
                .to_doc()
                .append(wrap_args(elems.iter().map(|e| self.wrap_expr(e)))),

            UntypedExpr::BitString { segments, .. } => bit_string(
                segments
                    .iter()
                    .map(|s| bit_string_segment(s, |e| self.expr(e))),
            ),
        };
        commented(document, comments)
    }

    pub fn case(&mut self, subjects: &[UntypedExpr], clauses: &[UntypedClause]) -> Document {
        let subjects_doc = concat(
            subjects
                .into_iter()
                .map(|s| self.expr(s))
                .intersperse(", ".to_doc()),
        );

        let clauses_doc = concat(
            clauses
                .into_iter()
                .enumerate()
                .map(|(i, c)| self.clause(c, i)),
        );

        "case "
            .to_doc()
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

    pub fn bin_op(&mut self, name: &BinOp, left: &UntypedExpr, right: &UntypedExpr) -> Document {
        let precedence = name.precedence();
        let left_precedence = left.binop_precedence();
        let right_precedence = right.binop_precedence();
        let left = self.expr(left);
        let right = self.expr(right);
        self.operator_side(left, precedence, left_precedence)
            .append(name)
            .append(self.operator_side(right, precedence, right_precedence))
    }

    pub fn operator_side(&mut self, doc: Document, op: u8, side: u8) -> Document {
        if op > side {
            delim("{")
                .append(doc)
                .nest(INDENT)
                .append(break_("", " "))
                .append("}")
                .group()
        } else {
            doc
        }
    }

    fn pipe(&mut self, left: &UntypedExpr, right: &UntypedExpr, location_start: usize) -> Document {
        let left_precedence = left.binop_precedence();
        let right_precedence = right.binop_precedence();
        let left = self.wrap_expr(left);

        // Get comments before right but after left
        let comments = self.pop_comments(location_start);

        let right = match right {
            UntypedExpr::Fn {
                is_capture: true,
                body,
                ..
            } => self.pipe_capture_right_hand_side(body),

            _ => self.wrap_expr(right),
        };

        // Wrap sides if required
        let left = self.operator_side(left, 5, left_precedence);
        let right = self.operator_side(right, 4, right_precedence);

        force_break()
            .append(left)
            .append(line())
            .append(commented("|> ".to_doc().append(right), comments))
    }

    fn pipe_capture_right_hand_side(&mut self, fun: &UntypedExpr) -> Document {
        let (fun, args) = match fun {
            UntypedExpr::Call { fun, args, .. } => (fun, args),
            _ => crate::error::fatal_compiler_bug(
                "Function capture found not to have a function call body when formatting",
            ),
        };

        let hole_in_first_position = match args.get(0) {
            Some(CallArg {
                value: UntypedExpr::Var { name, .. },
                ..
            }) => name == CAPTURE_VARIABLE,
            _ => false,
        };

        if hole_in_first_position && args.len() == 1 {
            // x |> fun(_)
            self.expr(fun)
        } else if hole_in_first_position {
            // x |> fun(_, 2, 3)
            self.expr(fun)
                .append(wrap_args(args.iter().skip(1).map(|a| self.call_arg(a))))
        } else {
            // x |> fun(1, _, 3)
            self.expr(fun)
                .append(wrap_args(args.iter().map(|a| self.call_arg(a))))
        }
    }

    fn fn_capture(&mut self, call: &UntypedExpr) -> Document {
        match call {
            UntypedExpr::Call { fun, args, .. } => self
                .expr(fun)
                .append(wrap_args(args.iter().map(|a| self.call_arg(a)))),

            // The body of a capture being not a fn shouldn't be possible...
            _ => crate::error::fatal_compiler_bug(
                "Function capture body found not to be a call in the formatter",
            ),
        }
    }

    pub fn record_constructor(&mut self, constructor: &RecordConstructor) -> Document {
        let comments = self.pop_comments(constructor.location.start);
        let doc_comments = self.doc_comments(constructor.location.start);

        let doc =
            if constructor.args.is_empty() {
                constructor.name.clone().to_doc()
            } else {
                constructor.name.to_string().to_doc().append(wrap_args(
                    constructor.args.iter().map(|(label, typ, arg_location)| {
                        let arg_comments = self.pop_comments(arg_location.start);
                        let arg = match label {
                            Some(l) => l
                                .to_string()
                                .to_doc()
                                .append(": ")
                                .append(self.type_ast(typ)),
                            None => self.type_ast(typ),
                        };

                        commented(
                            self.doc_comments(arg_location.start).append(arg).group(),
                            arg_comments,
                        )
                    }),
                ))
            };

        commented(doc_comments.append(doc).group(), comments)
    }

    pub fn custom_type(
        &mut self,
        public: bool,
        opaque: bool,
        name: &str,
        args: &[String],
        constructors: &[RecordConstructor],
        location: &SrcSpan,
    ) -> Document {
        self.pop_empty_lines(location.start);
        pub_(public)
            .to_doc()
            .append(if opaque { "opaque type " } else { "type " })
            .append(if args.is_empty() {
                name.clone().to_doc()
            } else {
                name.to_string()
                    .to_doc()
                    .append(wrap_args(args.iter().map(|e| e.clone().to_doc())))
            })
            .append(" {")
            .append(concat(constructors.into_iter().map(|c| {
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

    pub fn docs_opaque_custom_type(
        &mut self,
        public: bool,
        name: &str,
        args: &[String],
        location: &SrcSpan,
    ) -> Document {
        self.pop_empty_lines(location.start);
        pub_(public)
            .to_doc()
            .append("opaque type ")
            .append(if args.is_empty() {
                name.clone().to_doc()
            } else {
                name.to_string()
                    .to_doc()
                    .append(wrap_args(args.iter().map(|e| e.clone().to_doc())))
            })
    }

    pub fn docs_fn_signature(
        &mut self,
        public: bool,
        name: &str,
        args: &[TypedArg],
        return_type: Arc<Type>,
    ) -> Document {
        let mut printer = typ::pretty::Printer::new();

        pub_(public)
            .append("fn ")
            .append(name)
            .append(self.docs_fn_args(args, &mut printer))
            .append(" -> ".to_doc())
            .append(printer.print(return_type.as_ref()))
    }

    // Like fn_args but will always print the types, even if they were implicit in the original source
    pub fn docs_fn_args(
        &mut self,
        args: &[TypedArg],
        printer: &mut typ::pretty::Printer,
    ) -> Document {
        wrap_args(args.iter().map(|arg| {
            arg.names
                .to_doc()
                .append(": ".to_doc().append(printer.print(&arg.typ)))
                .group()
        }))
    }

    fn external_fn_arg(&mut self, arg: &ExternalFnArg) -> Document {
        let comments = self.pop_comments(arg.location.start);
        let doc = label(&arg.label).append(self.type_ast(&arg.typ));
        commented(doc.group(), comments)
    }

    fn external_fn_args(&mut self, args: &[ExternalFnArg]) -> Document {
        wrap_args(args.iter().map(|e| self.external_fn_arg(e)))
    }

    fn wrap_expr(&mut self, expr: &UntypedExpr) -> Document {
        match expr {
            UntypedExpr::Seq { .. } | UntypedExpr::Let { .. } => "{"
                .to_doc()
                .append(force_break())
                .append(line().append(self.expr(expr)).nest(INDENT))
                .append(line())
                .append("}"),

            _ => self.expr(expr),
        }
    }

    fn call_arg(&mut self, arg: &CallArg<UntypedExpr>) -> Document {
        match &arg.label {
            Some(s) => s.clone().to_doc().append(": "),
            None => nil(),
        }
        .append(self.wrap_expr(&arg.value))
    }

    fn tuple_index(&mut self, tuple: &UntypedExpr, index: u64) -> Document {
        match tuple {
            UntypedExpr::TupleIndex { .. } => self.expr(tuple).surround("{", "}"),
            _ => self.expr(tuple),
        }
        .append(".")
        .append(index)
    }

    fn hanging_expr(&mut self, expr: &UntypedExpr) -> Document {
        match expr {
            UntypedExpr::Seq { .. } | UntypedExpr::Let { .. } => "{"
                .to_doc()
                .append(line().append(self.expr(expr)).nest(INDENT).group())
                .append(line())
                .append(force_break())
                .append("}"),

            UntypedExpr::Call { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::BitString { .. } => self.expr(expr),

            _ => self.expr(expr).nest(INDENT),
        }
    }

    fn clause(&mut self, clause: &UntypedClause, index: usize) -> Document {
        let space_before = self.pop_empty_lines(clause.location.start);
        let after_position = clause.location.end;
        let clause_doc = concat(
            std::iter::once(&clause.pattern)
                .chain(clause.alternative_patterns.iter())
                .map(|p| concat(p.iter().map(|p| self.pattern(p)).intersperse(", ".to_doc())))
                .intersperse(" | ".to_doc()),
        );
        let clause_doc = match &clause.guard {
            None => clause_doc,
            Some(guard) => clause_doc.append(" if ").append(self.clause_guard(guard)),
        };

        // Remove any unused empty lines within the clause
        self.pop_empty_lines(after_position);

        if index == 0 {
            clause_doc
        } else if space_before {
            lines(2).append(clause_doc)
        } else {
            lines(1).append(clause_doc)
        }
        .append(" -> ")
        .append(self.hanging_expr(&clause.then))
    }

    pub fn external_type(&mut self, public: bool, name: &str, args: &[String]) -> Document {
        pub_(public)
            .append("external type ")
            .append(name.to_string())
            .append(if args.is_empty() {
                nil()
            } else {
                wrap_args(args.iter().map(|e| e.clone().to_doc()))
            })
    }

    fn list_cons(&mut self, head: &UntypedExpr, tail: &UntypedExpr) -> Document {
        let (elems, tail) = list_cons(head, tail, categorise_list_expr);
        let elems = concat(
            elems
                .iter()
                .map(|e| self.wrap_expr(e))
                .intersperse(delim(",")),
        );
        let tail = tail.map(|e| self.expr(e));
        list(elems, tail)
    }

    fn pattern(&mut self, pattern: &UntypedPattern) -> Document {
        let comments = self.pop_comments(pattern.location().start);
        let doc = match pattern {
            Pattern::Int { value, .. } => value.clone().to_doc(),

            Pattern::Float { value, .. } => value.clone().to_doc(),

            Pattern::String { value, .. } => value.clone().to_doc().surround("\"", "\""),

            Pattern::Var { name, .. } => name.to_string().to_doc(),

            Pattern::VarCall { name, .. } => name.to_string().to_doc(),

            Pattern::Let { name, pattern, .. } => self
                .pattern(&pattern)
                .append(" as ")
                .append(name.to_string()),

            Pattern::Discard { name, .. } => name.to_string().to_doc(),

            Pattern::Nil { .. } => "[]".to_doc(),

            Pattern::Cons { head, tail, .. } => {
                let (elems, tail) =
                    list_cons(head.as_ref(), tail.as_ref(), categorise_list_pattern);
                let elems = concat(
                    elems
                        .iter()
                        .map(|e| self.pattern(e))
                        .intersperse(delim(",")),
                );
                let tail = tail.map(|e| self.pattern(e));
                list(elems, tail)
            }

            Pattern::Constructor {
                name,
                args,
                module: None,
                ..
            } if args.is_empty() => name.to_string().to_doc(),

            Pattern::Constructor {
                name,
                args,
                module: Some(m),
                ..
            } if args.is_empty() => m.to_string().to_doc().append(".").append(name.to_string()),

            Pattern::Constructor {
                name,
                args,
                module: None,
                with_spread: false,
                ..
            } => name
                .to_string()
                .to_doc()
                .append(wrap_args(args.iter().map(|a| self.pattern_call_arg(a)))),

            Pattern::Constructor {
                name,
                args,
                module: None,
                with_spread: true,
                ..
            } => name.to_string().to_doc().append(wrap_args_with_spread(
                args.iter().map(|a| self.pattern_call_arg(a)),
            )),

            Pattern::Constructor {
                name,
                args,
                module: Some(m),
                with_spread: false,
                ..
            } => m
                .to_string()
                .to_doc()
                .append(".")
                .append(name.to_string())
                .append(wrap_args(args.iter().map(|a| self.pattern_call_arg(a)))),

            Pattern::Constructor {
                name,
                args,
                module: Some(m),
                with_spread: true,
                ..
            } => m
                .to_string()
                .to_doc()
                .append(".")
                .append(name.to_string())
                .append(wrap_args_with_spread(
                    args.iter().map(|a| self.pattern_call_arg(a)),
                )),

            Pattern::Tuple { elems, .. } => "tuple"
                .to_doc()
                .append(wrap_args(elems.iter().map(|e| self.pattern(e)))),

            Pattern::BitString { segments, .. } => bit_string(
                segments
                    .iter()
                    .map(|s| bit_string_segment(s, |e| self.pattern(e))),
            ),
        };
        commented(doc, comments)
    }

    fn pattern_call_arg(&mut self, arg: &CallArg<UntypedPattern>) -> Document {
        match &arg.label {
            Some(s) => s.clone().to_doc().append(": "),
            None => nil(),
        }
        .append(self.pattern(&arg.value))
    }

    fn clause_guard(&mut self, clause_guard: &UntypedClauseGuard) -> Document {
        match clause_guard {
            ClauseGuard::And { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" && ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::Or { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" || ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::Equals { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" == ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::NotEquals { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" != ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::GtInt { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" > ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::GtEqInt { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" >= ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::LtInt { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" < ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::LtEqInt { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" <= ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::GtFloat { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" >. ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::GtEqFloat { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" >=. ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::LtFloat { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" <. ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::LtEqFloat { left, right, .. } => self
                .clause_guard(left.as_ref())
                .append(" <=. ")
                .append(self.clause_guard(right.as_ref())),

            ClauseGuard::Var { name, .. } => name.to_string().to_doc(),

            ClauseGuard::Constant(constant) => self.const_expr(constant),
        }
    }

    fn constant_call_arg<A, B>(&mut self, arg: &CallArg<Constant<A, B>>) -> Document {
        match &arg.label {
            None => self.const_expr(&arg.value),
            Some(s) => s
                .clone()
                .to_doc()
                .append(": ")
                .append(self.const_expr(&arg.value)),
        }
    }
}

pub fn pretty_module(m: &UntypedModule, formatter: &mut Formatter<'_>) -> String {
    format(80, formatter.module(m))
}

impl Documentable for &ArgNames {
    fn to_doc(self) -> Document {
        match self {
            ArgNames::Discard { name } => name.to_string(),
            ArgNames::LabelledDiscard { label, name } => format!("{} {}", label, name),
            ArgNames::Named { name } => name.to_string(),
            ArgNames::NamedLabelled { name, label } => format!("{} {}", label, name),
        }
        .to_doc()
    }
}

fn pub_(public: bool) -> Document {
    if public {
        "pub ".to_doc()
    } else {
        nil()
    }
}

impl Documentable for &UnqualifiedImport {
    fn to_doc(self) -> Document {
        self.name.clone().to_doc().append(match &self.as_name {
            None => nil(),
            Some(s) => " as ".to_doc().append(s.clone()),
        })
    }
}

fn label(label: &Option<String>) -> Document {
    match label {
        Some(s) => s.clone().to_doc().append(": "),
        None => nil(),
    }
}

impl Documentable for &BinOp {
    fn to_doc(self) -> Document {
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

fn categorise_list_expr(expr: &UntypedExpr) -> ListType<&UntypedExpr, &UntypedExpr> {
    match expr {
        UntypedExpr::ListNil { .. } => ListType::Nil,

        UntypedExpr::ListCons { head, tail, .. } => ListType::Cons { head, tail },

        other => ListType::NotList(other),
    }
}

fn categorise_list_pattern(expr: &UntypedPattern) -> ListType<&UntypedPattern, &UntypedPattern> {
    match expr {
        UntypedPattern::Nil { .. } => ListType::Nil,

        UntypedPattern::Cons { head, tail, .. } => ListType::Cons { head, tail },

        other => ListType::NotList(other),
    }
}

pub fn wrap_args<I>(args: I) -> Document
where
    I: Iterator<Item = Document>,
{
    let mut args = args.peekable();
    if let None = args.peek() {
        return "()".to_doc();
    }
    break_("(", "(")
        .append(concat(args.intersperse(delim(","))))
        .nest(INDENT)
        .append(break_(",", ""))
        .append(")")
        .group()
}

pub fn wrap_args_with_spread<I>(args: I) -> Document
where
    I: Iterator<Item = Document>,
{
    let mut args = args.peekable();
    if let None = args.peek() {
        return "()".to_doc();
    }

    break_("(", "(")
        .append(concat(args.intersperse(delim(","))))
        .append(break_(",", ", "))
        .append("..")
        .nest(INDENT)
        .append(break_(",", ""))
        .append(")")
        .group()
}

fn list_cons<Categorise, Elem>(
    head: Elem,
    tail: Elem,
    categorise_element: Categorise,
) -> (Vec<Elem>, Option<Elem>)
where
    Categorise: Fn(Elem) -> ListType<Elem, Elem>,
{
    let mut elems = vec![head];
    let tail = collect_cons(tail, &mut elems, categorise_element);
    (elems, tail)
}

fn bit_string<'a>(segments: impl Iterator<Item = Document>) -> Document {
    break_("<<", "<<")
        .append(concat(segments.intersperse(delim(","))))
        .nest(INDENT)
        .append(break_(",", ""))
        .append(">>")
        .group()
}

fn list(elems: Document, tail: Option<Document>) -> Document {
    let doc = break_("[", "[").append(elems);

    match tail {
        None => doc.nest(INDENT).append(break_(",", "")),

        // Don't print tail if it is a discard
        Some(Document::Text(t)) if t == "_".to_string() => doc
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

fn collect_cons<F, E, T>(e: T, elems: &mut Vec<E>, f: F) -> Option<T>
where
    F: Fn(T) -> ListType<E, T>,
{
    match f(e) {
        ListType::Nil => None,

        ListType::Cons { head, tail } => {
            elems.push(head);
            collect_cons(tail, elems, f)
        }

        ListType::NotList(other) => Some(other),
    }
}

enum ListType<E, T> {
    Nil,
    Cons { head: E, tail: T },
    NotList(T),
}

fn printed_comments<'a>(comments: impl Iterator<Item = &'a str>) -> Option<Document> {
    let mut comments = comments.peekable();
    match comments.peek() {
        None => None,
        Some(_) => Some(concat(
            comments
                .map(|c| "//".to_doc().append(c))
                .intersperse(line()),
        )),
    }
}

fn commented<'a>(doc: Document, comments: impl Iterator<Item = &'a str>) -> Document {
    match printed_comments(comments) {
        Some(comments) => comments.append(force_break()).append(line()).append(doc),
        _ => doc,
    }
}

fn bit_string_segment<Value, Type, ToDoc>(
    segment: &BitStringSegment<Value, Type>,
    mut to_doc: ToDoc,
) -> Document
where
    ToDoc: FnMut(&Value) -> Document,
{
    match segment {
        BitStringSegment { value, options, .. } if options.is_empty() => to_doc(&value),

        BitStringSegment { value, options, .. } => to_doc(&value).append(":").append(concat(
            options
                .iter()
                .map(|o| segment_option(o, |e| to_doc(e)))
                .intersperse("-".to_doc()),
        )),
    }
}

fn segment_option<ToDoc, Value>(
    option: &BitStringSegmentOption<Value>,
    mut to_doc: ToDoc,
) -> Document
where
    ToDoc: FnMut(&Value) -> Document,
{
    match option {
        BitStringSegmentOption::Invalid { label, .. } => label.clone().to_doc(),

        BitStringSegmentOption::Binary { .. } => "binary".to_doc(),
        BitStringSegmentOption::Integer { .. } => "int".to_doc(),
        BitStringSegmentOption::Float { .. } => "float".to_doc(),
        BitStringSegmentOption::BitString { .. } => "bit_string".to_doc(),
        BitStringSegmentOption::UTF8 { .. } => "utf8".to_doc(),
        BitStringSegmentOption::UTF16 { .. } => "utf16".to_doc(),
        BitStringSegmentOption::UTF32 { .. } => "utf32".to_doc(),
        BitStringSegmentOption::UTF8Codepoint { .. } => "utf8_codepoint".to_doc(),
        BitStringSegmentOption::UTF16Codepoint { .. } => "utf16_codepoint".to_doc(),
        BitStringSegmentOption::UTF32Codepoint { .. } => "utf32_codepoint".to_doc(),
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
            .append(to_doc(value.as_ref()))
            .append(")"),

        BitStringSegmentOption::Size {
            value,
            short_form: true,
            ..
        } => to_doc(value.as_ref()),

        BitStringSegmentOption::Unit {
            value,
            short_form: false,
            ..
        } => "unit"
            .to_doc()
            .append("(")
            .append(to_doc(value.as_ref()))
            .append(")"),

        BitStringSegmentOption::Unit {
            value,
            short_form: true,
            ..
        } => to_doc(value.as_ref()),
    }
}
