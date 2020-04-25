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
}

impl<'a> Formatter<'a> {
    pub fn new() -> Self {
        Self {
            comments: &[],
            doc_comments: &[],
        }
    }

    pub fn with_comments(comments: &'a ModuleComments) -> Self {
        Self {
            comments: comments.comments.as_slice(),
            doc_comments: comments.doc_comments.as_slice(),
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

        imports
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
                ..
            } => self.fn_(public, name, args, return_annotation, body),

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
                ..
            } => self.custom_type(*public, name, args.as_slice(), constructors),

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
                    ".{".to_doc()
                        .append(concat(
                            unqualified
                                .iter()
                                .map(|e| e.clone().to_doc())
                                .intersperse(", ".to_doc()),
                        ))
                        .append("}")
                })
                .append(if let Some(name) = as_name {
                    format!(" as {}", name).to_doc()
                } else {
                    nil()
                }),
        }
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
                .append(delim(" ->"))
                .append(self.type_ast(retrn)),

            TypeAst::Var { name, .. } => name.clone().to_doc(),

            TypeAst::Tuple { elems, .. } => "tuple".to_doc().append(self.type_arguments(elems)),
        }
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
        let doc = arg
            .names
            .to_doc()
            .append(match &arg.annotation {
                Some(a) => ": ".to_doc().append(self.type_ast(a)),
                None => nil(),
            })
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
    ) -> Document {
        pub_(*public)
            .append("fn ")
            .append(name)
            .append(self.fn_args(args))
            .append(if let Some(anno) = return_annotation {
                " -> ".to_doc().append(self.type_ast(anno))
            } else {
                nil()
            })
            .append(" {")
            .append(line().append(self.expr(body)).nest(INDENT).group())
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

    fn expr_fn(&mut self, args: &[UntypedArg], body: &UntypedExpr) -> Document {
        "fn".to_doc()
            .append(self.fn_args(args).nest_current())
            .append(
                break_(" {", " { ")
                    .append(self.expr(body))
                    .nest(INDENT)
                    .append(delim(""))
                    .append("}")
                    .group(),
            )
    }

    fn expr(&mut self, expr: &UntypedExpr) -> Document {
        let comments = self.pop_comments(expr.start_byte_index());

        let document = match expr {
            UntypedExpr::Todo { .. } => "todo".to_doc(),

            UntypedExpr::Pipe { left, right, .. } => force_break()
                .append(self.expr(left))
                .append(line())
                .append("|> ")
                .append(self.expr(right)),

            UntypedExpr::Int { value, .. } => value.clone().to_doc(),

            UntypedExpr::Float { value, .. } => value.clone().to_doc(),

            UntypedExpr::String { value, .. } => value.clone().to_doc().surround("\"", "\""),

            UntypedExpr::Seq { first, then, .. } => force_break()
                .append(self.expr(first).group())
                .append(line())
                .append(self.expr(then.as_ref())),

            UntypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => "_".to_doc(),

            UntypedExpr::Var { name, .. } => name.clone().to_doc(),

            UntypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            UntypedExpr::Fn {
                is_capture: true, // TODO: render captures
                body,
                ..
            } => self.fn_capture(body.as_ref()),

            UntypedExpr::Fn {
                // is_capture, // TODO: render captures
                // return_annotation, // TODO: render this annotation
                args,
                body,
                ..
            } => self.expr_fn(args.as_slice(), body.as_ref()),

            UntypedExpr::ListNil { .. } => "[]".to_doc(),

            UntypedExpr::ListCons { head, tail, .. } => self.list_cons(head, tail),

            UntypedExpr::Call { fun, args, .. } => self
                .expr(fun)
                .append(wrap_args(args.iter().map(|a| self.call_arg(a)))),

            UntypedExpr::BinOp {
                name, left, right, ..
            } => self
                .expr(left)
                .append(name)
                .append(self.expr(right.as_ref())),

            UntypedExpr::Let {
                value,
                pattern,
                then,
                assert,
                ..
            } => force_break()
                .append(if *assert { "assert " } else { "let " })
                .append(self.pattern(pattern))
                .append(" = ")
                .append(self.hanging_expr(value.as_ref()))
                .append(line())
                .append(self.expr(then.as_ref())),

            UntypedExpr::Case {
                subjects, clauses, ..
            } => "case "
                .to_doc()
                .append(concat(
                    subjects
                        .into_iter()
                        .map(|s| self.expr(s))
                        .intersperse(", ".to_doc()),
                ))
                .append(" {")
                .append(
                    line()
                        .append(concat(
                            clauses
                                .into_iter()
                                .map(|c| self.clause(c).group())
                                .intersperse(lines(1)),
                        ))
                        .nest(INDENT),
                )
                .append(line())
                .append("}"),

            UntypedExpr::FieldAccess {
                label, container, ..
            } => self.expr(container).append(".").append(label.clone()),

            UntypedExpr::Tuple { elems, .. } => "tuple"
                .to_doc()
                .append(wrap_args(elems.iter().map(|e| self.wrap_expr(e)))),
        };
        commented(document, comments)
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

    fn record_constructor(&mut self, constructor: &RecordConstructor) -> Document {
        let comments = self.pop_comments(constructor.location.start);

        let doc =
            if constructor.args.is_empty() {
                constructor.name.clone().to_doc()
            } else {
                constructor.name.to_string().to_doc().append(wrap_args(
                    constructor.args.iter().map(|(label, typ)| match label {
                        Some(l) => l
                            .to_string()
                            .to_doc()
                            .append(": ")
                            .append(self.type_ast(typ)),
                        None => self.type_ast(typ),
                    }),
                ))
            };

        commented(
            self.doc_comments(constructor.location.start).append(doc),
            comments,
        )
    }

    pub fn custom_type(
        &mut self,
        public: bool,
        name: &str,
        args: &[String],
        constructors: &[RecordConstructor],
    ) -> Document {
        pub_(public)
            .to_doc()
            .append("type ")
            .append(if args.is_empty() {
                name.clone().to_doc()
            } else {
                name.to_string()
                    .to_doc()
                    .append(wrap_args(args.iter().map(|e| e.clone().to_doc())))
            })
            .append(" {")
            .append(concat(constructors.into_iter().map(|c| {
                line()
                    .append(self.record_constructor(c))
                    .nest(INDENT)
                    .group()
            })))
            .append(line())
            .append("}")
    }

    pub fn docs_fn_signature(
        &mut self,
        public: bool,
        name: &str,
        args: &[TypedArg],
        return_type: Arc<Type>,
    ) -> Document {
        pub_(public)
            .append("fn ")
            .append(name.to_string())
            .append(self.fn_args(args)) // TODO: Always show types
            .append(" -> ".to_doc())
            .append(typ::pretty::Printer::new().to_doc(return_type.as_ref()))
    }

    fn external_fn_arg(&mut self, arg: &ExternalFnArg) -> Document {
        let comments = self.pop_comments(arg.location.start);
        let doc = label(&arg.label).append(self.type_ast(&arg.typ));
        commented(doc, comments)
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
                .append(force_break())
                .append(line().append(self.expr(expr)).nest(INDENT))
                .append(line())
                .append("}"),

            UntypedExpr::Fn { .. } | UntypedExpr::Case { .. } => self.expr(expr),

            _ => self.expr(expr).nest(INDENT),
        }
    }

    fn clause(&mut self, clause: &UntypedClause) -> Document {
        let doc = concat(
            std::iter::once(&clause.pattern)
                .chain(clause.alternative_patterns.iter())
                .map(|p| concat(p.iter().map(|p| self.pattern(p)).intersperse(", ".to_doc())))
                .intersperse(" | ".to_doc()),
        );
        match &clause.guard {
            None => doc,
            Some(guard) => doc.append(" if ").append(guard),
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

// TODO: surround sub-expressions where needed
impl Documentable for &UntypedClauseGuard {
    fn to_doc(self) -> Document {
        match self {
            ClauseGuard::And { left, right, .. } => {
                left.as_ref().to_doc().append(" && ").append(right.as_ref())
            }

            ClauseGuard::Or { left, right, .. } => {
                left.as_ref().to_doc().append(" || ").append(right.as_ref())
            }

            ClauseGuard::Equals { left, right, .. } => {
                left.as_ref().to_doc().append(" == ").append(right.as_ref())
            }

            ClauseGuard::NotEquals { left, right, .. } => {
                left.as_ref().to_doc().append(" != ").append(right.as_ref())
            }

            ClauseGuard::GtInt { left, right, .. } => {
                left.as_ref().to_doc().append(" > ").append(right.as_ref())
            }

            ClauseGuard::GtEqInt { left, right, .. } => {
                left.as_ref().to_doc().append(" >= ").append(right.as_ref())
            }

            ClauseGuard::LtInt { left, right, .. } => {
                left.as_ref().to_doc().append(" < ").append(right.as_ref())
            }

            ClauseGuard::LtEqInt { left, right, .. } => {
                left.as_ref().to_doc().append(" <= ").append(right.as_ref())
            }

            ClauseGuard::GtFloat { left, right, .. } => {
                left.as_ref().to_doc().append(" >. ").append(right.as_ref())
            }

            ClauseGuard::GtEqFloat { left, right, .. } => left
                .as_ref()
                .to_doc()
                .append(" >=. ")
                .append(right.as_ref()),

            ClauseGuard::LtFloat { left, right, .. } => {
                left.as_ref().to_doc().append(" <. ").append(right.as_ref())
            }

            ClauseGuard::LtEqFloat { left, right, .. } => left
                .as_ref()
                .to_doc()
                .append(" <=. ")
                .append(right.as_ref()),

            ClauseGuard::Int { value, .. } => value.to_string().to_doc(),

            ClauseGuard::Float { value, .. } => value.to_string().to_doc(),

            ClauseGuard::Var { name, .. } => name.to_string().to_doc(),
        }
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

fn list(elems: Document, tail: Option<Document>) -> Document {
    let doc = break_("[", "[").append(elems);

    match tail {
        None => doc.nest(INDENT).append(break_(",", "")),

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

fn commented<'a>(doc: Document, comments: impl Iterator<Item = &'a str>) -> Document {
    let mut comments = comments.peekable();
    match comments.peek() {
        None => doc,
        Some(_) => concat(
            comments
                .map(|c| "//".to_doc().append(c))
                .intersperse(line()),
        )
        .append(force_break())
        .append(line())
        .append(doc),
    }
}
