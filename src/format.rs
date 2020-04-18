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
    let mut formatter = Formatter::new(&comments);
    Ok(pretty_module(&ast, &mut formatter))
}

pub struct Formatter<'a> {
    comments: &'a [Comment<'a>],
    doc_comments: &'a [Comment<'a>],
}

impl<'a> Formatter<'a> {
    pub fn new(comments: &'a ModuleComments) -> Self {
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

    fn commented(&mut self, limit: usize, doc: Document) -> Document {
        let mut comments = self.pop_comments(limit).peekable();
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
                    let statement = self.statement(statement);
                    imports.push(self.commented(start, statement))
                }

                _other => {
                    has_declarations = true;
                    let statement = self.documented_statement(statement);
                    declarations.push(self.commented(start, statement))
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

        imports.append(sep).append(declarations).append(line())
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
            } => fn_(public, name, args, return_annotation, body),

            Statement::TypeAlias {
                alias,
                args,
                resolved_type,
                public,
                ..
            } => type_alias(*public, alias, args, resolved_type),

            Statement::CustomType {
                name,
                args,
                public,
                constructors,
                ..
            } => custom_type(*public, name, args.as_slice(), constructors),

            Statement::ExternalFn {
                public,
                args,
                name,
                retrn,
                module,
                fun,
                ..
            } => external_fn_signature(*public, name, args, retrn)
                .append(" =")
                .append(line())
                .append(format!("  \"{}\" ", module))
                .append(format!("\"{}\"", fun)),

            Statement::ExternalType {
                public, name, args, ..
            } => external_type(*public, name, args),

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
        let comments = doc_comments(s.location().start, self);
        comments.append(self.statement(s)).group()
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

impl<A> Documentable for &Arg<A> {
    fn to_doc(self) -> Document {
        self.names
            .to_doc()
            .append(match &self.annotation {
                Some(a) => ": ".to_doc().append(a),
                None => nil(),
            })
            .group()
    }
}

impl Documentable for &RecordConstructor {
    fn to_doc(self) -> Document {
        if self.args.is_empty() {
            self.name.clone().to_doc()
        } else {
            self.name
                .to_string()
                .to_doc()
                .append(wrap_args(self.args.iter().map(
                    |(label, typ)| match label {
                        Some(l) => l.to_string().to_doc().append(": ").append(typ),
                        None => typ.to_doc(),
                    },
                )))
        }
    }
}

fn doc_comments(limit: usize, env: &mut Formatter) -> Document {
    let mut comments = env.pop_doc_comments(limit).peekable();
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

pub fn external_type(public: bool, name: &str, args: &[String]) -> Document {
    pub_(public)
        .append("external type ")
        .append(name.to_string())
        .append(if args.is_empty() {
            nil()
        } else {
            wrap_args(args.iter().map(|e| e.clone().to_doc()))
        })
}

pub fn type_alias(public: bool, name: &str, args: &[String], typ: &TypeAst) -> Document {
    pub_(public)
        .append("type ")
        .append(name.to_string())
        .append(if args.is_empty() {
            nil()
        } else {
            wrap_args(args.iter().map(|e| e.clone().to_doc()))
        })
        .append(" =")
        .append(line().append(type_ast(typ)).group().nest(INDENT))
}

pub fn custom_type(
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
        .append(concat(
            constructors
                .into_iter()
                .map(|c| line().append(c).nest(INDENT).group()),
        ))
        .append(line())
        .append("}")
}

pub fn fn_(
    public: &bool,
    name: &str,
    args: &Vec<UntypedArg>,
    return_annotation: &Option<TypeAst>,
    body: &UntypedExpr,
) -> Document {
    pub_(*public)
        .append("fn ")
        .append(name)
        .append(wrap_args(args.iter().map(|e| e.to_doc())))
        .append(if let Some(anno) = return_annotation {
            " -> ".to_doc().append(anno)
        } else {
            nil()
        })
        .append(" {")
        .append(line().append(body).nest(INDENT).group())
        .append(line())
        .append("}")
}

pub fn external_fn_signature(
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
        .append(wrap_args(args.iter().map(|e| e.to_doc())))
        .append(" -> ".to_doc())
        .append(retrn)
}

pub fn docs_fn_signature(
    public: bool,
    name: &str,
    args: &[TypedArg],
    return_type: Arc<Type>,
) -> Document {
    pub_(public)
        .append("fn ")
        .append(name.to_string())
        .append(wrap_args(args.iter().map(|e| e.to_doc())))
        .append(" -> ".to_doc())
        .append(typ::pretty::Printer::new().to_doc(return_type.as_ref()))
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

impl Documentable for &ExternalFnArg {
    fn to_doc(self) -> Document {
        label(&self.label).append(self.typ.to_doc())
    }
}

fn label(label: &Option<String>) -> Document {
    match label {
        Some(s) => s.clone().to_doc().append(": "),
        None => nil(),
    }
}

impl Documentable for &CallArg<UntypedExpr> {
    fn to_doc(self) -> Document {
        match &self.label {
            Some(s) => s.clone().to_doc().append(": "),
            None => nil(),
        }
        .append(wrap_expr(&self.value))
    }
}

impl Documentable for &CallArg<UntypedPattern> {
    fn to_doc(self) -> Document {
        match &self.label {
            Some(s) => s.clone().to_doc().append(": "),
            None => nil(),
        }
        .append(&self.value)
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

impl Documentable for &UntypedPattern {
    fn to_doc(self) -> Document {
        match self {
            Pattern::Int { value, .. } => value.clone().to_doc(),

            Pattern::Float { value, .. } => value.clone().to_doc(),

            Pattern::String { value, .. } => value.clone().to_doc().surround("\"", "\""),

            Pattern::Var { name, .. } => name.to_string().to_doc(),

            Pattern::Let { name, pattern, .. } => {
                pattern.to_doc().append(" as ").append(name.to_string())
            }

            Pattern::Discard { name, .. } => name.to_string().to_doc(),

            Pattern::Nil { .. } => "[]".to_doc(),

            Pattern::Cons { head, tail, .. } => list_cons(
                head.as_ref(),
                tail.as_ref(),
                |e| e.to_doc(),
                categorise_list_pattern,
            ),

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
                ..
            } => name
                .to_string()
                .to_doc()
                .append(wrap_args(args.iter().map(|a| a.to_doc()))),

            Pattern::Constructor {
                name,
                args,
                module: Some(m),
                ..
            } => m
                .to_string()
                .to_doc()
                .append(".")
                .append(name.to_string())
                .append(wrap_args(args.iter().map(|a| a.to_doc()))),

            Pattern::Tuple { elems, .. } => "tuple"
                .to_doc()
                .append(wrap_args(elems.iter().map(|e| e.to_doc()))),
        }
    }
}

impl Documentable for &UntypedClause {
    fn to_doc(self) -> Document {
        let doc = concat(
            std::iter::once(&self.pattern)
                .chain(self.alternative_patterns.iter())
                .map(|p| concat(p.iter().map(|p| p.to_doc()).intersperse(", ".to_doc())))
                .intersperse(" | ".to_doc()),
        );
        match &self.guard {
            None => doc,
            Some(guard) => doc.append(" if ").append(guard),
        }
        .append(" -> ")
        .append(hanging_expr(&self.then))
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

            ClauseGuard::Var { name, .. } => name.to_string().to_doc(),
        }
    }
}

impl Documentable for &UntypedExpr {
    fn to_doc(self) -> Document {
        match self {
            UntypedExpr::Todo { .. } => "todo".to_doc(),

            UntypedExpr::Pipe { left, right, .. } => left
                .to_doc()
                .append(force_break())
                .append(line())
                .append("|> ")
                .append(right.as_ref()),

            UntypedExpr::Int { value, .. } => value.clone().to_doc(),

            UntypedExpr::Float { value, .. } => value.clone().to_doc(),

            UntypedExpr::String { value, .. } => value.clone().to_doc().surround("\"", "\""),

            UntypedExpr::Seq { first, then, .. } => first
                .to_doc()
                .append(force_break())
                .append(line())
                .append(then.as_ref()),

            UntypedExpr::Var { name, .. } => name.clone().to_doc(),

            UntypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, *index),

            UntypedExpr::Fn {
                // is_capture, // TODO: render captures
                // return_annotation, // TODO: render this annotation
                args,
                body,
                ..
            } => expr_fn(args.as_slice(), body.as_ref()),

            UntypedExpr::ListNil { .. } => "[]".to_doc(),

            UntypedExpr::ListCons { head, tail, .. } => list_cons(
                head.as_ref(),
                tail.as_ref(),
                wrap_expr,
                categorise_list_expr,
            ),

            UntypedExpr::Call { fun, args, .. } => fun
                .to_doc()
                .append(wrap_args(args.iter().map(|a| a.to_doc()))),

            UntypedExpr::BinOp {
                name, left, right, ..
            } => left.to_doc().append(name).append(right.as_ref()),

            UntypedExpr::Let {
                value,
                pattern,
                then,
                ..
            } => force_break()
                .append("let ")
                .append(pattern)
                .append(" = ")
                .append(hanging_expr(value.as_ref()))
                .append(line())
                .append(then.as_ref()),

            UntypedExpr::Case {
                subjects, clauses, ..
            } => "case "
                .to_doc()
                .append(concat(
                    subjects
                        .into_iter()
                        .map(|s| s.to_doc())
                        .intersperse(", ".to_doc()),
                ))
                .append(" {")
                .append(
                    line()
                        .append(concat(
                            clauses
                                .into_iter()
                                .map(|c| c.to_doc().group())
                                .intersperse(lines(1)),
                        ))
                        .nest(INDENT),
                )
                .append(line())
                .append("}"),

            UntypedExpr::FieldAccess {
                label, container, ..
            } => container.to_doc().append(format!(".{}", label)),

            UntypedExpr::Tuple { elems, .. } => "tuple"
                .to_doc()
                .append(wrap_args(elems.iter().map(wrap_expr))),
        }
    }
}

fn expr_fn(args: &[UntypedArg], body: &UntypedExpr) -> Document {
    "fn".to_doc()
        .append(wrap_args(args.iter().map(|e| e.to_doc())).nest_current())
        .append(
            break_(" {", " { ")
                .append(body)
                .nest(INDENT)
                .append(delim(""))
                .append("}")
                .group(),
        )
}

fn tuple_index(tuple: &UntypedExpr, index: u64) -> Document {
    match tuple {
        UntypedExpr::TupleIndex { .. } => tuple.to_doc().surround("{", "}"),
        _ => tuple.to_doc(),
    }
    .append(".")
    .append(index)
}

fn wrap_expr(expr: &UntypedExpr) -> Document {
    match expr {
        UntypedExpr::Seq { .. } | UntypedExpr::Let { .. } => "{"
            .to_doc()
            .append(force_break())
            .append(line().append(expr).nest(INDENT))
            .append(line())
            .append("}"),

        _ => expr.to_doc(),
    }
}

fn hanging_expr(expr: &UntypedExpr) -> Document {
    match expr {
        UntypedExpr::Seq { .. } | UntypedExpr::Let { .. } => "{"
            .to_doc()
            .append(force_break())
            .append(line().append(expr).nest(INDENT))
            .append(line())
            .append("}"),

        UntypedExpr::Fn { .. } | UntypedExpr::Case { .. } => expr.to_doc(),

        _ => expr.to_doc().nest(INDENT),
    }
}

pub fn type_ast(t: &TypeAst) -> Document {
    match t {
        TypeAst::Constructor { name, args, .. } if args.is_empty() => name.to_string().to_doc(),

        TypeAst::Constructor { name, args, .. } => name
            .to_string()
            .to_doc()
            .append(wrap_args(args.iter().map(|e| e.to_doc()))),

        TypeAst::Fn { args, retrn, .. } => "fn"
            .to_string()
            .to_doc()
            .append(wrap_args(args.iter().map(|e| e.to_doc())))
            .append(delim(" ->"))
            .append(retrn.to_doc()),

        TypeAst::Var { name, .. } => name.clone().to_doc(),

        TypeAst::Tuple { elems, .. } => "tuple"
            .to_doc()
            .append(wrap_args(elems.iter().map(|e| e.to_doc()))),
    }
}

impl Documentable for &TypeAst {
    fn to_doc(self) -> Document {
        type_ast(self)
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

fn list_cons<ToDoc: Copy, Categorise, Elem>(
    head: Elem,
    tail: Elem,
    to_doc: ToDoc,
    categorise_element: Categorise,
) -> Document
where
    ToDoc: Fn(Elem) -> Document,
    Categorise: Fn(Elem) -> ListType<Elem, Elem>,
{
    let mut elems = vec![head];
    let final_tail = collect_cons(tail, &mut elems, categorise_element);

    let elems = concat(elems.into_iter().map(to_doc).intersperse(delim(",")));

    let doc = break_("[", "[").append(elems);

    match final_tail {
        None => doc.nest(INDENT).append(break_(",", "")),

        Some(final_tail) => doc
            .append(break_(",", " "))
            .append("| ")
            .append(to_doc(final_tail))
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
