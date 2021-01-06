pub(crate) mod command;
#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    fs::Utf8Writer,
    parse::extra::Comment,
    pretty::*,
    typ::{self, Type},
    Error, Result,
};
use itertools::Itertools;
use std::{path::PathBuf, sync::Arc};

const INDENT: isize = 2;

pub fn pretty(writer: &mut impl Utf8Writer, src: &str) -> Result<()> {
    let (module, extra) = crate::parse::parse_module(&src).map_err(|error| Error::Parse {
        path: PathBuf::from("<standard input>"),
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

macro_rules! map_intersperse {
    ($self:expr, $method:ident, $args:expr, $delim:expr) => {{
        map_intersperse!($self, $method, $args, $delim, 0)
    }};

    ($self:expr, $method:ident, $args:expr, $delim:expr, $skip:expr) => {{
        let mut args = $args;
        if args.is_empty() {
            return "()".to_doc();
        }
        let mut args_doc = Vec::with_capacity((args.len() - $skip) * 2 - 1);
        for (i, a) in args.iter().skip($skip).enumerate() {
            if i != 0 {
                args_doc.push($delim);
                args_doc.push($self.$method(a));
            }
        }
        args_doc
    }};
}

macro_rules! bit_string {
    ($self:expr, $method:ident, $segments:expr, $is_simple:expr) => {{
        let comma = if $is_simple {
            delim(",").flex_break()
        } else {
            delim(",")
        };
        let mut docs = Vec::with_capacity($segments.len() * 2);
        for (i, segment) in $segments.iter().enumerate() {
            if i != 0 {
                docs.push(comma);
                docs.push(bit_string_segment!($self, $method, segment));
            }
        }
        break_("<<", "<<")
            .append(docs)
            .nest(INDENT)
            .append(break_(",", ""))
            .append(">>")
            .group()
    }};
}

macro_rules! bit_string_segment {
    ($self:expr, $method:ident, $segment:expr) => {{
        let BitStringSegment { value, options, .. } = $segment;
        if options.is_empty() {
            return $self.$method(value);
        }
        let docs = Vec::with_capacity(options.len() * 2 + 2);
        docs.push($self.$method(value));
        docs.push(":".to_doc());
        for (i, e) in options.iter().enumerate() {
            if i != 0 {
                docs.push("-".to_doc());
            }
            docs.push(segment_option!($self, $method, e));
        }
        docs.to_doc()
    }};
}

macro_rules! segment_option {
    ($self:expr, $method:ident, $option:expr) => {{
        match $option {
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
                .append($self.$method(value.as_ref()))
                .append(")"),

            BitStringSegmentOption::Size {
                value,
                short_form: true,
                ..
            } => $self.$method(value.as_ref()),

            BitStringSegmentOption::Unit {
                value,
                short_form: false,
                ..
            } => "unit"
                .to_doc()
                .append("(")
                .append($self.$method(value.as_ref()))
                .append(")"),

            BitStringSegmentOption::Unit {
                value,
                short_form: true,
                ..
            } => $self.$method(value.as_ref()),
        }
    }};
}

macro_rules! wrap_args {
    ($self:expr, $method:ident, $args:expr) => {{
        let args_doc = map_intersperse!($self, $method, $args, delim(","));
        wrap_args_doc(args_doc)
    }};

    ($self:expr, $method:ident, $args:expr, $delim:expr) => {{
        let args_doc = map_intersperse!($self, $method, $args, $delim);
        wrap_args_doc(args_doc)
    }};

    ($self:expr, $method:ident, $args:expr, $delim:expr, $skip:expr) => {{
        let args_doc = map_intersperse!($self, $method, $args, $delim, $skip);
        wrap_args_doc(args_doc)
    }};
}

struct Intermediate<'a> {
    comments: Vec<Comment<'a>>,
    doc_comments: Vec<Comment<'a>>,
    module_comments: Vec<Comment<'a>>,
    empty_lines: &'a [usize],
}

#[derive(Debug, Clone)]
pub struct Formatter<'a> {
    comments: &'a [Comment<'a>],
    doc_comments: &'a [Comment<'a>],
    module_comments: &'a [Comment<'a>],
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

    fn with_comments(extra: &'a Intermediate<'a>) -> Self {
        Self {
            comments: extra.comments.as_slice(),
            doc_comments: extra.doc_comments.as_slice(),
            module_comments: extra.module_comments.as_slice(),
            empty_lines: &extra.empty_lines,
        }
    }

    // Pop comments that occur before a byte-index in the source
    fn pop_comments(&'a mut self, limit: usize) -> impl Iterator<Item = &'a str> {
        let (popped, rest) = comments_before(self.comments, limit);
        self.comments = rest;
        popped
    }

    // Pop doc comments that occur before a byte-index in the source
    fn pop_doc_comments(&'a mut self, limit: usize) -> impl Iterator<Item = &'a str> {
        let (popped, rest) = comments_before(self.doc_comments, limit);
        self.doc_comments = rest;
        popped
    }

    fn pop_empty_lines(&'a mut self, limit: usize) -> bool {
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

    fn module(&'a mut self, module: &'a UntypedModule) -> Document<'a> {
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
                .iter()
                .map(|comment| line().append("///").append(comment.content)),
        );
        let comments = concat(
            self.comments
                .iter()
                .map(|comment| line().append("//").append(comment.content)),
        );

        let module_comments = if !self.module_comments.is_empty() {
            let comments = self
                .module_comments
                .iter()
                .map(|s| "////".to_doc().append((*s).to_string()).append(line()));
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

    fn statement(&'a mut self, statement: &'a UntypedStatement) -> Document<'a> {
        match statement {
            Statement::Fn {
                name,
                args,
                body,
                public,
                return_annotation,
                end_location,
                ..
            } => self.statement_fn(*public, name, args, return_annotation, body, *end_location),

            Statement::TypeAlias {
                alias,
                args,
                resolved_type,
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
            } => self.custom_type(
                *public,
                *opaque,
                name,
                parameters.as_slice(),
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
                        .map(|e| e.to_doc())
                        .intersperse(delim(",").flex_break());
                    let unqualified = break_("", "")
                        .append(concat(unqualified))
                        .nest(INDENT)
                        .append(break_(",", ""))
                        .group();
                    ".{".to_doc().append(unqualified).append("}")
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

    fn const_expr<A, B>(&'a mut self, value: &'a Constant<A, B>) -> Document<'a> {
        match value {
            Constant::Int { value, .. } | Constant::Float { value, .. } => value.clone().to_doc(),

            Constant::String { value, .. } => value.clone().to_doc().surround("\"", "\""),

            Constant::List { elements, .. } => {
                let comma = if elements.iter().all(|e| e.is_simple()) {
                    delim(",").flex_break()
                } else {
                    delim(",")
                };
                let elements = map_intersperse!(self, const_expr, elements, comma).to_doc();
                list(elements, None)
            }

            Constant::Tuple { elements, .. } => "tuple"
                .to_doc()
                .append(wrap_args!(self, const_expr, elements))
                .group(),

            Constant::BitString { segments, .. } => {
                let simple = segments.iter().all(|s| s.value.is_simple());
                bit_string!(self, const_expr, segments, simple)
            }

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
                .append(wrap_args!(self, constant_call_arg, args))
                .group(),

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
                .append(wrap_args!(self, constant_call_arg, args))
                .group(),
        }
    }

    pub fn docs_const_expr(
        &'a mut self,
        public: bool,
        name: &'a str,
        value: &'a TypedConstant,
    ) -> Document<'a> {
        let mut printer = typ::pretty::Printer::new();

        pub_(public)
            .append("const ")
            .append(name.to_string())
            .append(": ")
            .append(printer.print(value.typ().as_ref()))
            .append(" = ")
            .append(self.const_expr(value))
    }

    fn documented_statement(&'a mut self, s: &'a UntypedStatement) -> Document<'a> {
        let comments = self.doc_comments(s.location().start);
        comments.append(self.statement(s)).group()
    }

    fn doc_comments(&'a mut self, limit: usize) -> Document<'a> {
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
        &'a mut self,
        module: &'a Option<String>,
        name: &'a str,
        args: &'a [TypeAst],
    ) -> Document<'a> {
        let head = match module {
            None => name.to_doc(),
            Some(qualifier) => qualifier.to_string().to_doc().append(".").append(name),
        };

        if args.is_empty() {
            head
        } else {
            head.append(wrap_args!(self, type_ast, args))
        }
    }

    fn type_ast(&'a mut self, t: &'a TypeAst) -> Document<'a> {
        match t {
            TypeAst::Hole { name, .. } => name.clone().to_doc(),

            TypeAst::Constructor {
                name, args, module, ..
            } => self.type_ast_constructor(module, name, args),

            TypeAst::Fn { args, retrn, .. } => "fn"
                .to_string()
                .to_doc()
                .append(wrap_args!(self, type_ast, args))
                .group()
                .append(" ->")
                .append(break_("", " ").append(self.type_ast(retrn)).nest(INDENT)),

            TypeAst::Var { name, .. } => name.clone().to_doc(),

            TypeAst::Tuple { elems, .. } => {
                "tuple".to_doc().append(wrap_args!(self, type_ast, elems))
            }
        }
        .group()
    }

    pub fn str_doc(&'a self, s: &'a str) -> Document<'a> {
        s.to_doc()
    }

    pub fn type_alias(
        &'a mut self,
        public: bool,
        name: &'a str,
        args: &'a [String],
        typ: &'a TypeAst,
    ) -> Document<'a> {
        let head = pub_(public).append("type ").append(name);

        let head = if args.is_empty() {
            head
        } else {
            head.append(wrap_args!(self, str_doc, args).group())
        };

        head.append(" =")
            .append(line().append(self.type_ast(typ)).group().nest(INDENT))
    }

    fn fn_arg<A>(&'a mut self, arg: &'a Arg<A>) -> Document<'a> {
        let comments = self.pop_comments(arg.location.start);
        let doc = match &arg.annotation {
            None => arg.names.to_doc(),
            Some(a) => arg.names.to_doc().append(": ").append(self.type_ast(a)),
        }
        .group();
        commented(doc, comments)
    }

    fn statement_fn(
        &'a mut self,
        public: bool,
        name: &'a str,
        args: &'a [UntypedArg],
        return_annotation: &'a Option<TypeAst>,
        body: &'a UntypedExpr,
        end_location: usize,
    ) -> Document<'a> {
        // Fn name and args
        let head = pub_(public)
            .append("fn ")
            .append(name)
            .append(wrap_args!(self, fn_arg, args));

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

    pub fn external_fn_signature(
        &'a mut self,
        public: bool,
        name: &'a str,
        args: &'a [ExternalFnArg],
        retrn: &'a TypeAst,
    ) -> Document<'a> {
        pub_(public)
            .to_doc()
            .append("external fn ")
            .append(name.to_string())
            .append(self.external_fn_args(args))
            .append(" -> ".to_doc().append(self.type_ast(retrn)))
            .group()
    }

    fn expr_fn(
        &'a mut self,
        args: &'a [UntypedArg],
        return_annotation: Option<&'a TypeAst>,
        body: &'a UntypedExpr,
    ) -> Document<'a> {
        let args = wrap_args!(self, fn_arg, args).group();
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
                    .append(delim(""))
                    .append("}"),
            )
            .group()
    }

    fn seq(&'a mut self, first: &'a UntypedExpr, then: &'a UntypedExpr) -> Document<'a> {
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
        &'a mut self,
        pattern: &'a UntypedPattern,
        value: &'a UntypedExpr,
        then: &'a UntypedExpr,
        kind: BindingKind,
        annotation: &'a Option<TypeAst>,
    ) -> Document<'a> {
        self.pop_empty_lines(pattern.location().end);

        let keyword = match kind {
            BindingKind::Let => "let ",
            BindingKind::Try => "try ",
            BindingKind::Assert => "assert ",
        };

        let pattern = self.pattern(pattern);

        let annotation = annotation
            .as_ref()
            .map(|a| ": ".to_doc().append(self.type_ast(a)));

        force_break()
            .append(keyword)
            .append(pattern.append(annotation).group())
            .append(" =")
            .append(self.assigned_value(value))
            .append(if self.pop_empty_lines(then.start_byte_index()) {
                lines(2)
            } else {
                line()
            })
            .append(self.expr(then))
    }

    fn expr(&'a mut self, expr: &'a UntypedExpr) -> Document<'a> {
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

            UntypedExpr::Call { fun, args, .. } => self.call(fun, args),

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
                .append(wrap_args!(self, wrap_expr, elems))
                .group(),

            UntypedExpr::BitString { segments, .. } => {
                let simple = segments.iter().all(|s| s.value.is_simple_constant());
                bit_string!(self, expr, segments, simple)
            }

            UntypedExpr::RecordUpdate {
                constructor,
                spread,
                args,
                ..
            } => self.record_update(constructor, spread, args),
        };
        commented(document, comments)
    }

    fn pattern_constructor(
        &'a mut self,
        name: &'a str,
        args: &'a [CallArg<UntypedPattern>],
        module: &'a Option<String>,
        with_spread: bool,
    ) -> Document<'a> {
        fn is_breakable(expr: &UntypedPattern) -> bool {
            match expr {
                Pattern::Tuple { .. } | Pattern::Cons { .. } | Pattern::BitString { .. } => true,
                Pattern::Constructor { args, .. } => !args.is_empty(),
                _ => false,
            }
        }

        let name = match module {
            Some(m) => m.to_string().to_doc().append(".").append(name.to_string()),
            None => name.to_string().to_doc(),
        };

        if args.is_empty() && with_spread {
            name.append("(..)")
        } else if args.is_empty() {
            name
        } else if with_spread {
            name.append(self.wrap_pattern_args_with_spread(args))
        } else {
            match &*args {
                [arg] if is_breakable(&arg.value) => name
                    .append("(")
                    .append(self.pattern_call_arg(arg))
                    .append(")")
                    .group(),

                _ => name
                    .append(wrap_args!(self, pattern_call_arg, args))
                    .group(),
            }
        }
    }

    fn call(&'a mut self, fun: &'a UntypedExpr, args: &'a [CallArg<UntypedExpr>]) -> Document<'a> {
        fn is_breakable(expr: &UntypedExpr) -> bool {
            matches!(expr,
                UntypedExpr::Fn { .. }
                | UntypedExpr::Seq { .. }
                | UntypedExpr::Let { .. }
                | UntypedExpr::Call { .. }
                | UntypedExpr::Case { .. }
                | UntypedExpr::Tuple { .. }
                | UntypedExpr::ListCons { .. }
                | UntypedExpr::BitString { .. }
            )
        }

        match args {
            [arg] if is_breakable(&arg.value) => self
                .expr(fun)
                .append("(")
                .append(self.call_arg(arg))
                .append(")")
                .group(),

            _ => self
                .expr(fun)
                .append(wrap_args!(self, call_arg, args))
                .group(),
        }
    }

    pub fn case(
        &'a mut self,
        subjects: &'a [UntypedExpr],
        clauses: &'a [UntypedClause],
    ) -> Document<'a> {
        let subjects_doc = map_intersperse!(self, expr, subjects, ", ".to_doc());
        let mut clauses_doc = Vec::with_capacity(clauses.len());
        for (i, c) in clauses.iter().enumerate() {
            clauses_doc.push(self.clause(c, i));
        }

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

    pub fn record_update(
        &'a mut self,
        constructor: &'a UntypedExpr,
        spread: &'a RecordUpdateSpread,
        args: &'a [UntypedRecordUpdateArg],
    ) -> Document<'a> {
        let constructor_doc = self.expr(constructor);
        let mut all_arg_docs = Vec::with_capacity(args.len() + 1);
        all_arg_docs.push("..".to_doc().append(spread.clone().name.to_doc()));
        for a in args {
            all_arg_docs.push(self.record_update_arg(a));
        }
        constructor_doc.append(wrap_args_doc(all_arg_docs)).group()
    }

    pub fn bin_op(
        &'a mut self,
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

    pub fn operator_side(&'a mut self, doc: Document<'a>, op: u8, side: u8) -> Document<'a> {
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

    fn pipe(
        &'a mut self,
        left: &'a UntypedExpr,
        right: &'a UntypedExpr,
        location_start: usize,
    ) -> Document<'a> {
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

    fn pipe_capture_right_hand_side(&'a mut self, fun: &'a UntypedExpr) -> Document<'a> {
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
                .append(wrap_args!(self, call_arg, args, delim(","), 1).group())
        } else {
            // x |> fun(1, _, 3)
            self.expr(fun)
                .append(wrap_args!(self, call_arg, args).group())
        }
    }

    fn fn_capture(&'a mut self, call: &'a UntypedExpr) -> Document<'a> {
        match call {
            UntypedExpr::Call { fun, args, .. } => self
                .expr(fun)
                .append(wrap_args!(self, call_arg, args).group()),

            // The body of a capture being not a fn shouldn't be possible...
            _ => crate::error::fatal_compiler_bug(
                "Function capture body found not to be a call in the formatter",
            ),
        }
    }

    pub fn record_constructor_arg(
        &'a mut self,
        (label, typ, arg_location): &'a (Option<String>, TypeAst, SrcSpan),
    ) -> Document<'a> {
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
    }

    pub fn record_constructor(&'a mut self, constructor: &'a RecordConstructor) -> Document<'a> {
        let comments = self.pop_comments(constructor.location.start);
        let doc_comments = self.doc_comments(constructor.location.start);

        let doc = if constructor.args.is_empty() {
            constructor.name.clone().to_doc()
        } else {
            constructor
                .name
                .to_string()
                .to_doc()
                .append(wrap_args!(self, record_constructor_arg, constructor.args))
                .group()
        };

        commented(doc_comments.append(doc).group(), comments)
    }

    pub fn custom_type(
        &'a mut self,
        public: bool,
        opaque: bool,
        name: &'a str,
        args: &'a [String],
        constructors: &'a [RecordConstructor],
        location: &'a SrcSpan,
    ) -> Document<'a> {
        self.pop_empty_lines(location.start);
        let doc = pub_(public)
            .append(if opaque { "opaque type " } else { "type " })
            .append(if args.is_empty() {
                name.to_string().to_doc()
            } else {
                name.to_string()
                    .to_doc()
                    .append(wrap_args!(self, str_doc, args))
                    .group()
            })
            .append(" {");

        let doc_constructors = Vec::with_capacity(constructors.len() * 2);
        for c in constructors {
            let doc = if self.pop_empty_lines(c.location.start) {
                lines(2)
            } else {
                line()
            }
            .append(self.record_constructor(c))
            .nest(INDENT)
            .group();
            doc_constructors.push(doc);
        }

        doc.append(doc_constructors).append(line()).append("}")
    }

    pub fn docs_opaque_custom_type(
        &'a mut self,
        public: bool,
        name: &'a str,
        args: &'a [String],
        location: &'a SrcSpan,
    ) -> Document<'a> {
        self.pop_empty_lines(location.start);
        pub_(public)
            .to_doc()
            .append("opaque type ")
            .append(if args.is_empty() {
                name.to_string().to_doc()
            } else {
                name.to_string()
                    .to_doc()
                    .append(wrap_args!(self, str_doc, args))
            })
    }

    pub fn docs_fn_signature(
        &'a mut self,
        public: bool,
        name: &'a str,
        args: &'a [TypedArg],
        return_type: Arc<Type>,
    ) -> Document<'a> {
        let mut printer = typ::pretty::Printer::new();

        pub_(public)
            .append("fn ")
            .append(name)
            .append(self.docs_fn_args(args, &mut printer))
            .append(" -> ".to_doc())
            .append(printer.print(return_type.as_ref()))
    }

    // Will always print the types, even if they were implicit in the original source
    pub fn docs_fn_args(
        &'a mut self,
        args: &'a [TypedArg],
        printer: &'a mut typ::pretty::Printer,
    ) -> Document<'a> {
        let mut args = args.iter().peekable();
        if args.peek().is_none() {
            return "()".to_doc();
        }
        let mut args_doc = Vec::with_capacity(args.len() * 2 - 1);
        for (i, arg) in args.enumerate() {
            if i != 0 {
                args_doc.push(delim(","));
                args_doc.push(
                    arg.names
                        .to_doc()
                        .append(": ".to_doc().append(printer.print(&arg.typ)))
                        .group(),
                );
            }
        }

        wrap_args_doc(args_doc)
    }

    fn external_fn_arg(&'a mut self, arg: &'a ExternalFnArg) -> Document<'a> {
        let comments = self.pop_comments(arg.location.start);
        let doc = label(&arg.label).append(self.type_ast(&arg.typ));
        commented(doc.group(), comments)
    }

    fn external_fn_args(&'a mut self, args: &'a [ExternalFnArg]) -> Document<'a> {
        wrap_args!(self, external_fn_arg, args)
    }

    fn wrap_expr(&'a mut self, expr: &'a UntypedExpr) -> Document<'a> {
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

    fn call_arg(&'a mut self, arg: &'a CallArg<UntypedExpr>) -> Document<'a> {
        match &arg.label {
            Some(s) => commented(
                s.clone().to_doc().append(": "),
                self.pop_comments(arg.location.start),
            ),
            None => nil(),
        }
        .append(self.wrap_expr(&arg.value))
    }

    fn record_update_arg(&'a mut self, arg: &'a UntypedRecordUpdateArg) -> Document<'a> {
        arg.label
            .clone()
            .to_doc()
            .append(": ")
            .append(self.wrap_expr(&arg.value))
    }

    fn tuple_index(&'a mut self, tuple: &'a UntypedExpr, index: u64) -> Document<'a> {
        match tuple {
            UntypedExpr::TupleIndex { .. } => self.expr(tuple).surround("{", "}"),
            _ => self.expr(tuple),
        }
        .append(".")
        .append(index)
    }

    fn case_clause_value(&'a mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::Seq { .. } | UntypedExpr::Let { .. } => " {"
                .to_doc()
                .append(line().append(self.expr(expr)).nest(INDENT).group())
                .append(line())
                .append(force_break())
                .append("}"),

            UntypedExpr::Fn { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::ListCons { .. }
            | UntypedExpr::BitString { .. } => " ".to_doc().append(self.expr(expr)).group(),

            UntypedExpr::Case { .. } => line().append(self.expr(expr)).nest(INDENT).group(),

            _ => break_("", " ").append(self.expr(expr)).nest(INDENT).group(),
        }
    }

    fn assigned_value(&'a mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::Case { .. } => " ".to_doc().append(self.expr(expr)).group(),
            _ => self.case_clause_value(expr),
        }
    }

    fn clause_pattern(&'a mut self, patterns: &'a [UntypedPattern]) -> Document<'a> {
        let mut docs = Vec::with_capacity(patterns.len() * 2 - 1);
        for (i, p) in patterns.iter().enumerate() {
            if i != 0 {
                docs.push(", ".to_doc());
                docs.push(self.pattern(p));
            }
        }
        docs.to_doc()
    }

    fn clause(&'a mut self, clause: &'a UntypedClause, index: usize) -> Document<'a> {
        let space_before = self.pop_empty_lines(clause.location.start);
        let after_position = clause.location.end;
        let mut clause_docs = Vec::with_capacity(clause.alternative_patterns.len() * 2 + 1);
        clause_docs.push(self.clause_pattern(&clause.pattern));
        for p in clause.alternative_patterns {
            clause_docs.push(" | ".to_doc());
            clause_docs.push(self.clause_pattern(p.as_slice()));
        }
        let clause_doc = match &clause.guard {
            None => clause_docs.to_doc(),
            Some(guard) => clause_docs
                .to_doc()
                .append(" if ")
                .append(self.clause_guard(guard)),
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
        .append(" ->")
        .append(self.case_clause_value(&clause.then))
    }

    pub fn external_type(
        &'a mut self,
        public: bool,
        name: &'a str,
        args: &'a [String],
    ) -> Document<'a> {
        pub_(public)
            .append("external type ")
            .append(name.to_string())
            .append(if args.is_empty() {
                nil()
            } else {
                wrap_args!(self, str_doc, args)
            })
    }

    fn list_cons(&'a mut self, head: &'a UntypedExpr, tail: &'a UntypedExpr) -> Document<'a> {
        let (elems, tail) = list_cons(head, tail, categorise_list_expr);
        let comma: fn() -> Document<'a> =
            if tail.is_none() && elems.iter().all(|e| e.is_simple_constant()) {
                || delim(",").flex_break()
            } else {
                || delim(",")
            };
        let mut elem_docs = Vec::with_capacity(elems.len() * 2 - 1);
        for (i, e) in elems.iter().enumerate() {
            if i != 0 {
                elem_docs.push(comma());
                elem_docs.push(self.wrap_expr(e));
            }
        }
        let tail = tail.map(|e| self.expr(e));
        list(Document::Vec(elem_docs), tail)
    }

    fn pattern(&'a mut self, pattern: &'a UntypedPattern) -> Document<'a> {
        let comments = self.pop_comments(pattern.location().start);
        let doc = match pattern {
            Pattern::Int { value, .. } => value.clone().to_doc(),

            Pattern::Float { value, .. } => value.clone().to_doc(),

            Pattern::String { value, .. } => value.clone().to_doc().surround("\"", "\""),

            Pattern::Var { name, .. } => name.to_string().to_doc(),

            Pattern::VarCall { name, .. } => name.to_string().to_doc(),

            Pattern::Let { name, pattern, .. } => self
                .pattern(pattern)
                .append(" as ")
                .append(name.to_string()),

            Pattern::Discard { name, .. } => name.to_string().to_doc(),

            Pattern::Nil { .. } => "[]".to_doc(),

            Pattern::Cons { head, tail, .. } => {
                let (elems, tail) =
                    list_cons(head.as_ref(), tail.as_ref(), categorise_list_pattern);
                let mut elem_docs = Vec::with_capacity(elems.len() * 2 - 1);
                for (i, e) in elems.iter().enumerate() {
                    if i != 0 {
                        elem_docs.push(delim(","));
                    }
                    elem_docs.push(self.pattern(e).group());
                }
                let tail = tail.map(|e| self.pattern(e));
                list(elem_docs.to_doc(), tail)
            }

            Pattern::Constructor {
                name,
                args,
                module,
                with_spread,
                ..
            } => self.pattern_constructor(name, args, module, *with_spread),

            Pattern::Tuple { elems, .. } => "tuple"
                .to_doc()
                .append(wrap_args!(self, pattern, elems))
                .group(),

            Pattern::BitString { segments, .. } => bit_string!(self, pattern, segments, false),
        };
        commented(doc, comments)
    }

    fn pattern_call_arg(&'a mut self, arg: &'a CallArg<UntypedPattern>) -> Document<'a> {
        match &arg.label {
            Some(s) => s.clone().to_doc().append(": "),
            None => nil(),
        }
        .append(self.pattern(&arg.value))
    }

    fn clause_guard(&'a mut self, clause_guard: &'a UntypedClauseGuard) -> Document<'a> {
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

            ClauseGuard::TupleIndex { tuple, index, .. } => self
                .clause_guard(tuple.as_ref())
                .append(".")
                .append(index.to_string())
                .to_doc(),

            ClauseGuard::Constant(constant) => self.const_expr(constant),
        }
    }

    fn constant_call_arg<A, B>(&'a mut self, arg: &'a CallArg<Constant<A, B>>) -> Document<'a> {
        match &arg.label {
            None => self.const_expr(&arg.value),
            Some(s) => s
                .clone()
                .to_doc()
                .append(": ")
                .append(self.const_expr(&arg.value)),
        }
    }

    pub fn wrap_pattern_args_with_spread(
        &'a mut self,
        args: &'a [CallArg<UntypedPattern>],
    ) -> Document<'a> {
        if args.is_empty() {
            return "()".to_doc();
        }

        let mut docs = Vec::with_capacity(args.len() * 2 + 3);
        docs.push(break_("(", "("));
        for (i, a) in args.iter().enumerate() {
            if i != 0 {
                docs.push(delim(","));
                docs.push(self.pattern_call_arg(a));
            }
        }
        docs.push(break_(",", ", "));
        docs.push("..".to_doc());

        Document::Vec(docs)
            .nest(INDENT)
            .append(break_(",", ""))
            .append(")")
            .group()
    }
}

fn wrap_args_doc(args_doc: Vec<Document<'_>>) -> Document<'_> {
    break_("(", "(")
        .append(args_doc)
        .nest(INDENT)
        .append(break_(",", ""))
        .append(")")
}

impl<'a> Documentable<'a> for &'a ArgNames {
    fn to_doc(self) -> Document<'a> {
        match self {
            ArgNames::Discard { name } => name.to_string(),
            ArgNames::LabelledDiscard { label, name } => format!("{} {}", label, name),
            ArgNames::Named { name } => name.to_string(),
            ArgNames::NamedLabelled { name, label } => format!("{} {}", label, name),
        }
        .to_doc()
    }
}

fn pub_<'a>(public: bool) -> Document<'a> {
    if public {
        "pub ".to_doc()
    } else {
        nil()
    }
}

impl<'a> Documentable<'a> for &'a UnqualifiedImport {
    fn to_doc(self) -> Document<'a> {
        self.name.clone().to_doc().append(match &self.as_name {
            None => nil(),
            Some(s) => " as ".to_doc().append(s.clone()),
        })
    }
}

fn label<'a>(label: &'a Option<String>) -> Document<'a> {
    match label {
        Some(s) => s.clone().to_doc().append(": "),
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

fn list<'a>(elems: Document<'a>, tail: Option<Document<'a>>) -> Document<'a> {
    let doc = break_("[", "[").append(elems);

    match tail {
        None => doc.nest(INDENT).append(break_(",", "")),

        // Don't print tail if it is a discard
        Some(Document::Text(t)) if t == *"_" => doc
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

fn printed_comments<'a>(comments: impl Iterator<Item = &'a str> + 'a) -> Option<Document<'a>> {
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

fn commented<'a>(doc: Document<'a>, comments: impl Iterator<Item = &'a str> + 'a) -> Document<'a> {
    match printed_comments(comments) {
        Some(comments) => comments.append(force_break()).append(line()).append(doc),
        None => doc,
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
    let popped = comments[0..end].iter().map(|c| c.content);
    (popped, &comments[end..])
}
