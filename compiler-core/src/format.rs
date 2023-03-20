#[cfg(test)]
mod tests;

use crate::{
    ast::{
        CustomType, ExternalFunction, ExternalType, Function, Import, ModuleConstant, TypeAlias,
        Use, *,
    },
    docvec,
    io::Utf8Writer,
    parse::extra::{Comment, ModuleExtra},
    pretty::*,
    type_::{self, Type},
    Error, Result,
};
use itertools::Itertools;
use smol_str::SmolStr;
use std::{path::Path, sync::Arc};
use vec1::Vec1;

const INDENT: isize = 2;

pub fn pretty(writer: &mut impl Utf8Writer, src: &SmolStr, path: &Path) -> Result<()> {
    let (module, extra) = crate::parse::parse_module(src).map_err(|error| Error::Parse {
        path: path.to_path_buf(),
        src: src.clone(),
        error,
    })?;
    let intermediate = Intermediate::from_extra(&extra, src);
    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, writer)
}

pub(crate) struct Intermediate<'a> {
    comments: Vec<Comment<'a>>,
    doc_comments: Vec<Comment<'a>>,
    module_comments: Vec<Comment<'a>>,
    empty_lines: &'a [u32],
}

impl<'a> Intermediate<'a> {
    pub fn from_extra(extra: &'a ModuleExtra, src: &'a SmolStr) -> Intermediate<'a> {
        Intermediate {
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
        }
    }
}

/// Hayleigh's bane
#[derive(Debug, Clone, Default)]
pub struct Formatter<'a> {
    comments: &'a [Comment<'a>],
    doc_comments: &'a [Comment<'a>],
    module_comments: &'a [Comment<'a>],
    empty_lines: &'a [u32],
}

impl<'comments> Formatter<'comments> {
    pub fn new() -> Self {
        Default::default()
    }

    pub(crate) fn with_comments(extra: &'comments Intermediate<'comments>) -> Self {
        Self {
            comments: &extra.comments,
            doc_comments: &extra.doc_comments,
            module_comments: &extra.module_comments,
            empty_lines: extra.empty_lines,
        }
    }

    // Pop comments that occur before a byte-index in the source, consuming
    // and retaining any empty lines contained within.
    fn pop_comments(&mut self, limit: u32) -> impl Iterator<Item = Option<&'comments str>> {
        let (popped, rest, empty_lines) =
            comments_before(self.comments, self.empty_lines, limit, true);
        self.comments = rest;
        self.empty_lines = empty_lines;
        popped
    }

    // Pop doc comments that occur before a byte-index in the source, consuming
    // and dropping any empty lines contained within.
    fn pop_doc_comments(&mut self, limit: u32) -> impl Iterator<Item = Option<&'comments str>> {
        let (popped, rest, empty_lines) =
            comments_before(self.doc_comments, self.empty_lines, limit, false);
        self.doc_comments = rest;
        self.empty_lines = empty_lines;
        popped
    }

    // Remove between 0 and `limit` empty lines following the current position,
    // returning true if any empty lines were removed.
    fn pop_empty_lines(&mut self, limit: u32) -> bool {
        let mut end = 0;
        for (i, &position) in self.empty_lines.iter().enumerate() {
            if position > limit {
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
                ModuleStatement::Import(Import { .. }) => {
                    has_imports = true;
                    let comments = self.pop_comments(start);
                    let statement = self.module_statement(statement);
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

        let imports = join(imports.into_iter(), line());
        let declarations = join(declarations.into_iter(), lines(2));

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

    pub(crate) fn module<'a>(&mut self, module: &'a UntypedModule) -> Document<'a> {
        let groups = join(
            module.statements.iter().map(|t| self.target_group(t)),
            lines(2),
        );

        // Now that `groups` has been collected, only freestanding comments (//)
        // and doc comments (///) remain. Freestanding comments aren't associated
        // with any statement, and are moved to the bottom of the module.
        let doc_comments = join(
            self.doc_comments.iter().map(|comment| {
                "///"
                    .to_doc()
                    .append(Document::String(comment.content.to_string()))
            }),
            line(),
        );

        let comments = match printed_comments(self.pop_comments(u32::MAX), false) {
            Some(comments) => comments,
            None => nil(),
        };

        let module_comments = if !self.module_comments.is_empty() {
            let comments = self.module_comments.iter().map(|s| {
                "////"
                    .to_doc()
                    .append(Document::String(s.content.to_string()))
            });
            join(comments, line()).append(line())
        } else {
            nil()
        };

        let non_empty = vec![module_comments, groups, doc_comments, comments]
            .into_iter()
            .filter(|doc| !doc.is_empty());

        join(non_empty, line()).append(line())
    }

    fn module_statement<'a>(&mut self, statement: &'a UntypedModuleStatement) -> Document<'a> {
        match statement {
            ModuleStatement::Function(Function {
                name,
                arguments: args,
                body,
                public,
                return_annotation,
                end_position,
                ..
            }) => self.statement_fn(
                public,
                name,
                args,
                return_annotation,
                body.as_slice(),
                *end_position,
            ),

            ModuleStatement::TypeAlias(TypeAlias {
                alias,
                parameters: args,
                type_ast: resolved_type,
                public,
                ..
            }) => self.type_alias(*public, alias, args, resolved_type),

            ModuleStatement::CustomType(CustomType {
                name,
                parameters,
                public,
                constructors,
                location,
                opaque,
                ..
            }) => self.custom_type(*public, *opaque, name, parameters, constructors, location),

            ModuleStatement::ExternalFunction(ExternalFunction {
                public,
                arguments: args,
                name,
                return_: retrn,
                module,
                fun,
                ..
            }) => self
                .external_fn_signature(*public, name, args, retrn)
                .append(" =")
                .append(line())
                .append("  \"")
                .append(module.as_str())
                .append("\" \"")
                .append(fun.as_str())
                .append("\""),

            ModuleStatement::ExternalType(ExternalType {
                public,
                name,
                arguments: args,
                ..
            }) => self.external_type(*public, name, args),

            ModuleStatement::Import(Import {
                module,
                as_name,
                unqualified,
                ..
            }) => "import "
                .to_doc()
                .append(module.as_str())
                .append(if unqualified.is_empty() {
                    nil()
                } else {
                    let unqualified = Itertools::intersperse(
                        unqualified
                            .iter()
                            .sorted_by(|a, b| a.name.cmp(&b.name))
                            .map(|e| e.to_doc()),
                        flex_break(",", ", "),
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

            ModuleStatement::ModuleConstant(ModuleConstant {
                public,
                name,
                annotation,
                value,
                ..
            }) => {
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
            Constant::Int { value, .. } => self.int(value),

            Constant::Float { value, .. } => self.float(value),

            Constant::String { value, .. } => self.string(value),

            Constant::List { elements, .. } => self.const_list(elements),

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

            Constant::Var {
                name, module: None, ..
            } => name.to_doc(),

            Constant::Var {
                name,
                module: Some(module),
                ..
            } => docvec![module, ".", name],
        }
    }

    fn const_list<'a, A, B>(&mut self, elements: &'a [Constant<A, B>]) -> Document<'a> {
        if elements.is_empty() {
            return "[]".to_doc();
        }

        let comma: fn() -> Document<'a> = if elements.iter().all(Constant::is_simple) {
            || flex_break(",", ", ")
        } else {
            || break_(",", ", ")
        };
        docvec![
            break_("[", "["),
            join(elements.iter().map(|e| self.const_expr(e)), comma())
        ]
        .nest(INDENT)
        .append(break_(",", ""))
        .append("]")
        .group()
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

    fn documented_statement<'a>(&mut self, s: &'a UntypedModuleStatement) -> Document<'a> {
        let comments = self.doc_comments(s.location().start);
        comments.append(self.module_statement(s).group()).group()
    }

    fn doc_comments<'a>(&mut self, limit: u32) -> Document<'a> {
        let mut comments = self.pop_doc_comments(limit).peekable();
        match comments.peek() {
            None => nil(),
            Some(_) => join(
                comments.map(|c| match c {
                    Some(c) => "///".to_doc().append(Document::String(c.to_string())),
                    None => unreachable!("empty lines dropped by pop_doc_comments"),
                }),
                line(),
            )
            .append(line())
            .force_break(),
        }
    }

    fn type_ast_constructor<'a>(
        &mut self,
        module: &'a Option<SmolStr>,
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
        args: &'a [SmolStr],
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
        body: &'a [UntypedStatement],
        end_location: u32,
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
        let body = self.statements(body);

        // Add any trailing comments
        let body = match printed_comments(self.pop_comments(end_location), false) {
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
        body: &'a [UntypedStatement],
    ) -> Document<'a> {
        let args = wrap_args(args.iter().map(|e| self.fn_arg(e))).group();
        let body = self.statements(body);
        let header = "fn".to_doc().append(args);

        let header = match return_annotation {
            None => header,
            Some(t) => header.append(" -> ").append(self.type_ast(t)),
        };

        header.append(" ").append(wrap_block(body)).group()
    }

    fn statements<'a>(&mut self, statements: &'a [UntypedStatement]) -> Document<'a> {
        let count = statements.len();
        let mut documents = Vec::with_capacity(count * 2);
        for (i, statement) in statements.iter().enumerate() {
            let preceeding_newline = self.pop_empty_lines(statement.start_byte_index());
            if i != 0 && preceeding_newline {
                documents.push(lines(2));
            } else if i != 0 {
                documents.push(lines(1));
            }
            documents.push(self.statement(statement).group());
        }
        if count > 1 {
            documents.to_doc().force_break()
        } else {
            documents.to_doc()
        }
    }

    fn assignment<'a>(&mut self, assignment: &'a UntypedAssignment) -> Document<'a> {
        let Assignment {
            pattern,
            value,
            kind,
            annotation,
            ..
        } = assignment;

        let _ = self.pop_empty_lines(pattern.location().end);

        let keyword = match kind {
            AssignmentKind::Let => "let ",
            AssignmentKind::Assert => "let assert ",
        };

        let pattern = self.pattern(pattern);

        let annotation = annotation
            .as_ref()
            .map(|a| ": ".to_doc().append(self.type_ast(a)));

        keyword
            .to_doc()
            .append(pattern.append(annotation).group())
            .append(" =")
            .append(self.assigned_value(value))
    }

    fn expr<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        let comments = self.pop_comments(expr.start_byte_index());

        let document = match expr {
            UntypedExpr::Panic { .. } => "panic".to_doc(),

            UntypedExpr::Todo { label: None, .. } => "todo".to_doc(),

            UntypedExpr::Todo { label: Some(l), .. } => docvec!["todo(\"", l, "\")"],

            UntypedExpr::PipeLine { expressions, .. } => self.pipeline(expressions),

            UntypedExpr::Int { value, .. } => self.int(value),

            UntypedExpr::Float { value, .. } => self.float(value),

            UntypedExpr::String { value, .. } => self.string(value),

            UntypedExpr::Block { statements, .. } => self.block(statements),

            UntypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => "_".to_doc(),

            UntypedExpr::Var { name, .. } => name.to_doc(),

            UntypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            UntypedExpr::NegateInt { value, .. } => self.negate_int(value),

            UntypedExpr::NegateBool { value, .. } => self.negate_bool(value),

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

            UntypedExpr::Case {
                subjects, clauses, ..
            } => self.case(subjects, clauses),

            UntypedExpr::FieldAccess {
                label, container, ..
            } => if let UntypedExpr::TupleIndex { .. } = container.as_ref() {
                self.expr(container).surround("{ ", " }")
            } else {
                self.expr(container)
            }
            .append(".")
            .append(label.as_str()),

            UntypedExpr::Tuple { elems, .. } => "#"
                .to_doc()
                .append(wrap_args(elems.iter().map(|e| self.expr(e))))
                .group(),

            UntypedExpr::BitString { segments, .. } => bit_string(
                segments
                    .iter()
                    .map(|s| bit_string_segment(s, |e| self.bit_string_segment_expr(e))),
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

    fn string<'a>(&self, string: &'a SmolStr) -> Document<'a> {
        let doc = string.to_doc().surround("\"", "\"");
        if string.contains('\n') {
            doc.force_break()
        } else {
            doc
        }
    }

    fn float<'a>(&self, value: &'a str) -> Document<'a> {
        // Create parts
        let mut parts = value.split('.');
        let integer_part = parts.next().unwrap_or_default();
        // floating point part
        let fp_part = parts.next().unwrap_or_default();
        let integer_doc = self.underscore_integer_string(integer_part);
        let dot_doc = ".".to_doc();

        // Split fp_part into a regular fractional and maybe a scientific part
        let (fp_part_fractional, fp_part_scientific) = fp_part.split_at(
            fp_part
                .chars()
                .position(|ch| ch == 'e')
                .unwrap_or(fp_part.len()),
        );

        // Trim right any consequtive '0's
        let mut fp_part_fractional = fp_part_fractional.trim_end_matches('0').to_string();
        // If there is no fractional part left, add a '0', thus that 1. becomes 1.0 etc.
        if fp_part_fractional.is_empty() {
            fp_part_fractional.push('0');
        }
        let fp_doc = fp_part_fractional.chars().collect::<SmolStr>();

        integer_doc
            .append(dot_doc)
            .append(fp_doc)
            .append(fp_part_scientific)
    }

    fn int<'a>(&self, value: &'a str) -> Document<'a> {
        if value.starts_with("0x") || value.starts_with("0b") || value.starts_with("0o") {
            return value.to_doc();
        }

        self.underscore_integer_string(value)
    }

    fn underscore_integer_string<'a>(&self, value: &'a str) -> Document<'a> {
        let underscore_ch = '_';
        let minus_ch = '-';

        let len = value.len();
        let underscore_ch_cnt = value.matches(underscore_ch).count();
        let reformat_watershed = if value.starts_with(minus_ch) { 6 } else { 5 };
        let insert_underscores = (len - underscore_ch_cnt) >= reformat_watershed;

        let mut new_value = String::new();
        let mut j = 0;
        for (i, ch) in value.chars().rev().enumerate() {
            if ch == '_' {
                continue;
            }

            if insert_underscores && i != 0 && ch != minus_ch && i < len && j % 3 == 0 {
                new_value.push(underscore_ch);
            }
            new_value.push(ch);

            j += 1;
        }

        new_value.chars().rev().collect::<SmolStr>().to_doc()
    }

    fn pattern_constructor<'a>(
        &mut self,
        name: &'a str,
        args: &'a [CallArg<UntypedPattern>],
        module: &'a Option<SmolStr>,
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
        let expr = match fun {
            UntypedExpr::PipeLine { .. } => break_block(self.expr(fun)),

            UntypedExpr::BinOp { .. }
            | UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Block { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::BitString { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::NegateInt { .. } => self.expr(fun),
        };

        match args {
            [arg] if is_breakable_expr(&arg.value) => expr
                .append("(")
                .append(self.call_arg(arg))
                .append(")")
                .group(),

            _ => expr
                .append(wrap_args(args.iter().map(|a| self.call_arg(a))).group())
                .group(),
        }
    }

    pub fn case<'a>(
        &mut self,
        subjects: &'a [UntypedExpr],
        clauses: &'a [UntypedClause],
    ) -> Document<'a> {
        let subjects_doc = break_("case", "case ")
            .append(join(
                subjects.iter().map(|s| self.expr(s)),
                break_(",", ", "),
            ))
            .nest(INDENT)
            .append(break_("", " "))
            .append("{")
            .group();

        let clauses_doc = concat(
            clauses
                .iter()
                .enumerate()
                .map(|(i, c)| self.clause(c, i as u32)),
        );

        subjects_doc
            .append(line().append(clauses_doc).nest(INDENT))
            .append(line())
            .append("}")
            .force_break()
    }

    pub fn record_update<'a>(
        &mut self,
        constructor: &'a UntypedExpr,
        spread: &'a RecordUpdateSpread,
        args: &'a [UntypedRecordUpdateArg],
    ) -> Document<'a> {
        use std::iter::once;
        let constructor_doc = self.expr(constructor);
        let comments = self.pop_comments(spread.base.location().start);
        let spread_doc = commented("..".to_doc().append(self.expr(&spread.base)), comments);
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
            wrap_block(doc).group()
        } else {
            doc
        }
    }

    fn pipeline<'a>(&mut self, expressions: &'a Vec1<UntypedExpr>) -> Document<'a> {
        let mut docs = Vec::with_capacity(expressions.len() * 3);
        let first = expressions.first();
        let first_precedence = first.binop_precedence();
        let first = self.expr(first);
        docs.push(self.operator_side(first, 5, first_precedence));

        for expr in expressions.iter().skip(1) {
            let comments = self.pop_comments(expr.location().start);
            let doc = match expr {
                UntypedExpr::Fn {
                    is_capture: true,
                    body,
                    ..
                } => {
                    let body = match body.first() {
                        Statement::Expression(expression) => expression,
                        Statement::Assignment(_) | Statement::Use(_) => {
                            unreachable!("Non expression capture body")
                        }
                    };
                    self.pipe_capture_right_hand_side(body)
                }

                _ => self.expr(expr),
            };
            docs.push(line());
            docs.push(commented("|> ".to_doc(), comments));
            docs.push(self.operator_side(doc, 4, expr.binop_precedence()));
        }

        docs.to_doc().force_break()
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

    fn fn_capture<'a>(&mut self, call: &'a [UntypedStatement]) -> Document<'a> {
        // The body of a capture being multiple statements shouldn't be possible...
        if call.len() != 1 {
            panic!("Function capture found not to have a single statement call");
        }

        match call.first() {
            Some(Statement::Expression(UntypedExpr::Call {
                fun,
                arguments: args,
                ..
            })) => match args.as_slice() {
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
            _ => panic!("Function capture body found not to be a call in the formatter"),
        }
    }

    pub fn record_constructor<'a, A>(
        &mut self,
        constructor: &'a RecordConstructor<A>,
    ) -> Document<'a> {
        let comments = self.pop_comments(constructor.location.start);
        let doc_comments = self.doc_comments(constructor.location.start);

        let doc = if constructor.arguments.is_empty() {
            constructor.name.as_str().to_doc()
        } else {
            constructor
                .name
                .as_str()
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
        args: &'a [SmolStr],
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
        args: &'a [SmolStr],
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

    fn call_arg<'a>(&mut self, arg: &'a CallArg<UntypedExpr>) -> Document<'a> {
        match &arg.label {
            Some(s) => commented(
                s.to_doc().append(": "),
                self.pop_comments(arg.location.start),
            ),
            None => nil(),
        }
        .append(self.expr(&arg.value))
    }

    fn record_update_arg<'a>(&mut self, arg: &'a UntypedRecordUpdateArg) -> Document<'a> {
        let comments = self.pop_comments(arg.location.start);
        let doc = arg
            .label
            .as_str()
            .to_doc()
            .append(": ")
            .append(self.expr(&arg.value));
        commented(doc, comments)
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
            UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Block { .. }
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

    fn clause<'a>(&mut self, clause: &'a UntypedClause, index: u32) -> Document<'a> {
        let space_before = self.pop_empty_lines(clause.location.start);
        let comments = self.pop_comments(clause.location.start);
        let clause_doc = join(
            std::iter::once(&clause.pattern)
                .chain(&clause.alternative_patterns)
                .map(|p| join(p.iter().map(|p| self.pattern(p)), ", ".to_doc())),
            break_("", " ").append("| "),
        )
        .group();

        let clause_doc = match &clause.guard {
            None => clause_doc,
            Some(guard) => clause_doc.append(" if ").append(self.clause_guard(guard)),
        };

        let clause_doc = commented(clause_doc, comments);

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
        args: &'a [SmolStr],
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
        if elements.is_empty() {
            return match tail {
                Some(tail) => self.expr(tail),
                None => "[]".to_doc(),
            };
        }

        let comma = if tail.is_none() && elements.iter().all(UntypedExpr::is_simple_constant) {
            flex_break(",", ", ")
        } else {
            break_(",", ", ")
        };
        let elements = join(elements.iter().map(|e| self.expr(e)), comma);

        let doc = break_("[", "[").append(elements);

        match tail {
            None => doc.nest(INDENT).append(break_(",", "")),

            Some(tail) => {
                let comments = self.pop_comments(tail.location().start);
                let tail = commented(docvec!["..", self.expr(tail)], comments);
                doc.append(break_(",", ", "))
                    .append(tail)
                    .nest(INDENT)
                    .append(break_("", ""))
            }
        }
        .append("]")
        .group()
    }

    fn pattern<'a>(&mut self, pattern: &'a UntypedPattern) -> Document<'a> {
        let comments = self.pop_comments(pattern.location().start);
        let doc = match pattern {
            Pattern::Int { value, .. } => self.int(value),

            Pattern::Float { value, .. } => self.float(value),

            Pattern::String { value, .. } => self.string(value),

            Pattern::Var { name, .. } => name.to_doc(),

            Pattern::VarUsage { name, .. } => name.to_doc(),

            Pattern::Assign { name, pattern, .. } => {
                self.pattern(pattern).append(" as ").append(name.as_str())
            }

            Pattern::Discard { name, .. } => name.to_doc(),

            Pattern::List { elements, tail, .. } => self.list_pattern(elements, tail),

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

            Pattern::Concatenate {
                left_side_string: left,
                right_side_assignment: right,
                ..
            } => {
                let right = match right {
                    AssignName::Variable(name) => name.to_doc(),
                    AssignName::Discard(name) => name.to_doc(),
                };
                docvec![self.string(left), " <> ", right]
            }
        };
        commented(doc, comments)
    }

    fn list_pattern<'a>(
        &mut self,
        elements: &'a [Pattern<()>],
        tail: &'a Option<Box<Pattern<()>>>,
    ) -> Document<'a> {
        if elements.is_empty() {
            return match tail {
                Some(tail) => self.pattern(tail),
                None => "[]".to_doc(),
            };
        }
        let elements = join(elements.iter().map(|e| self.pattern(e)), break_(",", ", "));
        let doc = break_("[", "[").append(elements);
        match tail {
            None => doc.nest(INDENT).append(break_(",", "")),

            Some(tail) => {
                let comments = self.pop_comments(tail.location().start);
                let tail = if tail.is_discard() {
                    "..".to_doc()
                } else {
                    docvec!["..", self.pattern(tail)]
                };
                let tail = commented(tail, comments);
                doc.append(break_(",", ", "))
                    .append(tail)
                    .nest(INDENT)
                    .append(break_("", ""))
            }
        }
        .append("]")
        .group()
    }

    fn pattern_call_arg<'a>(&mut self, arg: &'a CallArg<UntypedPattern>) -> Document<'a> {
        arg.label
            .as_ref()
            .map(|s| s.to_doc().append(": "))
            .unwrap_or_else(nil)
            .append(self.pattern(&arg.value))
    }

    pub fn clause_guard_bin_op<'a>(
        &mut self,
        name: &'a str,
        name_precedence: u8,
        left: &'a UntypedClauseGuard,
        right: &'a UntypedClauseGuard,
    ) -> Document<'a> {
        let left_precedence = left.precedence();
        let right_precedence = right.precedence();
        let left = self.clause_guard(left);
        let right = self.clause_guard(right);
        self.operator_side(left, name_precedence, left_precedence)
            .append(name)
            .append(self.operator_side(right, name_precedence, right_precedence - 1))
    }

    fn clause_guard<'a>(&mut self, clause_guard: &'a UntypedClauseGuard) -> Document<'a> {
        match clause_guard {
            ClauseGuard::And { left, right, .. } => {
                self.clause_guard_bin_op(" && ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::Or { left, right, .. } => {
                self.clause_guard_bin_op(" || ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::Equals { left, right, .. } => {
                self.clause_guard_bin_op(" == ", clause_guard.precedence(), left, right)
            }

            ClauseGuard::NotEquals { left, right, .. } => {
                self.clause_guard_bin_op(" != ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::GtInt { left, right, .. } => {
                self.clause_guard_bin_op(" > ", clause_guard.precedence(), left, right)
            }

            ClauseGuard::GtEqInt { left, right, .. } => {
                self.clause_guard_bin_op(" >= ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::LtInt { left, right, .. } => {
                self.clause_guard_bin_op(" < ", clause_guard.precedence(), left, right)
            }

            ClauseGuard::LtEqInt { left, right, .. } => {
                self.clause_guard_bin_op(" <= ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::GtFloat { left, right, .. } => {
                self.clause_guard_bin_op(" >. ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::GtEqFloat { left, right, .. } => {
                self.clause_guard_bin_op(" >=. ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::LtFloat { left, right, .. } => {
                self.clause_guard_bin_op(" <. ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::LtEqFloat { left, right, .. } => {
                self.clause_guard_bin_op(" <=. ", clause_guard.precedence(), left, right)
            }

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

    fn negate_bool<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::BinOp { .. } => "!".to_doc().append(wrap_block(self.expr(expr))),
            _ => docvec!["!", self.expr(expr)],
        }
    }

    fn negate_int<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            // Always nest repeated negation in a block to avoid confusion with
            // the pre-decrement operator (which does not exist)
            UntypedExpr::BinOp { .. } | UntypedExpr::NegateInt { .. } => {
                "- ".to_doc().append(wrap_block(self.expr(expr)))
            }

            _ => docvec!["-", self.expr(expr)],
        }
    }

    fn use_<'a>(&mut self, use_: &'a Use) -> Document<'a> {
        let call = if use_.call.is_call() {
            docvec![" ", self.expr(&use_.call)]
        } else {
            docvec![break_("", " "), self.expr(&use_.call)].nest(INDENT)
        }
        .group();

        if use_.assignments.is_empty() {
            docvec!["use <-", call]
        } else {
            let assignments = use_.assignments.iter().map(|pattern| self.pattern(pattern));
            let assignments = Itertools::intersperse(assignments, break_(",", ", "));
            let left = ["use".to_doc(), break_("", " ")]
                .into_iter()
                .chain(assignments);
            let left = concat(left).nest(INDENT).append(break_("", " ")).group();
            docvec![left, "<-", call].group()
        }
    }

    fn bit_string_segment_expr<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::BinOp { .. } => wrap_block(self.expr(expr)),

            UntypedExpr::Int { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Fn { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::PipeLine { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::FieldAccess { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::TupleIndex { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::BitString { .. }
            | UntypedExpr::RecordUpdate { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::NegateInt { .. }
            | UntypedExpr::Block { .. } => self.expr(expr),
        }
    }

    fn statement<'a>(&mut self, statement: &'a Statement<(), UntypedExpr>) -> Document<'a> {
        match statement {
            Statement::Expression(expression) => self.expr(expression),
            Statement::Assignment(assignment) => self.assignment(assignment),
            Statement::Use(use_) => self.use_(use_),
        }
    }

    fn block<'a>(&mut self, statements: &'a [UntypedStatement]) -> Document<'a> {
        docvec![
            "{",
            docvec![break_("", " "), self.statements(statements)].nest(INDENT),
            break_("", " "),
            "}"
        ]
        .group()
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
        self.name.as_str().to_doc().append(match &self.as_name {
            None => nil(),
            Some(s) => " as ".to_doc().append(s.as_str()),
        })
    }
}

fn label(label: &Option<SmolStr>) -> Document<'_> {
    match label {
        Some(s) => s.to_doc().append(": "),
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
            BinOp::RemainderInt => " % ",
            BinOp::Concatenate => " <> ",
        }
        .to_doc()
    }
}

pub fn break_block(doc: Document<'_>) -> Document<'_> {
    "{".to_doc()
        .append(line().append(doc).nest(INDENT))
        .append(line())
        .append("}")
        .force_break()
}

pub fn wrap_block(doc: Document<'_>) -> Document<'_> {
    break_("{", "{ ")
        .append(doc)
        .nest(INDENT)
        .append(break_("", " "))
        .append("}")
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
        .append(join(args, break_(",", ", ")))
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
        .append(join(args, break_(",", ", ")))
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
        flex_break(",", ", ")
    } else {
        break_(",", ", ")
    };
    break_("<<", "<<")
        .append(join(segments.into_iter(), comma))
        .nest(INDENT)
        .append(break_(",", ""))
        .append(">>")
        .group()
}

fn printed_comments<'a, 'comments>(
    comments: impl IntoIterator<Item = Option<&'comments str>>,
    trailing_newline: bool,
) -> Option<Document<'a>> {
    let mut comments = comments.into_iter().peekable();
    let _ = comments.peek()?;

    let mut doc = Vec::new();
    while let Some(c) = comments.next() {
        // There will never be consecutive empty lines (None values),
        // and whenever we peek a None, we advance past it.
        let c = c.expect("no consecutive empty lines");
        doc.push("//".to_doc().append(Document::String(c.to_string())));
        match comments.peek() {
            // Next line is a comment
            Some(Some(_)) => doc.push(line()),
            // Next line is empty
            Some(None) => {
                let _ = comments.next();
                match comments.peek() {
                    Some(_) => doc.push(lines(2)),
                    None => {
                        if trailing_newline {
                            doc.push(lines(2));
                        }
                    }
                }
            }
            // We've reached the end, there are no more lines
            None => {
                if trailing_newline {
                    doc.push(line());
                }
            }
        }
    }
    let doc = concat(doc);
    if trailing_newline {
        Some(doc.force_break())
    } else {
        Some(doc)
    }
}

fn commented<'a, 'comments>(
    doc: Document<'a>,
    comments: impl IntoIterator<Item = Option<&'comments str>>,
) -> Document<'a> {
    match printed_comments(comments, true) {
        Some(comments) => comments.append(doc.group()),
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

        BitStringSegment { value, options, .. } => to_doc(value).append(":").append(join(
            options.iter().map(|o| segment_option(o, |e| to_doc(e))),
            "-".to_doc(),
        )),
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
            .append(Document::String(format!("{value}")))
            .append(")"),
    }
}

pub fn comments_before<'a>(
    comments: &'a [Comment<'a>],
    empty_lines: &'a [u32],
    limit: u32,
    retain_empty_lines: bool,
) -> (
    impl Iterator<Item = Option<&'a str>>,
    &'a [Comment<'a>],
    &'a [u32],
) {
    let end_comments = comments
        .iter()
        .position(|c| c.start > limit)
        .unwrap_or(comments.len());
    let end_empty_lines = empty_lines
        .iter()
        .position(|l| *l > limit)
        .unwrap_or(empty_lines.len());
    let popped_comments = comments
        .get(0..end_comments)
        .expect("0..end_comments is guaranteed to be in bounds")
        .iter()
        .map(|c| (c.start, Some(c.content)));
    let popped_empty_lines = if retain_empty_lines { empty_lines } else { &[] }
        .get(0..end_empty_lines)
        .unwrap_or(&[])
        .iter()
        .map(|i| (i, i))
        // compact consecutive empty lines into a single line
        .coalesce(|(a_start, a_end), (b_start, b_end)| {
            if *a_end + 1 == *b_start {
                Ok((a_start, b_end))
            } else {
                Err(((a_start, a_end), (b_start, b_end)))
            }
        })
        .map(|l| (*l.0, None));
    let popped = popped_comments
        .merge_by(popped_empty_lines, |(a, _), (b, _)| a < b)
        .skip_while(|(_, comment_or_line)| comment_or_line.is_none())
        .map(|(_, comment_or_line)| comment_or_line);
    (
        popped,
        comments.get(end_comments..).expect("in bounds"),
        empty_lines.get(end_empty_lines..).expect("in bounds"),
    )
}

fn is_breakable_expr(expr: &UntypedExpr) -> bool {
    matches!(
        expr,
        UntypedExpr::Fn { .. }
            | UntypedExpr::Block { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::Case { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::Tuple { .. }
            | UntypedExpr::BitString { .. }
    )
}
