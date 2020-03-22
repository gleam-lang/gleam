use crate::ast::*;
use crate::pretty::*;
use itertools::Itertools;

impl Documentable for UntypedModule {
    fn to_doc(self) -> Document {
        let imports = self.statements.iter().filter_map(|s| match s {
            import @ Statement::Import { .. } => Some(import.clone().to_doc()),
            _ => None,
        });

        let statements = self.statements.iter().filter_map(|s| match s {
            Statement::Import { .. } => None,
            something => Some(something.clone().to_doc()),
        });

        concat(imports).append(line()).append(concat(statements))
    }
}

impl Documentable for ArgNames {
    fn to_doc(self) -> Document {
        match self {
            ArgNames::Discard => "_".to_string(),
            ArgNames::LabelledDiscard { label } => format!("{} _", label),
            ArgNames::Named { name } => name,
            ArgNames::NamedLabelled { name, label } => format!("{} {}", label, name),
        }
        .to_doc()
    }
}

impl Documentable for Arg {
    fn to_doc(self) -> Document {
        self.names
            .to_doc()
            .append(
                self.annotation
                    .map(|a| ": ".to_doc().append(a))
                    .unwrap_or(nil()),
            )
            .group()
    }
}

impl Documentable for RecordConstructor {
    fn to_doc(self) -> Document {
        if self.args.is_empty() {
            self.name.to_doc()
        } else {
            format!("{}(", self.name)
                .to_doc()
                .append(args_to_doc(
                    self.args
                        .into_iter()
                        .map(|(label, typ)| match label {
                            Some(l) => Arg {
                                meta: empty_meta(),
                                names: ArgNames::Named { name: l.clone() },
                                annotation: Some(typ.clone()),
                            }
                            .to_doc(),
                            None => typ.to_doc(),
                        })
                        .collect(),
                ))
                .append(")")
        }
    }
}

impl Documentable for UntypedStatement {
    fn to_doc(self) -> Document {
        match self {
            Statement::Fn {
                name,
                args,
                body,
                public,
                return_annotation,
                ..
            } => if public { "pub ".to_doc() } else { nil() }
                .append(format!("fn {}(", name))
                .append(args_to_doc(args))
                .append(")")
                .append(if let Some(anno) = return_annotation {
                    " -> ".to_doc().append(anno)
                } else {
                    nil()
                })
                .append(" {")
                .append(line().append(body).nest(4).group())
                .append(line())
                .append("}")
                .append(lines(2)),

            Statement::TypeAlias {
                alias,
                args,
                resolved_type,
                public,
                ..
            } => format!("{}type {}", if public { "pub " } else { "" }, alias)
                .to_doc()
                .append(if args.is_empty() {
                    nil()
                } else {
                    "(".to_doc().append(args_to_doc(args)).append(")")
                })
                .append(" = ")
                .group()
                .append(resolved_type)
                .append(lines(2)),

            Statement::CustomType {
                name,
                args,
                public,
                constructors,
                ..
            } => if public { "pub ".to_doc() } else { nil() }
                .to_doc()
                .append("type ")
                .append(if args.is_empty() {
                    name.to_doc()
                } else {
                    format!("{}(", name)
                        .to_doc()
                        .append(args_to_doc(args))
                        .append(")")
                })
                .append(" {")
                .append(concat(
                    constructors
                        .into_iter()
                        .map(|c| line().append(c).nest(4).group()),
                ))
                .append(line())
                .append("}")
                .append(lines(2)),

            Statement::ExternalFn {
                public,
                args,
                name,
                retrn,
                module,
                fun,
                ..
            } => if public { "pub ".to_doc() } else { nil() }
                .to_doc()
                .append("external fn ")
                .group()
                .append(format!("{}(", name))
                .append(args_to_doc(args))
                .append(")")
                .append(" -> ".to_doc().append(retrn))
                .append(" =")
                .append(
                    line()
                        .append(format!("\"{}\" ", module))
                        .append(format!("\"{}\"", fun))
                        .nest(4)
                        .group(),
                )
                .append("\n\n"),

            Statement::ExternalType {
                public, name, args, ..
            } => if public { "pub ".to_doc() } else { nil() }
                .to_doc()
                .append(if args.is_empty() {
                    format!("external type {}", name).to_doc()
                } else {
                    format!("external type {}(", name)
                        .to_doc()
                        .append(args_to_doc(args))
                        .append(")")
                })
                .group()
                .append("\n\n"),

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
                    ".{".to_doc().append(args_to_doc(unqualified)).append("}")
                })
                .append(if let Some(name) = as_name {
                    format!(" as {}", name).to_doc()
                } else {
                    nil()
                })
                .append(line()),
        }
    }
}

impl Documentable for UnqualifiedImport {
    fn to_doc(self) -> Document {
        format!("{}", self.name).to_doc().append(
            self.as_name
                .map(|n| format!(" as {}", n).to_doc())
                .unwrap_or(nil()),
        )
    }
}

impl Documentable for ExternalFnArg {
    fn to_doc(self) -> Document {
        let typ = self.typ.clone();
        let typ_doc = self.typ.clone().to_doc();
        self.label
            .map(|l| format!("{}:", l).to_doc().append(typ).group())
            .unwrap_or(typ_doc)
    }
}

impl<A: Documentable> Documentable for CallArg<A> {
    fn to_doc(self) -> Document {
        self.label
            .map(|l| format!("{}: ", l))
            .unwrap_or("".to_string())
            .to_doc()
            .append(self.value.to_doc())
    }
}

impl Documentable for BinOp {
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

impl Documentable for UntypedPattern {
    fn to_doc(self) -> Document {
        match self {
            Pattern::Int { value, .. } => value.to_doc(),

            Pattern::Float { value, .. } => value.to_doc(),

            Pattern::String { value, .. } => value.to_doc().surround("\"", "\""),

            Pattern::Var { name, .. } => name.to_doc(),

            Pattern::Let { name, pattern, .. } => pattern.to_doc().append("as").append(name),

            Pattern::Discard { .. } => "_".to_doc(),

            Pattern::Nil { .. } => "[]".to_doc(),

            Pattern::Cons { head, tail, .. } => head
                .to_doc()
                .append("|")
                .append((*tail).clone())
                .surround("[", "]"),

            Pattern::Constructor { .. } => todo!(), // TODO

            Pattern::Tuple { elems, .. } => args_to_doc(elems).surround("(", ")"),
        }
    }
}

impl Documentable for UntypedClause {
    fn to_doc(self) -> Document {
        "pattern"
            .to_doc()
            .append(self.guard.map(|g| g.to_doc()).unwrap_or(nil()))
            .append(" -> ")
            .append(match self.then {
                UntypedExpr::Let { .. } => "{"
                    .to_doc()
                    .append(line().append(self.then).nest(4).group())
                    .append(line())
                    .append("}"),
                _ => self.then.to_doc().append(line()),
            })
    }
}

impl<A: Clone> Documentable for ClauseGuard<A> {
    fn to_doc(self) -> Document {
        " guard".to_doc()
    }
}

impl Documentable for UntypedExpr {
    fn to_doc(self) -> Document {
        match self {
            UntypedExpr::Todo { .. } => "todo".to_doc(),

            UntypedExpr::Pipe { left, right, .. } => left.to_doc().append(" |> ").append(*right),

            UntypedExpr::Int { value, .. } => value.to_doc(),

            UntypedExpr::Float { value, .. } => value.to_doc(),

            UntypedExpr::String { value, .. } => value.to_doc().surround("\"", "\""),

            UntypedExpr::Seq { .. } => todo!(),

            UntypedExpr::Var { name, .. } => name.to_doc(),

            UntypedExpr::TupleIndex { tuple, index, .. } => {
                tuple.to_doc().append(".").append(index)
            }

            UntypedExpr::Fn {
                // is_capture, // TODO: render captures
                // return_annotation, // TODO: render this annotation
                args,
                body,
                ..
            } => "fn("
                .to_doc()
                .append(args_to_doc(args).nest_current())
                .append(")")
                .append(" {\n")
                .append((*body).clone())
                .append("\n}"),

            UntypedExpr::Nil { .. } => "[]".to_doc(),

            UntypedExpr::Cons { head, tail, .. } => {
                Document::Cons(Box::new(head.to_doc().append("|")), Box::new(tail.to_doc()))
                    .surround("[", "]")
            }

            UntypedExpr::Call { fun, args, .. } => fun
                .to_doc()
                .append(args_to_doc(args).nest_current().surround("(", ")")),

            UntypedExpr::BinOp {
                name, left, right, ..
            } => left.to_doc().append(name).append(*right.clone()),

            UntypedExpr::Let {
                value,
                pattern,
                then,
                ..
            } => "let "
                .to_doc()
                .append(pattern)
                .append(" = ")
                .append(*value.clone())
                .append(line())
                .append(*then),

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
                .append(concat(
                    clauses
                        .into_iter()
                        .map(|c| line().append(c).nest(4).group()),
                ))
                .append(line())
                .append("}"),

            UntypedExpr::FieldAccess {
                label, container, ..
            } => container.to_doc().append(format!(".{}", label)),

            UntypedExpr::Tuple { elems, .. } => {
                args_to_doc(elems).nest(4).group().surround("(", ")")
            }
        }
    }
}

impl Documentable for TypeAst {
    fn to_doc(self) -> Document {
        match self {
            TypeAst::Constructor { name, args, .. } => {
                if args.is_empty() {
                    name.to_doc()
                } else {
                    format!("{}(", name)
                        .to_doc()
                        .append(args_to_doc(args).nest(4).group())
                        .append(")")
                }
            }

            TypeAst::Fn { args, retrn, .. } => delim("(")
                .append(args_to_doc(args).nest(4))
                .append(") -> ")
                .append(retrn.to_doc()),

            TypeAst::Var { name, .. } => name.to_doc(),

            TypeAst::Tuple { elems, .. } => args_to_doc(elems).nest(4).surround("(", ")"),
        }
    }
}

fn args_to_doc<A: Documentable + Clone>(args: Vec<A>) -> Document {
    args.split_last()
        .map(|(tail, head)| {
            let elems: Vec<Document> = head
                .into_iter()
                .map(|a| a.clone().to_doc().append(", ").group())
                .collect();
            elems.to_doc().append(tail.clone().to_doc())
        })
        .unwrap_or(nil())
}

fn empty_meta() -> Meta {
    Meta { start: 0, end: 0 }
}

#[test]
fn end_to_end_test() {
    let already_formatted = "
import other
import something/else
import library.{ThingA, ThingB}
import doctor as seuss

pub type RoseTree(a) {
  Node(val: a, children: List(RoseTree(a)))
  Leaf(val: a)
}

type Option(a) = Result(a, Nil)

pub external type Opaque

pub external fn random_float() -> Float = \"rand\" \"uniform\"

fn fully_typed(first: Int) -> Int {
    first + 1
}

fn id(x: a, y: b) {
    x
}

pub fn x() {
    id(1.0, 1)
}

fn lets() {
    let x = 1
    let y = 2
    x + y
}

fn patterns(x) {
    case x {
        1 -> 42
        _other -> {
            let x = 3
            3 + 4
        }
    }
}
";
    let stripped = crate::parser::strip_extra(already_formatted);
    if let Ok(module) = crate::grammar::ModuleParser::new().parse(&stripped) {
        // println!("MODULE\n----------------\n{:#?}", module);
        println!("FORMATTED\n-------------\n{}", format(80, module.to_doc()));
    } else {
        println!("ERROR");
    }
}
