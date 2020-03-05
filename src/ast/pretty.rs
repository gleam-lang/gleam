use crate::ast::*;
use crate::pretty::*;
use itertools::Itertools;

impl<B: Clone> Documentable for Module<TypedExpr, B> {
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
    self
      .names
      .to_doc()
      .append(
        self
          .annotation
          .map(|a| ": ".to_doc().append(a))
          .unwrap_or(nil()),
      )
      .group()
  }
}

impl Documentable for RecordConstructor {
  fn to_doc(self) -> Document {
    format!("{}(", self.name)
      .to_doc()
      .append(args_to_doc(
        self
          .args
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

impl Documentable for Statement<TypedExpr> {
  fn to_doc(self) -> Document {
    match self {
      Statement::Fn {
        meta,
        name,
        args,
        body,
        public,
        return_annotation,
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
        meta,
        alias,
        args,
        resolved_type,
        public,
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
        meta,
        name,
        args,
        public,
        constructors,
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
        .append_all(
          constructors
            .into_iter()
            .map(|c| line().append(c).nest(4).group())
            .collect(),
        )
        .append(line())
        .append("}")
        .append(lines(2)),

      Statement::ExternalFn {
        meta,
        public,
        args,
        name,
        retrn,
        module,
        fun,
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
        meta,
        public,
        name,
        args,
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
        meta,
        module,
        as_name,
        unqualified,
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
      self
        .as_name
        .map(|n| format!(" as {}", n).to_doc())
        .unwrap_or(nil()),
    )
  }
}

impl Documentable for ExternalFnArg {
  fn to_doc(self) -> Document {
    let typ = self.typ.clone();
    let typ_doc = self.typ.clone().to_doc();
    self
      .label
      .map(|l| format!("{}:", l).to_doc().append(typ).group())
      .unwrap_or(typ_doc)
  }
}

impl<A: Documentable> Documentable for CallArg<A> {
  fn to_doc(self) -> Document {
    self
      .label
      .map(|l| format!("{}: ", l))
      .unwrap_or("".to_string())
      .to_doc()
      .append(self.value.to_doc())
  }
}

impl Documentable for BinOp {
  fn to_doc(self) -> Document {
    match self {
      BinOp::Pipe => " |> ",
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

impl<C: Clone> Documentable for Pattern<C> {
  fn to_doc(self) -> Document {
    match self {
      Pattern::Int { meta, value } => value.to_doc(),

      Pattern::Float { meta, value } => value.to_doc(),

      Pattern::String { meta, value } => value.to_doc().surround("\"", "\""),

      Pattern::Var { meta, name } => name.to_doc(),

      Pattern::Let { name, pattern } => pattern.to_doc().append("as").append(name),

      Pattern::Discard { meta } => "_".to_doc(),

      Pattern::Nil { meta } => "[]".to_doc(),

      Pattern::Cons { meta, head, tail } => head
        .to_doc()
        .append("|")
        .append((*tail).clone())
        .surround("[", "]"),

      Pattern::Constructor {
        meta,
        name,
        args,
        module,
        constructor,
      } => nil(),

      Pattern::Tuple { meta, elems } => args_to_doc(elems).surround("(", ")"),
    }
  }
}

impl<A: Documentable, B: Clone, C: Clone> Documentable for Clause<A, B, C> {
  fn to_doc(self) -> Document {
    "pattern"
      .to_doc()
      .append(self.guard.map(|g| g.to_doc()).unwrap_or(nil()))
      .append(" -> ")
      .append(self.then.to_doc())
  }
}

impl<A: Clone> Documentable for ClauseGuard<A> {
  fn to_doc(self) -> Document {
    " guard".to_doc()
  }
}

impl Documentable for TypedExpr {
  fn to_doc(self) -> Document {
    match self {
      TypedExpr::Int { meta, typ, value } => value.to_doc(),

      TypedExpr::Float { meta, typ, value } => value.to_doc(),

      TypedExpr::String { meta, typ, value } => value.to_doc().surround("\"", "\""),

      TypedExpr::Seq { typ, first, then } => nil(),

      TypedExpr::Var {
        meta,
        constructor,
        name,
      } => name.to_doc(),

      TypedExpr::Fn {
        meta,
        typ,
        is_capture,
        args,
        body,
        return_annotation,
      } => "fn("
        .to_doc()
        .append(args_to_doc(args).nest_current())
        .append(")")
        .append(" {\n")
        .append((*body).clone())
        .append("\n}"),

      TypedExpr::Nil { meta, typ } => "[]".to_doc(),

      TypedExpr::Cons {
        meta,
        typ,
        head,
        tail,
      } => Document::Cons(Box::new(head.to_doc().append("|")), Box::new(tail.to_doc()))
        .surround("[", "]"),

      TypedExpr::Call {
        meta,
        typ,
        fun,
        args,
      } => fun
        .to_doc()
        .append(args_to_doc(args).nest_current().surround("(", ")")),

      TypedExpr::BinOp {
        meta,
        typ,
        name,
        left,
        right,
      } => left.to_doc().append(name).append(*right.clone()),

      TypedExpr::Let {
        meta,
        typ,
        value,
        pattern,
        then,
      } => "let "
        .to_doc()
        .append(pattern)
        .append(" = ")
        .append(*value.clone())
        .append(line())
        .append(*then),

      TypedExpr::Case {
        meta,
        typ,
        subjects,
        clauses,
      } => "case "
        .to_doc()
        .append_all(
          subjects
            .into_iter()
            .map(|s| s.to_doc())
            .intersperse(", ".to_doc())
            .collect(),
        )
        .append(" {")
        .append_all(
          clauses
            .into_iter()
            .map(|c| line().append(c).nest(4).group())
            .collect(),
        )
        .append(line())
        .append("}"),

      TypedExpr::RecordAccess {
        meta,
        typ,
        label,
        index,
        record,
      } => record.to_doc().append(format!(".{}", label)),

      TypedExpr::ModuleSelect {
        meta,
        typ,
        label,
        module_name,
        module_alias,
        constructor,
      } => nil(),

      TypedExpr::Tuple { meta, typ, elems } => {
        args_to_doc(elems).nest(4).group().surround("(", ")")
      }
    }
  }
}

impl Documentable for TypeAst {
  fn to_doc(self) -> Document {
    match self {
      TypeAst::Constructor {
        meta,
        module,
        name,
        args,
      } => {
        if args.is_empty() {
          name.to_doc()
        } else {
          format!("{}(", name)
            .to_doc()
            .append(args_to_doc(args).nest(4).group())
            .append(")")
        }
      }

      TypeAst::Fn { meta, args, retrn } => delim("(")
        .append(args_to_doc(args).nest(4))
        .append(delim_end(") -> "))
        .append(retrn.to_doc()),

      TypeAst::Var { meta, name } => name.to_doc(),

      TypeAst::Tuple { meta, elems } => args_to_doc(elems).nest(4).surround("(", ")"),
    }
  }
}

fn args_to_doc<A: Documentable>(args: Vec<A>) -> Document {
  args
    .split_last()
    .map(|(tail, head)| {
      let elems: Vec<Document> = head
        .into_iter()
        .map(|a| a.clone().to_doc().append(", ").group())
        .collect();
      nil().append_all(vec![elems, vec![tail.clone().to_doc()]].concat())
    })
    .unwrap_or(nil())
}

fn empty_meta() -> Meta {
  Meta { start: 0, end: 0 }
}

////////////////////////////////////////////////////
///
///
///
///
/// ////////////////////////////////////////////////

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
  let (stripped, _) = crate::parser::strip_extra(already_formatted);
  if let Ok(module) = crate::grammar::ModuleParser::new().parse(&stripped) {
    // println!("MODULE\n----------------\n{:#?}", module);
    println!("FORMATTED\n-------------\n{}", format(80, module.to_doc()));
  } else {
    println!("ERROR");
  }
}

////////////////////////////////////////////////////
///
///
///
///
/// ////////////////////////////////////////////////

#[test]
fn type_doc_test() {
  let ast = TypeAst::Constructor {
    meta: Meta { start: 0, end: 0 },
    module: None,
    name: "Test".to_string(),
    args: vec![
      TypeAst::Var {
        meta: empty_meta(),
        name: "Int".to_string(),
      },
      TypeAst::Var {
        meta: empty_meta(),
        name: "Int".to_string(),
      },
    ],
  };
  let expected = "
Test(Int, Int)
"
  .trim();
  assert_eq!(pretty::format(30, ast.clone().to_doc()), expected);

  let expected = "
Test(
  Int,
  Int
)
"
  .trim();
  assert_eq!(pretty::format(5, ast.clone().to_doc()), expected);
}

#[test]
fn tuple_type_test() {
  let ast = TypeAst::Tuple {
    meta: Meta { start: 0, end: 0 },
    elems: vec![
      TypeAst::Var {
        meta: Meta { start: 0, end: 0 },
        name: "A".to_string(),
      },
      TypeAst::Var {
        meta: Meta { start: 0, end: 0 },
        name: "B".to_string(),
      },
    ],
  };

  assert_eq!(pretty::format(10, ast.clone().to_doc()), "(A, B)");
  assert_eq!(pretty::format(3, ast.clone().to_doc()), "(A,\nB)");
}

#[test]
fn function_type_test() {
  let ast = TypeAst::Fn {
    meta: empty_meta(),
    args: vec![
      TypeAst::Var {
        meta: empty_meta(),
        name: "Int".to_string(),
      },
      TypeAst::Var {
        meta: empty_meta(),
        name: "Int".to_string(),
      },
    ],
    retrn: Box::new(TypeAst::Var {
      meta: empty_meta(),
      name: "Int".to_string(),
    }),
  };

  let expected = "( Int, Int ) -> Int";

  assert_eq!(format(30, ast.clone().to_doc()), expected);

  let expected = "(Int, Int) -> Int";
  assert_eq!(format(10, ast.clone().to_doc()), expected);
}
