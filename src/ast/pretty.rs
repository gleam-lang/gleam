use crate::ast::*;
use crate::pretty::*;

impl<A, B> Documentable for Module<A, B> {
  fn to_doc(self) -> Document {
    nil()
  }
}

impl<A> Documentable for Statement<A> {
  fn to_doc(self) -> Document {
    nil()
  }
}

impl Documentable for UntypedExpr {
  fn to_doc(self) -> Document {
    nil()
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
      } => format!("{}(", name)
        .to_doc()
        .append(type_args_to_doc(args))
        .nest(4)
        .append(")"),

      TypeAst::Fn { meta, args, retrn } => type_args_to_doc(args)
        .surround("(", ")")
        .group()
        .append(" ->")
        .append(retrn.to_doc()),

      TypeAst::Var { meta, name } => name.to_doc(),

      TypeAst::Tuple { meta, elems } => type_args_to_doc(elems).surround("(", ")"),
    }
  }
}

fn type_args_to_doc(args: Vec<TypeAst>) -> Document {
  args
    .split_last()
    .map(|(tail, head)| {
      let elems: Vec<Document> = head
        .into_iter()
        .map(|a| a.clone().to_doc().append(delim(",")).group())
        .collect();
      nil().append_all(vec![elems, vec![tail.clone().to_doc()]].concat())
    })
    .unwrap_or(nil())
}

fn empty_meta() -> Meta {
  Meta { start: 0, end: 0 }
}

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
