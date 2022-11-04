use crate::lsp::*;

#[test]
fn lsp_complete_fun_ret1() {
    let data: &str = r#"pub type Cat {
   Cat(cuteness: Int)
}

fn random_cat() { 
  let x = fn(a : Int) { Cat(cuteness: a) }
  x(1). 
}
"#;
    //line 6, char 7
    let ret = dot_for_node2(data, 6, 7, true);

    assert_eq!(
        ret,
        Some(WhatToDisplay::Names(["cuteness".to_string()].to_vec()))
    );
}

#[test]
fn lsp_complete_fun_ret2() {
    let contents: &str = "import imaginary_import as io 

type Was{
 Was( in_struct: Int )
}

fn main() {
  ErrorOnPurpose
  otherError
}

fn main() {
  let io = fn(a) { Was( in_struct: a) };
  io(1).
}
";

    let res = dot_for_node2(contents, 13, 8, true);
    assert!(res.is_some());
    //todo should be ["in_struct"]
}

#[test]
fn lsp_complete_import() {
    let contents: &str = "import gleam

pub fn main() {
  gleam.
}
";

    let res = dot_for_node2(contents, 3, 8, true);

    assert!(res.is_some());
}

#[test]
fn lsp_complete_import_as() {
    let contents: &str = "import gleam as koto

pub fn main() {
  koto.
}
";

    let res = dot_for_node2(contents, 3, 7, true);
    assert!(res.is_some());
}

#[test]
fn lsp_complete_bind() {
    let contents: &str = "import gleam/io.{println}

type Pub {
  Pub(something: Int)
}

pub fn main() {
  let io = Pub(1);
  io.
}
";

    let res = dot_for_node2(contents, 8, 5, true);
    assert_eq!(
        res,
        Some(WhatToDisplay::Names(["something".to_string()].to_vec()))
    );
}

#[test]
fn partial_infer_statement_names() {
    let data: &str = r#"
pub type Headers =
       List(#(String, String))

pub type Cat {
   Cat(name: String, cuteness: Int)
}
 
pub external fn random_float() -> Float = "rand" "uniform"
 
pub fn random_cat() -> Int { 0 }
 
pub external type Queue(a)
 
import unix/cat
// Import with alias
import animal/cat as kitty
 
pub const start_year = 2101
const end_year = 2111
"#;
    let mut modules: im::HashMap<String, gleam_core::type_::Module> = im::HashMap::new();
    let mut warnings: Vec<gleam_core::type_::Warning> = Vec::new();

    let name = ["random_name".to_string()];
    let mut ids: gleam_core::uid::UniqueIdGenerator = gleam_core::uid::UniqueIdGenerator::new();
    let _ = modules.insert(
        "gleam".to_string(),
        gleam_core::type_::build_prelude(&mut ids),
    );
    let mut environment =
        gleam_core::type_::Environment::new(ids.clone(), &name, &modules, &mut warnings);

    let pi = PartiallyInferedModule::new(data.to_string(), &mut environment);
    assert!(pi.name_to_statement.get("Headers").is_some());
    assert!(pi.name_to_statement.get("Cat").is_some());
    assert!(pi.name_to_statement.get("random_float").is_some());
    assert!(pi.name_to_statement.get("Queue").is_some());
    assert!(pi.name_to_statement.get("Cat").is_some());
    assert!(pi.name_to_statement.get("kitty").is_some());
    assert!(pi.name_to_statement.get("start_year").is_some());
    assert!(pi.name_to_statement.get("end_year").is_some());
    assert!(pi.name_to_statement.get("random_cat").is_some());
}

#[test]
fn partial_infer() {
    let data: &str = r#"
pub type Headers =
       List(#(String, String))

pub type Cat {
   Cat(name: String, cuteness: Int)
}
 
pub external fn random_float() -> Float = "rand" "uniform"
 
pub fn random_cat() -> Int { 0 }
 
pub external type Queue(a)
 
import unix/cat
// Import with alias
import animal/cat as kitty
 
pub const start_year = 2101
const end_year = 2111
"#;

    let mut modules: im::HashMap<String, gleam_core::type_::Module> = im::HashMap::new();
    let mut warnings: Vec<gleam_core::type_::Warning> = Vec::new();

    let name = ["random_name".to_string()];
    let mut ids: gleam_core::uid::UniqueIdGenerator = gleam_core::uid::UniqueIdGenerator::new();
    let _ = modules.insert(
        "gleam".to_string(),
        gleam_core::type_::build_prelude(&mut ids),
    );
    let mut environment =
        gleam_core::type_::Environment::new(ids.clone(), &name, &modules, &mut warnings);

    let pi = PartiallyInferedModule::new(data.to_string(), &mut environment);
    println!("{:#?}", pi.statements_typed);
    assert!(false);
}

#[test]
fn lsp_binds_in_scope() {
    let contents: &str = r#"fn main(p,  p1 : Bool) {
  let a = 1
  let #(b, _) = #(1, 1)
  let c : Int = 1
  io.
}
const co = "lol";
"#;
    //    let lex = gleam_core::parse::lexer::make_tokenizer(contents);
    //    let mut parser = gleam_core::parse::Parser::new(lex);
    //    let expr = parser.parse_statement();
    //    //let expr = parser.ensure_no_errors_or_remaining_input(expr);
    //
    //    let tree = new_tree(contents);
    //
    //    tree.tree.root_node().print(0);
    //
    //    let res = binds_in_scope(&tree, tree_sitter::Point::new(4, 4));
    //    assert_eq!(
    //        res,
    //        [
    //            ("a".to_string(), "Int".to_string()),
    //            ("b".to_string(), "Int".to_string()),
    //            ("c".to_string(), "unknown".to_string()),
    //            ("p".to_string(), "unknown".to_string()),
    //            ("p1".to_string(), "Wow1".to_string())
    //        ]
    //        .to_vec()
    //    );
    assert!(false);
}
