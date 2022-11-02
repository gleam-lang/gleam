use std::collections::HashMap;
use tree_sitter::{Parser, Tree, TreeCursor};

use tree_sitter::Point;

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
    let lex = gleam_core::parse::lexer::make_tokenizer(contents);
    let mut parser = gleam_core::parse::Parser::new(lex);
    let expr = parser.parse_statement();
    //let expr = parser.ensure_no_errors_or_remaining_input(expr);

    println!("wtf");
    println!("expression: {expr:#?}");

    let tree = new_tree(contents);

    print_tree(&tree.tree.root_node(), 0);

    let res = binds_in_scope(&tree, tree_sitter::Point::new(4, 4));
    assert_eq!(
        res,
        [
            ("a".to_string(), "Int".to_string()),
            ("b".to_string(), "Int".to_string()),
            ("c".to_string(), "unknown".to_string()),
            ("p".to_string(), "unknown".to_string()),
            ("p1".to_string(), "Wow1".to_string())
        ]
        .to_vec()
    );
}

//let mut statements : Vec<gleam_core::ast::Statement> = Vec::new();

#[derive(Debug)]
struct Bindings {
    name: String,
    type_notation: String,
}

use gleam_core::ast::{Arg, ArgNames};

fn parse_function_args(text: &str) -> Vec<Bindings> {
    let lex = gleam_core::parse::lexer::make_tokenizer(text);
    let mut parser = gleam_core::parse::Parser::new(lex);

    let _ = parser.expect_one(&gleam_core::parse::token::Token::LeftParen);
    let expr = parser.parse_function_args(false);
    // let expr = parser.ensure_no_errors_or_remaining_input(expr);

    println!("{expr:#?}");

    match expr {
        Ok(args) => args
            .iter()
            .map(|arg| match arg {
                Arg {
                    names: ArgNames::Named { name: n },
                    annotation: ann,
                    ..
                } => Some(Bindings {
                    name: n.clone(),
                    type_notation: "".to_string(),
                }),
                _ => None,
            })
            .filter(|x| x.is_some())
            .map(|x| x.expect("is some"))
            .collect(),
        _ => Vec::new(),
    }
}

fn parse_function_body(text: &str) -> Vec<Bindings> {
    let lex = gleam_core::parse::lexer::make_tokenizer(text);
    let mut parser = gleam_core::parse::Parser::new(lex);

    let expr = parser.parse_expression_seq();

    println!("{text:#?}");
    println!("{expr:#?}");

    let mut outbindings = Vec::new();

    if let Ok(expr) = expr {
        if let Some(ast) = expr {
            extract_bindings_from_ast(&ast.0, &mut outbindings)
        }
    }

    outbindings
}

fn extract_binding_from_assign(pattern: &gleam_core::ast::Pattern<(), ()>, to: &mut Vec<Bindings>) {
    match pattern {
        gleam_core::ast::Pattern::Var { name: n, .. } => to.push(Bindings {
            name: n.to_string(),
            type_notation: "unknown".to_string(),
        }),
        gleam_core::ast::Pattern::Tuple { elems, .. } => elems
            .iter()
            .for_each(|x| extract_binding_from_assign(x, to)),
        _ => (),
    }
}

fn extract_bindings_from_ast(ast: &gleam_core::ast::UntypedExpr, to: &mut Vec<Bindings>) {
    match ast {
        gleam_core::ast::UntypedExpr::Assignment { pattern: p, .. } => {
            extract_binding_from_assign(p, to)
        }
        gleam_core::ast::UntypedExpr::Sequence {
            expressions: exprs, ..
        } => exprs.iter().for_each(|x| extract_bindings_from_ast(x, to)),
        something => println!("unhandled: {something:?}"),
    }
}

fn binds_in_scope(module: &PartialParsedModule, point: Point) -> Vec<(String, String)> {
    let prev_point = Point {
        column: point.column - 1,
        ..point
    };
    let mut node = module
        .tree
        .root_node()
        .named_descendant_for_point_range(prev_point, point);

    if node.is_none() {
        println!("from cursor was none");
        return Vec::new();
    }

    //are we in a function ?
    //let tc : TreeCursor = node.expect("has one").walk();
    let mut collected: Vec<tree_sitter::Node> = Vec::new();
    let mut bindings: Vec<Bindings> = Vec::new();

    // get its parameters
    // get the previous statements, parent by parent
    loop {
        if node == None {
            break;
        }
        let parent = node.expect("already has").parent();
        //reached top level
        if parent.is_none() {
            break;
        }

        let prev_sibling = node.expect("already has").prev_sibling();
        //reached top level
        if prev_sibling.is_none() {
            node = parent;
        } else {
            match node.expect("already has").kind() {
                "function_body" => bindings.append(&mut parse_function_body(
                    node.expect("")
                        .utf8_text(module.source_code.as_bytes())
                        .expect("source should have code"),
                )),
                "function_parameters" => bindings.append(&mut parse_function_args(
                    node.expect("")
                        .utf8_text(module.source_code.as_bytes())
                        .expect("source should have code"),
                )),
                "function" => collected.push(node.unwrap()),
                _ => (),
            }
            node = prev_sibling;
        }
    }
    println!("{collected:?}");
    println!("bindings {bindings:?}");

    Vec::new()
}

#[test]
fn lsp_complete_as_import() {
    let contents: &str = "import gleam/io as koto

pub fn main() {
  io.println(\"Hello from t1!\");
  koto.
}

";

    let tree = new_tree(contents);

    print_tree(&tree.tree.root_node(), 0);
    let res = dot_for_node(&tree, tree_sitter::Point::new(4, 6), LspEnv {});
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

    let tree = new_tree(contents);
    let res = dot_for_node(&tree, tree_sitter::Point::new(8, 4), LspEnv {});
    println!("{res:?}");
    assert_eq!(
        res,
        Some(WhatToDisplay::Names(["something".to_string()].to_vec()))
    );
}

#[test]
fn lsp_complete_import() {
    let contents: &str = "import gleam/io.{println}

pub fn main() {
  io.println(\"Hello from t1!\");
  io.
}

";

    let tree = new_tree(contents);
    let res = dot_for_node(&tree, tree_sitter::Point::new(4, 4), LspEnv {});

    assert!(res.is_some());
}

struct PartialParsedModule {
    tree: Tree,
    imports: Vec<Import>,
    source_code: String,
}

#[derive(Clone, Debug)]
struct Import {
    module: String,
    module_import_name: String,
    qualified_imports: Vec<String>,
    //   as_alias: Option<String>
}

trait Seek {
    fn seek_next(self: &mut Self, kind: &String) -> bool;
}
impl Seek for TreeCursor<'_> {
    fn seek_next(self: &mut Self, kind: &String) -> bool {
        loop {
            if self.node().kind() == kind {
                return true;
            }
            if !(self.goto_next_sibling()) {
                return false;
            }
        }
    }
}

trait Extractor {
    fn print(&self, level: usize);
    fn as_import(&self, source_code: &str) -> Option<Import>;
    fn all_childs_of(&self, kind: &str) -> Vec<tree_sitter::Node>;
    fn as_string(&self, source_code: &str) -> String;
}

trait Navigator {
    fn as_kind(&self, kind: &str) -> Option<tree_sitter::Node>;
    fn first_child_of_kind(&self, kind: &str) -> Option<tree_sitter::Node>;
    fn next_sibling(&self) -> Option<tree_sitter::Node>;
}

impl Navigator for tree_sitter::Node<'_> {
    fn next_sibling(&self) -> Option<tree_sitter::Node> {
        let mut cursor = self.walk();
        if cursor.goto_next_sibling() {
            return Some(cursor.node());
        }
        return None;
    }

    fn first_child_of_kind(&self, kind: &str) -> Option<tree_sitter::Node> {
        let mut cursor = self.walk();

        if cursor.goto_first_child() && cursor.seek_next(&kind.to_string()) {
            return Some(cursor.node());
        }
        return None;
    }

    fn as_kind(&self, kind: &str) -> Option<tree_sitter::Node> {
        if self.kind() == kind {
            Some(*self)
        } else {
            None
        }
    }
}

impl Navigator for Option<tree_sitter::Node<'_>> {
    fn next_sibling(&self) -> Option<tree_sitter::Node> {
        if let Some(node) = self {
            return node.next_sibling();
        }
        return None;
    }

    fn first_child_of_kind(&self, kind: &str) -> Option<tree_sitter::Node> {
        if let Some(node) = self {
            return node.first_child_of_kind(kind);
        }
        return None;
    }

    fn as_kind(&self, kind: &str) -> Option<tree_sitter::Node> {
        if let Some(node) = self {
            return node.as_kind(kind);
        }
        return None;
    }
}

impl Extractor for tree_sitter::Node<'_> {
    fn as_string(&self, source_code: &str) -> String {
        self.utf8_text(source_code.as_bytes())
            .expect("should work")
            .to_string()
    }

    fn print(&self, level: usize) {
        print!(
            "{}",
            std::iter::repeat("    ").take(level).collect::<String>()
        );
        println!("{:?}", self);

        for i in 0..self.child_count() {
            let x = self.child(i).expect("should exists");
            x.print(level + 1);
        }
    }

    fn all_childs_of(&self, kind: &str) -> Vec<tree_sitter::Node> {
        let mut ret = Vec::new();

        for i in 0..self.child_count() {
            let x = self.child(i).expect("should exists");
            if x.kind() == kind {
                ret.push(x);
            }
        }

        ret
    }

    fn as_import(&self, source_code: &str) -> Option<Import> {
        match self.kind() {
            "import" => {
                let mut qa: Vec<String> = Vec::new();

                let module = self.child_by_field_name("module").and_then(|x| {
                    Some(
                        x.utf8_text(source_code.as_bytes())
                            .expect("should work")
                            .to_string(),
                    )
                });

                match self.all_childs_of("unqualified_imports").first() {
                    Some(nx) => {
                        qa = nx
                            .all_childs_of("unqualified_import")
                            .iter()
                            .map(|x| {
                                x.utf8_text(source_code.as_bytes())
                                    .expect("should work")
                                    .to_string()
                            })
                            .collect()
                    }
                    _ => (),
                }

                let mut cursor = self.walk();
                let mut as_alias = None;

                if cursor.goto_first_child()
                    && cursor.seek_next(&"as".to_string())
                    && cursor.goto_next_sibling()
                {
                    let as_node = cursor.node();
                    if as_node.kind() == "identifier" {
                        as_alias = Some(
                            as_node
                                .utf8_text(source_code.as_bytes())
                                .expect("should work")
                                .to_string(),
                        );
                    }
                }

                module.and_then(|m| {
                    let import_name = m.split("/").last().expect("at least one").to_string();
                    let module_import_name = as_alias.as_ref().unwrap_or(&import_name).clone();

                    Some(Import {
                        module: m.to_string(),
                        qualified_imports: qa,
                        module_import_name: module_import_name,
                        //as_alias: as_alias.clone()
                    })
                })
            }

            _ => None,
        }
    }
}

fn collect_all_imports(tree: &Tree, source_code: &str) -> Vec<Import> {
    let node = tree.root_node();

    node.all_childs_of("import")
        .iter()
        .map(|x| x.as_import(source_code))
        .filter(|x| x.is_some())
        .map(|x| x.expect("after filter"))
        .collect()
}

fn new_tree(source_code: &str) -> PartialParsedModule {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_gleam::language())
        .expect("Error loading gleam language");
    let tree: Tree = parser.parse(source_code, None).unwrap();

    let imports = collect_all_imports(&tree, source_code);
    println!("{:?}", imports);

    //let mut cursor = Tree::walk(&tree);

    //print_tree(&mut cursor, 0);

    PartialParsedModule {
        tree: tree,
        imports: imports,
        source_code: source_code.to_string(),
    }
}

#[derive(Debug, Eq, PartialEq)]
enum WhatToDisplay {
    Names(Vec<String>),
}

#[derive(Debug)]
struct LspEnv {}

macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( let _ = map.insert($key, $val); )*
         map
    }}
}

impl LspEnv {
    fn get_importable_module(self: Self, module_name: &str) -> Option<Module> {
        if (module_name == "gleam/io") {
            let values = hashmap!["println".to_string() => "1".to_string()];
            Some(Module { values: values })
        } else {
            None
        }
    }
}
//gleam_core::type_::

#[derive(Debug)]
struct Module {
    values: HashMap<String, String>,
}

fn dot_for_node(
    module: &PartialParsedModule,
    point: tree_sitter::Point,
    environment: LspEnv,
) -> Option<WhatToDisplay> {
    let prev_point = Point {
        column: point.column - 1,
        ..point
    };
    let node = module
        .tree
        .root_node()
        .named_descendant_for_point_range(prev_point, point);

    println!("node {node:?}");
    let res = node.and_then(|node| match node.kind() {
        "identifier" => {
            let text = node
                .utf8_text(module.source_code.as_bytes())
                .expect("should have text");
            let imported = module
                .imports
                .iter()
                .find(|x| x.module_import_name == *text);
            let module_name = match imported {
                Some(alias) => &alias.module,
                _ => text,
            };

            println!("identifier {text:?} {module_name:?}",);
            if let Some(module) = environment.get_importable_module(module_name) {
                Some(WhatToDisplay::Names(
                    module.values.keys().map(|x| x.to_string()).collect(),
                ))
            } else {
                None
            }
        }
        _ => None,
    });

    println!("{res:?}");
    res
}

fn print_tree<'a>(node: &tree_sitter::Node<'a>, level: usize) {
    node.print(0);
}

use crate::fs::ProjectIO;

pub fn main() -> Result<(), gleam_core::Error> {
    //    println!("hello world");
    //    let config = if paths::root_config().exists() {
    //        tracing::info!("gleam_project_detected");
    //        Some(crate::config::root_config()?)
    //    } else {
    //        tracing::info!("gleam_project_not_found");
    //        None
    //    };
    //
    //    let config = config.expect("no config");
    //
    //    let mut compiler = crate::lsp::LspProjectCompiler::new(config, ProjectIO::new())?;
    //    let _ = compiler.compile();
    //    let info = &compiler.project_compiler.get_importable_modules()["gleam/base"];
    //
    //    println!(
    //        "{:#?}",
    //        compiler.project_compiler.get_importable_modules()["gleam/io"]
    //    );
    //
    //    println!("{:#?}", info.values.keys());

    Ok(())
}

#[derive(Debug)]
pub struct PartiallyInferedModule {
    tree: Tree,
    source_code: String,
    //components
    statements: Vec<Option<gleam_core::ast::Statement<(), (), (), ()>>>,
    infer_state: Vec<usize>,
    ts_nodes: Vec<usize>,

    //indexes
    name_to_statement: HashMap<String, usize>,
}

impl PartiallyInferedModule {
    fn new(source_code: String) -> Self {
        let mut statements: Vec<Option<gleam_core::ast::Statement<(), (), (), ()>>> = Vec::new();
        let mut infer_state: Vec<usize> = Vec::new();
        let mut ts_nodes: Vec<usize> = Vec::new();

        let mut name_to_statement: HashMap<String, usize> = HashMap::new();

        let tree = new_tree(&source_code);

        let node = tree.tree.root_node();

        for i in 0..node.child_count() {
            let x = node.child(i).unwrap();

            ts_nodes.push(node.id());
            statements.push(None);
            infer_state.push(0);

            let name = get_statement_name(&source_code, x);
            if let Some(name) = name {
                let _ = name_to_statement.insert(name, i);
            }
        }

        tree.tree.root_node().print(0);

        PartiallyInferedModule {
            tree: tree.tree,
            source_code: source_code,
            statements: statements,
            infer_state: infer_state,
            ts_nodes: ts_nodes,

            name_to_statement: name_to_statement,
        }
    }
}

pub fn get_statement_name(source_code: &str, node: tree_sitter::Node) -> Option<String> {
    match node.kind() {
        "function" => get_fn_name(source_code, node),
        "external_function" => get_fn_name(source_code, node),
        "constant" => get_const_name(source_code, node),
        "type" => get_type_name(source_code, node),
        "type_alias" => get_type_name(source_code, node),
        "external_type" => get_type_name(source_code, node),
        "import" => get_import_name(source_code, node),
        "type_definition" => get_type_name(source_code, node),
        _ => None,
    }
}

fn get_fn_name(source_code: &str, node: tree_sitter::Node) -> Option<String> {
    node.first_child_of_kind("fn")
        .next_sibling()
        .as_kind("identifier")
        .map(|node| node.as_string(source_code))
}

fn get_const_name(source_code: &str, node: tree_sitter::Node) -> Option<String> {
    node.first_child_of_kind("const")
        .next_sibling()
        .as_kind("identifier")
        .map(|node| node.as_string(source_code))
}
fn get_type_name(source_code: &str, node: tree_sitter::Node) -> Option<String> {
    let mut cursor = node.walk();

    if cursor.goto_first_child()
        && cursor.seek_next(&"type".to_string())
        && cursor.goto_next_sibling()
    {
        let as_node = cursor.node();
        let name = as_node.as_string(source_code);
        if as_node.kind() == "type_name" {
            if let Some(n) = as_node.child(0) {
                return Some(n.as_string(source_code));
            }
        }
    }
    return None;
}

fn get_import_name<'a>(source_code: &str, node: tree_sitter::Node<'a>) -> Option<String> {
    let module_as = node
        .first_child_of_kind("as")
        .next_sibling()
        .as_kind("identifier")
        .map(|node| node.as_string(source_code));

    if module_as.is_some() {
        return module_as;
    }

    //default name
    node.first_child_of_kind("import")
        .next_sibling()
        .as_kind("module")
        .map(|node| {
            node.as_string(source_code)
                .split("/")
                .last()
                .expect("should have at least one")
                .to_string()
        })
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
    let pi = PartiallyInferedModule::new(data.to_string());
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
