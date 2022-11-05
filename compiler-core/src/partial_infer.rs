use crate::ast::UntypedExpr;
use crate::parse::error::ParseError;
use std::collections::HashMap;
use tree_sitter::Tree;
//use crate::uid::UniqueIdGenerator;
use crate::ast::Statement;
use crate::build::Origin;
use crate::type_::environment::Environment;
use tree_sitter::TreeCursor;

type UntypedStatements = Vec<Result<Option<Statement<(), UntypedExpr, (), ()>>, ParseError>>;

#[derive(Debug)]
pub struct PartiallyParsedModule {
    pub tree: Tree,
    pub source_code: String,
    //components
    pub statements: UntypedStatements,
    pub infer_state: Vec<usize>,
    pub node_byte_range: Vec<std::ops::Range<usize>>,
    pub untyped: Vec<Result<Option<(UntypedExpr, u32)>, ParseError>>,

    //indexes
    pub name_to_statement: HashMap<String, usize>,
}

#[derive(Debug)]
pub struct PartiallyInferedModule {
    pub tree: Tree,
    pub source_code: String,
    //components
    pub statements: UntypedStatements,
    pub infer_state: Vec<usize>,
    pub node_byte_range: Vec<std::ops::Range<usize>>,
    pub untyped: Vec<Result<Option<(UntypedExpr, u32)>, ParseError>>,
    pub statements_typed: Vec<Result<crate::ast::TypedStatement, crate::type_::Error>>,
    pub partial_module: crate::ast::TypedModule,

    //indexes
    pub name_to_statement: HashMap<String, usize>,
}

impl PartiallyParsedModule {
    pub fn new<'a>(source_code: String, environment: &mut Environment<'a>) -> Self {
        let mut statements: UntypedStatements = Vec::new();
        let mut infer_state: Vec<usize> = Vec::new();
        let mut node_byte_range: Vec<std::ops::Range<usize>> = Vec::new();
        let untyped: Vec<Result<Option<(UntypedExpr, u32)>, ParseError>> = Vec::new();

        let mut name_to_statement: HashMap<String, usize> = HashMap::new();

        let tree = tree_from_source(&source_code);

        let root_node = tree.root_node();

        for i in 0..root_node.child_count() {
            let node = root_node.child(i).unwrap();

            node_byte_range.push(node.byte_range());

            let text = node.utf8_text(&source_code.as_bytes()).unwrap();
            //println!("{:#?}", text);
            let lex = crate::parse::lexer::make_tokenizer(text);
            let mut parser = crate::parse::Parser::new(lex);

            let expr = parser.parse_statement();
            //let expr = parser.ensure_no_errors_or_remaining_input(expr);

            statements.push(expr);
            infer_state.push(0);
            //untyped.push();

            let name = get_statement_name(&source_code, node);
            if let Some(name) = name {
                let _ = name_to_statement.insert(name, i);
            }
        }

        PartiallyParsedModule {
            tree,
            source_code,
            statements,
            infer_state,
            node_byte_range,
            untyped,

            name_to_statement,
        }
    }

    pub fn dependencies(&self, target: crate::build::Target) -> Vec<(String, crate::ast::SrcSpan)> {
        self.iter_statements(target)
            .flat_map(|s| match s {
                Statement::Import {
                    module, location, ..
                } => Some((module.join("/"), *location)),
                _ => None,
            })
            .collect()
    }

    pub fn iter_statements(&self, target: crate::build::Target) -> impl Iterator<Item = &crate::ast::UntypedStatement> {
        self.statements
            .iter()
            .filter(move |st| match st {
                Ok(Some(st)) =>
                  st.is_for(target) ,
               _ =>
                 false
            }
                    )
            .collect()
    }
}

impl PartiallyInferedModule {
    pub fn new<'a>(parsed: PartiallyParsedModule, environment: &mut Environment<'a>) -> Self {
        let PartiallyParsedModule {
            tree,
            source_code,
            statements,
            infer_state,
            node_byte_range,
            untyped,

            name_to_statement,
        } = parsed;

        let name = ["random_name".to_string()].to_vec();

        let mut type_names = HashMap::with_capacity(statements.len());
        let mut value_names = HashMap::with_capacity(statements.len());
        let mut hydrators = HashMap::with_capacity(statements.len());

        // Register any modules, types, and values being imported
        // We process imports first so that anything imported can be referenced
        // anywhere in the module.
        for s in statements.iter() {
            if let Ok(Some(s)) = s {
                let _ = crate::type_::register_import(s, environment);
            }
        }

        // Register types so they can be used in constructors and functions
        // earlier in the module.
        for s in statements.iter() {
            if let Ok(Some(s)) = s {
                let _ = crate::type_::register_types(
                    s,
                    &name,
                    &mut hydrators,
                    &mut type_names,
                    environment,
                );
            }
        }

        // Register values so they can be used in functions earlier in the module.
        for s in statements.iter() {
            if let Ok(Some(s)) = s {
                let _ = crate::type_::register_values(
                    s,
                    &name,
                    &mut hydrators,
                    &mut value_names,
                    environment,
                );
            }
        }

        // Infer the types of each statement in the module
        // We first infer all the constants so they can be used in functions defined
        // anywhere in the module.
        let mut new_statements = Vec::with_capacity(statements.len());
        let mut consts = vec![];
        let mut not_consts = vec![];
        for statement in statements.iter() {
            if let Ok(Some(statement)) = statement {
                match statement {
                    Statement::Fn { .. }
                    | Statement::TypeAlias { .. }
                    | Statement::CustomType { .. }
                    | Statement::ExternalFn { .. }
                    | Statement::ExternalType { .. }
                    | Statement::Import { .. } => not_consts.push(statement),

                    Statement::ModuleConstant { .. } => consts.push(statement),
                }
            }
        }

        for statement in consts.into_iter().chain(not_consts) {
            let statement = crate::type_::infer_statement(
                statement.clone(),
                &name,
                &mut hydrators,
                environment,
            );
            new_statements.push(statement);
        }

        // Generalise functions now that the entire module has been inferred
        //let statements = statements
        //    .into_iter()
        //    .map(|s| generalise_statement(s, &name, &mut environment))
        //    .collect();

        // Remove private and imported types and values to create the public interface
        environment
            .module_types
            .retain(|_, info| info.public && info.module == name);
        environment.module_values.retain(|_, info| info.public);
        environment
            .accessors
            .retain(|_, accessors| accessors.public);

        let types = environment.module_types.clone();
        let types_constructors = environment.module_types_constructors.clone();
        let values = environment.module_values.clone();
        let accessors = environment.accessors.clone();

        let fs: Vec<String> = Vec::new();
        let partial_module = crate::ast::TypedModule {
            documentation: Vec::new(),
            name: name.clone(),
            statements: Vec::new(),
            type_info: crate::type_::Module {
                name,
                types,
                types_constructors,
                values,
                accessors,
                origin: crate::build::Origin::Src,
                package: "root_project".to_string(),
            },
        };

        PartiallyInferedModule {
            tree,
            source_code,
            statements,
            infer_state,
            node_byte_range,
            untyped,
            statements_typed: new_statements,
            partial_module,

            name_to_statement,
        }
    }
}

pub fn tree_from_source(source_code: &str) -> Tree {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_gleam::language())
        .expect("Error loading gleam language");
    parser.parse(source_code, None).unwrap()
}

pub fn get_statement_name(source_code: &str, node: tree_sitter::Node<'_>) -> Option<String> {
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

fn get_fn_name(source_code: &str, node: tree_sitter::Node<'_>) -> Option<String> {
    node.first_child_of_kind("fn")
        .next_sibling()
        .as_kind("identifier")
        .map(|node| node.as_string(source_code))
}

fn get_const_name(source_code: &str, node: tree_sitter::Node<'_>) -> Option<String> {
    node.first_child_of_kind("const")
        .next_sibling()
        .as_kind("identifier")
        .map(|node| node.as_string(source_code))
}
fn get_type_name(source_code: &str, node: tree_sitter::Node<'_>) -> Option<String> {
    let mut cursor = node.walk();

    if cursor.goto_first_child()
        && cursor.seek_next(&"type".to_string())
        && cursor.goto_next_sibling()
    {
        let as_node = cursor.node();
        let _name = as_node.as_string(source_code);
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
    //    fn as_import(&self, source_code: &str) -> Option<Import>;
    fn all_childs_of(&self, kind: &str) -> Vec<tree_sitter::Node<'_>>;
    fn as_string(&self, source_code: &str) -> String;
}

trait Navigator {
    fn as_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>>;
    fn first_child_of_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>>;
    fn next_sibling(&self) -> Option<tree_sitter::Node<'_>>;
}

impl Navigator for tree_sitter::Node<'_> {
    fn next_sibling(&self) -> Option<tree_sitter::Node<'_>> {
        let mut cursor = self.walk();
        if cursor.goto_next_sibling() {
            return Some(cursor.node());
        }
        return None;
    }

    fn first_child_of_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>> {
        let mut cursor = self.walk();

        if cursor.goto_first_child() && cursor.seek_next(&kind.to_string()) {
            return Some(cursor.node());
        }
        return None;
    }

    fn as_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>> {
        if self.kind() == kind {
            Some(*self)
        } else {
            None
        }
    }
}

impl Navigator for Option<tree_sitter::Node<'_>> {
    fn next_sibling(&self) -> Option<tree_sitter::Node<'_>> {
        if let Some(node) = self {
            return node.next_sibling();
        }
        return None;
    }

    fn first_child_of_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>> {
        if let Some(node) = self {
            return node.first_child_of_kind(kind);
        }
        return None;
    }

    fn as_kind(&self, kind: &str) -> Option<tree_sitter::Node<'_>> {
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

    fn all_childs_of(&self, kind: &str) -> Vec<tree_sitter::Node<'_>> {
        let mut ret = Vec::new();

        for i in 0..self.child_count() {
            let x = self.child(i).expect("should exists");
            if x.kind() == kind {
                ret.push(x);
            }
        }

        ret
    }

    //    fn as_import(&self, source_code: &str) -> Option<Import> {
    //        match self.kind() {
    //            "import" => {
    //                let mut qa: Vec<String> = Vec::new();
    //
    //                let module = self.child_by_field_name("module").and_then(|x| {
    //                    Some(
    //                        x.utf8_text(source_code.as_bytes())
    //                            .expect("should work")
    //                            .to_string(),
    //                    )
    //                });
    //
    //                match self.all_childs_of("unqualified_imports").first() {
    //                    Some(nx) => {
    //                        qa = nx
    //                            .all_childs_of("unqualified_import")
    //                            .iter()
    //                            .map(|x| {
    //                                x.utf8_text(source_code.as_bytes())
    //                                    .expect("should work")
    //                                    .to_string()
    //                            })
    //                            .collect()
    //                    }
    //                    _ => (),
    //                }
    //
    //                let mut cursor = self.walk();
    //                let mut as_alias = None;
    //
    //                if cursor.goto_first_child()
    //                    && cursor.seek_next(&"as".to_string())
    //                    && cursor.goto_next_sibling()
    //                {
    //                    let as_node = cursor.node();
    //                    if as_node.kind() == "identifier" {
    //                        as_alias = Some(
    //                            as_node
    //                                .utf8_text(source_code.as_bytes())
    //                                .expect("should work")
    //                                .to_string(),
    //                        );
    //                    }
    //                }
    //
    //                module.and_then(|m| {
    //                    let import_name = m.split("/").last().expect("at least one").to_string();
    //                    let module_import_name = as_alias.as_ref().unwrap_or(&import_name).clone();
    //
    //                    Some(Import {
    //                        module: m.to_string(),
    //                        qualified_imports: qa,
    //                        module_import_name: module_import_name,
    //                        //as_alias: as_alias.clone()
    //                    })
    //                })
    //            }
    //
    //            _ => None,
    //        }
    //    }
}

//pub fn collect_all_imports(tree: &Tree, source_code: &str) -> Vec<Import> {
//    let node = tree.root_node();
//
//    node.all_childs_of("import")
//        .iter()
//        .map(|x| x.as_import(source_code))
//        .filter(|x| x.is_some())
//        .map(|x| x.expect("after filter"))
//        .collect()
//}
//
//#[derive(Clone, Debug)]
//pub struct Import {
//    module: String,
//    module_import_name: String,
//    qualified_imports: Vec<String>,
//}

pub struct Package {
    pub sources: Vec<crate::build::package_compiler::Source>,
    pub root: std::path::PathBuf,
}

pub fn module_deps_for_graph(
    target: crate::build::Target,
    module: &PartiallyParsedModule,
) -> (String, Vec<String>) {
    let name = module.name.clone();
    let deps: Vec<_> = module
        .ast
        .dependencies(target)
        .into_iter()
        .map(|(dep, _span)| dep)
        .collect();
    (name, deps)
}


impl Package {
    pub fn new() {}
    pub fn read_package<'a, IO>(&mut self, io: &mut IO)
    where
        IO: crate::io::FileSystemIO + Clone,
    {
        self.read_source_files(io);
        //        let parsed_modules = self.parse_sources(
        //            &self.config.name,
        //            std::mem::take(&mut self.sources)
        //        );
        //
        //        // Determine order in which modules are to be processed
        let parsed_modules: Vec<PartiallyParsedModule> = Vec::new();

        let dep_tree = parsed_modules
                .iter()
                .map(|m| {
            //        module_deps_for_graph(self.target.target(), m)
            ("".to_string(), Vec::new())
                })
                .collect();
 
        let sequence = toposort_deps(
dep_tree
       );
    }

    pub fn read_source_files<'a, IO>(&mut self, io: &mut IO) -> crate::error::Result<()>
    where
        IO: crate::io::FileSystemIO + Clone,
    {
        //let span = tracing::info_span!("load", package = %self.config.name.as_str());
        //let _enter = span.enter();
        tracing::info!("Reading source files");
        let src = self.root.join("src");
        let test = self.root.join("test");

        // Src
        for path in io.gleam_source_files(&src) {
            self.add_module(io, path, &src, Origin::Src)?;
        }

        Ok(())
    }

    fn add_module<'a, IO>(
        &mut self,
        io: &mut IO,
        path: std::path::PathBuf,
        dir: &std::path::Path,
        origin: Origin,
    ) -> crate::error::Result<()>
    where
        IO: crate::io::FileSystemIO + Clone,
    {
        let name = crate::build::package_compiler::module_name(&dir, &path);
        let code = io.read(&path)?;
        self.sources.push(crate::build::package_compiler::Source {
            name,
            path,
            code,
            origin,
        });
        Ok(())
    }
}

use petgraph::{algo::Cycle, graph::NodeIndex, Direction};
use std::collections::HashSet;
use std::hash::Hash;

#[cfg(test)]
use pretty_assertions::assert_eq;

#[derive(Debug, Default)]
pub struct DependencyTree<T> {
    graph: petgraph::Graph<T, ()>,
    indexes: HashMap<T, NodeIndex>,
    values: HashMap<NodeIndex, T>,
}

pub fn toposort_deps(inputs: Vec<(String, Vec<String>)>) -> Result<Vec<String>, Error> {
    let mut graph = petgraph::Graph::<(), ()>::with_capacity(inputs.len(), inputs.len() * 5);
    let mut values = HashMap::with_capacity(inputs.len());
    let mut indexes = HashMap::with_capacity(inputs.len());

    for (value, _deps) in &inputs {
        let index = graph.add_node(());
        let _ = indexes.insert(value.clone(), index);
        let _ = values.insert(index, value.clone());
    }

    for (value, deps) in inputs {
        let &from_index = indexes.get(&value).expect("Finding index for value");
        for &to_index in deps.into_iter().filter_map(|dep| indexes.get(&dep)) {
            let _ = graph.add_edge(from_index, to_index, ());
        }
    }

    match petgraph::algo::toposort(&graph, None) {
        Err(e) => Err(Error::Cycle(import_cycle(e, &graph, values))),

        Ok(seq) => Ok(seq
            .into_iter()
            .map(|i| values.remove(&i).expect("Finding value for index"))
            .rev()
            .collect()),
    }
}

// TODO: test
fn import_cycle(
    cycle: Cycle<NodeIndex>,
    graph: &petgraph::Graph<(), ()>,
    mut values: HashMap<NodeIndex, String>,
) -> Vec<String> {
    let origin = cycle.node_id();
    let mut path = vec![];
    let _ = find_cycle(origin, origin, &graph, &mut path, &mut HashSet::new());
    path.iter()
        .map(|index| {
            values
                .remove(index)
                .expect("dep_tree::import_cycle(): cannot find values for index")
        })
        .collect()
}

fn find_cycle(
    origin: NodeIndex,
    parent: NodeIndex,
    graph: &petgraph::Graph<(), ()>,
    path: &mut Vec<NodeIndex>,
    seen: &mut HashSet<NodeIndex>,
) -> bool {
    let _ = seen.insert(parent);
    for node in graph.neighbors_directed(parent, Direction::Outgoing) {
        if node == origin {
            path.push(node);
            return true;
        }
        if seen.contains(&node) {
            continue;
        }
        if find_cycle(origin, node, graph, path, seen) {
            path.push(node);
            return true;
        }
    }
    false
}
