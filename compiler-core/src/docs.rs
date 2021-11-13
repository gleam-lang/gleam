mod source_links;
#[cfg(test)]
mod tests;

use crate::{
    ast::{Statement, TypedStatement},
    build::Module,
    config::{DocsPage, PackageConfig},
    docs::source_links::SourceLinker,
    format,
    io::OutputFile,
    paths, pretty,
};
use askama::Template;
use itertools::Itertools;

const MAX_COLUMNS: isize = 65;
const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn generate_html(
    config: &PackageConfig,
    analysed: &[Module],
    docs_pages: &[DocsPage],
) -> Vec<OutputFile> {
    let modules = analysed.iter();
    let out = paths::build_docs(&config.name);

    // Define user-supplied (or README) pages
    let pages: Vec<_> = docs_pages
        .iter()
        .map(|page| Link {
            name: page.title.to_string(),
            path: page.path.to_string(),
        })
        .collect();

    let doc_links = config.links.iter().map(|doc_link| Link {
        name: doc_link.title.to_string(),
        path: doc_link.href.to_string(),
    });

    let repo_link = config.repository.url().map(|path| Link {
        name: "Repository".to_string(),
        path,
    });

    let links: Vec<_> = doc_links.chain(repo_link).collect();

    let mut files = vec![];

    let modules_links: Vec<_> = modules
        .clone()
        .map(|m| {
            let path = [&m.name, ".html"].concat();
            Link {
                path,
                name: m.name.to_string(),
            }
        })
        .sorted()
        .collect();

    // Generate user-supplied (or README) pages
    for page in docs_pages {
        let content = std::fs::read_to_string(&page.source).unwrap_or_default();

        let temp = PageTemplate {
            gleam_version: VERSION,
            unnest: ".".to_string(),
            links: &links,
            pages: &pages,
            modules: &modules_links,
            project_name: &config.name,
            page_title: &config.name,
            project_version: &config.version.to_string(),
            content: render_markdown(&content),
        };

        files.push(OutputFile {
            path: out.join(&page.path),
            text: temp.render().expect("Page template rendering"),
        });
    }

    // Generate module documentation pages
    for module in modules {
        let name = module.name.clone();

        // Read module src & create line number lookup structure
        let source_links = SourceLinker::new(config, module);

        let mut unnest: String =
            Itertools::intersperse(module.name.split('/').skip(1).map(|_| ".."), "/").collect();
        if unnest.is_empty() {
            unnest = ".".to_string();
        }

        let template = ModuleTemplate {
            gleam_version: VERSION,
            unnest,
            links: &links,
            pages: &pages,
            documentation: render_markdown(&module.ast.documentation.iter().join("\n")),
            modules: &modules_links,
            project_name: &config.name,
            page_title: &format!("{} - {}", name, config.name),
            module_name: name,
            project_version: &config.version.to_string(),
            functions: module
                .ast
                .statements
                .iter()
                .flat_map(|statement| function(&source_links, statement))
                .sorted()
                .collect(),
            types: module
                .ast
                .statements
                .iter()
                .flat_map(|statement| type_(&source_links, statement))
                .sorted()
                .collect(),
            constants: module
                .ast
                .statements
                .iter()
                .flat_map(|statement| constant(&source_links, statement))
                .sorted()
                .collect(),
        };

        files.push(OutputFile {
            path: out.join(&format!("{}.html", module.name)),
            text: template
                .render()
                .expect("Module documentation template rendering"),
        });
    }

    // Render static assets
    files.push(OutputFile {
        path: out.join("index.css"),
        text: std::include_str!("../templates/index.css").to_string(),
    });

    files.push(OutputFile {
        path: out.join("gleam.js"),
        text: std::include_str!("../templates/gleam.js").to_string(),
    });

    files.push(OutputFile {
        path: out.join("highlightjs-gleam.js"),
        text: std::include_str!("../templates/highlightjs-gleam.js").to_string(),
    });

    files
}

fn function<'a>(
    source_links: &SourceLinker,
    statement: &'a TypedStatement,
) -> Option<Function<'a>> {
    let mut formatter = format::Formatter::new();

    match statement {
        Statement::ExternalFn {
            public: true,
            name,
            doc,
            return_: retrn,
            arguments: args,
            location,
            ..
        } => Some(Function {
            name,
            documentation: markdown_documentation(doc),
            signature: print(formatter.external_fn_signature(true, name, args, retrn)),
            source_url: source_links.url(location),
        }),

        Statement::Fn {
            public: true,
            name,
            doc,
            arguments: args,
            return_type: ret,
            location,
            ..
        } => Some(Function {
            name,
            documentation: markdown_documentation(doc),
            signature: print(formatter.docs_fn_signature(true, name, args, ret.clone())),
            source_url: source_links.url(location),
        }),

        _ => None,
    }
}

fn markdown_documentation(doc: &Option<String>) -> String {
    doc.as_deref().map(render_markdown).unwrap_or_default()
}

fn render_markdown(text: &str) -> String {
    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = pulldown_cmark::Parser::new_ext(text, pulldown_cmark::Options::all());
    pulldown_cmark::html::push_html(&mut s, p);
    s
}

fn type_<'a>(source_links: &SourceLinker, statement: &'a TypedStatement) -> Option<Type<'a>> {
    let mut formatter = format::Formatter::new();

    match statement {
        Statement::ExternalType {
            public: true,
            name,
            doc,
            arguments: args,
            location,
            ..
        } => Some(Type {
            name,
            definition: print(formatter.external_type(true, name, args)),
            documentation: markdown_documentation(doc),
            constructors: vec![],
            source_url: source_links.url(location),
        }),

        Statement::CustomType {
            public: true,
            opaque: false,
            name,
            parameters,
            doc,
            constructors: cs,
            location,
            ..
        } => Some(Type {
            name,
            // TODO: Don't use the same printer for docs as for the formatter
            definition: print(formatter.custom_type(true, false, name, parameters, cs, location)),
            documentation: markdown_documentation(doc),
            constructors: cs
                .iter()
                .map(|constructor| TypeConstructor {
                    definition: print(formatter.record_constructor(constructor)),
                    documentation: markdown_documentation(&constructor.documentation),
                    arguments: constructor
                        .arguments
                        .iter()
                        .filter_map(|arg| arg.label.as_ref().map(|label| (arg, label)))
                        .map(|(argument, label)| TypeConstructorArg {
                            name: label.trim_end().to_string(),
                            doc: markdown_documentation(&argument.doc),
                        })
                        .filter(|arg| !arg.doc.is_empty())
                        .collect(),
                })
                .collect(),
            source_url: source_links.url(location),
        }),

        Statement::CustomType {
            public: true,
            opaque: true,
            name,
            parameters,
            doc,
            location,
            ..
        } => Some(Type {
            name,
            definition: print(formatter.docs_opaque_custom_type(true, name, parameters, location)),
            documentation: markdown_documentation(doc),
            constructors: vec![],
            source_url: source_links.url(location),
        }),

        Statement::TypeAlias {
            public: true,
            alias: name,
            type_ast: typ,
            doc,
            parameters: args,
            location,
            ..
        } => Some(Type {
            name,
            definition: print(formatter.type_alias(true, name, args, typ)),
            documentation: markdown_documentation(doc),
            constructors: vec![],
            source_url: source_links.url(location),
        }),

        _ => None,
    }
}

fn constant<'a>(
    source_links: &SourceLinker,
    statement: &'a TypedStatement,
) -> Option<Constant<'a>> {
    let mut formatter = format::Formatter::new();
    match statement {
        Statement::ModuleConstant {
            public: true,
            doc,
            name,
            value,
            location,
            ..
        } => Some(Constant {
            name,
            definition: print(formatter.docs_const_expr(true, name, value)),
            documentation: markdown_documentation(doc),
            source_url: source_links.url(location),
        }),

        _ => None,
    }
}

fn print(doc: pretty::Document<'_>) -> String {
    doc.to_pretty_string(MAX_COLUMNS)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Link {
    name: String,
    path: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Function<'a> {
    name: &'a str,
    signature: String,
    documentation: String,
    source_url: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TypeConstructor {
    definition: String,
    documentation: String,
    arguments: Vec<TypeConstructorArg>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TypeConstructorArg {
    name: String,
    doc: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Type<'a> {
    name: &'a str,
    definition: String,
    documentation: String,
    constructors: Vec<TypeConstructor>,
    source_url: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Constant<'a> {
    name: &'a str,
    definition: String,
    documentation: String,
    source_url: String,
}

#[derive(Template)]
#[template(path = "documentation_page.html")]
struct PageTemplate<'a> {
    gleam_version: &'a str,
    unnest: String,
    page_title: &'a str,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link],
    links: &'a [Link],
    modules: &'a [Link],
    content: String,
}

#[derive(Template)]
#[template(path = "documentation_module.html")]
struct ModuleTemplate<'a> {
    gleam_version: &'a str,
    unnest: String,
    page_title: &'a str,
    module_name: String,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link],
    links: &'a [Link],
    modules: &'a [Link],
    functions: Vec<Function<'a>>,
    types: Vec<Type<'a>>,
    constants: Vec<Constant<'a>>,
    documentation: String,
}
