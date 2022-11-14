mod source_links;

use std::{path::PathBuf, time::SystemTime};

use crate::{
    ast::{Statement, TypedStatement},
    build::Module,
    config::{DocsPage, PackageConfig},
    docs::source_links::SourceLinker,
    format,
    io::OutputFile,
    pretty,
};
use askama::Template;
use itertools::Itertools;
use serde::Serialize;
use serde_json::to_string as serde_to_string;

const MAX_COLUMNS: isize = 65;
const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn generate_html(
    config: &PackageConfig,
    analysed: &[Module],
    docs_pages: &[DocsPage],
) -> Vec<OutputFile> {
    let modules = analysed.iter().filter(|module| !module.is_test());
    let rendering_timestamp = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("get current timestamp")
        .as_secs()
        .to_string();

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

    let mut search_indexes = vec![];

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
        let unnest = page_unnest(&page.path);

        let temp = PageTemplate {
            gleam_version: VERSION,
            links: &links,
            pages: &pages,
            modules: &modules_links,
            project_name: &config.name,
            page_title: &config.name,
            project_version: &config.version.to_string(),
            content: render_markdown(&content),
            rendering_timestamp: &rendering_timestamp,
            unnest: &unnest,
        };

        files.push(OutputFile {
            path: PathBuf::from(&page.path),
            text: temp.render().expect("Page template rendering"),
        });

        search_indexes.push(SearchIndex {
            doc: config.name.to_string(),
            title: config.name.to_string(),
            content,
            url: page.path.to_string(),
        })
    }

    // Generate module documentation pages
    for module in modules {
        let name = module.name.clone();
        let unnest = page_unnest(&module.name);

        // Read module src & create line number lookup structure
        let source_links = SourceLinker::new(config, module);

        let page_title = format!("{} - {}", name, config.name);

        let functions: Vec<Function<'_>> = module
            .ast
            .statements
            .iter()
            .flat_map(|statement| function(&source_links, statement))
            .sorted()
            .collect();

        let types: Vec<Type<'_>> = module
            .ast
            .statements
            .iter()
            .flat_map(|statement| type_(&source_links, statement))
            .sorted()
            .collect();

        let constants: Vec<Constant<'_>> = module
            .ast
            .statements
            .iter()
            .flat_map(|statement| constant(&source_links, statement))
            .sorted()
            .collect();

        types.iter().for_each(|typ| {
            let constructors = typ
                .constructors
                .iter()
                .map(|constructor| {
                    let arguments = constructor
                        .arguments
                        .iter()
                        .map(|argument| format!("{}\n{}", argument.name, argument.doc))
                        .join("\n");

                    format!(
                        "{}\n{}\n{}",
                        constructor.definition, constructor.text_documentation, arguments
                    )
                })
                .join("\n");

            search_indexes.push(SearchIndex {
                doc: module.name.to_string(),
                title: typ.name.to_string(),
                content: format!(
                    "{}\n{}\n{}\n{}",
                    typ.definition,
                    typ.text_documentation,
                    constructors,
                    import_synonyms(&module.name, typ.name)
                ),
                url: format!("{}.html#{}", module.name, typ.name),
            })
        });
        constants.iter().for_each(|constant| {
            search_indexes.push(SearchIndex {
                doc: module.name.to_string(),
                title: constant.name.to_string(),
                content: format!(
                    "{}\n{}\n{}",
                    constant.definition,
                    constant.text_documentation,
                    import_synonyms(&module.name, constant.name)
                ),
                url: format!("{}.html#{}", module.name, constant.name),
            })
        });
        functions.iter().for_each(|function| {
            search_indexes.push(SearchIndex {
                doc: module.name.to_string(),
                title: function.name.to_string(),
                content: format!(
                    "{}\n{}\n{}",
                    function.signature,
                    function.text_documentation,
                    import_synonyms(&module.name, function.name)
                ),
                url: format!("{}.html#{}", module.name, function.name),
            })
        });
        search_indexes.push(SearchIndex {
            doc: module.name.to_string(),
            title: module.name.to_string(),
            content: module.ast.documentation.iter().join("\n"),
            url: format!("{}.html", module.name),
        });

        let template = ModuleTemplate {
            gleam_version: VERSION,
            unnest,
            links: &links,
            pages: &pages,
            documentation: render_markdown(&module.ast.documentation.iter().join("\n")),
            modules: &modules_links,
            project_name: &config.name,
            page_title: &page_title,
            module_name: name,
            project_version: &config.version.to_string(),
            functions,
            types,
            constants,
            rendering_timestamp: &rendering_timestamp,
        };

        files.push(OutputFile {
            path: PathBuf::from(format!("{}.html", module.name)),
            text: template
                .render()
                .expect("Module documentation template rendering"),
        });
    }

    // Render static assets

    files.push(OutputFile {
        path: PathBuf::from("css/atom-one-light.min.css"),
        text: std::include_str!("../templates/docs-css/atom-one-light.min.css").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("css/atom-one-dark.min.css"),
        text: std::include_str!("../templates/docs-css/atom-one-dark.min.css").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("css/index.css"),
        text: std::include_str!("../templates/docs-css/index.css").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("js/highlight.min.js"),
        text: std::include_str!("../templates/docs-js/highlight.min.js").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("js/highlightjs-gleam.js"),
        text: std::include_str!("../templates/docs-js/highlightjs-gleam.js").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("js/highlightjs-erlang.min.js"),
        text: std::include_str!("../templates/docs-js/highlightjs-erlang.min.js").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("js/highlightjs-elixir.min.js"),
        text: std::include_str!("../templates/docs-js/highlightjs-elixir.min.js").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("js/highlightjs-javascript.min.js"),
        text: std::include_str!("../templates/docs-js/highlightjs-javascript.min.js").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("js/lunr.min.js"),
        text: std::include_str!("../templates/docs-js/lunr.min.js").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("js/index.js"),
        text: std::include_str!("../templates/docs-js/index.js").to_string(),
    });

    files.push(OutputFile {
        path: PathBuf::from("search-data.js"),
        text: format!(
            "window.Gleam.initSearch({});",
            serde_to_string(&escape_html_contents(search_indexes))
                .expect("search index serialization")
        ),
    });

    files
}

fn page_unnest(path: &str) -> String {
    let unnest = path
        .strip_prefix('/')
        .unwrap_or(path)
        .split('/')
        .skip(1)
        .map(|_| "..")
        .join("/");
    if unnest.is_empty() {
        ".".to_string()
    } else {
        unnest
    }
}

#[test]
fn page_unnest_test() {
    // Pages
    assert_eq!(page_unnest("wibble.html"), ".");
    assert_eq!(page_unnest("/wibble.html"), ".");
    assert_eq!(page_unnest("/wibble/woo.html"), "..");
    assert_eq!(page_unnest("/wibble/wobble/woo.html"), "../..");

    // Modules
    assert_eq!(page_unnest("string"), ".");
    assert_eq!(page_unnest("gleam/string"), "..");
    assert_eq!(page_unnest("gleam/string/inspect"), "../..");
}

fn escape_html_content(it: String) -> String {
    it.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('\"', "&quot;")
        .replace('\'', "&#39;")
}

fn escape_html_contents(indexes: Vec<SearchIndex>) -> Vec<SearchIndex> {
    indexes
        .into_iter()
        .map(|idx| SearchIndex {
            doc: idx.doc,
            title: idx.title,
            content: escape_html_content(idx.content),
            url: idx.url,
        })
        .collect::<Vec<SearchIndex>>()
}

fn import_synonyms(parent: &str, child: &str) -> String {
    format!("Synonyms:\n{}.{}\n{} {}", parent, child, parent, child)
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
            text_documentation: text_documentation(doc),
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
            text_documentation: text_documentation(doc),
            signature: print(formatter.docs_fn_signature(true, name, args, ret.clone())),
            source_url: source_links.url(location),
        }),

        _ => None,
    }
}

fn text_documentation(doc: &Option<String>) -> String {
    let raw_text = doc
        .as_ref()
        .map(|it| it.to_string())
        .unwrap_or_else(|| "".to_string());

    // TODO: parse markdown properly and extract the text nodes
    raw_text.replace("```gleam", "").replace("```", "")
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
            text_documentation: text_documentation(doc),
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
            // TODO: Don't use the same printer for docs as for the formatter.
            // We are not interested in showing the exact implementation in the
            // documentation and we could add things like colours, etc.
            definition: print(formatter.custom_type(true, false, name, parameters, cs, location)),
            documentation: markdown_documentation(doc),
            text_documentation: text_documentation(doc),
            constructors: cs
                .iter()
                .map(|constructor| TypeConstructor {
                    definition: print(formatter.record_constructor(constructor)),
                    documentation: markdown_documentation(&constructor.documentation),
                    text_documentation: text_documentation(&constructor.documentation),
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
            text_documentation: text_documentation(doc),
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
            text_documentation: text_documentation(doc),
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
            text_documentation: text_documentation(doc),
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
    text_documentation: String,
    source_url: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TypeConstructor {
    definition: String,
    documentation: String,
    text_documentation: String,
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
    text_documentation: String,
    source_url: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Constant<'a> {
    name: &'a str,
    definition: String,
    documentation: String,
    text_documentation: String,
    source_url: String,
}

#[derive(Template)]
#[template(path = "documentation_page.html")]
struct PageTemplate<'a> {
    gleam_version: &'a str,
    unnest: &'a str,
    page_title: &'a str,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link],
    links: &'a [Link],
    modules: &'a [Link],
    content: String,
    rendering_timestamp: &'a str,
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
    rendering_timestamp: &'a str,
}

#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct SearchIndex {
    doc: String,
    title: String,
    content: String,
    url: String,
}
