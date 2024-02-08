mod source_links;
#[cfg(test)]
mod tests;

use std::time::SystemTime;

use camino::Utf8PathBuf;

use crate::{
    ast::{CustomType, Definition, Function, ModuleConstant, TypeAlias, TypedDefinition},
    build::{Module, Package},
    config::{DocsPage, PackageConfig},
    docs::source_links::SourceLinker,
    format,
    io::Content,
    io::OutputFile,
    package_interface::PackageInterface,
    paths::ProjectPaths,
    pretty,
    type_::Deprecation,
    version::COMPILER_VERSION,
};
use askama::Template;
use ecow::EcoString;
use itertools::Itertools;
use serde::Serialize;
use serde_json::to_string as serde_to_string;

const MAX_COLUMNS: isize = 65;

pub fn generate_html(
    paths: &ProjectPaths,
    config: &PackageConfig,
    analysed: &[Module],
    docs_pages: &[DocsPage],
    rendering_timestamp: SystemTime,
) -> Vec<OutputFile> {
    let modules = analysed
        .iter()
        .filter(|module| !module.is_test())
        .filter(|module| !config.is_internal_module(&module.name));

    let rendering_timestamp = rendering_timestamp
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
        name: "Repository".into(),
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
        let rendered_content = render_markdown(&content);
        let unnest = page_unnest(&page.path);

        let page_path_without_ext = page.path.split('.').next().unwrap_or("");
        let page_title = match page_path_without_ext {
            // The index page, such as README, should not push it's page title
            "index" => format!("{} · v{}", config.name, config.version),
            // Other page title's should say so
            _other => format!("{} · {} · v{}", page.title, config.name, config.version),
        };
        let page_meta_description = match page_path_without_ext {
            "index" => config.description.to_string().clone(),
            _other => "".to_owned(),
        };

        let temp = PageTemplate {
            gleam_version: COMPILER_VERSION,
            links: &links,
            pages: &pages,
            modules: &modules_links,
            project_name: &config.name,
            page_title: &page_title,
            page_meta_description: &page_meta_description,
            project_version: &config.version.to_string(),
            content: rendered_content,
            rendering_timestamp: &rendering_timestamp,
            unnest: &unnest,
        };

        files.push(OutputFile {
            path: Utf8PathBuf::from(&page.path),
            content: Content::Text(temp.render().expect("Page template rendering")),
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
        let source_links = SourceLinker::new(paths, config, module);

        let documentation_content = module.ast.documentation.iter().join("\n");
        let rendered_documentation = render_markdown(&documentation_content.clone());

        let functions: Vec<DocsFunction<'_>> = module
            .ast
            .definitions
            .iter()
            .flat_map(|statement| function(&source_links, statement))
            .sorted()
            .collect();

        let types: Vec<Type<'_>> = module
            .ast
            .definitions
            .iter()
            .flat_map(|statement| type_(&source_links, statement))
            .sorted()
            .collect();

        let constants: Vec<Constant<'_>> = module
            .ast
            .definitions
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
            content: documentation_content,
            url: format!("{}.html", module.name),
        });

        let page_title = format!("{} · {} · v{}", name, config.name, config.version);
        let page_meta_description = "";

        let template = ModuleTemplate {
            gleam_version: COMPILER_VERSION,
            unnest,
            links: &links,
            pages: &pages,
            documentation: rendered_documentation,
            modules: &modules_links,
            project_name: &config.name,
            page_title: &page_title,
            page_meta_description,
            module_name: name,
            project_version: &config.version.to_string(),
            functions,
            types,
            constants,
            rendering_timestamp: &rendering_timestamp,
        };

        files.push(OutputFile {
            path: Utf8PathBuf::from(format!("{}.html", module.name)),
            content: Content::Text(
                template
                    .render()
                    .expect("Module documentation template rendering"),
            ),
        });
    }

    // Render static assets

    files.push(OutputFile {
        path: Utf8PathBuf::from("css/atom-one-light.min.css"),
        content: Content::Text(
            std::include_str!("../templates/docs-css/atom-one-light.min.css").to_string(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("css/atom-one-dark.min.css"),
        content: Content::Text(
            std::include_str!("../templates/docs-css/atom-one-dark.min.css").to_string(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("css/index.css"),
        content: Content::Text(std::include_str!("../templates/docs-css/index.css").to_string()),
    });

    // highlightjs:

    files.push(OutputFile {
        path: Utf8PathBuf::from("js/highlight.min.js"),
        content: Content::Text(
            std::include_str!("../templates/docs-js/highlight.min.js").to_string(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("js/highlightjs-gleam.js"),
        content: Content::Text(
            std::include_str!("../templates/docs-js/highlightjs-gleam.js").to_string(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("js/highlightjs-erlang.min.js"),
        content: Content::Text(
            std::include_str!("../templates/docs-js/highlightjs-erlang.min.js").to_string(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("js/highlightjs-elixir.min.js"),
        content: Content::Text(
            std::include_str!("../templates/docs-js/highlightjs-elixir.min.js").to_string(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("js/highlightjs-javascript.min.js"),
        content: Content::Text(
            std::include_str!("../templates/docs-js/highlightjs-javascript.min.js").to_string(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("js/highlightjs-typescript.min.js"),
        content: Content::Text(
            std::include_str!("../templates/docs-js/highlightjs-typescript.min.js").to_string(),
        ),
    });

    // lunr.min.js, search-data.js and index.js:

    files.push(OutputFile {
        path: Utf8PathBuf::from("js/lunr.min.js"),
        content: Content::Text(std::include_str!("../templates/docs-js/lunr.min.js").to_string()),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("search-data.js"),
        content: Content::Text(format!(
            "window.Gleam.initSearch({});",
            serde_to_string(&escape_html_contents(search_indexes))
                .expect("search index serialization")
        )),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("js/index.js"),
        content: Content::Text(std::include_str!("../templates/docs-js/index.js").to_string()),
    });

    // web fonts:

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/karla-v23-regular-latin-ext.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/karla-v23-regular-latin-ext.woff2").to_vec(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/karla-v23-regular-latin.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/karla-v23-regular-latin.woff2").to_vec(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/karla-v23-bold-latin-ext.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/karla-v23-bold-latin-ext.woff2").to_vec(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/karla-v23-bold-latin.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/karla-v23-bold-latin.woff2").to_vec(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/ubuntu-mono-v15-regular-cyrillic-ext.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/ubuntu-mono-v15-regular-cyrillic-ext.woff2")
                .to_vec(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/ubuntu-mono-v15-regular-cyrillic.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/ubuntu-mono-v15-regular-cyrillic.woff2")
                .to_vec(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/ubuntu-mono-v15-regular-greek-ext.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/ubuntu-mono-v15-regular-greek-ext.woff2")
                .to_vec(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/ubuntu-mono-v15-regular-greek.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/ubuntu-mono-v15-regular-greek.woff2").to_vec(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/ubuntu-mono-v15-regular-latin-ext.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/ubuntu-mono-v15-regular-latin-ext.woff2")
                .to_vec(),
        ),
    });

    files.push(OutputFile {
        path: Utf8PathBuf::from("fonts/ubuntu-mono-v15-regular-latin.woff2"),
        content: Content::Binary(
            include_bytes!("../templates/docs-fonts/ubuntu-mono-v15-regular-latin.woff2").to_vec(),
        ),
    });

    files
}

pub fn generate_json_package_interface(path: Utf8PathBuf, package: &Package) -> OutputFile {
    OutputFile {
        path,
        content: Content::Text(
            serde_json::to_string(&PackageInterface::from_package(package))
                .expect("JSON module interface serialisation"),
        ),
    }
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
        ".".into()
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
    format!("Synonyms:\n{parent}.{child}\n{parent} {child}")
}

fn function<'a>(
    source_links: &SourceLinker,
    statement: &'a TypedDefinition,
) -> Option<DocsFunction<'a>> {
    let mut formatter = format::Formatter::new();

    match statement {
        Definition::Function(Function {
            public: true,
            name,
            documentation: doc,
            arguments: args,
            return_type: ret,
            location,
            deprecation,
            ..
        }) => Some(DocsFunction {
            name,
            documentation: markdown_documentation(doc),
            text_documentation: text_documentation(doc),
            signature: print(
                formatter
                    .docs_fn_signature(true, name, args, ret.clone())
                    .group(),
            ),
            source_url: source_links.url(*location),
            deprecation_message: match deprecation {
                Deprecation::NotDeprecated => "".to_string(),
                Deprecation::Deprecated { message } => message.to_string(),
            },
        }),

        _ => None,
    }
}

fn text_documentation(doc: &Option<EcoString>) -> String {
    let raw_text = doc
        .as_ref()
        .map(|it| it.to_string())
        .unwrap_or_else(|| "".into());

    // TODO: parse markdown properly and extract the text nodes
    raw_text.replace("```gleam", "").replace("```", "")
}

fn markdown_documentation(doc: &Option<EcoString>) -> String {
    doc.as_deref().map(render_markdown).unwrap_or_default()
}

fn render_markdown(text: &str) -> String {
    // Doc comments start with "///\s", which can confuse the markdown parser
    // and prevent tables from rendering correctly, so remove that first space.
    let text = text
        .split('\n')
        .map(|s| s.strip_prefix(' ').unwrap_or(s))
        .join("\n");

    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = pulldown_cmark::Parser::new_ext(&text, pulldown_cmark::Options::all());
    pulldown_cmark::html::push_html(&mut s, p);
    s
}

fn type_<'a>(source_links: &SourceLinker, statement: &'a TypedDefinition) -> Option<Type<'a>> {
    let mut formatter = format::Formatter::new();

    match statement {
        Definition::CustomType(ct) if ct.public && !ct.opaque => Some(Type {
            name: &ct.name,
            // TODO: Don't use the same printer for docs as for the formatter.
            // We are not interested in showing the exact implementation in the
            // documentation and we could add things like colours, etc.
            definition: print(formatter.custom_type(ct)),
            documentation: markdown_documentation(&ct.documentation),
            text_documentation: text_documentation(&ct.documentation),
            deprecation_message: match &ct.deprecation {
                Deprecation::NotDeprecated => "".to_string(),
                Deprecation::Deprecated { message } => message.to_string(),
            },
            constructors: ct
                .constructors
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
            source_url: source_links.url(ct.location),
        }),

        Definition::CustomType(CustomType {
            public: true,
            opaque: true,
            name,
            parameters,
            documentation: doc,
            location,
            deprecation,
            ..
        }) => Some(Type {
            name,
            definition: print(
                formatter
                    .docs_opaque_custom_type(true, name, parameters, location)
                    .group(),
            ),
            documentation: markdown_documentation(doc),
            text_documentation: text_documentation(doc),
            constructors: vec![],
            source_url: source_links.url(*location),
            deprecation_message: match deprecation {
                Deprecation::NotDeprecated => "".to_string(),
                Deprecation::Deprecated { message } => message.to_string(),
            },
        }),

        Definition::TypeAlias(TypeAlias {
            public: true,
            alias: name,
            type_ast: typ,
            documentation: doc,
            parameters: args,
            location,
            deprecation,
            ..
        }) => Some(Type {
            name,
            definition: print(
                formatter
                    .type_alias(true, name, args, typ, deprecation)
                    .group(),
            ),
            documentation: markdown_documentation(doc),
            text_documentation: text_documentation(doc),
            constructors: vec![],
            source_url: source_links.url(*location),
            deprecation_message: match deprecation {
                Deprecation::NotDeprecated => "".to_string(),
                Deprecation::Deprecated { message } => message.to_string(),
            },
        }),

        _ => None,
    }
}

fn constant<'a>(
    source_links: &SourceLinker,
    statement: &'a TypedDefinition,
) -> Option<Constant<'a>> {
    let mut formatter = format::Formatter::new();
    match statement {
        Definition::ModuleConstant(ModuleConstant {
            public: true,
            documentation: doc,
            name,
            value,
            location,
            ..
        }) => Some(Constant {
            name,
            definition: print(formatter.docs_const_expr(true, name, value)),
            documentation: markdown_documentation(doc),
            text_documentation: text_documentation(doc),
            source_url: source_links.url(*location),
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
struct DocsFunction<'a> {
    name: &'a str,
    signature: String,
    documentation: String,
    text_documentation: String,
    source_url: String,
    deprecation_message: String,
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
    deprecation_message: String,
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
    page_meta_description: &'a str,
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
    page_meta_description: &'a str,
    module_name: EcoString,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link],
    links: &'a [Link],
    modules: &'a [Link],
    functions: Vec<DocsFunction<'a>>,
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
