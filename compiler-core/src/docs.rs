mod printer;
mod source_links;
#[cfg(test)]
mod tests;

use std::{collections::HashMap, time::SystemTime};

use camino::Utf8PathBuf;
use hexpm::version::Version;
use printer::Printer;

use crate::{
    build::{Module, Package},
    config::{DocsPage, PackageConfig},
    docs::source_links::SourceLinker,
    io::{Content, FileSystemReader, OutputFile},
    package_interface::PackageInterface,
    paths::ProjectPaths,
    type_::{self},
    version::COMPILER_VERSION,
};
use askama::Template;
use ecow::EcoString;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use serde_json::to_string as serde_to_string;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum DocContext {
    HexPublish,
    Build,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct PackageInformation {
    #[serde(rename = "gleam.toml")]
    package_config: PackageConfig,
}

/// Like `ManifestPackage`, but lighter and cheaper to clone as it is all that
/// we need for printing documentation.
#[derive(Debug, Clone)]
pub struct Dependency {
    pub version: Version,
    pub kind: DependencyKind,
}

#[derive(Debug, Clone, Copy)]
pub enum DependencyKind {
    Hex,
    Path,
    Git,
}

#[derive(Debug)]
pub struct DocumentationConfig<'a> {
    pub package_config: &'a PackageConfig,
    pub dependencies: HashMap<EcoString, Dependency>,
    pub analysed: &'a [Module],
    pub docs_pages: &'a [DocsPage],
    pub rendering_timestamp: SystemTime,
    pub context: DocContext,
}

pub fn generate_html<IO: FileSystemReader>(
    paths: &ProjectPaths,
    config: DocumentationConfig<'_>,
    fs: IO,
) -> Vec<OutputFile> {
    let DocumentationConfig {
        package_config: config,
        dependencies,
        analysed,
        docs_pages,
        rendering_timestamp,
        context: is_hex_publish,
    } = config;

    let modules = analysed
        .iter()
        .filter(|module| module.origin.is_src())
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

    let repo_link = config
        .repository
        .as_ref()
        .map(|r| r.url())
        .map(|path| Link {
            name: "Repository".into(),
            path,
        });

    let host = if is_hex_publish == DocContext::HexPublish {
        "https://hexdocs.pm"
    } else {
        ""
    };

    // https://github.com/gleam-lang/gleam/issues/3020
    let links: Vec<_> = match is_hex_publish {
        DocContext::HexPublish => doc_links
            .chain(repo_link)
            .chain([Link {
                name: "Hex".into(),
                path: format!("https://hex.pm/packages/{0}", config.name).to_string(),
            }])
            .collect(),
        DocContext::Build => doc_links.chain(repo_link).collect(),
    };

    let mut files = vec![];

    let mut search_items = vec![];

    let modules_links: Vec<_> = modules
        .clone()
        .map(|m| {
            let path = [&m.name, ".html"].concat();
            Link {
                path,
                name: m.name.split('/').join("<wbr />/"),
            }
        })
        .sorted()
        .collect();

    // Generate user-supplied (or README) pages
    for page in docs_pages {
        let content = fs.read(&page.source).unwrap_or_default();
        let rendered_content = render_markdown(&content, MarkdownSource::Standalone);
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
        let path = Utf8PathBuf::from(&page.path);

        let temp = PageTemplate {
            gleam_version: COMPILER_VERSION,
            links: &links,
            pages: &pages,
            modules: &modules_links,
            project_name: &config.name,
            page_title: &page_title,
            page_meta_description: &page_meta_description,
            file_path: &path.clone(),
            project_version: &config.version.to_string(),
            content: rendered_content,
            rendering_timestamp: &rendering_timestamp,
            host,
            unnest: &unnest,
        };

        files.push(OutputFile {
            path,
            content: Content::Text(temp.render().expect("Page template rendering")),
        });

        search_items.push(SearchItem {
            type_: SearchItemType::Page,
            parent_title: config.name.to_string(),
            title: config.name.to_string(),
            content: escape_html_content(content),
            reference: page.path.to_string(),
        })
    }

    // Generate module documentation pages
    for module in modules {
        let name = module.name.clone();
        let unnest = page_unnest(&module.name);

        // Read module src & create line number lookup structure
        let source_links = SourceLinker::new(paths, config, module);

        let documentation_content = module.ast.documentation.iter().join("\n");
        let rendered_documentation =
            render_markdown(&documentation_content.clone(), MarkdownSource::Comment);

        let mut printer = Printer::new(
            module.ast.type_info.package.clone(),
            module.name.clone(),
            &module.ast.names,
            &dependencies,
        );

        let types: Vec<TypeDefinition<'_>> = module
            .ast
            .definitions
            .iter()
            .filter_map(|definition| printer.type_definition(&source_links, definition))
            .sorted()
            .collect();

        let values: Vec<DocsValues<'_>> = module
            .ast
            .definitions
            .iter()
            .filter_map(|definition| printer.value(&source_links, definition))
            .sorted()
            .collect();

        types.iter().for_each(|type_| {
            let constructors = type_
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

            search_items.push(SearchItem {
                type_: SearchItemType::Type,
                parent_title: module.name.to_string(),
                title: type_.name.to_string(),
                content: format!(
                    "{}\n{}\n{}\n{}",
                    type_.definition,
                    type_.text_documentation,
                    constructors,
                    import_synonyms(&module.name, type_.name)
                ),
                reference: format!("{}.html#{}", module.name, type_.name),
            })
        });
        values.iter().for_each(|constant| {
            search_items.push(SearchItem {
                type_: SearchItemType::Value,
                parent_title: module.name.to_string(),
                title: constant.name.to_string(),
                content: format!(
                    "{}\n{}\n{}",
                    constant.definition,
                    constant.text_documentation,
                    import_synonyms(&module.name, constant.name)
                ),
                reference: format!("{}.html#{}", module.name, constant.name),
            })
        });

        search_items.push(SearchItem {
            type_: SearchItemType::Module,
            parent_title: module.name.to_string(),
            title: module.name.to_string(),
            content: documentation_content,
            reference: format!("{}.html", module.name),
        });

        let page_title = format!("{} · {} · v{}", name, config.name, config.version);
        let page_meta_description = "";
        let path = Utf8PathBuf::from(format!("{}.html", module.name));

        let template = ModuleTemplate {
            gleam_version: COMPILER_VERSION,
            host,
            unnest,
            links: &links,
            pages: &pages,
            documentation: rendered_documentation,
            modules: &modules_links,
            project_name: &config.name,
            page_title: &page_title,
            page_meta_description,
            module_name: EcoString::from(&name),
            file_path: &path.clone(),
            project_version: &config.version.to_string(),
            types,
            values,
            rendering_timestamp: &rendering_timestamp,
        };

        files.push(OutputFile {
            path,
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

    // lunr.min.js, search_data.json and index.js

    files.push(OutputFile {
        path: Utf8PathBuf::from("js/lunr.min.js"),
        content: Content::Text(std::include_str!("../templates/docs-js/lunr.min.js").to_string()),
    });

    let search_data_json = serde_to_string(&SearchData {
        items: search_items,
        programming_language: SearchProgrammingLanguage::Gleam,
    })
    .expect("search index serialization");

    files.push(OutputFile {
        path: Utf8PathBuf::from("search_data.json"),
        content: Content::Text(search_data_json.to_string()),
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

pub fn generate_json_package_interface(
    path: Utf8PathBuf,
    package: &Package,
    cached_modules: &im::HashMap<EcoString, type_::ModuleInterface>,
) -> OutputFile {
    OutputFile {
        path,
        content: Content::Text(
            serde_json::to_string(&PackageInterface::from_package(package, cached_modules))
                .expect("JSON module interface serialisation"),
        ),
    }
}

pub fn generate_json_package_information(path: Utf8PathBuf, config: PackageConfig) -> OutputFile {
    OutputFile {
        path,
        content: Content::Text(package_information_as_json(config)),
    }
}

fn package_information_as_json(config: PackageConfig) -> String {
    let info = PackageInformation {
        package_config: config,
    };
    serde_json::to_string_pretty(&info).expect("JSON module information serialisation")
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

#[test]
fn escape_html_content_test() {
    assert_eq!(
        escape_html_content("&<>\"'".to_string()),
        "&amp;&lt;&gt;&quot;&#39;"
    );
}

fn import_synonyms(parent: &str, child: &str) -> String {
    format!("Synonyms:\n{parent}.{child}\n{parent} {child}")
}

fn text_documentation(doc: &Option<(u32, EcoString)>) -> String {
    let raw_text = doc
        .as_ref()
        .map(|(_, it)| it.to_string())
        .unwrap_or_else(|| "".into());

    // TODO: parse markdown properly and extract the text nodes
    raw_text.replace("```gleam", "").replace("```", "")
}

fn markdown_documentation(doc: &Option<(u32, EcoString)>) -> String {
    doc.as_ref()
        .map(|(_, doc)| render_markdown(doc, MarkdownSource::Comment))
        .unwrap_or_default()
}

/// An enum to represent the source of a Markdown string to render.
enum MarkdownSource {
    /// A Markdown string that comes from the documentation of a
    /// definition/module. This means that each line is going to be preceded by
    /// a whitespace.
    Comment,
    /// A Markdown string coming from a standalone file like a README.md.
    Standalone,
}

fn render_markdown(text: &str, source: MarkdownSource) -> String {
    let text = match source {
        MarkdownSource::Standalone => text.into(),
        // Doc comments start with "///\s", which can confuse the markdown parser
        // and prevent tables from rendering correctly, so remove that first space.
        MarkdownSource::Comment => text
            .split('\n')
            .map(|s| s.strip_prefix(' ').unwrap_or(s))
            .join("\n"),
    };

    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = pulldown_cmark::Parser::new_ext(&text, pulldown_cmark::Options::all());
    pulldown_cmark::html::push_html(&mut s, p);
    s
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Link {
    name: String,
    path: String,
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
struct TypeDefinition<'a> {
    name: &'a str,
    definition: String,
    documentation: String,
    constructors: Vec<TypeConstructor>,
    text_documentation: String,
    source_url: String,
    deprecation_message: String,
    opaque: bool,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct DocsValues<'a> {
    name: &'a str,
    definition: String,
    documentation: String,
    text_documentation: String,
    source_url: String,
    deprecation_message: String,
}

#[derive(Template)]
#[template(path = "documentation_page.html")]
struct PageTemplate<'a> {
    gleam_version: &'a str,
    unnest: &'a str,
    host: &'a str,
    page_title: &'a str,
    page_meta_description: &'a str,
    file_path: &'a Utf8PathBuf,
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
    host: &'a str,
    page_title: &'a str,
    page_meta_description: &'a str,
    file_path: &'a Utf8PathBuf,
    module_name: EcoString,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link],
    links: &'a [Link],
    modules: &'a [Link],
    types: Vec<TypeDefinition<'a>>,
    values: Vec<DocsValues<'a>>,
    documentation: String,
    rendering_timestamp: &'a str,
}

#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct SearchData {
    items: Vec<SearchItem>,
    #[serde(rename = "proglang")]
    programming_language: SearchProgrammingLanguage,
}

#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct SearchItem {
    #[serde(rename = "type")]
    type_: SearchItemType,
    #[serde(rename = "parentTitle")]
    parent_title: String,
    title: String,
    #[serde(rename = "doc")]
    content: String,
    #[serde(rename = "ref")]
    reference: String,
}

#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[serde(rename_all = "lowercase")]
enum SearchItemType {
    Value,
    Module,
    Page,
    Type,
}

#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[serde(rename_all = "lowercase")]
enum SearchProgrammingLanguage {
    // Elixir,
    // Erlang,
    Gleam,
}

#[test]
fn package_config_to_json() {
    let input = r#"
name = "my_project"
version = "1.0.0"
licences = ["Apache-2.0", "MIT"]
description = "Pretty complex config"
target = "erlang"
repository = { type = "github", user = "example", repo = "my_dep" }
links = [{ title = "Home page", href = "https://example.com" }]
internal_modules = ["my_app/internal"]
gleam = ">= 0.30.0"

[dependencies]
gleam_stdlib = ">= 0.18.0 and < 2.0.0"
my_other_project = { path = "../my_other_project" }

[dev-dependencies]
gleeunit = ">= 1.0.0 and < 2.0.0"

[documentation]
pages = [{ title = "My Page", path = "my-page.html", source = "./path/to/my-page.md" }]

[erlang]
application_start_module = "my_app/application"
extra_applications = ["inets", "ssl"]

[javascript]
typescript_declarations = true
runtime = "node"

[javascript.deno]
allow_all = false
allow_ffi = true
allow_env = ["DATABASE_URL"]
allow_net = ["example.com:443"]
allow_read = ["./database.sqlite"]
"#;

    let config = toml::from_str::<PackageConfig>(&input).unwrap();
    let info = PackageInformation {
        package_config: config.clone(),
    };
    let json = package_information_as_json(config);
    let output = format!("--- GLEAM.TOML\n{input}\n\n--- EXPORTED JSON\n\n{json}");
    insta::assert_snapshot!(output);

    let roundtrip: PackageInformation = serde_json::from_str(&json).unwrap();
    assert_eq!(info, roundtrip);
}

#[test]
fn barebones_package_config_to_json() {
    let input = r#"
name = "my_project"
version = "1.0.0"
"#;

    let config = toml::from_str::<PackageConfig>(&input).unwrap();
    let json = package_information_as_json(config);
    let output = format!("--- GLEAM.TOML\n{input}\n\n--- EXPORTED JSON\n\n{json}");
    insta::assert_snapshot!(output);
}
