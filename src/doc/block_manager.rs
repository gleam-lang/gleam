use crate::ast::{Statement, TypedModule, UntypedStatement};
use crate::doc::doc::*;
use crate::pretty::Documentable;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
struct RawComment {
    pub start: usize,
    pub end: usize,
    pub content: String,
}

#[derive(Debug, PartialEq)]
pub struct DocBlockManager {
    pub(self) current_block: Vec<RawComment>,
    pub(self) blocks: Vec<Vec<RawComment>>,
}

pub fn gen_doc_chunk(module: &TypedModule) -> EEP48DocChunk {
    let mut docs = Vec::<EEP48Doc>::new();
    for statement in module.statements.iter() {
        match statement {
            Statement::Fn {
                name,
                args,
                public: true,
                return_annotation,
                doc,
                ..
            } => {
                let pretty = crate::format::fn_signature(&true, name, args, return_annotation);
                let doc = doc.as_ref().map(|d| {
                    vec![("en-US".to_string(), d.to_string())]
                        .into_iter()
                        .collect()
                });

                docs.push(EEP48Doc {
                    name: name.to_string(),
                    arity: args.len(),
                    signature: vec![crate::pretty::format(70, pretty)],
                    typ: DocType::Fn,
                    doc,
                });
            }

            Statement::TypeAlias {
                doc,
                location,
                args,
                alias,
                resolved_type,
                public: true,
            } => {
                let pretty = (&UntypedStatement::TypeAlias {
                    doc: None,
                    location: location.clone(),
                    args: args.clone(),
                    alias: alias.clone(),
                    resolved_type: resolved_type.clone(),
                    public: true,
                })
                    .to_doc();
                let fn_docs = doc.as_ref().map(|d| {
                    vec![("en-US".to_string(), d.to_string())]
                        .into_iter()
                        .collect()
                });
                docs.push(EEP48Doc {
                    name: alias.to_string(),
                    arity: args.len(),
                    signature: vec![crate::pretty::format(80, pretty)],
                    doc: fn_docs,
                    typ: DocType::TypeAlias,
                });
            }

            Statement::CustomType {
                location,
                name,
                args,
                constructors,
                doc,
                ..
            } => {
                let statement = UntypedStatement::CustomType {
                    doc: None,
                    location: location.clone(),
                    args: args.clone(),
                    name: name.clone(),
                    constructors: constructors.clone(),
                    public: true,
                };
                let pretty = (&statement).to_doc();
                let fn_docs = doc.as_ref().map(|d| {
                    vec![("en-US".to_string(), d.to_string())]
                        .into_iter()
                        .collect()
                });
                docs.push(EEP48Doc {
                    name: name.to_string(),
                    arity: args.len(),
                    signature: vec![crate::pretty::format(80, pretty)],
                    doc: fn_docs,
                    typ: DocType::CustomType,
                });
            }

            Statement::ExternalFn {
                name,
                args,
                retrn,
                public: true,
                doc,
                ..
            } => {
                let pretty = crate::format::external_fn_signature(&true, name, args, retrn);
                let fn_docs = doc.as_ref().map(|d| {
                    vec![("en-US".to_string(), d.to_string())]
                        .into_iter()
                        .collect()
                });
                docs.push(EEP48Doc {
                    name: name.to_string(),
                    arity: args.len(),
                    signature: vec![crate::pretty::format(80, pretty.to_doc())],
                    doc: fn_docs,
                    typ: DocType::Fn,
                });
            }

            Statement::ExternalType {
                location,
                name,
                args,
                doc,
                ..
            } => {
                let pretty = UntypedStatement::ExternalType {
                    doc: None,
                    location: location.clone(),
                    args: args.clone(),
                    name: name.clone(),
                    public: true,
                }
                .to_doc();
                let fn_docs = doc.as_ref().map(|d| {
                    vec![("en-US".to_string(), d.to_string())]
                        .into_iter()
                        .collect()
                });
                docs.push(EEP48Doc {
                    name: name.to_string(),
                    arity: args.len(),
                    signature: vec![crate::pretty::format(80, pretty)],
                    doc: fn_docs,
                    typ: DocType::ExternalType,
                });
            }
            _ => {}
        }
    }
    EEP48DocChunk {
        anno: ErlAnno {
            line: 0,
            column: 0,
            file: module.name_string(),
        },
        module_doc: HashMap::new(),
        docs,
    }
}
