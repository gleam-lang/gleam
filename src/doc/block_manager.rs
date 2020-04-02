use crate::ast::{Statement, TypedModule, UntypedStatement};
use crate::doc::doc::*;
use crate::pretty::Documentable;
use itertools::Itertools;
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

impl DocBlockManager {
    pub fn new() -> Self {
        DocBlockManager {
            current_block: vec![],
            blocks: vec![],
        }
    }

    pub(self) fn find_block_for_line(self: &Self, line_no: usize) -> Option<String> {
        for block in &self.blocks {
            if block
                .last()
                .and_then(|b: &RawComment| Some(b.end + 1 == line_no))
                .unwrap_or(false)
            {
                let doc_text = block.iter().map(|b| b.content.to_string()).join("\n");
                return Some(doc_text);
            }
        }

        if self
            .current_block
            .last()
            .and_then(|b: &RawComment| Some(b.end + 1 == line_no))
            .unwrap_or(false)
        {
            let doc_text = self
                .current_block
                .iter()
                .map(|b| b.content.to_string())
                .join("\n");
            return Some(doc_text);
        }

        None
    }

    pub fn add_line(self: &mut Self, end: usize, content: String) {
        let length = content.len();
        let start = end - length - 3;
        let new_line = RawComment {
            start,
            end,
            content,
        };
        match self.current_block.last() {
            Some(RawComment { end: e, .. }) => {
                if (new_line.start) == *e + 1 {
                    self.current_block.push(new_line);
                } else {
                    let copy = std::mem::replace(&mut self.current_block, vec![]);
                    self.blocks.push(copy);
                    self.current_block.push(new_line);
                }
            }
            None => {
                self.current_block.push(new_line);
            }
        }
    }

    pub fn gen_doc_chunk(self: &Self, module: &TypedModule) -> EEP48DocChunk {
        let mut module_doc = HashMap::new();
        let mut first_statement_line_no = None;

        let mut docs = Vec::<EEP48Doc>::new();
        for statement in module.statements.iter() {
            match statement {
                Statement::Fn {
                    location,
                    name,
                    args,
                    public: true,
                    return_annotation,
                    ..
                } => {
                    if first_statement_line_no.is_none() {
                        first_statement_line_no = Some(location.start);
                    }
                    let doc = crate::format::fn_signature(&true, name, args, return_annotation);
                    let fn_docs: Option<HashMap<String, String>> =
                        self.find_block_for_line(location.start).map(|d| {
                            vec![("en-US".to_string(), d.trim().to_string())]
                                .into_iter()
                                .collect()
                        });

                    docs.push(EEP48Doc {
                        name: name.to_string(),
                        arity: args.len(),
                        signature: vec![crate::pretty::format(70, doc)],
                        doc: fn_docs,
                        typ: DocType::Fn,
                    });
                }

                Statement::TypeAlias {
                    doc: _,
                    location,
                    args,
                    alias,
                    resolved_type,
                    public: true,
                } => {
                    if first_statement_line_no.is_none() {
                        first_statement_line_no = Some(location.start);
                    }
                    let doc = (&UntypedStatement::TypeAlias {
                        doc: None,
                        location: location.clone(),
                        args: args.clone(),
                        alias: alias.clone(),
                        resolved_type: resolved_type.clone(),
                        public: true,
                    })
                        .to_doc();
                    let fn_docs: Option<HashMap<String, String>> = self
                        .find_block_for_line(location.start)
                        .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
                    docs.push(EEP48Doc {
                        name: alias.to_string(),
                        arity: args.len(),
                        signature: vec![crate::pretty::format(80, doc)],
                        doc: fn_docs,
                        typ: DocType::TypeAlias,
                    });
                }

                Statement::CustomType {
                    location,
                    name,
                    args,
                    constructors,
                    ..
                } => {
                    if first_statement_line_no.is_none() {
                        first_statement_line_no = Some(location.start);
                    }
                    let statement = UntypedStatement::CustomType {
                        doc: None,
                        location: location.clone(),
                        args: args.clone(),
                        name: name.clone(),
                        constructors: constructors.clone(),
                        public: true,
                    };
                    let doc = (&statement).to_doc();
                    let fn_docs: Option<HashMap<String, String>> = self
                        .find_block_for_line(location.start)
                        .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
                    docs.push(EEP48Doc {
                        name: name.to_string(),
                        arity: args.len(),
                        signature: vec![crate::pretty::format(80, doc)],
                        doc: fn_docs,
                        typ: DocType::CustomType,
                    });
                }

                Statement::ExternalFn {
                    location,
                    name,
                    args,
                    retrn,
                    public: true,
                    ..
                } => {
                    if first_statement_line_no.is_none() {
                        first_statement_line_no = Some(location.start);
                    }
                    let doc = crate::format::external_fn_signature(&true, name, args, retrn);
                    let fn_docs: Option<HashMap<String, String>> = self
                        .find_block_for_line(location.start)
                        .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
                    docs.push(EEP48Doc {
                        name: name.to_string(),
                        arity: args.len(),
                        signature: vec![crate::pretty::format(80, doc.to_doc())],
                        doc: fn_docs,
                        typ: DocType::Fn,
                    });
                }

                Statement::ExternalType {
                    location,
                    name,
                    args,
                    ..
                } => {
                    if first_statement_line_no.is_none() {
                        first_statement_line_no = Some(location.start);
                    }
                    let doc = UntypedStatement::ExternalType {
                        doc: None,
                        location: location.clone(),
                        args: args.clone(),
                        name: name.clone(),
                        public: true,
                    }
                    .to_doc();
                    let fn_docs: Option<HashMap<String, String>> = self
                        .find_block_for_line(location.start)
                        .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
                    docs.push(EEP48Doc {
                        name: name.to_string(),
                        arity: args.len(),
                        signature: vec![crate::pretty::format(80, doc)],
                        doc: fn_docs,
                        typ: DocType::ExternalType,
                    });
                }
                _ => {}
            }
        }

        module_doc.insert(
            "en-US".to_string(),
            self.blocks
                .first()
                .or(Some(&self.current_block))
                .filter(|block| {
                    block
                        .first()
                        .map(|comment| {
                            comment.start == 0
                                && first_statement_line_no.map(|no| no > 0).unwrap_or(true)
                        })
                        .unwrap_or(false)
                })
                .unwrap_or(&vec![])
                .iter()
                .map(|rc| rc.content.to_string())
                .join("\n")
                .to_string(),
        );
        EEP48DocChunk {
            anno: ErlAnno {
                line: 0,
                column: 0,
                file: module.name_string(),
            },
            module_doc: module_doc,
            docs,
        }
    }
}
