use super::*;
use crate::{fs::test::InMemoryFile, typ};
use std::{io::BufReader, iter::FromIterator};

fn roundtrip(input: &Module) -> Module {
    let mut buffer = InMemoryFile::new();
    ModuleEncoder::new(input).write(buffer.clone()).unwrap();
    let buffer = buffer.into_contents().unwrap();
    ModuleDecoder::new()
        .read(BufReader::new(buffer.as_slice()))
        .unwrap()
}

#[test]
fn empty_module() {
    let module = Module {
        name: vec!["one".to_string(), "two".to_string()],
        types: HashMap::new(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}

#[test]
fn module_with_app_type() {
    let module = Module {
        name: vec!["a".to_string(), "b".to_string()],
        types: [(
            "t".to_string(),
            TypeConstructor {
                typ: typ::int(),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![typ::int(), typ::list(typ::nil())],
            },
        )]
        .iter()
        .cloned()
        .collect(),
        values: HashMap::new(),
        accessors: HashMap::new(),
    };
    assert_eq!(roundtrip(&module), module);
}
