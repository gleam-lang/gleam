use super::*;
use crate::{
    fs::test::InMemoryFile,
    typ::{self, TypeVar},
};
use std::{io::BufReader, iter::FromIterator};

// TODO: test type links

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
            "ListIntType".to_string(),
            TypeConstructor {
                typ: typ::list(typ::int()),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![],
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

#[test]
fn module_with_fn_type() {
    let module = Module {
        name: vec!["a".to_string(), "b".to_string()],
        types: [(
            "FnType".to_string(),
            TypeConstructor {
                typ: typ::fn_(vec![typ::nil(), typ::float()], typ::int()),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![],
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

#[test]
fn module_with_tuple_type() {
    let module = Module {
        name: vec!["a".to_string(), "b".to_string()],
        types: [(
            "TupleType".to_string(),
            TypeConstructor {
                typ: typ::tuple(vec![typ::nil(), typ::float(), typ::int()]),
                public: true,
                origin: Default::default(),
                module: vec!["the".to_string(), "module".to_string()],
                parameters: vec![],
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

#[test]
fn module_with_generic_type() {
    let t0 = typ::generic_var(0);
    let t1 = typ::generic_var(1);
    let t7 = typ::generic_var(7);
    let t8 = typ::generic_var(8);

    fn make(t1: Arc<Type>, t2: Arc<Type>) -> Module {
        Module {
            name: vec!["a".to_string(), "b".to_string()],
            types: [(
                "TupleType".to_string(),
                TypeConstructor {
                    typ: typ::tuple(vec![t1.clone(), t1.clone(), t2.clone()]),
                    public: true,
                    origin: Default::default(),
                    module: vec!["the".to_string(), "module".to_string()],
                    parameters: vec![t1, t2],
                },
            )]
            .iter()
            .cloned()
            .collect(),
            values: HashMap::new(),
            accessors: HashMap::new(),
        }
    }

    assert_eq!(roundtrip(&make(t7, t8)), make(t0, t1));
}
