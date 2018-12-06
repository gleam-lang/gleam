#![allow(dead_code)]

pub fn int() -> Type {
    Type::Const {
        name: "Int".to_string(),
        module: "".to_string(),
    }
}

pub fn float() -> Type {
    Type::Const {
        name: "Float".to_string(),
        module: "".to_string(),
    }
}

pub fn atom() -> Type {
    Type::Const {
        name: "Atom".to_string(),
        module: "".to_string(),
    }
}

pub fn string() -> Type {
    Type::Const {
        name: "String".to_string(),
        module: "".to_string(),
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Const { name: String, module: String },
}
