use crate::{
    eval::environment::Environment,
    parser::ast::{Block, Ident},
};
use nom::lib::std::{collections::HashMap, fmt};
use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    rc::Rc,
};

type Builtin = fn(Vec<Object>) -> Object;

#[derive(Clone, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Return(Box<Object>),
    Error(String),
    Function(Vec<Ident>, Block, Rc<RefCell<Environment>>),
    Builtin(String, Builtin),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(int) => write!(f, "{}", int),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "{}", s),
            Object::Return(ret) => write!(f, "{}", ret),
            Object::Error(e) => write!(f, "Error: {}", e),
            Object::Function(params, block, _) => {
                write!(f, "fn ({}) {{\n{:?}\n}}", params.join(", "), block)
            }
            Object::Builtin(name, _) => write!(f, "builtin function: {}", name),
            Object::Array(elements) => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(|object| format!("{}", object))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Hash(hash) => write!(
                f,
                "{{ {:?} }}",
                hash.iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Null => write!(f, "null"),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(int) => write!(f, "Object::Integer({})", int),
            Object::Boolean(b) => write!(f, "Object::Boolean({})", b),
            Object::String(s) => write!(f, r#"Object::String("{}")"#, s),
            Object::Return(ret) => write!(f, "Object::Return({:?})", ret),
            Object::Error(e) => write!(f, "Object::Error({:?})", e),
            Object::Function(params, block, _) => write!(
                f,
                "Object::Function(fn ({}) {{\n{:?}\n}})",
                params.join(", "),
                block
            ),
            Object::Builtin(name, _) => write!(f, "Object::Builtin({})", name),
            Object::Array(elements) => write!(f, "Object::Array({:?})", elements),
            Object::Hash(hash) => write!(f, "Object::Hash({:?})", hash),
            Object::Null => write!(f, "Object::Null"),
        }
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::String(s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Integer(a), Object::Integer(b)) => a == b,
            (Object::Boolean(a), Object::Boolean(b)) => a == b,
            (Object::String(a), Object::String(b)) => a == b,
            (Object::Return(a), Object::Return(b)) => a == b,
            (Object::Error(a), Object::Error(b)) => a == b,
            (
                Object::Function(args_a, block_a, env_a),
                Object::Function(args_b, block_b, env_b),
            ) => args_a == args_b && block_a == block_b && env_a == env_b,
            (Object::Builtin(name_a, func_a), Object::Builtin(name_b, func_b)) => {
                name_a == name_b && func_a == func_b
            }
            (Object::Array(array_a), Object::Array(array_b)) => array_a == array_b,
            (Object::Hash(hash_a), Object::Hash(hash_b)) => hash_a == hash_b,
            (Object::Null, Object::Null) => true,
            _ => false,
        }
    }
}
