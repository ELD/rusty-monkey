use crate::{
    eval::environment::Environment,
    parser::ast::{Block, Ident},
};
use nom::lib::std::fmt;
use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
};

#[derive(Eq, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Function(Vec<Ident>, Block, Rc<RefCell<Environment>>),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(int) => write!(f, "{}", int),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Return(ret) => write!(f, "{}", ret),
            Object::Error(e) => write!(f, "Error: {}", e),
            Object::Function(params, block, _) => {
                write!(f, "fn ({}) {{\n{:?}\n}}", params.join(", "), block)
            }
            Object::Null => write!(f, "null"),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(int) => write!(f, "Object::Integer({})", int),
            Object::Boolean(b) => write!(f, "Object::Boolean({})", b),
            Object::Return(ret) => write!(f, "Object::Return({:?})", ret),
            Object::Error(e) => write!(f, "Object::Error({:?})", e),
            Object::Function(params, block, _) => write!(
                f,
                "Object::Function(fn ({}) {{\n{:?}\n}})",
                params.join(", "),
                block
            ),
            Object::Null => write!(f, "Object::Null"),
        }
    }
}
