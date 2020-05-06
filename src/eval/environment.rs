use crate::eval::object::Object;
use nom::lib::std::collections::HashMap;
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_with_outer(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, entry: &str) -> Option<Object> {
        match self.store.get(entry) {
            Some(val) => Some(val.clone()),
            None => match self.outer {
                Some(ref outer) => {
                    let env = outer.borrow();
                    env.get(entry)
                }
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: &str, val: Object) -> Object {
        self.store.insert(key.to_string(), val.clone());
        val
    }
}
