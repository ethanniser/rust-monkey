use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Object> {
        self.store.get(key)
    }

    pub fn set(&mut self, key: String, value: Object) -> Option<Object> {
        self.store.insert(key, value)
    }
}
