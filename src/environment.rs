use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{evaluator::built_in_functions::BuiltIns, object::Object};

pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Rc<Object>>,
    pub outer: Option<Env>,
}

impl Environment {
    pub fn new() -> Env {
        let mut store = HashMap::new();
        let BuiltIns { functions } = BuiltIns::new();

        for (name, function) in functions {
            store.insert(name, Rc::new(Object::BuiltIn(function)));
        }

        Rc::new(RefCell::new(Self { store, outer: None }))
    }

    pub fn new_enclosed(outer: &Env) -> Env {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            outer: Some(Rc::clone(outer)),
        }))
    }

    pub fn get(&self, key: &str) -> Option<Rc<Object>> {
        match self.store.get(key) {
            Some(obj) => Some(Rc::clone(obj)),
            None => {
                if let Some(outer) = &self.outer {
                    return outer.borrow().get(key);
                } else {
                    return None;
                }
            }
        }
    }

    pub fn set(&mut self, key: String, value: Rc<Object>) {
        self.store.insert(key, value);
    }
}
