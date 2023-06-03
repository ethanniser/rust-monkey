use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Object> {
        self.store.get(key)
    }

    pub fn set(&mut self, key: String, value: Object) -> Option<Object> {
        self.store.insert(key, value)
    }
}
