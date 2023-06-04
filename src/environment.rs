use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::built_in_functions::BUILT_IN_FUNCTIONS;
use crate::object::Object;

pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Rc<Object>>,
    pub outer: Option<Env>,
}

impl Environment {
    pub fn new() -> Env {
        let env = Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            outer: None,
        }));
        for (name, function) in BUILT_IN_FUNCTIONS {
            env.borrow_mut()
                .store
                .insert(name.to_string(), Rc::new(Object::BuiltIn(function)));
        }
        env
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
                    return outer.borrow_mut().get(key);
                } else {
                    return None;
                }
            }
        }
    }

    pub fn set(&mut self, key: String, value: Rc<Object>) {
        self.store.insert(key, value);
    }

    pub fn debug_print(&self) {
        for (key, value) in &self.store {
            println!("{}: {}", key, value.assess_scope());
        }
    }

    pub fn get_keys(&self, indent: usize) -> String {
        let local_keys = self
            .store
            .keys()
            .map(|key| key.to_string())
            .collect::<Vec<_>>();
        let outer_keys = match &self.outer {
            Some(outer) => (**outer).clone().borrow().get_keys(indent + 4), // Increase indent by 4 spaces
            None => "no outer scope".to_string(),
        };

        let indentation = " ".repeat(indent); // Generate spaces for indentation

        format!(
            "\n{}LOCAL SCOPE: {:?}\n{}OUTER SCOPE: {}",
            indentation, local_keys, indentation, outer_keys
        )
    }
}

impl Object {
    pub fn assess_scope(&self) -> String {
        match self {
            Object::Function(function) => {
                format!("{}", (*function.env).clone().borrow().get_keys(0))
            }
            other => format!("{}", other.to_type()),
        }
    }
}
