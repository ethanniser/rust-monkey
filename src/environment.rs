use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::built_in_functions::{instaniate_std_lib, BUILT_IN_FUNCTIONS};
use crate::object::Object;

pub type Env = Rc<RefCell<Environment>>;

#[derive(PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Rc<Object>>,
    pub outer: Option<Env>,
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(self.store.iter().map(|(k, v)| {
                (
                    k,
                    match &**v {
                        Object::Function(_) => "Function { .. }",
                        _ => std::any::type_name::<Object>(),
                    },
                )
            }))
            .finish()
    }
}

impl Environment {
    pub fn blank() -> Env {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new() -> Env {
        let env = Environment::blank();
        for (name, function) in BUILT_IN_FUNCTIONS {
            env.borrow_mut()
                .store
                .insert(name.to_string(), Rc::new(Object::BuiltIn(function)));
        }
        instaniate_std_lib(&env);
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
}

pub fn debug_scope(env: &Env, indent: usize) {
    let indentation = " ".repeat(indent);
    let environment = env.borrow();
    for (name, object) in environment.store.iter() {
        match &**object {
            Object::Function(func) => {
                println!("{}Function: {}", indentation, name);
                debug_scope(&func.env, indent + 4);
            }
            _ => println!("{}Variable: {} = {:?}", indentation, name, object),
        }
    }
}
