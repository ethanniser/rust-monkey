use std::io::Write;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::built_in_functions::{instaniate_std_lib, BUILT_IN_FUNCTIONS};
use crate::object::Object;

pub type Env = Rc<RefCell<Environment>>;

pub struct Environment {
    pub store: HashMap<String, Rc<Object>>,
    pub outer: Option<Env>,
    pub output: Option<Rc<RefCell<dyn Write>>>,
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        Self {
            store: self.store.clone(),
            outer: self.outer.clone(),
            // use None or some default value for output because it cannot be cloned
            output: None,
        }
    }
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        self.store == other.store && self.outer == other.outer
    }
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
            output: None,
        }))
    }

    pub fn new_with_output<W: Write + 'static>(output: Rc<RefCell<W>>) -> Env {
        let env = Environment::blank();
        env.borrow_mut().output = Some(output);
        for (name, function) in BUILT_IN_FUNCTIONS {
            env.borrow_mut()
                .store
                .insert(name.to_string(), Rc::new(Object::BuiltIn(function)));
        }
        instaniate_std_lib(&env);
        env
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
            output: None,
        }))
    }

    pub fn get(&self, key: &str) -> Option<Rc<Object>> {
        match self.store.get(key) {
            Some(obj) => Some(Rc::clone(obj)),
            None => {
                if let Some(outer) = &self.outer {
                    return outer.borrow_mut().get(key);
                } else {
                    None
                }
            }
        }
    }

    pub fn set(&mut self, key: String, value: Rc<Object>) {
        self.store.insert(key, value);
    }
}

pub fn get_scope(env: &Env, indent: usize) -> String {
    let indentation = " ".repeat(indent);
    let environment = env.borrow();
    let mut output = String::new();
    for (name, object) in environment.store.iter() {
        match &**object {
            Object::Function(func) => {
                output.push_str(&format!(
                    "{}{} = {}\n",
                    indentation,
                    name,
                    Object::Function(func.clone())
                ));
                output.push_str(&get_scope(&func.env, indent + 4));
            }
            _ => {
                output.push_str(&format!("{indentation}{name} = {object}\n"));
            }
        }
    }
    output
}
