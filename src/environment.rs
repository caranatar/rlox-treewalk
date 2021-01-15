use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use crate::value::Value;
use crate::token::Token;

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

pub type EnvPtr = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> Self {
        Environment { values: HashMap::new(), enclosing: None }
    }

    pub fn with_enclosing(mut self, enclosing: EnvPtr) -> Self {
        self.enclosing = Some(enclosing);
        self
    }

    pub fn push_new(enclosing: EnvPtr) -> EnvPtr {
        Rc::new(RefCell::new(Environment::new().with_enclosing(enclosing)))
    }

    pub fn pop(inner: EnvPtr) -> EnvPtr {
        inner.borrow().enclosing.clone().unwrap()
    }

    pub fn ancestor(env: &EnvPtr, depth: usize) -> EnvPtr {
        let mut ret = env.clone();

        for _ in 0..depth {
            let next = match &ret.borrow().enclosing {
                Some(enc) => enc.clone(),
                None => unreachable!(),
            };
            ret = next;
        }

        ret
    }

    pub fn get_at(env: &EnvPtr, depth: usize, t: &Token) -> Option<Value> {
        Environment::ancestor(env, depth).borrow().get(t)
    }

    pub fn assign_at(&mut self, depth: usize, t: &Token, v: Value) -> Result<(), ()> {
        if depth == 0 {
            self.assign(t, v)
        } else {
            Environment::ancestor(self.enclosing.as_ref().unwrap(), depth - 1).borrow_mut().assign(t, v)
        }
    }

    pub fn define(&mut self, t: &Token, v: Value) -> Result<(), ()> {
        match &t.ty {
            crate::token::TokenType::Identifier(name) => {
                self.values.insert(name.clone(), v);
                Ok(())
            }
            _ => Err(())
        }
    }

    pub fn assign(&mut self, t: &Token, v: Value) -> Result<(), ()> {
        match &t.ty {
            crate::token::TokenType::Identifier(name) => {
                if self.values.contains_key(name) {
                    self.values.insert(name.clone(), v);
                    Ok(())
                } else {
                    match &mut self.enclosing {
                        Some(e) => e.borrow_mut().assign(t,v),
                        None => Err(())
                    }
                }
            }
            _ => Err(())
        }
    }

    pub fn get(&self, t: &Token) -> Option<Value> {
        let name = match &t.ty {
            crate::token::TokenType::Identifier(name) => Some(name.clone()),
            crate::token::TokenType::This => Some(String::from("this")),
            crate::token::TokenType::Super => Some(String::from("super")),
            _ => None
        };

        match name {
            Some(name) =>
                self.values.get(&name).and_then(|v: &Value| {
                    Some(v.clone())
                }).or_else(|| {
                    self.enclosing.as_ref().and_then(|e| e.borrow().get(t))
                }),
            _ => None,
        }
    }

    pub fn globals(env: &EnvPtr) -> EnvPtr {
        match &env.borrow().enclosing {
            Some(e) => Environment::globals(e),
            None => env.clone(),
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
    }
    
}
