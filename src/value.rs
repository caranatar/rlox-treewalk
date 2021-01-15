use crate::literal::Literal;
use crate::callable::CallableEnum;
use crate::class::{Class, Instance};
use std::{rc::Rc, cell::RefCell};

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    Function(CallableEnum),
    Class(Class),
    Instance(Rc<RefCell<Instance>>),
}

impl std::convert::From<Literal> for Value {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::String(s) => Value::String(s),
            Literal::Number(n) => Value::Number(n),
            Literal::Bool(b) => Value::Bool(b),
            Literal::Nil => Value::Nil,
        }
    }
    
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "{}", "nil"),
            Value::Function(_) => write!(f, "{}", "<function>"),
            Value::Class(c) => write!(f, "{}", c.name),
            Value::Instance(c) => write!(f, "{} instance", c.borrow().class.name),
        }
    }
    
}
