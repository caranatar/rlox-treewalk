use crate::callable::{CallableEnum, Callable};
use crate::{token::{Token, TokenType}, value::Value, function::Function};
use std::{collections::HashMap, rc::Rc, cell::RefCell};

#[derive(Clone, Debug)]
pub struct Class {
    pub(crate) name: String,
    pub(crate) methods: HashMap<String, Function>,
    pub(crate) superclass: Option<Box<Class>>,
}

impl Class {
    pub fn new(name: String, methods: HashMap<String, Function>, superclass: Option<Box<Class>>) -> Self {
        Class { name, methods, superclass }
    }

    pub fn find_method(&self, name: &String) -> Option<Function> {
        match self.methods.get(name) {
            Some(f) => Some(f.clone()),
            None => match &self.superclass {
                Some(c) => c.find_method(name),
                None => None,
            },
        }
    }
}

impl Callable for Class {
    fn call(&self, visitor: &mut dyn crate::eval::Eval, args: Vec<Value>) -> Result<Value, ()> {
        let instance = Rc::new(RefCell::new(Instance::new(self.clone())));

        if let Some(init) =  self.find_method(&String::from("init")) {
            init.bind_this(instance.clone()).call(visitor, args)?;
        }

        Ok(Value::Instance(instance).into())
    }

    fn arity(&self) -> usize {
        if let Some(init) = self.find_method(&String::from("init")) {
            init.arity()
        } else {
            0
        }
    }
}

#[derive(Clone, Debug)]
pub struct Instance {
    pub(crate) class: Class,
    pub(crate) fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: Class) -> Self {
        Instance { class, fields: HashMap::new() }
    }

    pub fn get(&self, name: &Token) -> Option<Value> {
        if let TokenType::Identifier(name) = &name.ty {
            match self.fields.get(name) {
                Some(v) => Some(v.clone()),
                None => None,
            }.or_else(|| {
                match self.class.find_method(name) {
                    Some(f) => Some(Value::Function(CallableEnum::Function(f))),
                    None => None,
                }
            })
        } else {
            None
        }
    }

    pub fn set(&mut self, name: &Token, value: Value) {
        if let TokenType::Identifier(name) = &name.ty {
            self.fields.insert(name.clone(), value);
        } else {
            unreachable!()
        }
    }
}
