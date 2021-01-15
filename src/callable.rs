use crate::value::Value;
use crate::eval::Eval;
use crate::function::Function;
use crate::class::Class;
use std::fmt::Debug;

#[derive(Clone, Debug)]
pub enum CallableEnum {
    Function(Function),
    Class(Class),
    Builtin(Box<dyn Callable>),
}

impl Callable for CallableEnum {
    fn call(&self, visitor: &mut dyn Eval, args: Vec<Value>) -> Result<Value, ()> {
        match self {
            Self::Function(f) => f.call(visitor, args),
            Self::Class(c) => c.call(visitor, args),
            Self::Builtin(b) => b.call(visitor, args),
        }
    }
    
    fn arity(&self) -> usize {
        match self {
            Self::Function(f) => f.arity(),
            Self::Class(c) => c.arity(),
            Self::Builtin(b) => b.arity(),
        }
    }
}

pub trait Callable where Self: CallableClone + Debug {
    fn call(&self, visitor: &mut dyn Eval, args: Vec<Value>) -> Result<Value, ()>;
    fn arity(&self) -> usize;
}

pub trait CallableClone {
    fn clone_callable(&self) -> Box<dyn Callable>;
}

impl<T> CallableClone for T where T: 'static + Callable + Clone {
    fn clone_callable(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Callable> {
    fn clone(&self) -> Self {
        self.clone_callable()
    }
}
