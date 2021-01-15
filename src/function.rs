use crate::stmt::FunStmt;
use crate::token::{Token, TokenType};
use crate::{value::Value, callable::Callable};
use crate::environment::{Environment, EnvPtr};
use crate::class::Instance;
use std::{rc::Rc, cell::RefCell};

#[derive(Clone)]
pub struct Function {
    pub(crate) declaration: FunStmt,
    pub(crate) closure: EnvPtr,
    pub(crate) is_init: bool,
}

impl Function {
    pub fn new(declaration: FunStmt, closure: EnvPtr, is_init: bool) -> Self {
        Function { declaration, closure, is_init }
    }

    pub fn bind_this(&self, instance: Rc<RefCell<Instance>>) -> Self {
        let env = Environment::push_new(self.closure.clone());
        let fake_this = Token { ty: TokenType::Identifier(String::from("this")), line: self.declaration.name.line };
        env.borrow_mut().define(&fake_this, Value::Instance(instance)).expect("");
        Function::new(self.declaration.clone(), env, self.is_init)
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "<user-defined function>")
    }
}

impl Callable for Function {
    fn call(&self, visitor: &mut dyn crate::eval::Eval, mut args: Vec<Value>) -> Result<Value, ()> {
        let mut ret = Ok(Value::Nil);
        let ctx = Environment::push_new(self.closure.clone());

        for (idx, arg) in args.drain(..).enumerate() {
            match ctx.borrow_mut().define(&self.declaration.params[idx], arg) {
                Ok(_) => continue,
                Err(_) => {
                    ret = Err(());
                    break;
                }
            }
        }

        if ret.is_ok() {
            let res = visitor.execute_block(self.declaration.body.clone(), ctx);
            match res {
                Ok(Some(v)) => ret = Ok(v),
                Ok(None) => (),
                Err(_) => ret = Err(()),
            }
        }

        let ret = ret?;
        if self.is_init {
            let this = Token {
                ty: TokenType::This,
                line: self.declaration.name.line,
            };
            match Environment::get_at(&self.closure, 0, &this) {
                Some(v) => Ok(v),
                None => Err(()),
            }
        } else {
            Ok(ret)
        }
    }

    fn arity(&self) -> usize {
        self.declaration.params.len()
    }
}
