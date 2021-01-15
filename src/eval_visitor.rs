use std::rc::Rc;
use std::{collections::HashMap, cell::RefCell};
use crate::value::Value;
use crate::{error_reporter::ErrorReporter, lox_error::LoxError, token::{Token, TokenType}};
use crate::{stmt::BlockStmt, environment::Environment};
use crate::{function::Function, clock::Clock};
use crate::eval::Eval;
use crate::{environment::EnvPtr, class::Class, callable::{CallableEnum, Callable}};
use float_cmp::approx_eq;

pub struct EvalVisitor {
    pub(crate) reporter: ErrorReporter,
    pub(crate) environment: EnvPtr,
}

impl EvalVisitor {
    pub fn new() -> Self {
        let environment = Rc::new(RefCell::new(Environment::new()));

        let clock_token = Token { ty: TokenType::Identifier(String::from("clock")), line: 0 };
        let clock_value = Value::Function(CallableEnum::Builtin(Box::new(Clock{})));
        environment.borrow_mut().define(&clock_token, clock_value).expect("Failed to define builtin function clock()");

        EvalVisitor {
            reporter: ErrorReporter::new(),
            environment,
        }
    }
}

fn truthy(v: &Value) -> bool {
    match v {
        Value::Bool(b) => *b,
        Value::Nil => false,
        _ => true,
    }
}

fn is_equal(u: Value, v: Value) -> bool {
    match (u, v) {
        (Value::String(s), Value::String(t)) => s == t,
        (Value::Number(m), Value::Number(n)) => {
            approx_eq!(f64, m, n, ulps = 2)
        }
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (_, _) => false,
    }
}

type ValueResult = Result<Value, ()>;

impl Eval for EvalVisitor {
    fn globals(&self) -> EnvPtr {
        Environment::globals(&self.environment)
    }

    fn visit_binary_expr(&mut self, e: &crate::expr::BinaryExpr) -> ValueResult {
        let left = e.left.eval(self)?;
        let right = e.right.eval(self)?;

        use TokenType::{
            BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Plus, Slash, Star,
        };
        use Value::String as LoxStr;
        use Value::{Bool, Number};
        match (&e.operator.ty, left, right) {
            (Minus, Number(l), Number(r)) => Ok(Number(l - r)),
            (Slash, Number(l), Number(r)) => Ok(Number(l / r)),
            (Star, Number(l), Number(r)) => Ok(Number(l * r)),
            (Plus, Number(l), Number(r)) => Ok(Number(l + r)),
            (Plus, LoxStr(s), LoxStr(t)) => Ok(LoxStr(format!("{}{}", s, t))),
            (Greater, Number(l), Number(r)) => Ok(Bool(l > r)),
            (GreaterEqual, Number(l), Number(r)) => Ok(Bool(l >= r)),
            (Less, Number(l), Number(r)) => Ok(Bool(l < r)),
            (LessEqual, Number(l), Number(r)) => Ok(Bool(l <= r)),
            (BangEqual, x, y) => Ok(Bool(!is_equal(x, y))),
            (EqualEqual, x, y) => Ok(Bool(is_equal(x, y))),
            (op, left, right) => {
                self.reporter.error(LoxError::BadOperands {
                    left,
                    operator: op.clone(),
                    right,
                }, e.operator.line);
                Err(())
            }
        }
    }

    fn visit_grouping_expr(&mut self, e: &crate::expr::GroupingExpr) -> ValueResult {
        e.expression.eval(self)
    }

    fn visit_literal_expr(&mut self, e: &crate::expr::LiteralExpr) -> ValueResult {
        Ok(e.value.clone().into())
    }

    fn visit_unary_expr(&mut self, e: &crate::expr::UnaryExpr) -> ValueResult {
        let v = e.right.eval(self)?;

        match (&e.operator.ty, v) {
            (TokenType::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
            (TokenType::Bang, x) => Ok(Value::Bool(!truthy(&x))),
            (op, right) => {
                self.reporter.error(LoxError::BadOperand {
                    operator: op.clone(),
                    right,
                }, e.operator.line);
                Err(())
            }
        }
    }

    fn visit_expr_stmt(&mut self, e: &crate::stmt::ExprStmt) -> Result<Option<Value>, ()> {
        e.expression.eval(self)?;
        Ok(None)
    }

    fn visit_print_stmt(&mut self, e: &crate::stmt::PrintStmt) -> Result<Option<Value>, ()> {
        let value = e.expression.eval(self)?;
        println!("{}", value);
        Ok(None)
    }
    
    fn visit_var_expr(&mut self, e: &crate::expr::VarExpr) -> Result<Value, ()> {
        let lookup = match e.get_depth() {
            Some(d) => Environment::get_at(&self.environment, d, &e.name),
            None => Environment::globals(&self.environment).borrow().get(&e.name),
        };

        match lookup {
            Some(v) => Ok(v),
            None => {
                let name = match &e.name.ty {
                    TokenType::Identifier(s) => s.clone(),
                    _ => unreachable!(),
                };
                self.reporter.error(LoxError::UndefinedVariable(name), e.name.line);
                Err(())
            },
        }
    }
    
    fn visit_var_stmt(&mut self, e: &crate::stmt::VarStmt) -> Result<Option<Value>, ()> {
        let v = match &e.init {
            Some(i) => i.eval(self),
            None => Ok(Value::Nil),
        }?;

        match self.environment.borrow_mut().define(&e.name, v) {
            Ok(_) => Ok(None),
            Err(_) => Err(()),
        }
    }

    fn visit_assign_expr(&mut self, e: &crate::expr::AssignExpr) -> Result<Value, ()> {
        let v = e.value.eval(self)?;
        let ret = v.clone();

        let lookup = match e.get_depth() {
            Some(d) => self.environment.borrow_mut().assign_at(d, &e.name, v),
            None => Environment::globals(&self.environment).borrow_mut().assign(&e.name, v),
        };

        match lookup {
            Ok(_) => Ok(ret),
            Err(_) => {
                let name = match &e.name.ty {
                    TokenType::Identifier(s) => s.clone(),
                    _ => unreachable!(),
                };
                self.reporter.error(LoxError::UndefinedVariable(name), e.name.line);
                Err(())
            },
        }
    }

    fn visit_block_stmt(&mut self, e: &BlockStmt) -> Result<Option<Value>, ()> {
        self.environment = Environment::push_new(self.environment.clone());

        let mut ret = Ok(None);
        for stmt in &e.statements {
            match stmt.eval(self) {
                Ok(Some(v)) => {
                    ret = Ok(Some(v));
                    break;
                },
                Ok(_) => continue,
                Err(_) => {
                    ret = Err(());
                    break;
                },
            }
        }

        self.environment = Environment::pop(self.environment.clone());

        ret
    }
    
    fn visit_if_stmt(&mut self, e: &crate::stmt::IfStmt) -> Result<Option<Value>, ()> {
        if truthy(&e.condition.eval(self)?) {
            e.then_branch.eval(self)
        } else if let Some(else_branch) = &e.else_branch {
            else_branch.eval(self)
        } else {
            Ok(None)
        }
    }

    fn visit_logical_expr(&mut self, e: &crate::expr::LogicalExpr) -> Result<Value, ()> {
        let left = e.left.eval(self)?;

        if e.operator.ty == TokenType::Or {
            if truthy(&left) {
                return Ok(left);
            }
        } else {
            if !truthy(&left) {
                return Ok(left);
            }
        }

        e.right.eval(self)
    }
    
    fn visit_while_stmt(&mut self, e: &crate::stmt::WhileStmt) -> Result<Option<Value>, ()> {
        while truthy(&e.condition.eval(self)?) {
            let v = e.body.eval(self)?;
            if let Some(val) = v {
                return Ok(Some(val));
            }
        }
        Ok(None)
    }

    fn visit_call_expr(&mut self, e: &crate::expr::CallExpr) -> Result<Value, ()> {
        let callee = e.callee.eval(self)?;

        let mut args = Vec::new();
        for arg in &e.args {
            args.push(arg.eval(self)?);
        }

        let callee: Box<dyn Callable> = if let Value::Function(callee) = callee {
            Box::new(callee)
        } else if let Value::Class(class) = callee {
            Box::new(class)
        } else {
            self.reporter.error(LoxError::NotCallable, e.paren.line);
            return Err(());
        };

        if args.len() != callee.arity() {
            self.reporter.error(LoxError::WrongNumberArguments, e.paren.line);
            Err(())
        } else {
            callee.call(self, args)
        }
    }
    
    fn visit_fun_stmt(&mut self, e: &crate::stmt::FunStmt) -> Result<Option<Value>, ()> {
        let fun = Function::new(e.clone(), self.environment.clone(), false);
        let fun = Value::Function(CallableEnum::Function(fun));
        match self.environment.borrow_mut().define(&e.name, fun) {
            Ok(_) => Ok(None),
            Err(_) => Err(())
        }
    }
    
    fn execute_block(&mut self, block: Box<dyn crate::stmt::Stmt>, env: EnvPtr) -> Result<Option<Value>, ()> {
        let mut visitor = EvalVisitor::new();
        visitor.environment = env;
        visitor.reporter = std::mem::take(&mut self.reporter);
        let res = block.eval(&mut visitor);
        self.reporter = visitor.reporter;
        res
    }
    
    fn visit_return_stmt(&mut self, e: &crate::stmt::ReturnStmt) -> Result<Option<Value>, ()> {
        let mut value = Some(Value::Nil);

        if let Some(v) = &e.value {
            value = Some(v.eval(self)?);
        }

        Ok(value)
    }
    
    fn visit_class_stmt(&mut self, e: &crate::stmt::ClassStmt) -> Result<Option<Value>, ()> {
        let superclass = match &e.superclass {
            Some(s) => {
                Some(match s.eval(self)? {
                    Value::Class(c) => Ok(c),
                    _ => {
                        self.reporter.error(LoxError::BadSuperclass, e.name.line);
                        Err(())
                    },
                }?)
            },
            None => None,
        };

        if let TokenType::Identifier(name) = &e.name.ty {
            self.environment.borrow_mut().define(&e.name, Value::Nil)?;

            if let Some(s) = &superclass {
                self.environment = Environment::push_new(self.environment.clone());
                let fake_super = Token { ty: TokenType::Identifier(String::from("super")), line: e.name.line };
                let sup = Value::Class(s.clone());
                self.environment.borrow_mut().define(&fake_super, sup)?;
            }

            let mut methods = HashMap::new();
            for method in &e.methods {
                if let TokenType::Identifier(name) = &method.name.ty {
                    let is_init = name == "init";
                    let fun = Function::new(method.clone(), self.environment.clone(), is_init);
                    methods.insert(name.clone(), fun);
                } else {
                    unreachable!()
                }
            }

            if superclass.is_some() {
                self.environment = Environment::pop(self.environment.clone());
            }

            let class = Class::new(name.clone(), methods, superclass.map(Box::new));

            self.environment.borrow_mut().assign(&e.name, Value::Class(class))?;
            Ok(None)
        } else {
            Err(())
        }
    }
    
    fn visit_get_expr(&mut self, e: &crate::expr::GetExpr) -> Result<Value, ()> {
        let obj = e.object.eval(self)?;
        if let Value::Instance(inst) = obj {
            match inst.borrow().get(&e.name) {
                Some(Value::Function(CallableEnum::Function(f))) => {
                    let ret = f.bind_this(inst.clone());
                    Ok(Value::Function(CallableEnum::Function(ret)))
                },
                Some(v) => Ok(v),
                None => {
                    self.reporter.error(LoxError::UndefinedProperty, e.name.line);
                    Err(())
                },
            }
        } else {
            self.reporter.error(LoxError::BadGet, e.name.line);
            Err(())
        }
    }
    
    fn visit_set_expr(&mut self, e: &crate::expr::SetExpr) -> Result<Value, ()> {
        let mut obj = e.object.eval(self)?;

        if let Value::Instance(inst) = &mut obj {
            let value = e.value.eval(self)?;
            inst.borrow_mut().set(&e.name, value.clone());
            Ok(value)
        } else {
            self.reporter.error(LoxError::BadGet, e.name.line);
            Err(())
        }
    }
    
    fn visit_this_expr(&mut self, e: &crate::expr::ThisExpr) -> Result<Value, ()> {
        let lookup = match e.get_depth() {
            Some(d) => Environment::get_at(&self.environment, d, &e.keyword),
            None => Environment::globals(&self.environment).borrow().get(&e.keyword),
        };

        match lookup {
            Some(v) => Ok(v),
            None => {
                self.reporter.error(LoxError::UndefinedVariable(String::from("this")), e.keyword.line);
                Err(())
            },
        }
    }
    
    fn visit_super_expr(&mut self, e: &crate::expr::SuperExpr) -> Result<Value, ()> {
        let distance = e.get_depth().unwrap();
        let superclass = Environment::get_at(&self.environment, distance, &e.keyword).unwrap();
        let fake_this = Token {
            ty: TokenType::Identifier(String::from("this")),
            line: 0,
        };
        let obj = Environment::get_at(&self.environment, distance - 1, &fake_this).unwrap();

        let method_name = match &e.method.ty {
            TokenType::Identifier(name) => name.clone(),
            _ => unreachable!(),
        };
        match superclass {
            Value::Class(c) => {
                match obj {
                    Value::Instance(o) => {
                        let method = c.find_method(&method_name).unwrap();
                        return Ok(Value::Function(CallableEnum::Function(method.bind_this(o.clone()))));
                    },
                    _ => unreachable!(),
                    
                }
            },
            _ => unreachable!(),
        }
    }
}
