use crate::expr::OwnedExpr;
use crate::eval::Eval;
use crate::token::Token;
use crate::value::Value;
use crate::resolver::Resolver;

type StmtResult = Result<Option<Value>, ()>;

pub trait Stmt: StmtClone {
    fn eval(&self, visitor: &mut dyn Eval) -> Result<Option<Value>, ()>;

    fn resolve(&mut self, visitor: &mut dyn Resolver);
}

pub trait StmtClone {
    fn clone_stmt(&self) -> Box<dyn Stmt>;
}

impl<T> StmtClone for T where T: 'static + Stmt + Clone {
    fn clone_stmt(&self) -> Box<dyn Stmt> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Stmt> {
    fn clone(&self) -> Self {
        self.clone_stmt()
    }
}

macro_rules! stmt {
    ($name:ident, $visitor:ident, $($i:ident : $t:ty),+) => {
        #[derive(Clone)]
        pub struct $name {
            $(pub(crate) $i: $t,)+
        }

        impl $name {
            pub fn new($($i: $t,)+) -> Self {
                $name { $($i),+ }
            }
        }

        impl Stmt for $name {
            fn eval(&self, visitor: &mut dyn Eval) -> StmtResult {
                visitor.$visitor(self)
            }

            fn resolve(&mut self, visitor: &mut dyn Resolver) {
                visitor.$visitor(self)
            }
        }
    };
}

stmt!(ExprStmt, visit_expr_stmt, expression: OwnedExpr);

stmt!(PrintStmt, visit_print_stmt, expression: OwnedExpr);

stmt!(VarStmt, visit_var_stmt, name: Token, init: Option<OwnedExpr>);

stmt!(BlockStmt, visit_block_stmt, statements: Vec<Box<dyn Stmt>>);

stmt!(IfStmt, visit_if_stmt, condition: OwnedExpr, then_branch: Box<dyn Stmt>, else_branch: Option<Box<dyn Stmt>>);

stmt!(WhileStmt, visit_while_stmt, condition: OwnedExpr, body: Box<dyn Stmt>);

stmt!(FunStmt, visit_fun_stmt, name: Token, params: Vec<Token>, body: Box<dyn Stmt>);

stmt!(ReturnStmt, visit_return_stmt, keyword: Token, value: Option<OwnedExpr>);

stmt!(ClassStmt, visit_class_stmt, name: Token, superclass: Option<OwnedExpr>, methods: Vec<FunStmt>);
