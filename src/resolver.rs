use crate::expr::{BinaryExpr, VarExpr, GroupingExpr, LiteralExpr, UnaryExpr};
use crate::expr::{AssignExpr, LogicalExpr, CallExpr, Expr, GetExpr, SetExpr};
use crate::expr::{OwnedExpr, SuperExpr, ThisExpr};
use crate::stmt::{ExprStmt, PrintStmt, VarStmt, BlockStmt, IfStmt, WhileStmt};
use crate::stmt::{FunStmt, ReturnStmt, Stmt, ClassStmt};
use crate::{error_reporter::ErrorReporter, token::{Token, TokenType}, lox_error::LoxError};
use std::collections::HashMap;

pub trait Resolver {
    fn visit_binary_expr(&mut self, e: &mut BinaryExpr);
    fn visit_grouping_expr(&mut self, e: &mut GroupingExpr);
    fn visit_literal_expr(&mut self, e: &mut LiteralExpr);
    fn visit_unary_expr(&mut self, e: &mut UnaryExpr);
    fn visit_var_expr(&mut self, e: &mut VarExpr);
    fn visit_assign_expr(&mut self, e: &mut AssignExpr);
    fn visit_logical_expr(&mut self, e: &mut LogicalExpr);
    fn visit_call_expr(&mut self, e: &mut CallExpr);
    fn visit_get_expr(&mut self, e: &mut GetExpr);
    fn visit_set_expr(&mut self, e: &mut SetExpr);
    fn visit_this_expr(&mut self, e: &mut ThisExpr);
    fn visit_super_expr(&mut self, e: &mut SuperExpr);

    fn visit_expr_stmt(&mut self, e: &mut ExprStmt);
    fn visit_print_stmt(&mut self, e: &mut PrintStmt);
    fn visit_var_stmt(&mut self, e: &mut VarStmt);
    fn visit_block_stmt(&mut self, e: &mut BlockStmt);
    fn visit_if_stmt(&mut self, e: &mut IfStmt);
    fn visit_while_stmt(&mut self, e: &mut WhileStmt);
    fn visit_fun_stmt(&mut self, e: &mut FunStmt);
    fn visit_return_stmt(&mut self, e: &mut ReturnStmt);
    fn visit_class_stmt(&mut self, e: &mut ClassStmt);
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum FunctionState {
    Function,
    Initializer,
    Method,
    None,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum ClassState {
    Class,
    None,
}

pub struct ResolveVisitor<'a> {
    scopes: Vec<HashMap<String, bool>>,
    function_state: FunctionState,
    class_state: ClassState,
    reporter: &'a mut ErrorReporter,
}

impl<'a> ResolveVisitor<'a> {
    pub fn new(reporter: &'a mut ErrorReporter) -> Self {
        ResolveVisitor { scopes: Vec::new(), function_state: FunctionState::None, reporter, class_state: ClassState::None, }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_stmt(&mut self, stmt: &mut Box<dyn Stmt>) {
        stmt.resolve(self)
    }

    pub fn resolve_stmts(&mut self, stmts: &mut Vec<Box<dyn Stmt>>) {
        for stmt in stmts {
            self.resolve_stmt(stmt)
        }
    }

    fn resolve_expr(&mut self, expr: &mut OwnedExpr) {
        expr.resolve(self)
    }

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            match &name.ty {
                TokenType::Identifier(i) => {
                    if scope.contains_key(i) {
                        self.reporter.error(LoxError::Redeclaration(i.clone()), name.line);
                    } else {
                        scope.insert(i.clone(), false);
                    }
                },
                _ => unreachable!(),
            }
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            match &name.ty {
                TokenType::Identifier(i) => {
                    scope.insert(i.clone(), true);
                },
                _ => unreachable!(),
            }
        }
    }

    fn resolve_local(&self, expr: &mut dyn Expr, t: &Token) {
        let name = if let TokenType::Identifier(name) = &t.ty {
            Some(name.clone())
        } else if let TokenType::This = &t.ty {
            Some(String::from("this"))
        } else if let TokenType::Super = &t.ty {
            Some(String::from("super"))
        } else {
            None
        };

        if let Some(name) = name {
            let mut i = self.scopes.len();
            while i > 0 {
                if self.scopes[i-1].contains_key(&name) {
                    let depth = self.scopes.len() - i;
                    expr.set_depth(depth);
                    break;
                }
                i -= 1;
            }
        } else {
            unreachable!()
        }
    }

    fn resolve_function(&mut self, e: &mut FunStmt, state: FunctionState) {
        let enc_state = self.function_state;
        self.function_state = state;

        self.begin_scope();
        for param in &e.params {
            self.declare(param);
            self.define(param);
        }
        self.resolve_stmt(&mut e.body);
        self.end_scope();

        self.function_state = enc_state;
    }
}

impl<'a> Resolver for ResolveVisitor<'a> {
    fn visit_binary_expr(&mut self, e: &mut BinaryExpr) {
        self.resolve_expr(&mut e.left);
        self.resolve_expr(&mut e.right);
    }

    fn visit_grouping_expr(&mut self, e: &mut GroupingExpr) {
        self.resolve_expr(&mut e.expression);
    }

    fn visit_literal_expr(&mut self, _e: &mut LiteralExpr) {
        // do nothing
    }

    fn visit_unary_expr(&mut self, e: &mut UnaryExpr) {
        self.resolve_expr(&mut e.right);
    }

    fn visit_var_expr(&mut self, e: &mut VarExpr) {
        if let Some(scope) = self.scopes.last() {
            if let TokenType::Identifier(name) = &e.name.ty {
                if scope.get(name) == Some(&false) {
                    self.reporter.error(LoxError::ReferenceInInitializer, e.name.line);
                    return;
                }
            }
        }

        let name = e.name.clone();
        self.resolve_local(e, &name);
    }

    fn visit_assign_expr(&mut self, e: &mut AssignExpr) {
        self.resolve_expr(&mut e.value);
        let name = e.name.clone();
        self.resolve_local(e, &name);
    }

    fn visit_logical_expr(&mut self, e: &mut LogicalExpr) {
        self.resolve_expr(&mut e.right);
    }

    fn visit_call_expr(&mut self, e: &mut CallExpr) {
        self.resolve_expr(&mut e.callee);

        for arg in &mut e.args {
            self.resolve_expr(arg);
        }
    }

    fn visit_expr_stmt(&mut self, e: &mut ExprStmt) {
        self.resolve_expr(&mut e.expression);
    }

    fn visit_print_stmt(&mut self, e: &mut PrintStmt) {
        self.resolve_expr(&mut e.expression);
    }

    fn visit_var_stmt(&mut self, e: &mut VarStmt) {
        self.declare(&e.name);
        if let Some(init) = &mut e.init {
            self.resolve_expr(init);
        }
        self.define(&e.name);
    }

    fn visit_block_stmt(&mut self, e: &mut BlockStmt) {
        self.begin_scope();
        self.resolve_stmts(&mut e.statements);
        self.end_scope();
    }

    fn visit_if_stmt(&mut self, e: &mut IfStmt) {
        self.resolve_expr(&mut e.condition);
        self.resolve_stmt(&mut e.then_branch);
        if let Some(branch) = &mut e.else_branch {
            self.resolve_stmt(branch);
        }
    }

    fn visit_while_stmt(&mut self, e: &mut WhileStmt) {
        self.resolve_expr(&mut e.condition);
        self.resolve_stmt(&mut e.body);
    }

    fn visit_fun_stmt(&mut self, e: &mut FunStmt) {
        self.declare(&e.name);
        self.define(&e.name);

        self.resolve_function(e, FunctionState::Function);
    }

    fn visit_return_stmt(&mut self, e: &mut ReturnStmt) {
        if self.function_state == FunctionState::None {
            self.reporter.error(LoxError::BadReturn, e.keyword.line);
        } else if let Some(v) = &mut e.value {
            if self.function_state == FunctionState::Initializer {
                self.reporter.error(LoxError::BadReturn, e.keyword.line);
            } else {
                self.resolve_expr(v);
            }
        }
    }
    
    fn visit_class_stmt(&mut self, e: &mut ClassStmt) {
        let curr = self.class_state;
        self.class_state = ClassState::Class;
        
        self.declare(&e.name);

        if let Some(superclass) = &mut e.superclass {
            if let OwnedExpr::VarExpr(v) = superclass {
                if v.name == e.name {
                    self.reporter.error(LoxError::InheritingSelf, v.name.line);
                } else {
                    self.resolve_expr(superclass);
                }
            }

            self.begin_scope();
            self.scopes.last_mut().unwrap().insert(String::from("super"), true);
        }

        self.begin_scope();
        self.scopes.last_mut().unwrap().insert(String::from("this"), true);

        for mut method in &mut e.methods {
            let decl = if TokenType::Identifier(String::from("init")) == method.name.ty {
                FunctionState::Initializer
            } else {
                FunctionState::Method
            };
            self.resolve_function(&mut method, decl);
        }

        self.define(&e.name);
        self.end_scope();

        if e.superclass.is_some() {
            self.end_scope();
        }

        self.class_state = curr;
    }
    
    fn visit_get_expr(&mut self, e: &mut GetExpr) {
        self.resolve_expr(&mut e.object);
    }
    
    fn visit_set_expr(&mut self, e: &mut SetExpr) {
        self.resolve_expr(&mut e.value);
        self.resolve_expr(&mut e.object);
    }
    
    fn visit_this_expr(&mut self, e: &mut ThisExpr) {
        if self.class_state == ClassState::None {
            self.reporter.error(LoxError::BadThis, e.keyword.line);
        } else {
            let t = e.keyword.clone();
            self.resolve_local(e, &t);
        }
    }
    
    fn visit_super_expr(&mut self, e: &mut SuperExpr) {
        let t = e.keyword.clone();
        self.resolve_local(e, &t);
    }
}
