use crate::expr::{BinaryExpr, GroupingExpr, LiteralExpr, GetExpr, SetExpr};
use crate::expr::{VarExpr, UnaryExpr, AssignExpr, LogicalExpr, CallExpr};
use crate::expr::{ThisExpr, SuperExpr};
use crate::value::Value;
use crate::stmt::{ExprStmt, PrintStmt, ReturnStmt, Stmt, ClassStmt};
use crate::stmt::{BlockStmt, VarStmt, IfStmt, WhileStmt, FunStmt};
use crate::environment::EnvPtr;

pub trait Eval {
    fn globals(&self) -> EnvPtr;
    
    fn execute_block(&mut self, block: Box<dyn Stmt>, env: EnvPtr) -> Result<Option<Value>, ()>;

    fn visit_binary_expr(&mut self, e: &BinaryExpr) -> Result<Value, ()>;
    fn visit_grouping_expr(&mut self, e: &GroupingExpr) -> Result<Value, ()>;
    fn visit_literal_expr(&mut self, e: &LiteralExpr) -> Result<Value, ()>;
    fn visit_unary_expr(&mut self, e: &UnaryExpr) -> Result<Value, ()>;
    fn visit_var_expr(&mut self, e: &VarExpr) -> Result<Value, ()>;
    fn visit_assign_expr(&mut self, e: &AssignExpr) -> Result<Value, ()>;
    fn visit_logical_expr(&mut self, e: &LogicalExpr) -> Result<Value, ()>;
    fn visit_call_expr(&mut self, e: &CallExpr) -> Result<Value, ()>;
    fn visit_get_expr(&mut self, e: &GetExpr) -> Result<Value, ()>;
    fn visit_set_expr(&mut self, e: &SetExpr) -> Result<Value, ()>;
    fn visit_this_expr(&mut self, e: &ThisExpr) -> Result<Value, ()>;
    fn visit_super_expr(&mut self, e: &SuperExpr) -> Result<Value, ()>;

    fn visit_expr_stmt(&mut self, e: &ExprStmt) -> Result<Option<Value>, ()>;
    fn visit_print_stmt(&mut self, e: &PrintStmt) -> Result<Option<Value>, ()>;
    fn visit_var_stmt(&mut self, e: &VarStmt) -> Result<Option<Value>, ()>;
    fn visit_block_stmt(&mut self, e: &BlockStmt) -> Result<Option<Value>, ()>;
    fn visit_if_stmt(&mut self, e: &IfStmt) -> Result<Option<Value>, ()>;
    fn visit_while_stmt(&mut self, e: &WhileStmt) -> Result<Option<Value>, ()>;
    fn visit_fun_stmt(&mut self, e: &FunStmt) -> Result<Option<Value>, ()>;
    fn visit_return_stmt(&mut self, e: &ReturnStmt) -> Result<Option<Value>, ()>;
    fn visit_class_stmt(&mut self, e: &ClassStmt) -> Result<Option<Value>, ()>;
}
