use crate::literal::Literal;
use crate::token::Token;
use crate::value::Value;
use crate::eval::Eval;
use crate::resolver::Resolver;

type ValueResult = Result<Value, ()>;

#[derive(Clone)]
pub enum OwnedExpr {
    BinaryExpr(Box<BinaryExpr>),
    GroupingExpr(Box<GroupingExpr>),
    LiteralExpr(Box<LiteralExpr>),
    UnaryExpr(Box<UnaryExpr>),
    VarExpr(Box<VarExpr>),
    AssignExpr(Box<AssignExpr>),
    LogicalExpr(Box<LogicalExpr>),
    CallExpr(Box<CallExpr>),
    GetExpr(Box<GetExpr>),
    SetExpr(Box<SetExpr>),
    ThisExpr(Box<ThisExpr>),
    SuperExpr(Box<SuperExpr>),
}

impl OwnedExpr {
    pub fn eval(&self, visitor: &mut dyn Eval) -> ValueResult {
        match self {
            Self::BinaryExpr(b) => b.eval(visitor),
            Self::GroupingExpr(b) => b.eval(visitor),
            Self::LiteralExpr(b) => b.eval(visitor),
            Self::UnaryExpr(b) => b.eval(visitor),
            Self::VarExpr(b) => b.eval(visitor),
            Self::AssignExpr(b) => b.eval(visitor),
            Self::LogicalExpr(b) => b.eval(visitor),
            Self::CallExpr(b) => b.eval(visitor),
            Self::GetExpr(b) => b.eval(visitor),
            Self::SetExpr(b) => b.eval(visitor),
            Self::ThisExpr(b) => b.eval(visitor),
            Self::SuperExpr(b) => b.eval(visitor),
        }
    }

    pub fn resolve(&mut self, visitor: &mut dyn Resolver) {
        match self {
            Self::BinaryExpr(b) => b.resolve(visitor),
            Self::GroupingExpr(b) => b.resolve(visitor),
            Self::LiteralExpr(b) => b.resolve(visitor),
            Self::UnaryExpr(b) => b.resolve(visitor),
            Self::VarExpr(b) => b.resolve(visitor),
            Self::AssignExpr(b) => b.resolve(visitor),
            Self::LogicalExpr(b) => b.resolve(visitor),
            Self::CallExpr(b) => b.resolve(visitor),
            Self::GetExpr(b) => b.resolve(visitor),
            Self::SetExpr(b) => b.resolve(visitor),
            Self::ThisExpr(b) => b.resolve(visitor),
            Self::SuperExpr(b) => b.resolve(visitor),
        }
    }
}

pub trait Expr: ExprClone {
    fn eval(&self, visitor: &mut dyn Eval) -> ValueResult;

    fn resolve(&mut self, visitor: &mut dyn Resolver);

    fn set_depth(&mut self, depth: usize);
}

pub trait ExprClone {
    fn clone_expr(&self) -> Box<dyn Expr>;
}

impl<T> ExprClone for T where T: 'static + Expr + Clone {
    fn clone_expr(&self) -> Box<dyn Expr> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Expr> {
    fn clone(&self) -> Self {
        self.clone_expr()
    }
}

macro_rules! depth_init {
    ($name:ident, true, $($i:ident),+) => {
        $name { depth: None, $($i),+ }
    };
    
    ($name:ident, false, $($i:ident),+) => {
        $name { $($i),+ }
    };
}

macro_rules! depth_struct {
    ($name:ident, true, $($i:ident : $t:ty),+) => {
        #[derive(Clone)]
        pub struct $name {
            pub(crate) depth: Option<usize>,
            $(pub(crate) $i: $t),+
        }
    };
    ($name:ident, false, $($i:ident : $t:ty),+) => {
        #[derive(Clone)]
        pub struct $name {
            $(pub(crate) $i: $t),+
        }
    };
}

macro_rules! set_depth {
    (true) => {
        fn set_depth(&mut self, depth: usize) {
            self.depth = Some(depth);
        }
    };
    
    (false) => {
        fn set_depth(&mut self, _: usize) {
        }
    };
}

macro_rules! get_depth {
    (true) => {
        pub fn get_depth(&self) -> Option<usize> {
            self.depth
        }
    };

    (false) => {
    };
}

macro_rules! expr {
    ($name:ident, $has_depth:tt, $visitor:ident, $($i:ident : $t:ty),+) => {
        depth_struct!($name, $has_depth, $($i: $t),+);

        impl $name {
            pub fn new($($i: $t,)+) -> Self {
                depth_init!($name, $has_depth, $($i),+)
            }

            get_depth!($has_depth);
        }

        impl Expr for $name {
            fn eval(&self, visitor: &mut dyn Eval) -> ValueResult {
                visitor.$visitor(self)
            }

            fn resolve(&mut self, visitor: &mut dyn Resolver) {
                visitor.$visitor(self)
            }

            set_depth!($has_depth);
        }

        impl std::convert::Into<OwnedExpr> for $name {
            fn into(self) -> OwnedExpr {
                OwnedExpr::$name(Box::new(self))
            }
        }
    }
}

expr!(BinaryExpr, false, visit_binary_expr, left: OwnedExpr, operator: Token, right: OwnedExpr);

expr!(GroupingExpr, false, visit_grouping_expr, expression: OwnedExpr);

expr!(LiteralExpr, false, visit_literal_expr, value: Literal);

expr!(UnaryExpr, false, visit_unary_expr, operator: Token, right: OwnedExpr);

expr!(VarExpr, true, visit_var_expr, name: Token);

expr!(AssignExpr, true, visit_assign_expr, name: Token, value: OwnedExpr);

expr!(LogicalExpr, false, visit_logical_expr, left: OwnedExpr, operator: Token, right: OwnedExpr);

expr!(CallExpr, false, visit_call_expr, callee: OwnedExpr, paren: Token, args: Vec<OwnedExpr>);

expr!(GetExpr, false, visit_get_expr, object: OwnedExpr, name: Token);

expr!(SetExpr, false, visit_set_expr, object: OwnedExpr, name: Token, value: OwnedExpr);

expr!(ThisExpr, true, visit_this_expr, keyword: Token);

expr!(SuperExpr, true, visit_super_expr, keyword: Token, method: Token);
