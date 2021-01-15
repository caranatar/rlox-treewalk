use crate::expr::BinaryExpr;
use crate::expr::GroupingExpr;
use crate::expr::{LiteralExpr, GetExpr, SetExpr, ThisExpr, SuperExpr};
use crate::expr::{VarExpr, UnaryExpr, AssignExpr, LogicalExpr, CallExpr, OwnedExpr};
use crate::literal::Literal;
use crate::stmt::{ExprStmt, PrintStmt, Stmt, VarStmt, BlockStmt, IfStmt, WhileStmt, FunStmt, ReturnStmt, ClassStmt};
use crate::token::Token;
use crate::token::TokenType;
use crate::{error_reporter::ErrorReporter, lox_error::LoxError};

//type ExprResult = Result<Box<dyn Expr>, ()>;
type ExprResult = Result<OwnedExpr, ()>;
type StmtResult = Result<Box<dyn Stmt>, ()>;

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    reporter: &'a mut ErrorReporter,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, reporter: &'a mut ErrorReporter) -> Self {
        Parser {
            tokens,
            current: 0,
            reporter,
        }
    }

    pub fn parse(&mut self) -> Vec<Box<dyn Stmt>> {
        let mut stmts: Vec<Box<dyn Stmt>> = Vec::new();

        while !self.at_end() {
            let stmt = match self.declaration() {
                Ok(s) => s,
                Err(_) => return stmts,
            };
            stmts.push(stmt);
        }

        stmts
    }

    fn declaration(&mut self) -> StmtResult {
        let result =  if self.matches_one(TokenType::Class) {
            self.class_declaration()
        }else if self.matches_one(TokenType::Fun) {
            self.function_declaration(LoxError::MissingFunctionName)
        } else if self.matches_one(TokenType::Var) {
            self.var_declaration()
        } else {
            self.statement()
        };

        match result {
            Ok(o) => Ok(o),
            Err(_) => {
                // TODO: sync
                Err(())
            }
        }
    }

    fn class_declaration(&mut self) -> StmtResult {
        let t = self.advance();
        if let TokenType::Identifier(_) = &t.ty {
            let superclass = if self.matches_one(TokenType::Less) {
                let token = self.advance();
                if let TokenType::Identifier(_) = &token.ty {
                    Some(VarExpr::new(token).into())
                } else {
                    self.reporter.error(LoxError::MissingClassName, t.line);
                    return Err(());
                }
            } else {
                None
            };

            self.consume(TokenType::LeftBrace, LoxError::MissingLeftBrace)?;

            let mut methods = Vec::new();
            while !self.check(TokenType::RightBrace) && !self.at_end() {
                methods.push(self.function(LoxError::MissingMethodName)?);
            }

            self.consume(TokenType::RightBrace, LoxError::MissingRightBrace)?;

            Ok(Box::new(ClassStmt::new(t.clone(), superclass, methods)))
        } else {
            self.reporter.error(LoxError::MissingClassName, t.line);
            Err(())
        }
    }

    fn function(&mut self, err: LoxError) -> Result<FunStmt, ()> {
        let prev = self.advance();

        if let TokenType::Identifier(_) = &prev.ty {
            self.consume(TokenType::LeftParen, LoxError::MissingLeftParen)?;

            let mut params = Vec::new();
            if !self.check(TokenType::RightParen) {
                loop {
                    if params.len() >= 255 {
                        self.reporter.error(LoxError::TooManyArguments, self.line());
                        return Err(());
                    }

                    let ident = self.advance();
                    if let TokenType::Identifier(_) = &ident.ty {
                        params.push(ident);
                    } else {
                        self.reporter.error(LoxError::MissingParameterName, self.line());
                        return Err(());
                    }

                    if !self.matches_one(TokenType::Comma) {
                        break;
                    }
                }
            }
            self.consume(TokenType::RightParen, LoxError::MissingRightParen)?;

            self.consume(TokenType::LeftBrace, LoxError::MissingLeftBrace)?;
            let body = self.block_statement()?;
            Ok(FunStmt::new(prev, params, body))
        } else {
            self.reporter.error(err, prev.line);
            Err(())
        }
    }

    fn function_declaration(&mut self, err: LoxError) -> StmtResult {
        Ok(Box::new(self.function(err)?))
    }

    fn var_declaration(&mut self) -> StmtResult {
        let prev = self.advance();
        
        match &prev.ty {
            TokenType::Identifier(_) => {
                let mut init = None;

                if self.matches_one(TokenType::Equal) {
                    init = Some(self.expression()?);
                }

                self.consume(TokenType::Semicolon, LoxError::MissingSemicolon)?;
                Ok(Box::new(VarStmt::new(prev, init)))
            },
            _ => {
                self.reporter.error(LoxError::MissingVariableName, self.line());
                Err(())
            }
        }
    }

    fn statement(&mut self) -> StmtResult {
        if self.matches_one(TokenType::For) {
            self.for_statement()
        } else if self.matches_one(TokenType::If) {
            self.if_statement()
        } else if self.matches_one(TokenType::While) {
            self.while_statement()
        } else if self.matches_one(TokenType::Return) {
            self.return_statement()
        } else if self.matches_one(TokenType::Print) {
            self.print_statement()
        } else if self.matches_one(TokenType::LeftBrace) {
            self.block_statement()
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> StmtResult {
        let keyword = self.previous();
        let mut value = None;

        if !self.check(TokenType::Semicolon) {
            value = Some(self.expression()?);
        }

        self.consume(TokenType::Semicolon, LoxError::MissingSemicolon)?;

        Ok(Box::new(ReturnStmt::new(keyword, value)))
    }

    fn for_statement(&mut self) -> StmtResult {
        self.consume(TokenType::LeftParen, LoxError::MissingLeftParen)?;
        let init = if self.matches_one(TokenType::Semicolon) {
            None
        } else if !self.matches_one(TokenType::Semicolon) && self.matches_one(TokenType::Var) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let mut condition = None;
        if !self.check(TokenType::Semicolon) {
            condition = Some(self.expression()?);
        }
        self.consume(TokenType::Semicolon, LoxError::MissingSemicolon)?;

        let mut inc = None;
        if !self.check(TokenType::RightParen) {
            inc = Some(self.expression()?);
        }
        self.consume(TokenType::RightParen, LoxError::MissingRightParen)?;

        let mut body = self.statement()?;

        if let Some(inc) = inc {
            body = Box::new(BlockStmt::new(vec![ body, Box::new(ExprStmt::new(inc)) ]))
        }

        let cond = if let Some(cond) = condition {
            cond
        } else {
            LiteralExpr::new(Literal::Bool(true)).into()
        };
        body = Box::new(WhileStmt::new(cond, body));

        if let Some(init) = init {
            body = Box::new(BlockStmt::new(vec![ init, body ]));
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> StmtResult {
        self.consume(TokenType::LeftParen, LoxError::MissingLeftParen)?;
        let cond = self.expression()?;
        self.consume(TokenType::RightParen, LoxError::MissingRightParen)?;
        let body = self.statement()?;
        Ok(Box::new(WhileStmt::new(cond, body)))
    }

    fn if_statement(&mut self) -> StmtResult {
        self.consume(TokenType::LeftParen, LoxError::MissingLeftParen)?;
        let cond = self.expression()?;
        self.consume(TokenType::RightParen, LoxError::MissingRightParen)?;

        let then_branch = self.statement()?;
        let mut else_branch = None;

        if self.matches_one(TokenType::Else) {
            else_branch = Some(self.statement()?);
        }

        Ok(Box::new(IfStmt::new(cond, then_branch, else_branch)))
    }

    fn block_statement(&mut self) -> StmtResult {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, LoxError::MissingRightBrace)?;

        Ok(Box::new(BlockStmt::new(statements)))
    }

    fn print_statement(&mut self) -> StmtResult {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, LoxError::MissingSemicolon)?;
        Ok(Box::new(PrintStmt::new(value)))
    }

    fn expression_statement(&mut self) -> StmtResult {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, LoxError::MissingSemicolon)?;
        Ok(Box::new(ExprStmt::new(expr)))
    }

    fn expression(&mut self) -> ExprResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ExprResult {
        let expr = self.or()?;

        if self.matches_one(TokenType::Equal) {
            let _ = self.previous();
            let v = self.assignment()?;

            match expr {
                OwnedExpr::VarExpr(e) => Ok(AssignExpr::new(e.name, v).into()),
                OwnedExpr::GetExpr(e) => Ok(SetExpr::new(e.object, e.name, v).into()),
                _ => Err(()),
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> ExprResult {
        let mut expr = self.and()?;

        while self.matches_one(TokenType::Or) {
            let operator = self.previous();
            let right = self.and()?;
            expr = LogicalExpr::new(expr, operator, right).into();
        }

        Ok(expr)
    }

    fn and(&mut self) -> ExprResult {
        let mut expr = self.equality()?;

        while self.matches_one(TokenType::And) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = LogicalExpr::new(expr, operator, right).into();
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ExprResult {
        use TokenType::{BangEqual, EqualEqual};
        let mut expr = self.comparison()?;

        while self.matches(vec![BangEqual, EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = BinaryExpr::new(expr, operator, right).into();
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ExprResult {
        use TokenType::{Greater, GreaterEqual, Less, LessEqual};
        let mut expr = self.term()?;

        while self.matches(vec![Greater, GreaterEqual, Less, LessEqual]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = BinaryExpr::new(expr, operator, right).into();
        }

        Ok(expr)
    }

    fn term(&mut self) -> ExprResult {
        use TokenType::{Minus, Plus};
        let mut expr = self.factor()?;

        while self.matches(vec![Minus, Plus]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = BinaryExpr::new(expr, operator, right).into();
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ExprResult {
        use TokenType::{Slash, Star};
        let mut expr = self.unary()?;

        while self.matches(vec![Slash, Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = BinaryExpr::new(expr, operator, right).into();
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ExprResult {
        use TokenType::{Bang, Minus};

        if self.matches(vec![Bang, Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            Ok(UnaryExpr::new(operator, right).into())
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ExprResult {
        let mut expr = self.primary()?;

        loop {
            if self.matches_one(TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.matches_one(TokenType::Dot) {
                let name = self.advance();
                if let TokenType::Identifier(_) = &name.ty {
                    expr = GetExpr::new(expr, name).into();
                } else {
                    self.reporter.error(LoxError::MissingVariableName, name.line);
                    return Err(());
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: OwnedExpr) -> ExprResult {
        let mut args = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                if args.len() >= 255 {
                    self.reporter.error(LoxError::TooManyArguments, self.line());
                    return Err(());
                }
                args.push(self.expression()?);

                if !self.matches_one(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, LoxError::MissingRightParen)?;
        let paren = self.previous();

        Ok(CallExpr::new(callee, paren, args).into())
    }

    fn primary(&mut self) -> ExprResult {
        if self.matches_one(TokenType::False) {
            Ok(LiteralExpr::new(Literal::Bool(false)).into())
        } else if self.matches_one(TokenType::True) {
            Ok(LiteralExpr::new(Literal::Bool(true)).into())
        } else if self.matches_one(TokenType::Nil) {
            Ok(LiteralExpr::new(Literal::Nil).into())
        } else if self.matches_one(TokenType::Super) {
            let keyword = self.previous();
            self.consume(TokenType::Dot, LoxError::MissingDot)?;
            let token = self.advance();
            if let TokenType::Identifier(_) = &token.ty {
                Ok(SuperExpr::new(keyword, token).into())
            } else {
                self.reporter.error(LoxError::MissingMethodName, keyword.line);
                Err(())
            }
        } else if self.matches_one(TokenType::This) {
            Ok(ThisExpr::new(self.previous()).into())
        } else {
            let prev = self.advance();
            match prev.ty {
                TokenType::Number(n) => Ok(LiteralExpr::new(Literal::Number(n)).into()),
                TokenType::String(s) => Ok(LiteralExpr::new(Literal::String(s)).into()),
                TokenType::Identifier(_) => Ok(VarExpr::new(prev).into()),
                TokenType::LeftParen => {
                    let expr = self.expression()?;
                    self.consume(TokenType::RightParen, LoxError::MissingRightParen)?;
                    Ok(GroupingExpr::new(expr).into())
                }
                _ => panic!("Internal error :("),
            }
        }
    }

    fn consume(&mut self, ty: TokenType, err: LoxError) -> Result<(), ()> {
        if self.matches_one(ty) {
            Ok(())
        } else {
            self.reporter.error(err, self.line());
            Err(())
        }
    }

    fn matches_one(&mut self, ty: TokenType) -> bool {
        if self.check(ty) {
            self.advance();
            return true;
        }

        return false;
    }

    fn matches(&mut self, mut tys: Vec<TokenType>) -> bool {
        for ty in tys.drain(..) {
            if self.matches_one(ty) {
                return true;
            }
        }

        return false;
    }

    fn check(&self, ty: TokenType) -> bool {
        if self.at_end() {
            false
        } else {
            self.peek().ty == ty
        }
    }

    fn advance(&mut self) -> Token {
        if !self.at_end() {
            self.current += 1;
        }

        self.previous().clone()
    }

    fn at_end(&self) -> bool {
        self.peek().ty == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn line(&self) -> usize {
        self.previous().line
    }
}
