use crate::token::{Token, TokenType};
use crate::{error_reporter::ErrorReporter, lox_error::LoxError};
use std::str::Chars;

pub struct Scanner<'a> {
    source: String,
    tokens: Vec<Token>,
    line: usize,
    reporter: &'a mut ErrorReporter,
}

impl<'a> Scanner<'a> {
    pub fn new(source: String, reporter: &'a mut ErrorReporter) -> Self {
        Scanner {
            source,
            tokens: Vec::default(),
            line: 1,
            reporter,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        let mut iter = self.source.chars();
        let mut token = Scanner::scan_token(&mut iter, &mut self.line, self.reporter);
        while token.is_some() {
            let t = token.unwrap();
            if t.ty != TokenType::Ignore {
                self.tokens.push(t);
            }
            token = Scanner::scan_token(&mut iter, &mut self.line, self.reporter);
        }

        self.tokens.push(Token {
            ty: TokenType::Eof,
            line: self.line,
        });
        self.tokens.clone()
    }

    fn advance<'c>(iter: &mut Chars<'c>) -> Option<char> {
        iter.next()
    }

    fn advance_if<'c>(iter: &mut Chars<'c>, c: char) -> Option<char> {
        match iter.clone().peekable().peek() {
            Some(p) => {
                if *p == c {
                    iter.next()
                } else {
                    None
                }
            }
            None => None,
        }
    }

    fn double_peek<'c>(iter: &mut Chars<'c>) -> Option<char> {
        let mut c = iter.clone();
        let _ = c.next();
        c.next()
    }

    fn scan_token<'c>(
        iter: &mut Chars<'c>,
        line: &mut usize,
        reporter: &'a mut ErrorReporter,
    ) -> Option<Token> {
        let c = match Scanner::advance(iter) {
            Some(c) => c,
            None => return None,
        };

        let ty = match c {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            '!' => match Scanner::advance_if(iter, '=') {
                Some(_) => TokenType::BangEqual,
                None => TokenType::Bang,
            },
            '=' => match Scanner::advance_if(iter, '=') {
                Some(_) => TokenType::EqualEqual,
                None => TokenType::Equal,
            },
            '<' => match Scanner::advance_if(iter, '=') {
                Some(_) => TokenType::LessEqual,
                None => TokenType::Less,
            },
            '>' => match Scanner::advance_if(iter, '=') {
                Some(_) => TokenType::GreaterEqual,
                None => TokenType::Greater,
            },
            '/' => match Scanner::advance_if(iter, '/') {
                Some(_) => {
                    while match iter.clone().peekable().peek() {
                        Some('\n') | None => false,
                        _ => true,
                    } {
                        let _ = iter.next();
                    }

                    TokenType::Ignore
                }
                None => TokenType::Slash,
            },
            ' ' | '\r' | '\t' => TokenType::Ignore,
            '\n' => {
                *line += 1;
                TokenType::Ignore
            }
            '"' => {
                let mut lit = String::default();
                let mut good = true;
                loop {
                    match iter.next() {
                        Some('"') => break,
                        None => {
                            good = false;
                            break;
                        }
                        Some(c) => {
                            if c == '\n' {
                                *line += 1;
                            }
                            lit.push(c);
                        }
                    }
                }

                if good {
                    TokenType::String(lit)
                } else {
                    reporter.error(LoxError::UnterminatedString, *line);
                    TokenType::Ignore
                }
            }
            '0'..='9' => {
                let mut lit = c.to_string();

                while let Some('0'..='9') = iter.clone().peekable().peek() {
                    lit.push(iter.next().unwrap());
                }

                if let Some('.') = iter.clone().peekable().peek() {
                    if let Some('0'..='9') = Scanner::double_peek(iter) {
                        lit.push(iter.next().unwrap());

                        while let Some('0'..='9') = iter.clone().peekable().peek() {
                            lit.push(iter.next().unwrap());
                        }
                    }
                }

                let num = lit.parse::<f64>().unwrap();

                TokenType::Number(num)
            }
            'a'..='z' | 'A'..='Z' => {
                let mut lit = c.to_string();

                while let Some('0'..='9') | Some('a'..='z') | Some('A'..='Z') | Some('_') =
                    iter.clone().peekable().peek()
                {
                    lit.push(iter.next().unwrap());
                }

                match lit.as_ref() {
                    "and" => TokenType::And,
                    "class" => TokenType::Class,
                    "else" => TokenType::Else,
                    "false" => TokenType::False,
                    "for" => TokenType::For,
                    "fun" => TokenType::Fun,
                    "if" => TokenType::If,
                    "nil" => TokenType::Nil,
                    "or" => TokenType::Or,
                    "print" => TokenType::Print,
                    "return" => TokenType::Return,
                    "super" => TokenType::Super,
                    "this" => TokenType::This,
                    "true" => TokenType::True,
                    "var" => TokenType::Var,
                    "while" => TokenType::While,
                    _ => TokenType::Identifier(lit),
                }
            },
            c => {
                reporter.error(LoxError::UnexpectedChar(c), *line);
                TokenType::Ignore
            },
        };

        Some(Token { ty, line: *line })
    }
}
