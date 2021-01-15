use crate::token::TokenType;
use crate::value::Value;

#[derive(Debug)]
pub enum LoxError {
    UnterminatedString,
    MissingSemicolon,
    BadOperands {
        left: Value,
        operator: TokenType,
        right: Value,
    },
    BadOperand {
        operator: TokenType,
        right: Value,
    },
    UnexpectedChar(char),
    MissingRightParen,
    MissingVariableName,
    MissingRightBrace,
    MissingLeftParen,
    TooManyArguments,
    WrongNumberArguments,
    NotCallable,
    MissingFunctionName,
    MissingParameterName,
    MissingLeftBrace,
    UndefinedVariable(String),
    Redeclaration(String),
    ReferenceInInitializer,
    BadReturn,
    MissingClassName,
    MissingMethodName,
    BadGet,
    UndefinedProperty,
    BadThis,
    InheritingSelf,
    BadSuperclass,
    MissingDot,
}
