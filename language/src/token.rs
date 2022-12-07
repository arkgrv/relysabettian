
/// Describes a single language token type
#[derive(Copy, Clone, PartialEq)]
pub enum TokenType {
    // Single character tokens
    OpenParen, CloseParen,
    OpenCurly, CloseCurly,
    Comma, Dot, Minus, Plus,
    Semicolon, Slash, Star,

    // Dual (or single) character tokens
    Excl, ExclEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Identifiers and types
    Identifier, String, Number,

    // Language keywords
    And, Class, Else, False,
    Func, For, If, Null, Or,
    Print, Return, Super, This,
    True, Var, While,

    // Errors and constants
    Error, Eof,

    // Bitwise operations
    BwAnd, BwOr, BwXor, BwNot,

    // Future array syntax
    OpenSquare, CloseSquare,
}

/// Represents a single language token
#[derive(Clone, PartialEq)]
pub struct Token {
    pub t_type: TokenType,
    pub text: String,
    pub line: i32,
}

/// Token implementation
impl Token {
    pub fn new(t_type: TokenType, text: String, line: i32) -> Token {
        Token {
            t_type,
            text: text.clone(),
            line
        }
    }
}
