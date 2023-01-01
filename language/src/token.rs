/// Describes a single language token type
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum TokenType {
    // Single character tokens
    OpenParen, CloseParen,
    OpenCurly, CloseCurly,
    Comma, Dot, Minus, Plus,
    Semicolon, Slash, Star,
    Caret, Tilde,

    // Dual (or single) character tokens
    Excl, ExclEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    Ampersand, Pipe,
    AmpersandAmpersand, PipePipe,

    // Identifiers and types
    Identifier, String, Number,

    // Language keywords
    Class, Else, False,
    Func, For, If, Null,
    Print, Return, Super, This,
    True, Var, While,

    // Errors and constants
    Error, Eof,
}

impl Into<u8> for TokenType {
    /// Converts a TokenType into an 8
    fn into(self) -> u8 {
        self as u8
    }
}

impl Into<usize> for TokenType {
    /// Converts a TokenType into a usize
    fn into(self) -> usize {
        self as usize
    }
}

/// Represents a single language token
#[derive(Clone, PartialEq, Debug)]
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
