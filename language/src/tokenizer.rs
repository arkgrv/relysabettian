use std::borrow::{BorrowMut, Borrow};

use super::token::{Token, TokenType};

/// Tokenizer:
///
/// separates input source code into defined
/// language tokens.
pub struct Tokenizer {
    pub source: String,
    pub start: usize,
    pub current: usize,
    pub line: i32,
}

/// Implementation for Tokenizer
impl Tokenizer {
    pub fn new(source: String) -> Tokenizer {
        Tokenizer {
            source: source.clone(),
            start: 0usize,
            current: 0usize,
            line: 1i32,
        }
    }
    
    /// Scan a token
    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        if c.is_numeric() {
            return self.number();
        }
        if c.is_alphabetic() {
            return self.identifier();
        }

        match c {
            '(' => {
                return self.make_token(TokenType::OpenParen);
            },
            ')' => {
                return self.make_token(TokenType::CloseParen);
            },
            '{' => {
                return self.make_token(TokenType::OpenCurly);
            },
            '}' => {
                return self.make_token(TokenType::CloseCurly);
            },
            ';' => {
                return self.make_token(TokenType::Semicolon);
            },
            ',' => {
                return self.make_token(TokenType::Comma);
            },
            '.' => {
                return self.make_token(TokenType::Dot);
            },
            '-' => {
                return self.make_token(TokenType::Minus);
            },
            '+' => {
                return self.make_token(TokenType::Plus);
            },
            '/' => {
                return self.make_token(TokenType::Slash);
            },
            '*' => {
                return self.make_token(TokenType::Star);
            },
            '^' => {
                return self.make_token(TokenType::BwXor);
            },
            '&' => {
                let t_type = if self.match_char('&') { TokenType::And } else { TokenType::BwAnd };
                return self.make_token(t_type);
            },
            '|' => {
                let t_type = if self.match_char('|') { TokenType::Or } else { TokenType::BwOr };
                return self.make_token(t_type);
            },
            '!' => {
                let t_type = if self.match_char('=') { TokenType::ExclEqual } else { TokenType::Equal };
                return self.make_token(t_type);
            },
            '=' => {
                let t_type = if self.match_char('=') { TokenType::EqualEqual } else { TokenType::Equal };
                return self.make_token(t_type);
            },
            '<' => {
                let t_type = if self.match_char('=') { TokenType::LessEqual } else { TokenType::Less };
                return self.make_token(t_type);
            },
            '>' => {
                let t_type = if self.match_char('=') { TokenType::GreaterEqual } else { TokenType::Greater };
                return self.make_token(t_type);
            },
            '"' => {
                return self.string('"');
            },
            '\'' => {
                return self.string('\'');
            },
            _ => {
                return self.error_token("Unidentified character!".to_string());
            }
        }
    }

    /// Check if tokenizer reached end of input
    fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    /// Advances tokenizer to next char
    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    /// Reads current char
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source.chars().nth(self.current).unwrap()
    }

    /// Reads next char
    fn peek_next(&self) -> char {
        if (self.current + 1) >= self.source.len() {
            return '\0';
        }

        self.source.chars().nth(self.current + 1).unwrap()
    }

    /// Match char and advance tokenizer
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() { return false; }
        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;
        true
    }
   
    /// Create a new token with specified type
    fn make_token(&self, t_type: TokenType) -> Token {
        let text: String = self.source.chars()
                    .skip(self.current)
                    .take(self.current - self.start)
                    .collect();

        Token {
            t_type: t_type,
            text: text.clone(),
            line: self.line
        }
    }

    /// Creates a new error
    fn error_token(&self, message: String) -> Token {
        Token {
            t_type: TokenType::Error,
            text: message.clone(),
            line: self.line,
        }
    }

    /// Skip whitespaces in input source
    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();

            match c {
                ' ' | '\r' | '\t' => { 
                    _ = self.advance()
                },
                '\n' => {
                    self.line += 1;
                    self.advance();
                },
                '/' => {
                    if self.peek_next() != '/' {
                        return;
                    }
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                },
                _ => return,
            }
        }
    }

    /// Check keywork and return token type
    fn check_keyword(&self, pos: usize, len: usize, rest: String, t_type: TokenType) -> TokenType {
        let part: String = self.source.chars()
                            .skip(self.start + pos)
                            .take(len)
                            .collect();

        if (self.current - self.start) == (pos + len) && part == rest {
            return t_type;
        }

        TokenType::Identifier
    }
    
    /// Check type of identifier
    fn identifier_type(&mut self) -> TokenType {
        let start = self.source.chars().nth(self.start).unwrap();

        match start {
            'c' => {
                return self.check_keyword(1, 4, "lass".to_string(), TokenType::Class);
            },
            'e' => {
                return self.check_keyword(1, 3, "lse".to_string(), TokenType::Else);
            },
            'f' => {
                if (self.current - self.start) > 1 {
                    let curr = self.source.chars().nth(self.start + 1).unwrap();
                    match curr {
                        'a' => {
                            return self.check_keyword(2, 3, "lse".to_string(), TokenType::False);
                        },
                        'o' => {
                            return self.check_keyword(2, 1, "r".to_string(), TokenType::For);
                        },
                        'u' => {
                            return self.check_keyword(2, 2, "nc".to_string(), TokenType::Func);
                        },
                        _ => return TokenType::Identifier,
                    }
                }
                return TokenType::Identifier;
            },
            'i' => {
                return self.check_keyword(1, 1, "f".to_string(), TokenType::If);
            },
            'n' => {
                return self.check_keyword(1, 3, "null".to_string(), TokenType::Null);
            },
            'p' => {
                return self.check_keyword(1, 4, "rint".to_string(), TokenType::Print);
            },
            'r' => {
                return self.check_keyword(1, 5, "eturn".to_string(), TokenType::Return);
            },
            's' => {
                return self.check_keyword(1, 4, "uper".to_string(), TokenType::Super);
            },
            't' => {
                if (self.current - self.start) > 1 {
                    let curr = self.source.chars().nth(self.start + 1).unwrap();
                    match curr {
                        'h' => {
                            return self.check_keyword(2, 2, "is".to_string(), TokenType::This);
                        },
                        'r' => {
                            return self.check_keyword(2, 2, "ue".to_string(), TokenType::True);
                        },
                        _ => return TokenType::Identifier,
                    }
                }
                return TokenType::Identifier;
            },
            'v' => {
                return self.check_keyword(1, 2, "ar".to_string(), TokenType::Var);
            },
            'w' => {
                return self.check_keyword(1, 4, "hile".to_string(), TokenType::While);
            },
            _ => return TokenType::Identifier,
        }
    }

    /// Marks and creates an identifier
    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let mut ident = self.identifier_type();
        self.make_token(ident)
    }

    /// Marks and creates a number
    fn number(&mut self) -> Token {
        while self.peek().is_numeric() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_numeric() {
            self.advance();
            while (self.peek().is_numeric()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    /// Marks and creates a string
    fn string(&mut self, open_char: char) -> Token {
        while self.peek() != open_char && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string!".to_string());
        }

        self.advance();
        self.make_token(TokenType::String)
    }
}
