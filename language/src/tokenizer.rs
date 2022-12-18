use super::token::{Token, TokenType};

/// Tokenizer:
///
/// separates input source code into defined
/// language tokens.
#[derive(Clone, Debug, PartialEq)]
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
            '(' => self.make_token(TokenType::OpenParen),
            ')' => self.make_token(TokenType::CloseParen),
            '{' => self.make_token(TokenType::OpenCurly),
            '}' => self.make_token(TokenType::CloseCurly),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '^' => self.make_token(TokenType::Caret),
            '~' => self.make_token(TokenType::Tilde),
            '&' => {
                let t_type = if self.match_char('&')
                                        { TokenType::AmpersandAmpersand } 
                                        else { TokenType::Ampersand };
                self.make_token(t_type)
            },
            '|' => {
                let t_type = if self.match_char('|')
                                        { TokenType::PipePipe }
                                        else { TokenType::Pipe };
                self.make_token(t_type)
            },
            '!' => {
                let t_type = if self.match_char('=') { TokenType::ExclEqual } else { TokenType::Equal };
                self.make_token(t_type)
            },
            '=' => {
                let t_type = if self.match_char('=') { TokenType::EqualEqual } else { TokenType::Equal };
                self.make_token(t_type)
            },
            '<' => {
                let t_type = if self.match_char('=') { TokenType::LessEqual } else { TokenType::Less };
                self.make_token(t_type)
            },
            '>' => {
                let t_type = if self.match_char('=') { TokenType::GreaterEqual } else { TokenType::Greater };
                self.make_token(t_type)
            },
            '"' => self.string('"'),
            '\'' => self.string('\''),
            _ => self.error_token("Unidentified character!".to_string())
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
                    .skip(self.start)
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
            'c' => self.check_keyword(1, 4, "lass".to_string(), TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse".to_string(), TokenType::Else),
            'f' => {
                if (self.current - self.start) > 1 {
                    let curr = self.source.chars().nth(self.start + 1).unwrap();
                    match curr {
                        'a' => return self.check_keyword(2, 3, "lse".to_string(), TokenType::False),
                        'o' => return self.check_keyword(2, 1, "r".to_string(), TokenType::For),
                        'u' => return self.check_keyword(2, 2, "nc".to_string(), TokenType::Func),
                        _ => return TokenType::Identifier,
                    }
                }
                return TokenType::Identifier;
            },
            'i' => self.check_keyword(1, 1, "f".to_string(), TokenType::If),
            'n' => self.check_keyword(1, 3, "null".to_string(), TokenType::Null),
            'p' => self.check_keyword(1, 4, "rint".to_string(), TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn".to_string(), TokenType::Return),
            's' => self.check_keyword(1, 4, "uper".to_string(), TokenType::Super),
            't' => {
                if (self.current - self.start) > 1 {
                    let curr = self.source.chars().nth(self.start + 1).unwrap();
                    match curr {
                        'h' => return self.check_keyword(2, 2, "is".to_string(), TokenType::This),
                        'r' => return self.check_keyword(2, 2, "ue".to_string(), TokenType::True),
                        _ => return TokenType::Identifier,
                    }
                }
                return TokenType::Identifier;
            },
            'v' => self.check_keyword(1, 2, "ar".to_string(), TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile".to_string(), TokenType::While),
            _ => return TokenType::Identifier,
        }
    }

    /// Marks and creates an identifier
    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let ident = self.identifier_type();
        self.make_token(ident)
    }

    /// Marks and creates a number
    fn number(&mut self) -> Token {
        while self.peek().is_numeric() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_numeric() {
            self.advance();
            while self.peek().is_numeric() {
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
