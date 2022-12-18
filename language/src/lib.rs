pub mod token;
pub mod tokenizer;
pub mod common;

#[cfg(test)]
mod tests {
    use crate::token::{TokenType, Token};

    use super::*;

    /// Check if the value of version is the one
    /// expected by this test suite
    #[test]
    fn is_version_ok() {
        const VERSION: &str = "1.0.0a";
        assert_eq!(VERSION, common::VERSION);
    }

    /// Check if the value of version full is the one
    /// expected by this test suite
    #[test]
    fn is_version_full_ok() {
        const VERSION_FULL: &str = "Elysabettian 1.0.0a Maurizio (Rust JIT)";
        assert_eq!(VERSION_FULL, common::VERSION_FULL);
    }

    /// Check if the number and type of tokens is
    /// consistent with the provided source code input
    #[test]
    fn tokenize_variable() {
        const SOURCE: &str = "var x = 5;";
        const MAX_TOKENS: usize = 5usize;
        let mut tokens = Vec::<token::Token>::new();
        let mut tokenizer = tokenizer::Tokenizer::new(SOURCE.to_string());

        loop {
            let token = tokenizer.scan_token();
            if token.t_type == TokenType::Eof {
                break;
            }
            tokens.push(token);
        }

        assert_eq!(tokens.len(), MAX_TOKENS);

        let expected = vec![
            token::Token::new(TokenType::Var, "var".to_string(), 1i32),
            token::Token::new(TokenType::Identifier, "x".to_string(), 1i32),
            token::Token::new(TokenType::Equal, "=".to_string(), 1i32),
            token::Token::new(TokenType::Number, "5".to_string(), 1i32),
            token::Token::new(TokenType::Semicolon, ";".to_string(), 1i32)
        ];

        assert_eq!(tokens, expected);
    }

    /// Check if the function is tokenized correctly
    #[test]
    fn tokenize_function() {
        const SOURCE: &str = "func ident(x) { return x; }";
        const MAX_TOKENS: usize = 10usize;
        let mut tokens = Vec::<token::Token>::new();
        let mut tokenizer = tokenizer::Tokenizer::new(SOURCE.to_string());
        loop {
            let token = tokenizer.scan_token();
            if token.t_type == TokenType::Eof {
                break;
            }
            tokens.push(token);
        }

        assert_eq!(tokens.len(), MAX_TOKENS);

        let expected = vec![
            token::Token::new(TokenType::Func, "func".to_string(), 1i32),
            token::Token::new(TokenType::Identifier, "ident".to_string(), 1i32),
            token::Token::new(TokenType::OpenParen, "(".to_string(), 1i32),
            token::Token::new(TokenType::Identifier, "x".to_string(), 1i32),
            token::Token::new(TokenType::CloseParen, ")".to_string(), 1i32),
            token::Token::new(TokenType::OpenCurly, "{".to_string(), 1i32),
            token::Token::new(TokenType::Return, "return".to_string(), 1i32),
            token::Token::new(TokenType::Identifier, "x".to_string(), 1i32),
            token::Token::new(TokenType::Semicolon, ";".to_string(), 1i32),
            token::Token::new(TokenType::CloseCurly, "}".to_string(), 1i32)
        ];

        assert_eq!(tokens, expected);
    }

    /// Check if the for loop is tokenized correctly
    #[test]
    fn tokenize_for() {
        const SOURCE: &str = "for (var i = 0; i < 4; i = i + 1) { print(1); }";
        const MAX_TOKENS: usize = 24;
        let mut tokens = Vec::<token::Token>::new();
        let mut tokenizer = tokenizer::Tokenizer::new(SOURCE.to_string());

        loop {
            let token = tokenizer.scan_token();
            if token.t_type == TokenType::Eof {
                break;
            }
            tokens.push(token);
        }

        assert_eq!(tokens.len(), MAX_TOKENS);

        let expected = vec![
            token::Token::new(TokenType::For, "for".to_string(), 1i32),
            token::Token::new(TokenType::OpenParen, "(".to_string(), 1i32),
            token::Token::new(TokenType::Var, "var".to_string(), 1i32),
            token::Token::new(TokenType::Identifier, "i".to_string(), 1i32),
            token::Token::new(TokenType::Equal, "=".to_string(), 1i32),
            token::Token::new(TokenType::Number, "0".to_string(), 1i32),
            token::Token::new(TokenType::Semicolon, ";".to_string(), 1i32),
            token::Token::new(TokenType::Identifier, "i".to_string(), 1i32),
            token::Token::new(TokenType::Less, "<".to_string(), 1i32),
            token::Token::new(TokenType::Number, "4".to_string(), 1i32),
            token::Token::new(TokenType::Semicolon, ";".to_string(), 1i32),
            token::Token::new(TokenType::Identifier, "i".to_string(), 1i32),
            token::Token::new(TokenType::Equal, "=".to_string(), 1i32),
            token::Token::new(TokenType::Identifier, "i".to_string(), 1i32),
            token::Token::new(TokenType::Plus, "+".to_string(), 1i32),
            token::Token::new(TokenType::Number, "1".to_string(), 1i32),
            token::Token::new(TokenType::CloseParen, ")".to_string(), 1i32),
            token::Token::new(TokenType::OpenCurly, "{".to_string(), 1i32),
            token::Token::new(TokenType::Print, "print".to_string(), 1i32),
            token::Token::new(TokenType::OpenParen, "(".to_string(), 1i32),
            token::Token::new(TokenType::Number, "1".to_string(), 1i32),
            token::Token::new(TokenType::CloseParen, ")".to_string(), 1i32),
            token::Token::new(TokenType::Semicolon, ";".to_string(), 1i32),
            token::Token::new(TokenType::CloseCurly, "}".to_string(), 1i32)
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_while() {
        const SOURCE: &str = "while (true) { print(\"Hello, World!\"); }";
        const MAX_TOKENS: usize = 11;

        let mut tokens = Vec::<token::Token>::new();
        let mut tokenizer = tokenizer::Tokenizer::new(SOURCE.to_string());

        loop {
            let token = tokenizer.scan_token();
            if token.t_type == TokenType::Eof {
                break;
            }

            tokens.push(token.clone());
        }

        assert_eq!(tokens.len(), MAX_TOKENS);

        let expected = vec![
            token::Token::new(TokenType::While, "while".to_string(), 1i32),
            token::Token::new(TokenType::OpenParen, "(".to_string(), 1i32),
            token::Token::new(TokenType::True, "true".to_string(), 1i32),
            token::Token::new(TokenType::CloseParen, ")".to_string(), 1i32),
            token::Token::new(TokenType::OpenCurly, "{".to_string(), 1i32),
            token::Token::new(TokenType::Print, "print".to_string(), 1i32),
            token::Token::new(TokenType::OpenParen, "(".to_string(), 1i32),
            token::Token::new(TokenType::String, "\"Hello, World!\"".to_string(), 1i32),
            token::Token::new(TokenType::CloseParen, ")".to_string(), 1i32),
            token::Token::new(TokenType::Semicolon, ";".to_string(), 1i32),
            token::Token::new(TokenType::CloseCurly, "}".to_string(), 1i32)
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_boolean_expr() {
        const SOURCE: &str = "var x = true || false;";
        const MAX_TOKENS: usize = 7;

        let mut tokens = Vec::<token::Token>::new();
        let mut tokenizer = tokenizer::Tokenizer::new(SOURCE.to_string());

        loop {
            let token = tokenizer.scan_token();
            if token.t_type == TokenType::Eof {
                break;
            }

            tokens.push(token.clone());
        }

        assert_eq!(tokens.len(), MAX_TOKENS);

        let expected = vec![
            Token::new(TokenType::Var, "var".to_string(), 1i32),
            Token::new(TokenType::Identifier, "x".to_string(), 1i32),
            Token::new(TokenType::Equal, "=".to_string(), 1i32),
            Token::new(TokenType::True, "true".to_string(), 1i32),
            Token::new(TokenType::PipePipe, "||".to_string(), 1i32),
            Token::new(TokenType::False, "false".to_string(), 1i32),
            Token::new(TokenType::Semicolon, ";".to_string(), 1i32),
        ];

        assert_eq!(tokens, expected);
    }
}
