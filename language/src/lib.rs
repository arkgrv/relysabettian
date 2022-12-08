pub mod token;
pub mod tokenizer;
pub mod common;

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

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
        // Input source code:
        const SOURCE: &str = "var x = 5;";
        // Number of tokens to be extracted
        const MAX_TOKENS: usize = 5usize;
        // Tokens vector
        let mut tokens = Vec::<token::Token>::new();
        // Create a new tokenizer with the input source code
        let mut tokenizer = tokenizer::Tokenizer::new(SOURCE.to_string());

        loop {
            let token = tokenizer.scan_token();
            if token.t_type == TokenType::Eof {
                break;
            }
            tokens.push(token);
        }

        // Check if length is 5
        assert_eq!(tokens.len(), MAX_TOKENS);

        // Tokens to compare
        let first_tok = token::Token {
            t_type: TokenType::Var,
            text: "var".to_string(),
            line: 1i32,
        };
        let second_tok = token::Token {
            t_type: TokenType::Identifier,
            text: "x".to_string(),
            line: 1i32,
        };
        let third_tok = token::Token {
            t_type: TokenType::Equal,
            text: "=".to_string(),
            line: 1i32,
        };
        let fourth_tok = token::Token {
            t_type: TokenType::Number,
            text: "5".to_string(),
            line: 1i32,
        };
        let fifth_tok = token::Token {
            t_type: TokenType::Semicolon,
            text: ";".to_string(),
            line: 1i32,
        };

        assert_eq!(first_tok, tokens[0]);
        assert_eq!(second_tok, tokens[1]);
        assert_eq!(third_tok, tokens[2]);
        assert_eq!(fourth_tok, tokens[3]);
        assert_eq!(fifth_tok, tokens[4]);
    }
}
