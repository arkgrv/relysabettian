pub mod token;
pub mod tokenizer;
pub mod common;

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

    use super::*;

    #[test]
    fn is_version_ok() {
        const VERSION: &str = "1.0.0a";
        assert_eq!(VERSION, common::VERSION);
    }

    #[test]
    fn is_version_full_ok() {
        const VERSION_FULL: &str = "Elysabettian 1.0.0a Maurizio (Rust JIT)";
        assert_eq!(VERSION_FULL, common::VERSION_FULL);
    }

    #[test]
    fn tokenize_variable() {
        // Input source code:
        const SOURCE: &str = "var x = 5";
        // Tokens vector
        let mut tokens = Vec::<token::Token>::new();
        // Create a new tokenizer with the input source code
        let mut tokenizer = tokenizer::Tokenizer::new(SOURCE.to_string());

        loop {
            let token = tokenizer.scan_token();
            if token.t_type == TokenType::Eof {
                break;
            }

            println!("{:?}", token);

            tokens.push(token);
        }
    }
}
