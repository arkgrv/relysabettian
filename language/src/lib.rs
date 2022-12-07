pub mod token;
pub mod tokenizer;
pub mod common;

#[cfg(test)]
mod tests {
    use super::*;
    use super::token::*;
    use super::tokenizer::*;
    use super::common::*;

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
}
