use std::{rc::Rc, cell::RefCell, borrow::BorrowMut};

use language::{token::{Token, TokenType}, tokenizer::Tokenizer};

use crate::{instruction::Opcode, compiler::{Compiler, ClassCompiler}, common::FunctionType, value::Function};

/// Implementation of a shared parsing structure
pub struct Parser {
    pub previous: Token,
    pub current: Token,
    pub scanner: Tokenizer,

    pub compiler: Rc<RefCell<Option<Compiler>>>,
    pub class_compiler: Rc<RefCell<Option<ClassCompiler>>>,

    pub had_error: bool,
    pub panic_mode: bool,
}

impl Parser {
    /// Constructs a new Parser
    /// 
    /// Parameters:
    /// * `source`: source code as String
    pub fn new(source: &String) -> Parser {
        let mut parser = Parser {
            previous: Token::new(TokenType::Eof, source.clone(), 0),
            current: Token::new(TokenType::Eof, source.clone(), 0),
            scanner: Tokenizer::new(source.clone()),
            compiler: Rc::new(RefCell::new(None)),
            class_compiler: Rc::new(RefCell::new(None)),
            had_error: false,
            panic_mode: false,
        };

        
        parser.advance();

        parser
    }

    pub fn compile() -> Option<Rc<Function>> {
        panic!();
    }

    pub fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.scan_token();
            if self.current.t_type != TokenType::Error { break };

            self.error_at_current(&self.current.text);
        }
    }

    /// Consumes the token and advances parser
    /// 
    /// Parameters:
    /// * `t_type`: type of token
    /// * `message`: error message printed on failure
    pub fn consume(&mut self, t_type: TokenType, message: &str) {
        if self.current.t_type == t_type {
            self.advance();
            return
        }

        self.error_at_current(message)
    }

    /// Checks the type of current token
    pub fn check(&mut self, t_type: TokenType) -> bool {
        self.current.t_type == t_type
    }

    /// Matches current token and advances parser
    pub fn match_token(&mut self, t_type: TokenType) -> bool {
        if !self.check(t_type) { return false };
        self.advance();
        true
    }

    /// Emits an instruction
    /// 
    /// Parameters:
    /// * `instr`: opcode of this instruction
    pub fn emit_instr(&mut self, instr: Opcode) {
        
    }

    pub fn emit_byte(&mut self, byte: u8) {

    }

    pub fn emit_instr_data(&mut self, instr: Opcode, data: u8) {

    }

    pub fn emit_two_instr(&mut self, instr1: Opcode, instr2: Opcode) {

    }

    pub fn emit_loop(&mut self, loop_start: usize) {

    }

    pub fn emit_jump(&mut self, instr: Opcode) -> usize {

    }

    pub fn error(&mut self, message: &str) {

    }

    pub fn error_at_current(&mut self, message: &str) {

    }

    pub fn current_chunk(&mut self) -> Rc<RefCell<Chunk>> {
        
    }
}