use std::any::Any;
use std::str::ParseBoolError;

use language::common;
use language::{tokenizer::Tokenizer, token::Token, token::TokenType};
use crate::opcodes::Opcode;
use crate::value::{Function, Class, Value, Chunk};

/// Expresses precedence of different expressions
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Precedence {
    None,
    Assignment, // =
    Or,         // ||
    And,        // &&
    Equality,   // == or !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! - +
    Call,       // . () []
    Primary,
}

/// Describes types of function
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum FunctionType {
    Function,
    Initializer,
    MemberFunction,
    Script,
}

/// Parser function type
type ParseFn = fn(bool) -> ();

/// Describes a parsing rule
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct ParseRule {
    pub prefix: fn(bool) -> (),
    pub infix: fn(bool) -> (),
    pub precedence: Precedence,
}

/// Inner implementation of a local value
#[derive(Clone, PartialEq, Debug)]
struct Local {
    pub false_value: bool,
    pub name: String,
    pub depth: i32,
    pub is_captured: bool,
}

impl Local {
    pub fn new(name: String, depth: i32) -> Local {
        Local {
            false_value: false,
            name: name.clone(),
            depth,
            is_captured: false,
        }
    }
}

/// Inner implementation of Upvalue
#[derive(Copy, Clone, PartialEq, Debug)]
struct Upvalue {
    pub index: u8,
    pub is_local: bool,
}

impl Upvalue {
    pub fn new(index: u8, is_local: bool) -> Upvalue {
        Upvalue {
            index,
            is_local,
        }
    }
}

/// Code generator generates bytecodes from parsed input source
/// code
#[derive(Clone, PartialEq, Debug)]
pub struct CodeGenerator {
    default_function: Function,
    parser: Option<Box<Parser>>,
    ftype: FunctionType,
    function: Function,
    enclosing: Option<Box<CodeGenerator>>,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    scope_depth: i32,
}

impl CodeGenerator {
    pub fn new(parser: Option<Box<Parser>>, ftype: FunctionType, enclosing: Option<Box<CodeGenerator>>) -> CodeGenerator {
        let default_function = Function::new(0, "".to_string());
        let mut gen = CodeGenerator {
            default_function: default_function.clone(),
            parser: parser.clone(),
            ftype,
            function: default_function.clone(),
            enclosing,
            locals: Vec::<Local>::new(),
            upvalues: Vec::<Upvalue>::new(),
            scope_depth: 0i32,
        };

        let first_local_name = if ftype == FunctionType::Function { "".to_string() } else { "this".to_string() };
        let first_local = Function::new(0, first_local_name);

        if ftype != FunctionType::Script && parser.is_some() {
            gen.function.name = parser.unwrap().previous.text;
        }

        gen
    }

    /// Adds a new local value to the virtualized memory environment
    pub fn add_local(&mut self, name: String) {
        if self.locals.len() == common::UINT8_COUNT.into() {
            self.parser.unwrap().error("Too many local variables in function");
            return;
        }
        self.locals.push(Local::new(name, -1));
    }

    /// Declares a new variable
    pub fn declare_variable(&mut self, name: String) {
        if self.scope_depth == 0 {
            return;
        }

        for i in self.locals.len() - 1..0 {
            if self.locals[i].depth != -1 && self.locals[i].depth < self.scope_depth {
                break;
            }
            if self.locals[i].name == name {
                self.parser.unwrap().error("Variable identifier already declared in this scope.");
            }
        }

        self.add_local(name)
    }

    /// Marks a variable (or expression) initialized
    pub fn mark_initialized(&mut self) {
        if self.scope_depth == 0 { return; }
        self.locals.last().unwrap().depth = self.scope_depth
    }

    /// Resolves (finds) a local value
    pub fn resolve_local(&mut self, name: String) -> i32 {
        for i in self.locals.len() - 1..0 {
            if self.locals[i].name == name {
                if self.locals[i].depth == -1 {
                    self.parser.unwrap().error("Cannot read local variable in its own initializer.");
                }

                return i as i32;
            }
        }

        return -1;
    }

    /// Resolves (finds) an upvalue
    pub fn resolve_upvalue(&mut self, name: String) -> i32 {
        if self.enclosing.is_none() { return -1; }
        
        let local = self.enclosing.unwrap().resolve_local(name);
        if local != -1 {
            self.enclosing.unwrap().locals[local as usize].is_captured = true;
            return self.add_upvalue(local as u8, true);
        }

        let upvalue = self.enclosing.unwrap().resolve_upvalue(name);
        if upvalue != -1 {
            return self.add_upvalue(upvalue as u8, false);
        }

        return -1;
    }

    /// Adds an upvalue
    pub fn add_upvalue(&mut self, index: u8, is_local: bool) -> i32 {
        for i in 0..self.upvalues.len() {
            if self.upvalues[i].index == index && self.upvalues[i].is_local == is_local {
                return i as i32;
            }
        }

        if self.upvalues.len() == common::UINT8_COUNT.into() {
            self.parser.unwrap().error("Too many local variables in function.");
            return 0;
        }

        self.upvalues.push(Upvalue::new(index, is_local));
        self.function.upvalue_count = self.upvalues.len();

        (self.function.upvalue_count - 1) as i32
    }

    /// Denotes start of scope, with depth increase
    pub fn begin_scope(&mut self) {
        self.scope_depth += 1
    }

    /// Denotes end of scope, with depth restoration
    pub fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                self.parser.unwrap().emit_op(Opcode::CloseUpvalue);
            } else {
                self.parser.unwrap().emit_op(Opcode::Pop);
            }

            self.locals.pop();
        }
    }

    /// Checks if a value is local
    pub fn is_local(&self) -> bool {
        self.scope_depth > 0
    }
}

/// Compiler for class types
#[derive(Clone, PartialEq, Debug)]
pub struct ClassCompiler {
    enclosing: Option<Box<ClassCompiler>>,
    has_superclass: bool,
}

impl ClassCompiler {
    pub fn new(enclosing: Option<Box<ClassCompiler>>) -> ClassCompiler {
        ClassCompiler {
            enclosing,
            has_superclass: false,
        }
    }
}

/// Parses code into different statements and expressions
#[derive(Clone, PartialEq, Debug)]
pub struct Parser {
    pub previous: Token,
    pub current: Token,
    pub scanner: Tokenizer,
    pub generator: Box<CodeGenerator>,
    pub class_compiler: Option<Box<ClassCompiler>>,

    pub had_error: bool,
    pub panic_mode: bool,
}

impl Parser {
    pub fn new(source: String) -> Parser {
        let mut p = Parser {
            previous: Token::new(TokenType::Eof, "".to_string(), 0i32),
            current: Token::new(TokenType::Eof, "".to_string(), 0i32),
            scanner: Tokenizer::new(source.clone()),
            generator: Box::new(CodeGenerator::new(None, FunctionType::Script, None)),
            class_compiler: None,
            had_error: false,
            panic_mode: false,
        };
        
        p.generator = Box::new(CodeGenerator::new(Some(Box::new(p)), FunctionType::Script, None));
        p
    }

    /// Advances parser checking for errors
    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.scan_token();
            if self.current.t_type == TokenType::Error { break; }

            self.error_at_current(self.current.text);
        }
    }

    /// Consumes a token with specified type
    fn consume(&mut self, t_type: TokenType, message: &str) {
        if self.current.t_type == t_type {
            self.advance();
            return;
        }

        self.error_at_current(message.to_string());
    }

    /// Check (compare) token types
    fn check(&mut self, t_type: TokenType) -> bool {
        self.current.t_type == t_type
    }

    /// Matches token
    fn match_token(&mut self, t_type: TokenType) -> bool {
        if !self.check(t_type) { return false; }
        self.advance();
        true
    }

    /// Emits byte
    fn emit_byte(&mut self, byte: u8) {
        self.current_chunk().write_byte(byte, self.previous.line)
    }

    /// Emit operation (opcode)
    fn emit_op(&mut self, op: Opcode) {
        self.current_chunk().write_opcode(op, self.previous.line)
    }

    /// Emit operation and specifics (opcode + byte)
    fn emit_op_byte(&mut self, op: Opcode, byte: u8) {
        self.emit_op(op);
        self.emit_byte(byte);
    }

    /// Emit two operations
    fn emit_two_op(&mut self, op1: Opcode, op2: Opcode) {
        self.emit_op(op1);
        self.emit_op(op2);
    }

    /// Emit a loop
    fn emit_loop(&mut self, start: i32) {
        self.emit_op(Opcode::Loop);

        let offset: i32 = self.current_chunk().count() as i32 - start + 2;
        if offset > u16::MAX.into() {
            self.error("Loop body too large.");
        }

        self.emit_byte(((offset >> 8u8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8)
    }

    /// Emit a jump
    fn emit_jump(&mut self, op: Opcode) -> i32 {
        self.emit_op(op);
        self.emit_byte(0xff_u8);
        self.emit_byte(0xff_u8);
        (self.current_chunk().count() - 2) as i32
    }

    /// Emit return instruction
    fn emit_return(&mut self) {
        if self.generator.ftype == FunctionType::Initializer {
            self.emit_op_byte(Opcode::GetLocal, 0);
        } else {
            self.emit_op(Opcode::Nop);
        }

        self.emit_op(Opcode::Ret)
    }

    /// Create a new constant
    fn make_constant(&mut self, value: Value) -> u8 {
        let constant = self.current_chunk().add_constant(value);
        if constant > u8::MAX.into() {
            self.error("Too many constant in one chunk.");
            return 0;
        }

        constant as u8
    }

    /// Emit constant
    fn emit_constant(&mut self, value: Value) {
        self.emit_op_byte(Opcode::Constant, self.make_constant(value))
    }

    /// Patch a jump for execution
    fn patch_jump(&mut self, offset: i32) {
        let jump = self.current_chunk().count() - offset - 2;

        if jump > u16::MAX.into() {
            self.error("Too much code to jump. Aborting");
        }

        self.current_chunk().code[offset as usize] = ((jump >> 8) & 0xff) as u8;
        self.current_chunk().code[(offset + 1) as usize] = (jump & 0xff) as u8;
    }

    /// Stops compilation
    fn end_compiler(&mut self) -> Function {
        self.emit_return();
        self.generator.function
    }

    /// Binary expression
    fn binary(&mut self, can_assign: bool) {
        let op_type = self.previous.t_type;
        let rule = self.get_rule(op_type);
    }

    fn call(&mut self, can_assign: bool) {

    }

    fn dot(&mut self, can_assign: bool) {

    }

    fn literal(&mut self, can_assign: bool) {

    }

    fn grouping(&mut self, can_assign: bool) {

    }

    fn number(&mut self, can_assign: bool) {

    }

    fn or(&mut self, can_assign: bool) {

    }

    fn string(&mut self, can_assign: bool) {

    }

    fn named_variable(&mut self, name: String, can_assign: bool) {

    }

    fn variable(&mut self, can_assign: bool) {

    }

    fn super_(&mut self, can_assign: bool) {

    }

    fn this(&mut self, can_assign: bool) {

    }

    fn and(&mut self, can_assign: bool) {

    }

    fn unary(&mut self, can_assign: bool) {

    }

    fn get_rule(&mut self, t_type: TokenType) -> &ParseRule {

    }

    fn identifier_constant(&mut self, name: String) -> i32 {

    }

    fn parse_variable(&mut self, error_message: &str) -> u8 {

    }

    fn define_variable(&mut self, global: u8) {

    }

    fn args_list(&mut self) -> u8 {

    }

    fn expression(&mut self) {

    }

    fn block(&mut self) {

    }

    fn function(&mut self, ftype: FunctionType) {

    }

    fn member_func(&mut self) {

    }

    fn class_declaration(&mut self) {

    }

    fn func_declaration(&mut self) {

    }

    fn var_declaration(&mut self) {

    }

    fn expression_statement(&mut self) {

    }

    fn for_statement(&mut self) {

    }

    fn if_statement(&mut self) {

    }

    fn declaration(&mut self) {

    }

    fn statement(&mut self) {

    }
    
    fn print_statement(&mut self) {

    }

    fn return_statement(&mut self) {

    }

    fn while_statement(&mut self) {

    }

    fn sync(&mut self) {

    }

    fn error_at(&mut self, token: Token, message: String) {

    }

    fn error(&mut self, message: &str) {

    }

    fn error_at_current(&mut self, message: String) {

    }

    /// Returns current memory chunk
    pub fn current_chunk(&self) -> &Chunk {
        &self.generator.function.chunk
    }

}
