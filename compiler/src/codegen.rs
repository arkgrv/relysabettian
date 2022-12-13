use language::{token::{Token, TokenType}, tokenizer::Tokenizer};
use crate::{common::{FunctionKind, Local, Upvalue, ParseRule, Precedence}, value::{FuncObj, Chunk, ValueType}, bytecode::Instruction};

pub struct RulesContainer {
    pub rules: Vec<Box<ParseRule>>,
}

impl RulesContainer {
    pub fn new() -> RulesContainer {
        RulesContainer {
            rules: vec![
                // Open Paren
                Box::new(ParseRule::new(Some(Box::new(Parser::grouping)), Some(Box::new(Parser::call)), Precedence::Call)),
                // Close Paren
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Open Curly
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Close Curly
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Comma
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Dot
                Box::new(ParseRule::new(None, Some(Box::new(Parser::dot)), Precedence::Call)),
                // Minus
                Box::new(ParseRule::new(Some(Box::new(Parser::unary)), Some(Box::new(Parser::binary)), Precedence::Term)),
                // Plus
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Term)),
                // Semicolon
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Slash
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Factor)),
                // Star
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Factor)),
                // Excl
                Box::new(ParseRule::new(Some(Box::new(Parser::unary)), None, Precedence::None)),
                // ExclEqual
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Equality)),
                // Equal
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // EqualEqual
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Equality)),
                // Greater
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Comparison)),
                // GreaterEqual
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Comparison)),
                // Less
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Comparison)),
                // LessEqual
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Comparison)),
                // Identifier
                Box::new(ParseRule::new(Some(Box::new(Parser::variable)), None, Precedence::None)),
                // String
                Box::new(ParseRule::new(Some(Box::new(Parser::string)), None, Precedence::None)),
                // Number
                Box::new(ParseRule::new(Some(Box::new(Parser::number)), None, Precedence::None)),
                // And
                Box::new(ParseRule::new(None, Some(Box::new(Parser::and)), Precedence::And)),
                // Class
                Box::new(ParseRule::new(None, None, Precedence::None)),
            ]
        }
    }
}

#[derive(Clone)]
pub struct Compiler {
    parser: Option<Box<Parser>>,
    func_type: FunctionKind,
    function: Box<FuncObj>,
    enclosing: Option<Box<Compiler>>,
    locals: Vec<Box<Local>>,
    upvalues: Vec<Box<Upvalue>>,
    scope_depth: i64,
}

impl Compiler {
    /// Create a new Compiler
    /// ### Arguments
    /// * `parser`: parser used in compilation phase
    /// * `func_type`: kind of compiler / parser function
    /// * `enclosing`: enclosing compiler
    pub fn new(parser: Option<Box<Parser>>, func_type: FunctionKind, enclosing: Option<Box<Compiler>>) -> Compiler {
        let mut comp = Compiler {
            parser,
            func_type: func_type.clone(),
            function: Box::new(FuncObj::new(0, "".to_string())),
            enclosing,
            locals: Vec::<Box<Local>>::new(),
            upvalues: Vec::<Box<Upvalue>>::new(),
            scope_depth: 0_i64,
        };

        let local_name = if func_type == FunctionKind::Function { "" } else { "this" };
        comp.locals.push(Box::new(Local::new(local_name.to_string(), 0)));

        if func_type != FunctionKind::Main {
            comp.function.name = comp.parser
                                    .as_ref()
                                    .unwrap().previous.text.clone();
        }

        comp
    }

    /// Adds a new local value
    /// ### Arguments
    /// * `name`: name of local value
    pub fn add_local(&mut self, name: &String) {
        if self.locals.len() == language::common::UINT8_COUNT.into() {
            self.parser.as_mut().unwrap().error("Too many local variables in function.");
            return;
        }

        self.locals.push(Box::new(Local::new(name.clone(), -1)));
    }

    /// Mark a value as initialized
    pub fn mark_initialized(&mut self) {
        if self.scope_depth == 0 { return; }

        let last = self.locals.len() - 1;
        self.locals[last].depth = self.scope_depth
    }

    /// Resolves (finds) a local value
    /// ### Arguments
    /// * `name`: name of local value to find
    pub fn resolve_local(&mut self, name: &String) -> i64 {
        for i in self.locals.len() - 1..0 {
            if name.eq(&self.locals[i].name) {
                if self.locals[i].depth == -1 {
                    self.parser.as_mut().unwrap().error("Cannot read local variable during initialization.");
                }

                return i as i64;
            }
        }

        -1
    }

    /// Resolves (finds) an upvalue
    /// ### Arguments
    /// * `name`: name of upvalue to find
    pub fn resolve_upvalue(&mut self, name: &String) -> i64 {
        if self.enclosing.is_none() { return -1 };

        let local = self.enclosing.as_mut().unwrap().resolve_local(name);
        if local != -1 {
            self.enclosing.as_mut().unwrap().locals[local as usize].is_captured = true;
            self.add_upvalue(local as u8, true);
        }

        let upvalue = self.enclosing.as_mut().unwrap().resolve_upvalue(name);
        if upvalue != -1 {
            self.add_upvalue(upvalue as u8, false);
        }

        -1
    }

    /// Adds a new upvalue
    /// ### Arguments
    /// * `index`: index of upvalue
    /// * `is_local`: locality of upvalue
    pub fn add_upvalue(&mut self, index: u8, is_local: bool) -> i64 {
        for i in 0..self.upvalues.len() {
            if self.upvalues[i].index == index && self.upvalues[i].is_local == is_local {
                i as i64;
            }
        }

        if self.upvalues.len() == language::common::UINT8_COUNT.into() {
            self.parser.as_mut().unwrap().error("Too many closure variables in this function.");
            0;
        }

        self.upvalues.push(Box::new(Upvalue::new(index, is_local)));
        let upvalue_count = self.upvalues.len();
        self.function.upvalue_count = upvalue_count;

        (upvalue_count - 1) as i64
    }

    /// Begins a new scope
    pub fn begin_scope(&mut self) {
        self.scope_depth += 1
    }

    /// Ends a scope
    pub fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while !self.locals.is_empty() && !self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                self.parser.as_mut().unwrap().emit_instruction(Instruction::CloseUVal);
            } else {
                self.parser.as_mut().unwrap().emit_instruction(Instruction::Pop);
            }
            self.locals.pop();
        }
    }

    /// Checks if current value is local
    pub fn is_local(&self) -> bool {
        self.scope_depth > 0
    }
}

#[derive(Clone)]
pub struct ClassCompiler {
    enclosing: Option<Box<ClassCompiler>>,
    has_superclass: bool,
}

#[derive(Clone)]
pub struct Parser {
    pub previous: Token,
    current: Token,
    scanner: Tokenizer,
    compiler: Option<Box<Compiler>>,
    class_compiler: Option<Box<ClassCompiler>>,
    had_error: bool,
    panic_mode: bool,
}

impl Parser {
    /// Creates a new Parser
    /// ### Arguments
    /// * `source`: source code to parse
    pub fn new(source: String) -> Parser {
        let mut parser = Parser {
            previous: Token::new(TokenType::Eof, source.clone(), 0_i32),
            current: Token::new(TokenType::Eof, source.clone(), 0_i32),
            scanner: Tokenizer::new(source.clone()),
            compiler: None,
            class_compiler: None,
            had_error: false,
            panic_mode: false,
        };

        parser.compiler = Some(Box::new(Compiler::new(Some(Box::new(parser.clone())), FunctionKind::Main, None)));
        parser
    }

    /// Advances the parser
    fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            self.current = self.scanner.scan_token();
            if self.current.t_type == TokenType::Error { break };
            
            let message = self.current.text.clone();
            self.error_at_current(&message);
        }
    }

    /// Consumes current token
    /// ### Arguments
    /// * `t_type`: current token type
    /// * `message`: error message if consuming fails
    fn consume(&mut self, t_type: TokenType, message: &str) {
        if self.current.t_type == t_type {
            self.advance();
            return
        }

        self.error_at_current(message)
    }

    /// Checks if the token has the same type as the current one
    /// ### Arguments
    /// * `t_type`: type of token to check
    fn check_ttype(&self, t_type: TokenType) -> bool {
        self.current.t_type == t_type
    }

    /// Matches current token and advances parser
    /// ### Arguments
    /// * `t_type`: type of token to match
    fn match_ttype(&mut self, t_type: TokenType) -> bool {
        if !self.check_ttype(t_type) { false; }
        self.advance();
        true
    }

    /// Emits a new byte in the instruction memory
    /// ### Arguments
    /// * `byte`: value of byte
    pub fn emit_byte(&mut self, byte: u8) {
        let line = self.previous.line;
        self.current_chunk().write_byte(byte, line)
    }

    /// Emits a new instruction in the instruction memory
    /// ### Arguments
    /// * `instruction`: instruction to write
    pub fn emit_instruction(&mut self, instruction: Instruction) {
        let line = self.previous.line;
        self.current_chunk().write_instr(instruction, line)
    }

    /// Emits new instruction with data in the instruction memory
    /// ### Arguments
    /// * `instruction`: instruction to write
    /// * `byte`: instruction data to write
    pub fn emit_instruction_data(&mut self, instruction: Instruction, byte: u8) {
        self.emit_instruction(instruction);
        self.emit_byte(byte)
    }

    /// Emits two new instructions
    /// ### Arguments
    /// * `instr1`: first instruction to emit
    /// * `instr2`: second instruction to emit
    pub fn emit_two_instructions(&mut self, instr1: Instruction, instr2: Instruction) {
        self.emit_instruction(instr1);
        self.emit_instruction(instr2)
    }

    /// Emits a new loop instruction with relative start
    /// ### Arguments
    /// * `loop_start`: start of the loop
    pub fn emit_loop(&mut self, loop_start: usize) {
        self.emit_instruction(Instruction::Loop);

        let offset = self.current_chunk().count() - loop_start + 2;
        if offset > u16::MAX.into() {
            self.error("Loop body too large.")
        }

        let first_byte = ((offset >> 8) & 0xff) as u8;
        let second_byte = (offset &0xff) as u8;
        self.emit_byte(first_byte);
        self.emit_byte(second_byte)
    }

    /// Emits a new jump instruction
    /// ### Arguments
    /// * `instr`: jump instruction
    pub fn emit_jump(&mut self, instr: Instruction) -> usize {
        self.emit_instruction(instr);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.current_chunk().count() - 2
    }

    /// Emits a return instruction
    pub fn emit_return(&mut self) {
        if self.compiler.as_mut().unwrap().func_type == FunctionKind::Initializer {
            self.emit_instruction_data(Instruction::GetLoc, 0x00)
        } else {
            self.emit_instruction(Instruction::Nop)
        }
        self.emit_instruction(Instruction::Ret)
    }

    /// Emits a new constant
    /// ### Arguments
    /// * `value`: value of constant to emit
    pub fn emit_constant(&mut self, value: Box<ValueType>) {
        let constant = self.make_constant(value);
        self.emit_instruction_data(Instruction::Const, constant)
    }

    /// Makes and adds a new constant to the memory
    /// ### Arguments
    /// * `value`: value of new constant
    fn make_constant(&mut self, value: Box<ValueType>) -> u8 {
        let constant = self.current_chunk().add_constant(value);
        if constant > u8::MAX.into() {
            self.error("Too many constants in one chunk.");
            return 0
        }

        constant as u8
    }

    /// Patches a jump
    /// ### Arguments
    /// * `offset`: offset of jump data
    fn patch_jump(&mut self, offset: usize) {
        let jump = self.current_chunk().count() - offset - 2;

        if jump > u16::MAX.into() {
            self.error("Too much code to jump over.");
        }

        let first_jump = ((jump >> 8) & 0xff) as u8;
        let second_jump = (jump & 0xff) as u8;
        self.current_chunk().set_code(offset, first_jump);
        self.current_chunk().set_code(offset + 1, second_jump);
    }

    /// Returns a reference to the current data chunk
    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.compiler.as_mut().unwrap().function.chunk
    }

    /// Ends the current compiler returning a function
    pub fn end_compiler(&mut self) -> Box<FuncObj> {
        self.emit_return();

        let function = &self.compiler.as_ref().unwrap().function;
        function.clone()
    }

    fn grouping(parser: &mut Parser, _: bool) {

    }

    fn unary(parser: &mut Parser, _: bool) {

    }

    fn binary(parser: &mut Parser, _: bool) {

    }

    fn call(parser: &mut Parser, _: bool) {

    }

    fn dot(parser: &mut Parser, _: bool) {

    }

    fn number(parser: &mut Parser, _: bool) {

    }

    fn string(parser: &mut Parser, _: bool) {

    }

    fn literal(parser: &mut Parser, _: bool) {

    }

    fn variable(parser: &mut Parser, _: bool) {

    }

    fn super_(parser: &mut Parser, _: bool) {

    }

    fn this(parser: &mut Parser, _: bool) {

    }

    fn and(parser: &mut Parser, _: bool) {

    }

    fn or(parser: &mut Parser, _: bool) {

    }

    /// Throws an error
    /// ### Arguments
    /// * `message`: error message  
    pub fn error(&mut self, message: &str) {
        self.error_at(self.previous.clone(), message)
    }

    /// Throws an error located within current token
    /// ### Arguments
    /// * `message`: error message
    pub fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current.clone(), message)
    }

    /// Throws an error located at a specific token
    /// ### Arguments
    /// * `token`: reference to token where the error is located
    /// * `message`: error message
    pub fn error_at(&mut self, token: Token, message: &str) {
        if self.panic_mode { return };

        self.panic_mode = true;

        eprint!("[line {} ] Error", token.line);
        if token.t_type == TokenType::Eof {
            eprint!(" at end");
        } else if token.t_type == TokenType::Error {
            ()
        } else {
            eprint!(" at '{}'", token.text);
        }

        eprintln!(": {}", message);
        self.had_error = true
    }
}