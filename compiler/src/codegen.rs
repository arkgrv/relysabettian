use std::any::Any;
use std::ops::Deref;
use std::str::ParseBoolError;

use language::common;
use language::{tokenizer::Tokenizer, token::Token, token::TokenType};
use crate::opcodes::Opcode;
use crate::value::{Function, Class, Value, Chunk};

/// Expresses precedence of different expressions
#[derive(Copy, Clone, PartialEq, Debug)]
#[repr(i32)]
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

impl Into<Precedence> for i32 {
   fn into(self) -> Precedence {
       let ret = unsafe {
           std::mem::transmute::<i32, Precedence>(self)
       };
       ret
   }
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
pub struct ParseRule {
    pub prefix: Option<fn(&mut CodeGenerator, bool) -> ()>,
    pub infix: Option<fn(&mut CodeGenerator, bool) -> ()>,
    pub precedence: Precedence,
}

impl ParseRule {
    pub fn new(prefix: Option<fn(&mut CodeGenerator, bool) -> ()>, infix: Option<fn(&mut CodeGenerator, bool) -> ()>, precedence: Precedence) -> ParseRule {
        ParseRule {
            prefix,
            infix,
            precedence
        }
    }
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
pub struct CodeGenerator {
    default_function: Function,
    ftype: FunctionType,
    function: Function,
    enclosing: Option<Box<CodeGenerator>>,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    scope_depth: i32,
    previous: Token,
    current: Token,
    scanner: Tokenizer,
    class_compiler: Option<Box<ClassCompiler>>,
    had_error: bool,
    panic_mode: bool,
    rules: Vec<ParseRule>,
}

impl CodeGenerator {
    pub fn new(ftype: FunctionType, enclosing: Option<Box<CodeGenerator>>) -> CodeGenerator {
        let default_function = Function::new(0, "".to_string());

        let grouping = 
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::grouping(code_gen, can_assign) };
        let unary =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::unary(code_gen, can_assign) };
        let binary =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::binary(code_gen, can_assign) };
        let call =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::call(code_gen, can_assign) };
        let dot =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::dot(code_gen, can_assign) };
        let number =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::number(code_gen, can_assign) };
        let string =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::string(code_gen, can_assign) };
        let literal =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::literal(code_gen, can_assign) };
        let variable =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::variable(code_gen, can_assign) };
        let super_ =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::super_(code_gen, can_assign) };
        let this =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::this(code_gen, can_assign) };
        let and =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::and(code_gen, can_assign) };
        let or =
            |code_gen: &mut CodeGenerator, can_assign: bool| { Self::or(code_gen, can_assign) };

        let rules: Vec<ParseRule> = vec![
            ParseRule::new(Some(Self::grouping), Some(Self::call), Precedence::Call), // Open paren
            ParseRule::new(None, None, Precedence::None), // Close paren
            ParseRule::new(None, None, Precedence::None), // Open curly
            ParseRule::new(None, None, Precedence::None), // Close curly
            ParseRule::new(None, None, Precedence::None), // Comma,
            ParseRule::new(None, Some(Self::dot), Precedence::Call), // Dot
            ParseRule::new(Some(Self::unary), Some(Self::binary), Precedence::Term), // Minus
            ParseRule::new(Some(Self::unary), Some(Self::binary), Precedence::Term), // Plus
            ParseRule::new(None, None, Precedence::None), // Semicolon
            ParseRule::new(None, Some(Self::binary), Precedence::Factor), // Slash
            ParseRule::new(None, Some(Self::binary), Precedence::Factor), // Star
            ParseRule::new(Some(Self::binary), None, Precedence::None), // Excl
            ParseRule::new(None, Some(Self::binary), Precedence::Equality), // ExclEqual
            ParseRule::new(None, None, Precedence::None), // Equal,
            ParseRule::new(None, Some(Self::binary), Precedence::Equality), // EqualEqual
            ParseRule::new(None, Some(Self::binary), Precedence::Comparison), // Greater
            ParseRule::new(None, Some(Self::binary), Precedence::Comparison), // GreaterEqual
            ParseRule::new(None, Some(Self::binary), Precedence::Comparison), // Less
            ParseRule::new(None, Some(Self::binary), Precedence::Comparison), // LessEqual
            ParseRule::new(Some(Self::variable), None, Precedence::None), // Variable
            ParseRule::new(Some(Self::string), None, Precedence::None), // String
            ParseRule::new(Some(Self::number), None, Precedence::None), // Number
            ParseRule::new(None, Some(Self::and), Precedence::And), // And
            ParseRule::new(None, None, Precedence::None), // Class
            ParseRule::new(None, None, Precedence::None), // Else
            ParseRule::new(Some(Self::literal), None, Precedence::None), // False
            ParseRule::new(None, None, Precedence::None), // Func
            ParseRule::new(None, None, Precedence::None), // For
            ParseRule::new(None, None, Precedence::None), // If
            ParseRule::new(Some(Self::literal), None, Precedence::None), // Null
            ParseRule::new(None, Some(Self::or), Precedence::Or), // Or
            ParseRule::new(None, None, Precedence::None), // Print
            ParseRule::new(None, None, Precedence::None), // Return
            ParseRule::new(Some(Self::super_), None, Precedence::None), // Super
            ParseRule::new(Some(Self::this), None, Precedence::None), // This
            ParseRule::new(Some(Self::literal), None, Precedence::None), // True
            ParseRule::new(None, None, Precedence::None), // Var
            ParseRule::new(None, None, Precedence::None), // While
            ParseRule::new(None, None, Precedence::None), // Error
            ParseRule::new(None, None, Precedence::None), // Eof
            ParseRule::new(None, Some(Self::binary), Precedence::Term), // BwAnd
            ParseRule::new(None, Some(Self::binary), Precedence::Term), // BwOr
            ParseRule::new(None, Some(Self::binary), Precedence::Term), // BwXor
            ParseRule::new(Some(Self::unary), None, Precedence::Unary) // BwNot
        ];

        let mut gen = CodeGenerator {
            default_function: default_function.clone(),
            ftype,
            function: default_function.clone(),
            enclosing,
            locals: Vec::<Local>::new(),
            upvalues: Vec::<Upvalue>::new(),
            scope_depth: 0i32,
            previous: Token::new(TokenType::Eof, "".to_string(), 0_i32),
            current: Token::new(TokenType::Eof, "".to_string(), 0_i32),
            scanner: Tokenizer::new("".to_string()),
            class_compiler: None,
            had_error: false,
            panic_mode: false,
            rules
        };

        let first_local_name = if ftype == FunctionType::Function { "".to_string() } else { "this".to_string() };
        let first_local = Function::new(0, first_local_name);

        if ftype != FunctionType::Script {
            gen.function.name = gen.previous.text.clone();
        }

        gen
    }

    /// Adds a new local value to the virtualized memory environment
    pub fn add_local(&mut self, name: String) {
        if self.locals.len() == common::UINT8_COUNT.into() {
            self.error("Too many local variables in function");
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
                self.error("Variable identifier already declared in this scope.");
            }
        }

        self.add_local(name)
    }

    /// Marks a variable (or expression) initialized
    pub fn mark_initialized(&mut self) {
        if self.scope_depth == 0 { return; }
        let last = self.locals.len();
        self.locals[last - 1].depth = self.scope_depth;
    }

    /// Resolves (finds) a local value
    pub fn resolve_local(&mut self, name: String) -> i32 {
        for i in self.locals.len() - 1..0 {
            if self.locals[i].name == name {
                if self.locals[i].depth == -1 {
                    self.error("Cannot read local variable in its own initializer.");
                }

                return i as i32;
            }
        }

        return -1;
    }

    /// Resolves (finds) an upvalue
    pub fn resolve_upvalue(&mut self, name: String) -> i32 {
        if self.enclosing.is_none() { return -1; }
        
        let local = self.enclosing.as_mut().unwrap().resolve_local(name.clone());
        if local != -1 {
            self.enclosing.as_mut().unwrap().locals[local as usize].is_captured = true;
            return self.add_upvalue(local as u8, true);
        }

        let upvalue = self.enclosing.as_mut().unwrap().resolve_upvalue(name);
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
            self.error("Too many local variables in function.");
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
                self.emit_op(Opcode::CloseUpvalue);
            } else {
                self.emit_op(Opcode::Pop);
            }

            self.locals.pop();
        }
    }

    /// Checks if a value is local
    pub fn is_local(&self) -> bool {
        self.scope_depth > 0
    }

    /// Advances parser checking for errors
    fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            self.current = self.scanner.scan_token();
            if self.current.t_type == TokenType::Error { break; }

            self.error_at_current(self.current.text.clone());
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
        let line = self.previous.line;
        self.current_chunk().write_byte(byte, line)
    }

    /// Emit operation (opcode)
    fn emit_op(&mut self, op: Opcode) {
        let line = self.previous.line;
        self.current_chunk().write_opcode(op, line)
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
        if self.ftype == FunctionType::Initializer {
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
        let value = self.make_constant(value);
        self.emit_op_byte(Opcode::Constant, value)
    }

    /// Patch a jump for execution
    fn patch_jump(&mut self, offset: i32) {
        let jump = self.current_chunk().count() - (offset as usize) - 2;

        if jump > u16::MAX.into() {
            self.error("Too much code to jump. Aborting");
        }

        self.current_chunk().code[offset as usize] = ((jump >> 8) & 0xff) as u8;
        self.current_chunk().code[(offset + 1) as usize] = (jump & 0xff) as u8;
    }

    /// Stops compilation
    fn end_compiler(&mut self) -> Function {
        self.emit_return();
        self.function.clone()
    }

    /// Binary expression
    fn binary(code_gen: &mut CodeGenerator, _can_assign: bool) {
        let op_type = code_gen.previous.t_type;
        let rule = Self::get_rule(code_gen, op_type);
        let new_precedence = (rule.precedence as i32) + 1;
        Self::parse_precedence(code_gen, new_precedence.into());

        // Emit operator instruction
        match op_type {
            TokenType::ExclEqual => code_gen.emit_two_op(Opcode::Equal, Opcode::Not),
            TokenType::EqualEqual => code_gen.emit_op(Opcode::Equal),
            TokenType::Greater => code_gen.emit_op(Opcode::Greater),
            TokenType::GreaterEqual => code_gen.emit_two_op(Opcode::Less, Opcode::Not),
            TokenType::Less => code_gen.emit_op(Opcode::Less),
            TokenType::LessEqual => code_gen.emit_two_op(Opcode::Greater, Opcode::Not),
            TokenType::Plus => code_gen.emit_op(Opcode::Add),
            TokenType::Minus => code_gen.emit_op(Opcode::Sub),
            TokenType::Star => code_gen.emit_op(Opcode::Mul),
            TokenType::Slash => code_gen.emit_op(Opcode::Div),
            TokenType::BwOr => code_gen.emit_op(Opcode::BwOr),
            TokenType::BwAnd => code_gen.emit_op(Opcode::BwAnd),
            TokenType::BwXor => code_gen.emit_op(Opcode::BwXor),
            _ => { return }
        }
    }

    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.args_list();
        self.emit_op_byte(Opcode::Call, arg_count)
    }

    fn dot(code_gen: &mut CodeGenerator, can_assign: bool) {
        code_gen.consume(TokenType::Identifier, "Expected property name after '.'.");
        let name = code_gen.identifier_constant(code_gen.previous.text.clone());
        
        if can_assign && code_gen.match_token(TokenType::Equal) {
            code_gen.expression();
            code_gen.emit_op_byte(Opcode::SetProperty, name as u8);
        } else if code_gen.match_token(TokenType::OpenParen) {
            let arg_count = code_gen.args_list();
            code_gen.emit_op_byte(Opcode::Invoke, name as u8);
            code_gen.emit_byte(arg_count);
        } else {
            code_gen.emit_op_byte(Opcode::GetProperty, name as u8);
        }
    }

    fn literal(code_gen: &mut CodeGenerator, _can_assign: bool) {
        match code_gen.previous.t_type {
            TokenType::False => code_gen.emit_op(Opcode::False),
            TokenType::Null => code_gen.emit_op(Opcode::Nop),
            TokenType::True => code_gen.emit_op(Opcode::True),
            _ => { return; }
        }
    }

    fn grouping(code_gen: &mut CodeGenerator, _can_assign: bool) {
        code_gen.expression();
        code_gen.consume(TokenType::CloseParen, "Expected ')' after expression.")
    }

    fn number(code_gen: &mut CodeGenerator, _can_assign: bool) {
        let value = code_gen.previous.text.parse::<f64>().unwrap();
        code_gen.emit_constant(Value::Double(value));
    }

    fn or(code_gen: &mut CodeGenerator, _can_assign: bool) {
        let else_jump = code_gen.emit_jump(Opcode::JmpNz);
        let end_jump = code_gen.emit_jump(Opcode::Jmp);

        code_gen.patch_jump(else_jump);
        code_gen.emit_op(Opcode::Pop);

        Self::parse_precedence(code_gen, Precedence::Or);
        code_gen.patch_jump(end_jump);
    }

    fn string(code_gen: &mut CodeGenerator, _can_assign: bool) {
        let string_val: String = code_gen.previous.text.chars()
            .skip(1)
            .take(code_gen.previous.text.len() - 1)
            .collect();
        
        code_gen.emit_constant(Value::String(string_val))
    }

    fn named_variable(code_gen: &mut CodeGenerator, name: String, can_assign: bool) {
        let get_op: Opcode;
        let set_op: Opcode;

        let mut arg = code_gen.resolve_local(name.clone());
        if arg != -1 {
            get_op = Opcode::GetLocal;
            set_op = Opcode::SetLocal;
        } else {
            arg = code_gen.resolve_upvalue(name.clone());
            if arg != -1 {
                get_op = Opcode::GetUpvalue;
                set_op = Opcode::SetUpvalue;
            } else {
                arg = code_gen.identifier_constant(name);
                get_op = Opcode::GetGlobal;
                set_op = Opcode::SetGlobal;
            }
        }

        if can_assign && code_gen.match_token(TokenType::Equal) {
            code_gen.expression();
            code_gen.emit_op_byte(set_op, arg as u8);
        } else {
            code_gen.emit_op_byte(get_op, arg as u8);
        }
    }

    fn variable(code_gen: &mut CodeGenerator, can_assign: bool) {
        Self::named_variable(code_gen, code_gen.previous.text.clone(), can_assign)
    }

    fn super_(code_gen: &mut CodeGenerator, _can_assign: bool) {
        if code_gen.class_compiler.is_none() {
            code_gen.error("'super' cannot be used outside of a class.");
        } else if !code_gen.class_compiler.as_mut().unwrap().has_superclass {
            code_gen.error("'super' cannot be called in a class that is not inheriting.");
        }

        code_gen.consume(TokenType::Dot, "Expected '.' after 'super'.");
        code_gen.consume(TokenType::Identifier, "Expected superclass member function name.");
        let name = code_gen.identifier_constant(code_gen.previous.text.clone());

        Self::named_variable(code_gen, "this".to_string(), false);
        Self::named_variable(code_gen, "super".to_string(), false);
        code_gen.emit_op_byte(Opcode::GetSuper, name as u8)
    }

    fn this(code_gen: &mut CodeGenerator, _can_assign: bool) {
        if code_gen.class_compiler.is_none() {
            code_gen.error("'this' cannot be used outside of a class instance.");
            return;
        }

        Self::variable(code_gen, false);
    }

    fn and(code_gen: &mut CodeGenerator, _can_assign: bool) {
        let end_jump = code_gen.emit_jump(Opcode::JmpNz);

        code_gen.emit_op(Opcode::Pop);
        Self::parse_precedence(code_gen, Precedence::And);

        code_gen.patch_jump(end_jump);
    }

    fn unary(&mut self, _can_assign: bool) {
        let op_type = self.previous.t_type;

        match op_type {
            TokenType::Excl => self.emit_op(Opcode::Not),
            TokenType::Minus => self.emit_op(Opcode::Neg),
            TokenType::BwNot => self.emit_op(Opcode::BwNot),
            _ => { return; }
        }
    }

    fn parse_precedence(code_gen: &mut CodeGenerator, precedence: Precedence) {
        code_gen.advance();
        let rule = Self::get_rule(code_gen, code_gen.previous.t_type);

        if rule.prefix.is_none() {
            code_gen.error("Expected expression.");
            return;
        }

        let can_assign = precedence as i32 <= Precedence::Assignment as i32;
        rule.prefix.as_mut().unwrap()(code_gen, can_assign);

        while precedence as i32 <= Self::get_rule(code_gen, code_gen.current.t_type).precedence as i32 {
            code_gen.advance();
            let infix_rule = &mut Self::get_rule(code_gen, code_gen.previous.t_type).infix;
            infix_rule.as_mut().unwrap()(code_gen, can_assign);
        }

        if can_assign && code_gen.match_token(TokenType::Equal) {
            code_gen.error("Invalid assignment target.");
            code_gen.expression();
        }
    }

    fn get_rule(code_gen: &mut CodeGenerator, t_type: TokenType) -> &mut ParseRule {
        &mut code_gen.rules[t_type as usize]
    }

    fn identifier_constant(&mut self, name: String) -> i32 {
        self.make_constant(Value::String(name.clone())).into()
    }

    fn parse_variable(&mut self, error_message: &str) -> u8 {
        self.consume(TokenType::Identifier, error_message);

        self.declare_variable(self.previous.text.clone());
        if self.is_local() { return 0; }

        self.identifier_constant(self.previous.text.clone()) as u8
    }

    fn define_variable(&mut self, global: u8) {
        if self.is_local() {
            self.mark_initialized();
            return;
        }

        self.emit_op_byte(Opcode::DefineGlobal, global)
    }

    fn args_list(&mut self) -> u8 {
        let mut arg_count = 0_u8;

        if !self.check(TokenType::CloseParen) {
            loop {
                self.expression();
                if arg_count == u8::MAX {
                    self.error("A function cannot have more than 255 arguments.");
                }

                arg_count += 1;

                if !self.match_token(TokenType::Comma) { break; }
            }
        }

        self.consume(TokenType::CloseParen, "Expected ')' after arguments.");
        arg_count
    }

    fn expression(&mut self) {
        Self::parse_precedence(self, Precedence::Assignment);
    }

    fn block(&mut self) {
        while !self.check(TokenType::CloseCurly) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::CloseCurly, "Expected '}' after block of instructions.");
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
    pub fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
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
