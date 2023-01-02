use std::{cell::RefCell, ops::Deref, rc::Rc, num::ParseFloatError};

use language::{
    token::{Token, TokenType},
    tokenizer::Tokenizer,
};

use crate::{
    common::{FunctionType, ParseRule, Precedence},
    compiler::{ClassCompiler, Compiler},
    instruction::Opcode,
    value::{Chunk, Function, Value},
};

/// Implementation of a shared parsing structure
#[derive(Clone)]
pub struct Parser {
    pub previous: Token,
    pub current: Token,
    pub scanner: Tokenizer,

    pub compiler: Rc<RefCell<Compiler>>,
    pub class_compiler: Rc<RefCell<Option<ClassCompiler>>>,

    pub had_error: bool,
    pub panic_mode: bool,
}

impl Parser {
    /// Constructs a new Parser
    ///
    /// Parameters:
    /// * `source`: source code as String
    #[allow(invalid_value)]
    pub fn new(source: &String) -> Parser {
        let parser = Parser {
            previous: Token::new(TokenType::Eof, source.clone(), 0),
            current: Token::new(TokenType::Eof, source.clone(), 0),
            scanner: Tokenizer::new(source.clone()),
            compiler: unsafe { std::mem::zeroed() },
            class_compiler: Rc::new(RefCell::new(None)),
            had_error: false,
            panic_mode: false,
        };

        let tp = Rc::new(RefCell::new(parser));
        (*tp).borrow_mut().compiler = Rc::new(RefCell::new(Compiler::new(
            Rc::clone(&tp),
            FunctionType::Main,
            Rc::new(RefCell::new(None)),
        )));
        (*tp).borrow_mut().advance();

        let result = (*tp).borrow().clone();
        result
    }

    /// Compiles the source code and returns a function
    pub fn compile(&mut self) -> Option<Function> {
        while !self.match_token(TokenType::Eof) {
            self.declaration();
        }

        let function = self.end_compiler();

        if self.had_error {
            return None;
        }

        Some(function)
    }

    /// Advances parser moving forward in the tokens chain
    pub fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            self.current = self.scanner.scan_token();
            if self.current.t_type != TokenType::Error {
                break;
            };

            self.error_at_current(&self.current.text.clone());
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
            return;
        }

        self.error_at_current(message)
    }

    /// Checks the type of current token
    pub fn check(&mut self, t_type: TokenType) -> bool {
        self.current.t_type == t_type
    }

    /// Matches current token and advances parser
    pub fn match_token(&mut self, t_type: TokenType) -> bool {
        if !self.check(t_type) {
            return false;
        };
        self.advance();
        true
    }

    /// Emits an instruction
    ///
    /// Parameters:
    /// * `instr`: opcode of this instruction
    pub fn emit_instr(&mut self, instr: Opcode) {
        (*self.current_chunk())
            .borrow_mut()
            .write_instr(instr, self.previous.line);
    }

    /// Emits a raw instruction or data padding
    ///
    /// Parameters:
    /// * `byte`: instruction's byte value or data padding value
    pub fn emit_byte(&mut self, byte: u8) {
        (*self.current_chunk())
            .borrow_mut()
            .write_byte(byte, self.previous.line);
    }

    /// Emits an instruction with correlated data or padding
    ///
    /// Parameters:
    /// * `instr`: instruction to emit
    /// * `data`: related data or padding
    pub fn emit_instr_data(&mut self, instr: Opcode, data: u8) {
        self.emit_instr(instr);
        self.emit_byte(data)
    }

    /// Emits two instructions
    ///
    /// Parameters:
    /// * `instr1`: first instruction
    /// * `instr2`: second instruction
    pub fn emit_two_instr(&mut self, instr1: Opcode, instr2: Opcode) {
        self.emit_instr(instr1);
        self.emit_instr(instr2)
    }

    /// Emits a loop
    ///
    /// Parameters:
    /// `loop_start`: start of the loop (offset)
    pub fn emit_loop(&mut self, loop_start: usize) {
        self.emit_instr(Opcode::Loop);

        let offset = (*self.current_chunk()).borrow().count() - loop_start - 2;
        if offset > u16::MAX.into() {
            self.error("Loop body contains too many instructions.");
        }

        let offset = (offset >> 8) & 0xff;
        self.emit_byte(offset as u8);
        self.emit_byte((offset & 0xff) as u8)
    }

    /// Emits a new jump
    ///
    /// Parameters:
    /// * `instr`: type of jump
    pub fn emit_jump(&mut self, instr: Opcode) -> usize {
        self.emit_instr(instr);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        (*self.current_chunk()).borrow().count() - 2
    }

    /// Emits a new return
    pub fn emit_return(&mut self) {
        if (*self.compiler).borrow().f_type == FunctionType::Initializer {
            self.emit_instr_data(Opcode::GetLocal, 0);
        } else {
            self.emit_instr(Opcode::Nop);
        }

        self.emit_instr(Opcode::Return);
    }

    /// Creates a new constant (new value)
    ///
    /// Parameters:
    /// * `value`: value of the new constant
    pub fn make_constant(&mut self, value: Value) -> Option<u8> {
        let constant = self
            .current_chunk()
            .deref()
            .borrow_mut()
            .add_constant(&value);
        if constant > u8::MAX.into() {
            self.error("Max limit of constants reached.");
            return None;
        }

        Some(constant as u8)
    }

    /// Emits a new constant
    ///
    /// Parameters:
    /// * `value`: Value of the new constant
    pub fn emit_constant(&mut self, value: Value) {
        let value = self.make_constant(value);
        if value.is_none() {
            panic!("Cannot move value into constant. An error occured during creation!");
        }
        self.emit_instr_data(Opcode::Constant, value.unwrap())
    }

    /// Patches a jump instruction with correct data
    ///
    /// Parameters:
    /// * `offset`: offset of the jump
    pub fn patch_jump(&mut self, offset: usize) {
        let jump = (*self.current_chunk()).borrow().count() - offset - 2;
        if jump > u16::MAX.into() {
            self.error("Jump is too long.");
        }

        let first_byte = ((jump >> 8) & 0xff) as u8;
        let second_byte = (jump & 0xff) as u8;
        (*self.current_chunk())
            .borrow_mut()
            .set_code(offset, first_byte);
        (*self.current_chunk())
            .borrow_mut()
            .set_code(offset + 1, second_byte);
    }

    /// Ends this compiler and returns the corresponding function
    pub fn end_compiler(&mut self) -> Function {
        self.emit_return();
        (*self.compiler).borrow().function.deref().borrow().clone()
    }

    pub fn get_rule(&mut self, t_type: TokenType) -> ParseRule {
        let rules = vec![
            // OpenParen
            ParseRule::new(Some(Parser::grouping), Some(Parser::grouping), Precedence::Call),
            // CloseParen
            ParseRule::new(None, None, Precedence::None),
            // OpenCurly
            ParseRule::new(None, None, Precedence::None),
            // CloseCurly
            ParseRule::new(None, None, Precedence::None),
            // Comma
            ParseRule::new(None, None, Precedence::None),
            // Dot
            ParseRule::new(None, Some(Parser::dot), Precedence::Call),
            // Minus
            ParseRule::new(Some(Parser::unary), None, Precedence::Term),
            // Plus
            ParseRule::new(None, Some(Parser::binary), Precedence::Term),
            // Semicolon
            ParseRule::new(None, None, Precedence::None),
            // Slash
            ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
            // Star
            ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
            // Caret
            ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
            // Tilde
            ParseRule::new(Some(Parser::unary), None, Precedence::Unary),
            // Excl
            ParseRule::new(Some(Parser::unary), None, Precedence::None),
            // ExclEqual
            ParseRule::new(None, Some(Parser::binary), Precedence::Equality),
            // Equal
            ParseRule::new(None, None, Precedence::None),
            // EqualEqual
            ParseRule::new(None, Some(Parser::binary), Precedence::Equality),
            // Greater
            ParseRule::new(None, Some(Parser::binary), Precedence::Comparison),
            // GreaterEqual
            ParseRule::new(None, Some(Parser::binary), Precedence::Comparison),
            // Less
            ParseRule::new(None, Some(Parser::binary), Precedence::Comparison),
            // LessEqual
            ParseRule::new(None, Some(Parser::binary), Precedence::Comparison),
            // Ampersand
            ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
            // Pipe
            ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
            // AmpersandAmpersand
            ParseRule::new(None, Some(Parser::and), Precedence::And),
            // PipePipe
            ParseRule::new(None, Some(Parser::or), Precedence::Or),
            // Identifier
            ParseRule::new(Some(Parser::variable), None, Precedence::None),
            // String
            ParseRule::new(Some(Parser::string), None, Precedence::None),
            // Number
            ParseRule::new(Some(Parser::number), None, Precedence::None),
            // Class
            ParseRule::new(None, None, Precedence::None),
            // Else
            ParseRule::new(None, None, Precedence::None),
            // False
            ParseRule::new(Some(Parser::literal), None, Precedence::None),
            // Func
            ParseRule::new(None, None, Precedence::None),
            // For
            ParseRule::new(None, None, Precedence::None),
            // If
            ParseRule::new(None, None, Precedence::None),
            // Null
            ParseRule::new(Some(Parser::literal), None, Precedence::None),
            // Print
            ParseRule::new(None, None, Precedence::None),
            // Return
            ParseRule::new(None, None, Precedence::None),
            // Super
            ParseRule::new(Some(Parser::super_), None, Precedence::None),
            // This
            ParseRule::new(Some(Parser::this), None, Precedence::None),
            // True
            ParseRule::new(Some(Parser::literal), None, Precedence::None),
            // Var
            ParseRule::new(None, None, Precedence::None),
            // While
            ParseRule::new(None, None, Precedence::None),
            // Eof
            ParseRule::new(None, None, Precedence::None),
            // Error
            ParseRule::new(None, None, Precedence::None),
        ];

        let index: usize = t_type.into();
        rules[index].clone()
    }

    /// Parses a grouping (group of expressions) expression
    pub fn grouping(parser: &mut Parser, _can_assign: bool) {
        parser.expression();
        parser.consume(TokenType::CloseParen, "Expected ')' after expression.");
    }

    /// Parses a unary expression
    pub fn unary(parser: &mut Parser, _can_assign: bool) {
        let op_type = parser.previous.t_type;

        // Parse the operand
        parser.parse_precedence(Precedence::Unary);

        // Emit the instruction
        match op_type {
            TokenType::Excl => parser.emit_instr(Opcode::Not),
            TokenType::Minus => parser.emit_instr(Opcode::Negate),
            TokenType::Tilde => parser.emit_instr(Opcode::BwNot),
            _ => { return; }
        }
    }

    /// Parses a binary expression, emitting the corresponding
    /// instructions.
    pub fn binary(parser: &mut Parser, _can_assign: bool) {
        // Let's save the operator
        let op_type = parser.previous.t_type;

        // Get the rule for this operator and parse using its precedence
        let rule = parser.get_rule(op_type);
        parser.parse_precedence(rule.precedence + 1);

        // Emit the correct instruction
        match op_type {
            TokenType::ExclEqual => parser.emit_two_instr(Opcode::Equal, Opcode::Not),
            TokenType::EqualEqual => parser.emit_instr(Opcode::Equal),
            TokenType::Greater => parser.emit_instr(Opcode::Greater),
            TokenType::GreaterEqual => parser.emit_two_instr(Opcode::Less, Opcode::Not),
            TokenType::Less => parser.emit_instr(Opcode::Less),
            TokenType::LessEqual => parser.emit_two_instr(Opcode::Greater, Opcode::Not),
            TokenType::Plus => parser.emit_instr(Opcode::Add),
            TokenType::Minus => parser.emit_instr(Opcode::Subtract),
            TokenType::Star => parser.emit_instr(Opcode::Multiply),
            TokenType::Slash => parser.emit_instr(Opcode::Divide),
            TokenType::Pipe => parser.emit_instr(Opcode::BwOr),
            TokenType::Ampersand => parser.emit_instr(Opcode::BwAnd),
            TokenType::Caret => parser.emit_instr(Opcode::BwXor),
            _ => { return; }
        }
    }

    /// Parses a call expression, emitting its corresponding instruction
    pub fn call(parser: &mut Parser, _can_assign: bool) {
        let arg_count = parser.args_list();
        parser.emit_instr_data(Opcode::Call, arg_count);
    }

    /// Parses a dot (reference to method or field of class) expression
    pub fn dot(parser: &mut Parser, can_assign: bool) {

        parser.consume(TokenType::Identifier, "Expected property name '.'.");
        let name = parser.identifier_constant(parser.previous.text.clone());

        if can_assign && parser.match_token(TokenType::Equal) {
            parser.expression();
            parser.emit_instr_data(Opcode::SetProperty, name.unwrap());
        } else if parser.match_token(TokenType::OpenParen) {
            let arg_count = parser.args_list();
            parser.emit_instr_data(Opcode::Invoke, name.unwrap());
            parser.emit_byte(arg_count);
        } else {
            parser.emit_instr_data(Opcode::GetProperty, name.unwrap());
        }
    }

    /// Parses a numeric literal value
    pub fn number(parser: &mut Parser, _can_assign: bool) {
        let numeric_value: Result<f64, ParseFloatError>  = parser.previous.text.parse();
        if numeric_value.is_err() {
            parser.error("Cannot parse non numeric value!");
        }

        let value = Value::Double(numeric_value.unwrap());
        parser.emit_constant(value);
    }

    /// Parses a literal string value
    pub fn string(parser: &mut Parser, _can_assign: bool) {
        let mut str = parser.previous.text.clone();
        str.remove(0);
        str.remove(str.len() - 1);
        parser.emit_constant(Value::String(str));
    }

    /// Parses a literal value (or expression of literal values)
    pub fn literal(parser: &mut Parser, _can_assign: bool) {
        match parser.previous.t_type {
            TokenType::False => parser.emit_instr(Opcode::False),
            TokenType::Null => parser.emit_instr(Opcode::Nop),
            TokenType::True => parser.emit_instr(Opcode::True),
            _ => { return ;}
        }
    }

    /// Parses a variable expression
    pub fn variable(parser: &mut Parser, can_assign: bool) {
        parser.named_variable(parser.previous.text.clone(), can_assign);
    }

    /// Parses a super (superclass) expression
    pub fn super_(parser: &mut Parser, _can_assign: bool) {
        if (*parser.class_compiler).borrow().is_none() {
            parser.error("'super' cannot be used outside of a class instance.");
        } else if !(*parser.class_compiler).borrow().as_ref().unwrap().has_superclass {
            parser.error("'super' cannot be used in a class that does not inherit.");
        }

        parser.consume(TokenType::Dot, "Expected '.' after 'super'.");
        parser.consume(TokenType::Identifier, "Expected superclass method name.");
        
        let name = parser.identifier_constant(parser.previous.text.clone());

        parser.named_variable("this".to_string(), false);
        parser.named_variable("super".to_string(), false);
        parser.emit_instr_data(Opcode::GetSuper, name.unwrap())
    }

    /// Parses a this (reference to current class) expression
    pub fn this(parser: &mut Parser, _can_assign: bool) {
        let end_jump = parser.emit_jump(Opcode::JumpIfFalse);

        parser.emit_instr(Opcode::Pop);
        parser.parse_precedence(Precedence::And);

        parser.patch_jump(end_jump);
    }

    /// Parses a boolean and expression
    pub fn and(parser: &mut Parser, _can_assign: bool) {
        let end_jump = parser.emit_jump(Opcode::JumpIfFalse);

        parser.emit_instr(Opcode::Pop);
        parser.parse_precedence(Precedence::And);

        parser.patch_jump(end_jump);
    }

    /// Parses an or expression. An expression that contains a boolean or
    pub fn or(parser: &mut Parser, _can_assign: bool) {
        let else_jump = parser.emit_jump(Opcode::JumpIfFalse);
        let end_jump = parser.emit_jump(Opcode::Jump);

        parser.patch_jump(else_jump);
        parser.emit_instr(Opcode::Pop);

        parser.parse_precedence(Precedence::Or);
        parser.patch_jump(end_jump);
    }

    /// Parses and declares a named variable
    /// 
    /// Parameters:
    /// * `name`: name of new variable
    /// * `can_assign`: tells whether this is an assigment target or not
    pub fn named_variable(&mut self, name: String, can_assign: bool) {
        let get_op: Opcode;
        let set_op: Opcode;
        let arg = (*self.compiler).borrow().resolve_local(name.clone());

        if arg.is_some() {
            get_op = Opcode::GetLocal;
            set_op = Opcode::SetLocal;
        } else {
            let arg = (*self.compiler).borrow_mut().resolve_upvalue(name);
            if arg.is_some() {
                get_op = Opcode::GetUpvalue;
                set_op = Opcode::SetUpvalue;
            } else {
                get_op = Opcode::GetGlobal;
                set_op = Opcode::SetGlobal;
            }
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.emit_instr_data(set_op, arg.unwrap() as u8);
        } else {
            self.emit_instr_data(get_op, arg.unwrap() as u8)
        }
    }

    /// Parses the expression with the given precedence
    /// 
    /// Parameters:
    /// * `precedence`: precedence to use during parsing phase
    pub fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = self.get_rule(self.previous.t_type).prefix;

        if prefix_rule.is_none() {
            self.error("Expected expression.");
            return;
        }

        let can_assign = precedence <= Precedence::Assignment;
        prefix_rule.unwrap()(self, can_assign);

        while precedence <= self.get_rule(self.current.t_type).precedence {
            self.advance();
            let infix_rule = self.get_rule(self.previous.t_type).infix;
            infix_rule.unwrap()(self, can_assign);
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error("Invalid assignment target.");
            self.expression();
        }
    }

    /// Creates a new constant for the given identifier
    /// 
    /// Parameters:
    /// * `name`: name of new identifier
    pub fn identifier_constant(&mut self, name: String) -> Option<u8> {
        let constant = self.make_constant(Value::String(name));
        constant
    }

    /// Parses a variable and declares it
    /// 
    /// Parameters:
    /// * `message`: error message if parsing fails
    pub fn parse_variable(&mut self, message: &str) -> u8 {
        self.consume(TokenType::Identifier, message);

        (*self.compiler).borrow_mut().declare_variable(self.previous.text.clone());
        if (*self.compiler).borrow().is_local() { return 0; }

        let result = self.identifier_constant(self.previous.text.clone());
        result.unwrap()
    }

    /// Defines a new variable
    /// 
    /// Parameters:
    /// * `global`: index of global variable
    pub fn define_variable(&mut self, global: u8) {
        if (*self.compiler).borrow().is_local() {
            (*self.compiler).borrow_mut().mark_initialized();
            return;
        }

        self.emit_instr_data(Opcode::DefineGlobal, global)
    }

    /// Builds the arguments list for a given function
    pub fn args_list(&mut self) -> u8 {
        let mut arg_count = 0_u8;

        if !self.check(TokenType::CloseParen) {
            loop {
                self.expression();
                if arg_count == u8::MAX {
                    self.error("Functions canno receive more than 255 arguments.");
                }
                arg_count += 1;

                if !self.match_token(TokenType::Comma) { break };
            }
        }

        self.consume(TokenType::CloseParen, "Expected ')' after arguments.");
        arg_count
    }

    /// Parses an expression
    pub fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    /// Parses a block of expressions or statements
    pub fn block(&mut self) {
        while !self.check(TokenType::CloseCurly) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::CloseCurly, "Expected '}' after block.")
    }

    /// Parses a function given its type
    /// 
    /// Parameters:
    /// * `f_type`: type of function
    pub fn function(&mut self, f_type: FunctionType) {
        let old_compiler = Rc::clone(&self.compiler);
        self.compiler = Rc::new(RefCell::new(Compiler::new(
            Rc::new(RefCell::new(self.clone())),
            f_type,
            Rc::new(RefCell::new(Some((*old_compiler).borrow().clone())))
        )));

        self.consume(TokenType::OpenParen, "Expected '(' after function name.");
        if !self.check(TokenType::CloseParen) {
            loop {
                (*self.compiler).borrow_mut().function.deref().borrow_mut().arity += 1;
                if (*self.compiler).borrow().function.deref().borrow().arity > u8::MAX.into() {
                    self.error_at_current("Functions cannot take more than 255 parameters.");
                }

                let constant = self.parse_variable("Expected parameter name.");
                self.define_variable(constant);

                if !self.match_token(TokenType::Comma) { break; }
            }
        }

        self.consume(TokenType::CloseParen, "Expected ')' after parameters.");
        self.consume(TokenType::OpenCurly, "Expected '{' before function body.");
        self.block();

        let function = self.end_compiler();
        let new_compiler = Rc::clone(&self.compiler);

        let enclosing = (*new_compiler).borrow().clone();
        self.compiler = Rc::new(RefCell::new(enclosing));

        let constant = self.make_constant(Value::Function(Rc::new(function)));
        self.emit_instr_data(Opcode::Closure, constant.unwrap());

        for upvalue in &(*new_compiler).borrow().upvalues {
            self.emit_byte(if upvalue.is_local { 1 } else { 0 });
            self.emit_byte(upvalue.index);
        }
    }

    /// Parses a method of a given class
    pub fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expected method name.");
        let constant = self.identifier_constant(self.previous.text.clone());
        let f_type = if self.previous.text == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };

        self.function(f_type);
        self.emit_instr_data(Opcode::Method, constant.unwrap());
    }

    /// Parses a class declaration
    pub fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expected class name.");
        let class_name = self.previous.text.clone();
        let name_const = self.identifier_constant(class_name.clone());

        (*self.compiler).borrow_mut().declare_variable(self.previous.text.clone());

        self.emit_instr_data(Opcode::Class, name_const.unwrap());
        self.define_variable(name_const.unwrap());

        if self.match_token(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expected superclass name.");
            Parser::variable(self, false);

            if class_name == self.previous.text {
                self.error("Classes cannot inherit from themselves as it wouldn't make any sense.");
            }

            (*self.compiler).borrow_mut().begin_scope();
            (*self.compiler).borrow_mut().add_local("super".to_string());
            self.define_variable(0);

            self.named_variable(class_name.clone(), false);
            self.emit_instr(Opcode::Inherit);
            (*self.class_compiler).borrow_mut().as_mut().unwrap().has_superclass = true;
        }

        self.named_variable(class_name.clone(), false);
        self.consume(TokenType::OpenCurly, "Expected '{' before class body.");

        while !self.check(TokenType::CloseCurly) && !self.check(TokenType::Eof) {
            self.method();
        }

        self.consume(TokenType::CloseCurly, "Expected '}' after class body.");
        self.emit_instr(Opcode::Pop);

        if (*self.class_compiler).borrow().as_ref().unwrap().has_superclass {
            (*self.compiler).borrow_mut().end_scope();
        }

        self.class_compiler = Rc::clone(&self.class_compiler)
    }

    /// Declares a new function parsing it
    pub fn func_declaration(&mut self) {
        let global = self.parse_variable("Expected function name.");
        (*self.compiler).borrow_mut().mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    /// Parses and declares a variable
    pub fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected variable name.");

        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_instr(Opcode::Nop);
        }

        self.consume(TokenType::Semicolon, "Expected ';' after variable declaration.");
        self.define_variable(global);
    }

    /// Parses an expression statement
    pub fn expression_statement(&mut self) {
        self.expression();
        self.emit_instr(Opcode::Pop);
        self.consume(TokenType::Semicolon, "Expected ';' after expression.");
    }

    /// Parses a for statement
    pub fn for_statement(&mut self) {
        (*self.compiler).borrow_mut().begin_scope();

        self.consume(TokenType::OpenParen, "Expected '(' after 'for'.");
        if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else if self.match_token(TokenType::Semicolon) {
            ()
        } else {
            self.expression_statement();
        }

        let mut loop_start = (*self.current_chunk()).borrow().count();
        let mut exit_jump = -1;

        if !self.match_token(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expected ';' after loop condition.");

            exit_jump = self.emit_jump(Opcode::JumpIfFalse) as i32;
            self.emit_instr(Opcode::Pop);
        }

        if !self.match_token(TokenType::CloseParen) {
            let body_jump = self.emit_jump(Opcode::Jump);
            let increment_start = (*self.current_chunk()).borrow().count();

            self.expression();
            self.emit_instr(Opcode::Pop);
            self.consume(TokenType::CloseParen, "Expected ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();

        self.emit_loop(loop_start);

        if exit_jump != -1 {
            self.patch_jump(exit_jump as usize);
            self.emit_instr(Opcode::Pop);
        }

        (*self.compiler).borrow_mut().end_scope();
    }

    /// Parses an if statement
    pub fn if_statement(&mut self) {
        self.consume(TokenType::OpenParen, "Expected '(' after if 'if'.");
        self.expression();
        self.consume(TokenType::CloseParen, "Expected ')' after condition.");

        let then_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_instr(Opcode::Pop);
        self.statement();
        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump);
        self.emit_instr(Opcode::Pop);

        if self.match_token(TokenType::Else) { 
            self.statement();
        }

        self.patch_jump(else_jump);
    }

    /// Parses a declaration
    pub fn declaration(&mut self) {
        if self.match_token(TokenType::Class) {
            self.class_declaration();
        } else if self.match_token(TokenType::Func) {
            self.func_declaration();
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode { self.sync() };
    }

    /// Parses a statement
    pub fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::For) {
            self.for_statement();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::Return) {
            self.return_statement();
        } else if self.match_token(TokenType::While) {
            self.while_statement();
        } else if self.match_token(TokenType::OpenCurly) {
            (*self.compiler).borrow_mut().begin_scope();
            self.block();
            (*self.compiler).borrow_mut().end_scope();
        } else {
            self.expression_statement();
        }
    }

    /// Parses a print statement
    pub fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after value.");
        self.emit_instr(Opcode::Print);
    }

    /// Parses a return statement
    pub fn return_statement(&mut self) {
        if (*self.compiler).borrow().f_type == FunctionType::Main {
            self.error("Cannot return from main.");
        }

        if self.match_token(TokenType::Semicolon) {
            self.emit_return();
        } else {
            if (*self.compiler).borrow().f_type == FunctionType::Initializer {
                self.error("Cannot return values from class initializers.");
            }

            self.expression();
            self.consume(TokenType::Semicolon, "Expected ';' after return value.");
            self.emit_instr(Opcode::Return);
        }
    }

    /// Parses a while statement
    pub fn while_statement(&mut self) {
        let loop_start = (*self.current_chunk()).borrow().count();

        self.consume(TokenType::OpenParen, "Expected '(' after 'while'.");
        self.expression();
        self.consume(TokenType::CloseParen, "Expected ')' after condition.");

        let exit_jump = self.emit_jump(Opcode::JumpIfFalse);

        self.emit_instr(Opcode::Pop);
        self.statement();

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_instr(Opcode::Pop);
    }

    /// Syncs the parser in case of failure
    pub fn sync(&mut self) {
        self.panic_mode = false;

        while self.current.t_type != TokenType::Eof {
            if self.previous.t_type == TokenType::Semicolon { return; }

            match self.current.t_type {
                TokenType::Class | TokenType::Func | TokenType::If | TokenType::While | TokenType::Print | TokenType::Return => { return; },
                _ => (),
            }

            self.advance();
        }
    }

    /// Errors at the last token
    /// 
    /// Parameters:
    /// * `message`: error message
    pub fn error(&mut self, message: &str) {
        self.error_at(self.previous.clone(), message);
    }

    /// Errors at current token
    /// 
    /// Parameters:
    /// * `message`: error message
    pub fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current.clone(), message);
    }

    /// Errors at the specified token
    /// 
    /// Parameters:
    /// * `token`: token to error at
    /// * `message`: error message
    pub fn error_at(&mut self, token: Token, message: &str) {
        if self.panic_mode { return; }

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

        self.had_error = true;
    }

    pub fn current_chunk(&mut self) -> Rc<RefCell<Chunk>> {
        Rc::clone(
            &(*self.compiler)
                .borrow_mut()
                .function
                .deref()
                .borrow_mut()
                .chunk,
        )
    }
}
