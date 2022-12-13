use language::{token::{Token, TokenType}, tokenizer::Tokenizer};
use crate::{common::{FunctionKind, Local, Upvalue, ParseRule, Precedence}, value::{FuncObj, Chunk, ValueType}, bytecode::Instruction};

/// Maximum number of arguments
const MAX_ARGS: u8 = 255;

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
                // Else
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // False
                Box::new(ParseRule::new(Some(Box::new(Parser::literal)), None, Precedence::None)),
                // Func
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // For
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // If
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Null
                Box::new(ParseRule::new(Some(Box::new(Parser::literal)), None, Precedence::None)),
                // Or
                Box::new(ParseRule::new(None, Some(Box::new(Parser::or)), Precedence::None)),
                // Print
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Return
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Super
                Box::new(ParseRule::new(Some(Box::new(Parser::super_)), None, Precedence::None)),
                // This
                Box::new(ParseRule::new(Some(Box::new(Parser::this)), None, Precedence::None)),
                // True
                Box::new(ParseRule::new(Some(Box::new(Parser::literal)), None, Precedence::None)),
                // Var
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // While
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Error
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // Eof
                Box::new(ParseRule::new(None, None, Precedence::None)),
                // BwAnd
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Term)),
                // BwOr
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Term)),
                // BwXor
                Box::new(ParseRule::new(None, Some(Box::new(Parser::binary)), Precedence::Term)),
                // BwNot
                Box::new(ParseRule::new(Some(Box::new(Parser::unary)), None, Precedence::Unary)),
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

    /// Declares a new variable
    /// ### Arguments
    /// `name`: name of the variable to declare
    pub fn declare_variable(&mut self, name: &String) {
        if self.scope_depth == 0 { return };

        for i in self.locals.len() - 1..0 {
            if self.locals[i].depth != -1 && self.locals[i].depth < self.scope_depth {
                break;
            }

            if self.locals[i].name.eq(name) {
                self.parser.as_mut().unwrap().error(&format!("Variable with name {} already defined in this scope.", name));
            }
        }

        self.add_local(name)
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

impl ClassCompiler {
    /// Creates a new ClassCompiler
    /// ### Arguments
    /// `enclosing`: enclosing class compiler
    pub fn new(enclosing: Option<Box<ClassCompiler>>) -> ClassCompiler {
        ClassCompiler {
            enclosing,
            has_superclass: false,
        }
    }
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

    /// Compiles the source code and returns an executable
    /// function object
    pub fn compile(&mut self) -> Option<Box<FuncObj>> {
        while !self.match_ttype(TokenType::Eof) {
            self.declaration();
        }
        let function = self.end_compiler();

        if self.had_error {
            return None
        }

        Some(function)
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

    /// Gets the relative parsing rule
    /// ### Arguments
    /// * `t_type`: token type of this rule
    fn get_rule(&mut self, t_type: TokenType) -> Box<ParseRule> {
        let cnt = RulesContainer::new();
        cnt.rules[t_type as usize].clone()
    }

    /// Parses the precedence of a given expression
    /// ### Arguments
    /// * `precedence`: precedence enumerator which selects how to 
    /// treat the expression
    fn parse_precedence(&mut self, precedence: Precedence) {
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

        if can_assign && self.match_ttype(TokenType::Equal) {
            self.error("Invalid assignment target.");
            //self.expression();
        }
    }

    /// Creates argument list for function
    fn args_list(&mut self) -> u8 {
        let mut argc = 0_u8;

        if !self.check_ttype(TokenType::CloseParen) {
            loop {
                if argc == MAX_ARGS {
                    self.error(&format!("Function cannot have more than {} arguments.", MAX_ARGS));
                }
                argc += 1;

                if !self.match_ttype(TokenType::Comma) { break };
            }
        }

        self.consume(TokenType::CloseParen, "Expected ')' after arguments.");
        argc
    }

    /// Creates constant for identifier
    /// ### Arguments
    /// * `name`: name of the constant
    fn identifier_constant(&mut self, name: String) -> u8 {
        self.make_constant(Box::new(ValueType::String(name)))
    }

    fn parse_variable(&mut self, error_message: &str) -> u8 {
        self.consume(TokenType::Identifier, error_message);

        self.compiler.as_mut().unwrap().declare_variable(&self.previous.text);
        if self.compiler.as_mut().unwrap().is_local() {
            return 0
        }

        self.identifier_constant(self.previous.text.clone())
    }

    /// Defines a new variable
    /// ### Arguments
    /// * `global`: index of a global variable
    fn define_variable(&mut self, global: u8) {
        if self.compiler.as_mut().unwrap().is_local() {
            self.compiler.as_mut().unwrap().mark_initialized();
            return
        }

        self.emit_instruction_data(Instruction::DefGlob, global)
    }

    /// Grouping (more than one expression as a group) expression
    /// evaluation. An error is detected when expression does not end
    /// with ')'
    fn grouping(&mut self, _: bool) {
        self.expression();
        self.consume(TokenType::CloseParen, "Expected ')' after expression.")
    }

    /// Parses a unary expression
    /// ### Arguments
    /// `_`: ignored boolean value
    fn unary(&mut self, _: bool) {
        let operator = self.previous.t_type;

        // Compile the operand
        self.parse_precedence(Precedence::Unary);

        // Emit instruction according to operator
        match operator {
            TokenType::Excl => self.emit_instruction(Instruction::Not),
            TokenType::Minus => self.emit_instruction(Instruction::Neg),
            TokenType::BwNot => self.emit_instruction(Instruction::BNot),
            _ => { return }
        }
    }

    /// Parses a binary expression
    /// ### Arguments
    /// `_`: ignored boolean value
    fn binary(&mut self, _: bool) {
        let operator = self.previous.t_type;

        // Compile right side of the operation
        let rule = self.get_rule(operator);
        self.parse_precedence((rule.precedence as u8 + 1_u8).into());

        match operator {
            TokenType::ExclEqual => self.emit_two_instructions(Instruction::Eq, Instruction::Not),
            TokenType::EqualEqual => self.emit_instruction(Instruction::Eq),
            TokenType::Greater => self.emit_instruction(Instruction::Gt),
            TokenType::GreaterEqual => self.emit_two_instructions(Instruction::Lt, Instruction::Not),
            TokenType::Less => self.emit_instruction(Instruction::Lt),
            TokenType::LessEqual => self.emit_two_instructions(Instruction::Gt, Instruction::Not),
            TokenType::Plus => self.emit_instruction(Instruction::Add),
            TokenType::Minus => self.emit_instruction(Instruction::Sub),
            TokenType::Star => self.emit_instruction(Instruction::Mul),
            TokenType::Slash => self.emit_instruction(Instruction::Div),
            TokenType::BwAnd => self.emit_instruction(Instruction::BAnd),
            TokenType::BwOr => self.emit_instruction(Instruction::BOr),
            TokenType::BwXor => self.emit_instruction(Instruction::BXor),
            _ => { return },
        };
    }

    /// Parses a call expression
    /// # Arguments:
    /// `_`: ignored boolean value
    fn call(&mut self, _: bool) {
        let argc = self.args_list();
        self.emit_instruction_data(Instruction::Call, argc)
    }

    /// Parsing function for dot
    /// ### Arguments
    /// * `can_assign`: tells if can receive an assignment
    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expected property name after '.'.");
        let name = self.identifier_constant(self.previous.text.clone());

        if can_assign && self.match_ttype(TokenType::Equal) {
            self.expression();
            self.emit_instruction_data(Instruction::SetProp, name);
        } else if self.match_ttype(TokenType::OpenParen) {
            let argc = self.args_list();
            self.emit_instruction_data(Instruction::Inv, name);
            self.emit_byte(argc);
        } else {
            self.emit_instruction_data(Instruction::GetProp, name)
        }
    }

    /// Parses a number and gets its value
    /// ### Arguments
    /// * `_`: ignored boolean value
    fn number(&mut self, _: bool) {
        let value = self.previous.text
                                                        .clone()
                                                        .parse::<f64>()
                                                        .unwrap();
        self.emit_constant(Box::new(ValueType::Number(value)))
    }

    /// Parses a string removing the opening and closing character
    /// ### Arguments
    /// * `_`: ignored boolean value
    fn string(&mut self, _: bool) {
        let mut str = self.previous.text.clone();

        // Remove first char
        str.chars().next();
        // Remove last char
        str.chars().next_back();

        // Convert back to string
        str = str.chars().as_str().to_string();
        self.emit_constant(Box::new(ValueType::String(str)))
    }

    /// Parses a literal value such as true or null
    /// ### Arguments
    /// * `_`: ignored boolean value
    fn literal(&mut self, _: bool) {
        let t_type = self.previous.t_type;
        match t_type {
            TokenType::False => self.emit_instruction(Instruction::False),
            TokenType::Null => self.emit_instruction(Instruction::Nop),
            TokenType::True => self.emit_instruction(Instruction::True),
            _ => { return }
        }
    }

    /// Creates a named variable
    /// ### Arguments
    /// * `name`: name of variable
    /// * `can_assign`: specifies if this variable can be an assignment target
    fn named_variable(&mut self, name: String, can_assign: bool) {
        let get_op: Instruction;
        let set_op: Instruction;

        // We try to see if the variable is a local, upvalue or global variable
        let mut arg = self.compiler.as_mut().unwrap().resolve_local(&name);
        if arg != -1 {
            get_op = Instruction::GetLoc;
            set_op = Instruction::SetLoc;
        } else {
            arg = self.compiler.as_mut().unwrap().resolve_upvalue(&name);
            if arg != -1 {
                get_op = Instruction::GetUVal;
                set_op = Instruction::SetUVal;
            } else {
                arg = self.identifier_constant(name.clone()).into();
                get_op = Instruction::GetGlob;
                set_op = Instruction::SetGlob;
            }
        }

        // If assignment, then it becomes target
        if can_assign && self.match_ttype(TokenType::Equal) {
            self.expression();
            self.emit_instruction_data(set_op, arg as u8);
        } else {
            self.emit_instruction_data(get_op, arg as u8)
        }
    }

    /// Parses a variable
    /// ### Arguments
    /// `can_assign`: specifies whether this variable is an assignment target
    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous.text.clone(), can_assign)
    }

    /// Parses superclass expression
    /// ### Arguments
    /// `_`: ignored boolean value
    fn super_(&mut self, _: bool) {
        if self.class_compiler.is_none() {
            self.error("'super' keyword cannot be used outside of a class.");
        } else if !self.class_compiler.as_mut().unwrap().has_superclass {
            self.error("'super' cannot be used in a class without superclass.");
        }

        self.consume(TokenType::Dot, "Expected '.' after 'super'.");
        self.consume(TokenType::Identifier, "Expected superclass method name.");
        let name = self.identifier_constant(self.previous.text.clone());
        self.emit_instruction_data(Instruction::GetParen, name)
    }

    /// Parses this expression
    /// ### Arguments
    /// `_`: ignored boolean value
    fn this(&mut self, _: bool) {
        if self.class_compiler.is_none() {
            self.error("'this' cannot be used outside of a class.");
            return;
        }

        self.variable(false)
    }

    /// Parses and generates instructions for and
    /// ### Arguments
    /// `_`: ignored boolean value
    fn and(&mut self, _: bool) {
        let end_jump = self.emit_jump(Instruction::JmpNz);

        self.emit_instruction(Instruction::Pop);
        self.parse_precedence(Precedence::And);

        self.patch_jump(end_jump)
    }

    /// Parses and generates code for or expression
    /// ### Arguments
    /// * `_`: ignored boolean value
    fn or(&mut self, _: bool) {
        let else_jump = self.emit_jump(Instruction::JmpNz);
        let end_jump = self.emit_jump(Instruction::Jmp);

        self.patch_jump(else_jump);
        self.emit_instruction(Instruction::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    /// Parsing of a single expression
    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    /// Parses a block of code
    fn block(&mut self) {
        while !self.check_ttype(TokenType::CloseCurly) && !self.check_ttype(TokenType::Eof) {
            self.declaration();
        }
    }

    /// Parses a function and generates its code
    /// ### Arguments
    /// `func_type`: function kind
    fn function(&mut self, func_type: FunctionKind) {
        // Create a new compiler for this function and set its enclosing value
        // to the current compiler
        let n_compiler = Box::new(Compiler::new(Some(Box::new(self.clone())), func_type, self.compiler.clone()));
        self.compiler = Some(n_compiler);

        self.consume(TokenType::OpenParen, "Expected '(' after function name.");
        if !self.check_ttype(TokenType::CloseParen) {
            loop {
                self.compiler.as_mut().unwrap().function.arity += 1;
                if self.compiler.as_mut().unwrap().function.arity > MAX_ARGS.into() {
                    self.error_at_current("Function cannot have more than 255 parameters.");
                }

                let constant = self.parse_variable("Expected parameter name.");
                self.define_variable(constant);

                if !self.match_ttype(TokenType::Comma) { break };
            }

            self.consume(TokenType::CloseParen, "Expected ')' after parameters.");
            self.consume(TokenType::OpenCurly, "Expected '{' before function body.");
            self.block();

            // End this compiler and restore its original value
            let function = self.end_compiler();
            let new_compiler = self.compiler.as_mut().unwrap();
            self.compiler = new_compiler.enclosing.clone();

            // Emit corresponding instruction
            let constant = self.make_constant(Box::new(ValueType::Func(function)));
            self.emit_instruction_data(Instruction::Clo, constant);

            for upvalue in &self.compiler.as_mut().unwrap().upvalues.clone() {
                let is_local = if upvalue.is_local { 1_u8 } else { 0_u8 };
                self.emit_byte(is_local);
                self.emit_byte(upvalue.index);
            }
        }
    }

    /// Parses and generates code for a bound instance function
    fn bound_func(&mut self) {
        self.consume(TokenType::Identifier, "Expected bound function name.");
        let constant = self.identifier_constant(self.previous.text.clone());
        let f_type = if self.previous.text == "init" {
            FunctionKind::Initializer
        } else {
            FunctionKind::Bound
        };

        self.function(f_type);
        self.emit_instruction_data(Instruction::IFunc, constant)
    }

    /// Parses class declaration
    fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expected class name.");
        let class_name = self.previous.text.clone();
        let name_constant = self.identifier_constant(class_name.clone());

        self.compiler.as_mut().unwrap().declare_variable(&self.previous.text);

        self.emit_instruction_data(Instruction::Class, name_constant);
        self.define_variable(name_constant);

        // Create the class compiler
        let class_compiler = Box::new(ClassCompiler::new(self.class_compiler.clone()));
        self.class_compiler = Some(class_compiler);

        if self.match_ttype(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expected superclass name.");
            self.variable(false);

            if class_name == self.previous.text {
                self.error("Class cannot inherit from itslef.");
            }

            self.compiler.as_mut().unwrap().begin_scope();
            self.compiler.as_mut().unwrap().add_local(&"super".to_string());
            self.define_variable(0_u8);

            self.named_variable(class_name.clone(), false);
            self.emit_instruction(Instruction::Inherit);

            // setup effective instance class compiler
            self.class_compiler.as_mut().unwrap().has_superclass = true;
        }

        self.named_variable(class_name, false);

        self.consume(TokenType::OpenCurly, "Expected '{' before class body.");
        while !self.check_ttype(TokenType::CloseCurly) && !self.check_ttype(TokenType::Eof) {
            self.bound_func();
        }
        self.consume(TokenType::CloseCurly, "Expected '}' after class body.");
        self.emit_instruction(Instruction::Pop);

        if self.class_compiler.as_mut().unwrap().has_superclass {
            self.compiler.as_mut().unwrap().end_scope();
        }

        let c_comp = self.class_compiler.as_mut().unwrap().enclosing.clone();
        self.class_compiler = c_comp;
    }

    /// Parses and declares a new function
    fn func_declaration(&mut self) {
        let global = self.parse_variable("Expected function name.");
        self.compiler.as_mut().unwrap().mark_initialized();
        self.function(FunctionKind::Function);
        self.define_variable(global)
    }

    /// Parses a variable declaration
    fn var_declaration(&mut self) {
        // parse variable
        let global = self.parse_variable("Expected variable name.");
        
        // check for presence of expression, if there is we need to evaluate it
        if self.match_ttype(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_instruction(Instruction::Nop);
        }
        self.consume(TokenType::Semicolon, "Expected ';' after variable declaration.");

        self.define_variable(global)
    }

    /// Parses and emits code for an expression statement
    fn expression_statement(&mut self) {
        self.expression();
        self.emit_instruction(Instruction::Pop);
        self.consume(TokenType::Semicolon, "Expected ';' after expression.");
    }

    /// Parses and generates code for a for statement
    fn for_statement(&mut self) {
        // Begin a new scope for this for loop
        self.compiler.as_mut().unwrap().begin_scope();

        self.consume(TokenType::OpenParen, "Expected '(' after 'for'.");
        if self.match_ttype(TokenType::Var) {
            self.var_declaration();
        } else if self.match_ttype(TokenType::Semicolon) {
            ()
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().count();
        let mut exit_jump = -1;

        if !self.match_ttype(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expected ';' after for loop condition.");

            exit_jump = self.emit_jump(Instruction::JmpNz) as i32;
            self.emit_instruction(Instruction::Pop);
        }

        // This starts the creation of the loop itself
        if !self.match_ttype(TokenType::CloseParen) {
            // Emit a jump and take its offset (start of body)
            let body_jump = self.emit_jump(Instruction::Jmp);
            let increment_start = self.current_chunk().count();

            // Parse every expression in block
            self.expression();
            self.emit_instruction(Instruction::Pop);
            self.consume(TokenType::CloseParen, "Expected ')' after for clauses.");

            // Emit the loop and jump to the beginning
            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();

        self.emit_loop(loop_start);

        if exit_jump != -1 {
            self.patch_jump(exit_jump as usize);
            self.emit_instruction(Instruction::Pop);
        }

        // End this scope
        self.compiler.as_mut().unwrap().end_scope();
    }

    /// Parses and generates code for 'if' statement
    fn if_statement(&mut self) {
        // Check and consume if condition
        self.consume(TokenType::OpenParen, "Expected '(' after 'if'.");
        self.expression();
        self.consume(TokenType::CloseParen, "Expected ')' after 'if' condition.");

        // Get start of the if block
        let then_jump = self.emit_jump(Instruction::JmpNz);
        self.emit_instruction(Instruction::Pop);
        // Parse the statement inside the then branch
        self.statement();

        // Emit the else branch
        let else_jump = self.emit_jump(Instruction::Jmp);
        self.patch_jump(then_jump);
        self.emit_instruction(Instruction::Pop);

        if self.match_ttype(TokenType::Else) { self.statement() };
        self.patch_jump(else_jump);
    }

    /// Parses and emits code for any declaration
    fn declaration(&mut self) {
        if self.match_ttype(TokenType::Class) {
            self.class_declaration();
        } else if self.match_ttype(TokenType::Func) {
            self.func_declaration();
        } else if self.match_ttype(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.sync()
        }
    }

    /// Parses and emits code for any statement
    fn statement(&mut self) {
        if self.match_ttype(TokenType::Print) {
            self.print_statement();
        } else if self.match_ttype(TokenType::For) {
            self.for_statement();
        } else if self.match_ttype(TokenType::If) {
            self.if_statement();
        } else if self.match_ttype(TokenType::Return) {
            self.return_statement();
        } else if self.match_ttype(TokenType::While) {
            self.while_statement();
        } else if self.match_ttype(TokenType::OpenCurly) {
            self.compiler.as_mut().unwrap().begin_scope();
            self.block();
            self.compiler.as_mut().unwrap().end_scope();
        } else {
            self.expression_statement()
        }
    }

    /// Parses and emits code for print statement
    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after expression.");
        self.emit_instruction(Instruction::Out)
    }

    /// Parses and emits code for return statement
    fn return_statement(&mut self) {
        // if it is in the main function error out
        if self.compiler.as_mut().unwrap().func_type == FunctionKind::Main {
            self.error("Cannot return from top level code.");
        }

        if self.match_ttype(TokenType::Semicolon) {
            self.emit_return();
        } else {
            // Error out even if it is an initializer
            if self.compiler.as_mut().unwrap().func_type == FunctionKind::Initializer {
                self.error("Cannot return fvalue from an initialier.");
            }

            self.expression();
            self.consume(TokenType::Semicolon, "Expected ';' after return value.");
            self.emit_instruction(Instruction::Ret)
        }
    }

    /// Parses and generates code for while statement
    fn while_statement(&mut self) {
        // Get start offset of the loo[]
        let loop_start = self.current_chunk().count();

        // Consume the condition
        self.consume(TokenType::OpenParen, "Expected '(' after 'while'.");
        self.expression();
        self.consume(TokenType::CloseParen, "Expected ')' after condition.");

        let exit_jump = self.emit_jump(Instruction::JmpNz);

        self.emit_instruction(Instruction::Pop);
        self.statement();

        self.emit_loop(loop_start);
        
        self.patch_jump(exit_jump);
        self.emit_instruction(Instruction::Pop)
    }

    fn sync(&mut self) {
        self.panic_mode = false;

        while self.current.t_type != TokenType::Eof {
            if self.previous.t_type == TokenType::Semicolon { return };

            match self.current.t_type {
                TokenType::Class => { return },
                TokenType::Func => { return },
                TokenType::If => { return },
                TokenType::While => { return },
                TokenType::Print => { return },
                TokenType::Return => { return },
                _ => ()
            }

            self.advance();
        }
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