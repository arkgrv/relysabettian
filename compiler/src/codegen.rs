use language::{token::Token, tokenizer::Tokenizer};

use crate::{common::{FunctionKind, Local, Upvalue}, value::FuncObj, bytecode::Instruction};

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
            self.parser.unwrap().error("Too many local variables in function.");
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
    pub fn resolve_local(&self, name: &String) -> i64 {
        for i in self.locals.len() - 1..0 {
            if name.eq(&self.locals[i].name) {
                if self.locals[i].depth == -1 {
                    self.parser.unwrap().error("Cannot read local variable during initialization.");
                }

                return i as i64;
            }
        }

        -1
    }

    /// Resolves (finds) an upvalue
    /// ### Arguments
    /// * `name`: name of upvalue to find
    pub fn resolve_upvalue(&self, name: &String) -> i64 {
        if self.enclosing.is_none() { return -1 };

        let local = self.enclosing.unwrap().resolve_local(name);
        if local != -1 {
            self.enclosing.unwrap().locals[local as usize].is_captured = true;
            self.add_upvalue(local as u8, true);
        }

        let upvalue = self.enclosing.unwrap().resolve_upvalue(name);
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
            self.parser.unwrap().error("Too many closure variables in this function.");
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
                self.parser.emit_instruction(Instruction::CloseUVal);
            } else {
                self.parser.emit_instruction(Instruction::Pop);
            }
            self.locals.pop();
        }
    }

    /// Checks if current value is local
    pub fn is_local(&self) -> bool {
        self.scope_depth > 0
    }
}

pub struct ClassCompiler {
    enclosing: Option<Box<ClassCompiler>>,
    has_superclass: bool,
}

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

}