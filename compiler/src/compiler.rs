use std::{cell::RefCell, rc::Rc};

use crate::{
    common::{FunctionType, InternalUpvalue, Local},
    instruction::Opcode,
    parser::Parser,
    value::{Function, UINT8_COUNT},
};

/// Implementation of a simple statements compiler
pub struct Compiler {
    pub parser: Rc<RefCell<Parser>>,
    pub f_type: FunctionType,
    pub function: Rc<Function>,
    pub enclosing: Option<Box<Compiler>>,
    pub locals: Vec<Local>,
    pub upvalues: Vec<InternalUpvalue>,
    pub scope_depth: i32,
}

impl Compiler {
    /// Constructs a new Compiler
    ///
    /// Parameters:
    /// * `parser`: the compiler's internal parser
    /// * `f_type`: type of the function undergoing compilation
    /// * `enclosing`: enclosing (internal scope) compiler
    pub fn new(
        parser: Rc<RefCell<Parser>>,
        f_type: FunctionType,
        enclosing: Option<Box<Compiler>>,
    ) -> Compiler {
        let mut compiler = Compiler {
            parser,
            f_type,
            function: Rc::new(Function::new(0, "".to_string())),
            enclosing,
            locals: Vec::new(),
            upvalues: Vec::new(),
            scope_depth: 0,
        };

        compiler.locals.push(Local::new(
            if f_type == FunctionType::Function {
                "".to_string()
            } else {
                "this".to_string()
            },
            0,
        ));

        if f_type != FunctionType::Main {
            // compiler.function.name = parser.previous.text.clone()
        }

        compiler
    }

    /// Adds a new local value
    ///
    /// Parameters:
    /// * `name`: new local value name
    pub fn add_local(&mut self, name: String) {
        if self.locals.len() == UINT8_COUNT.into() {
            self.parser
                .borrow_mut()
                .error("Functions do not allow more than 255 local variables.");
            return;
        }

        self.locals.push(Local::new(name, -1))
    }

    /// Declares a new variable
    ///
    /// Parameters:
    /// * `name`: name as string
    pub fn declare_variable(&mut self, name: String) {
        if self.scope_depth == 0 {
            return;
        };

        for i in (0..=self.locals.len() - 1).rev() {
            if self.locals[i].depth != -1 && self.locals[i].depth < self.scope_depth {
                break;
            }
            if self.locals[i].name == name {
                self.parser
                    .borrow_mut()
                    .error("Variable already declared in this scope.");
            }
        }

        self.add_local(name);
    }

    /// Marks the current instance as initialized
    pub fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        };
        self.locals.last_mut().unwrap().depth = self.scope_depth
    }

    /// Resolves a local variable
    ///
    /// Parameters:
    /// * `name`: name of the value to resolve (find)
    pub fn resolve_local(&mut self, name: String) -> Option<usize> {
        for i in (0..=self.locals.len()).rev() {
            if self.locals[i].name == name {
                if self.locals[i].depth == -1 {
                    self.parser
                        .borrow_mut()
                        .error("Cannot read local variable when inside initializer.");
                }

                return Some(i);
            }
        }

        None
    }

    /// Resolves an upvalue
    ///
    /// Parameters:
    /// * `name`: name of the upvalue to resolve (find)
    pub fn resolve_upvalue(&mut self, name: String) -> Option<usize> {
        if self.enclosing.is_none() {
            return None;
        }

        let local = self.enclosing.as_mut().unwrap().resolve_local(name.clone());
        if local.is_some() {
            self.enclosing.as_mut().unwrap().locals[local.unwrap()].is_captured = true;
            return self.add_upvalue(local.unwrap() as u8, true);
        }

        let upvalue = self.enclosing.as_mut().unwrap().resolve_upvalue(name);
        if upvalue.is_some() {
            return self.add_upvalue(upvalue.unwrap() as u8, false);
        }

        None
    }

    /// Adds a new upvalue
    ///
    /// Parameters:
    /// * `index`: memory location of upvalue
    /// * `is_local`: locality of upvalue
    pub fn add_upvalue(&mut self, index: u8, is_local: bool) -> Option<usize> {
        for i in 0..self.upvalues.len() {
            if self.upvalues[i].index == index && self.upvalues[i].is_local == is_local {
                return Some(i);
            }
        }

        if self.upvalues.len() == UINT8_COUNT.into() {
            self.parser.borrow_mut().error(
                "Limits of closure variables exceeded! Max amount of closure variables is 255.",
            );
            return None;
        }

        self.upvalues.push(InternalUpvalue::new(index, is_local));
        let upvalue_count = self.upvalues.len();
        self.function.upvalue_count = upvalue_count;

        Some(upvalue_count - 1)
    }

    /// Begins a new scope
    pub fn begin_scope(&mut self) {
        self.scope_depth += 1
    }

    /// Ends the scope removing related values
    pub fn end_scope(&mut self) {
        self.scope_depth -= 1;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                self.parser.borrow_mut().emit_instr(Opcode::CloseUpvalue);
            } else {
                self.parser.borrow_mut().emit_instr(Opcode::Pop);
            }

            self.locals.pop();
        }
    }

    /// Checks if the compiler is currently looking at the local scope
    pub fn is_local(&self) -> bool {
        self.scope_depth > 0
    }
}

/// Compiler for classes
pub struct ClassCompiler {
    pub enclosing: Option<Box<ClassCompiler>>,
    pub has_superclass: bool,
}

impl ClassCompiler {
    /// Constructs a new ClassCompiler
    ///
    /// Parameters:
    /// * `enclosing`: enclosing class compiler
    pub fn new(enclosing: Option<Box<ClassCompiler>>) -> ClassCompiler {
        ClassCompiler {
            enclosing,
            has_superclass: false,
        }
    }
}
