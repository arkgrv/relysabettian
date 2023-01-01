use std::rc::Rc;

/// Describes the different modes of parsing
/// precedence.
#[derive(Copy, Clone)]
pub enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

/// Describes the types of functions allowed in this language
#[derive(Copy, Clone)]
pub enum FunctionType {
    Function,
    Initializer,
    Method,
    Main,
}

/// Parsing function type
pub type ParseFn = fn(bool);

/// Implementation of a basic parsing rule
#[derive(Clone)]
pub struct ParseRule {
    pub prefix: Rc<ParseFn>,
    pub infix: Rc<ParseFn>,
    pub precedence: Precedence,
}

/// Implementation of a local variable reference
#[derive(Clone)]
pub struct Local {
    pub name: String,
    pub depth: i32,
    pub is_captured: bool,
}

impl Local {
    /// Constructs a new Local value
    /// 
    /// Parameters:
    /// * `name`: name as string
    /// * `depth`: scope depth of the value
    pub fn new(name: String, depth: i32) -> Local {
        Local {
            name: name.clone(),
            depth,
            is_captured: false,
        }
    }
}

/// Implementation of an internal compiler and parser
/// upvalue type
#[derive(Clone)]
pub struct InternalUpvalue {
    pub index: u8,
    pub is_local: bool,
}

impl InternalUpvalue {
    /// Constructs a new internal upvalue
    /// 
    /// Parameters:
    /// * `index`: memory location of the upvalue
    /// * `is_local`: locality of the upvalue
    pub fn new(index: u8, is_local: bool) -> InternalUpvalue {
        InternalUpvalue {
            index,
            is_local,
        }
    }
}
