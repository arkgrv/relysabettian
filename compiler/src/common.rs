use std::ops::Add;

use crate::parser::Parser;

/// Describes the different modes of parsing
/// precedence.
#[derive(Copy, Clone, PartialEq, PartialOrd)]
#[repr(i32)]
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

impl Into<Precedence> for i32 {
    /// Converts an i32 into a Precedence enum
    fn into(self) -> Precedence {
        unsafe { std::mem::transmute(self) }
    }
}

impl Into<i32> for Precedence {
    /// Converts a Precedence enum into an i32
    fn into(self) -> i32 {
        self as i32
    }
}

/// Adds addition support to the Precedence enum
impl Add for Precedence {
    fn add(self, rhs: Self) -> Self::Output {
        (self as i32 + rhs as i32).into()
    }

    type Output = Precedence;
}

/// Adds addition support to the Precedence enum
impl Add<i32> for Precedence {
    fn add(self, rhs: i32) -> Self::Output {
        (self as i32 + rhs).into()
    }

    type Output = Precedence;
}

/// Describes the types of functions allowed in this language
#[derive(Copy, Clone, PartialEq)]
pub enum FunctionType {
    Function,
    Initializer,
    Method,
    Main,
}

/// Parsing function type
pub type ParseFn = fn(&mut Parser, bool);

/// Implementation of a basic parsing rule
#[derive(Clone)]
pub struct ParseRule {
    pub prefix: Option<ParseFn>,
    pub infix: Option<ParseFn>,
    pub precedence: Precedence,
}

impl ParseRule {
    /// Constructs a new Parse Rule
    /// 
    /// Parameters:
    /// * `prefix`: prefix parsing function
    /// * `infix`: infix parsing function
    /// * `precedence`: precedence of parsing
    pub fn new(prefix: Option<ParseFn>, infix: Option<ParseFn>, precedence: Precedence) -> ParseRule {
        ParseRule {
            prefix,
            infix,
            precedence
        }
    }
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
