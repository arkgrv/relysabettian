/// This enum contains all the instruction supported by
/// the virtual CPU.
#[repr(u8)]
pub enum Opcode {
    Constant,
    Nop,
    True,
    False,
    Pop,
    GetLocal,
    GetGlobal,
    DefineGlobal,
    SetLocal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    GetSuper,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    BwAnd,
    BwOr,
    BwXor,
    BwNot,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Invoke,
    SuperInvoke,
    Closure,
    CloseUpvalue,
    Return,
    Class,
    Inherit,
    Method,
}

impl Into<Opcode> for u8 {
    /// Converts a u8 byte value into an Opcode using unsafe Rust.
    /// Please beware that any value that is not handled by the enum
    /// will yeld an invalid conversion.
    fn into(self) -> Opcode {
        let result: Opcode = unsafe { std::mem::transmute(self) };
        result
    }
}

impl Into<u8> for Opcode {
    /// Converts the Opcode into a byte
    fn into(self) -> u8 {
        self as u8
    }
}
