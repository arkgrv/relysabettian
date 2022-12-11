/// Virtual Machine operation code, roughly
/// equal to an instruction set of a CPU,
/// is the code that will be executed by
/// the runtime environment.
#[repr(u8)]
pub enum OpCode {
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
    Sub,
    Mul,
    Div,
    Not,
    Neg,
    Print,
    Jmp,
    JmpNz,
    Loop,
    Call,
    Invoke,
    SuperInvoke,
    Closure,
    CloseUpvalue,
    Ret,
    Class,
    Inherit,
    BwAnd,
    BwOr,
    BwXor,
    BwNot,
}

/// Enables explicit conversion of u8 into OpCode
impl Into<OpCode> for u8 {
    fn into(self) -> OpCode {
        unsafe { std::mem::transmute(self) }
    }
}
