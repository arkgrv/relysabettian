
/// Opcodes supported by JIT
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
    MembFunc,
    BwAnd,
    BwOr,
    BwXor,
    BwNot,
}

/// Using unsafe (but fast) Rust to convert to Opcode from u8
impl Into<Opcode> for u8 {
    fn into(self) -> Opcode {
        let ret = unsafe {
            std::mem::transmute::<u8, Opcode>(self)
        };
        ret
    }
}
