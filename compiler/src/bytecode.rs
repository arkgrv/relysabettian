/// Representation of virtual CPU instructions. 
/// Can be thought as an high-level macro-assembler
/// for a simple stack-based machine.
#[repr(u8)]
pub enum Instruction {
    /// Constant instruction
    Const,
    /// No operation
    Nop,
    /// Boolean true  
    True,
    /// Boolean false           
    False,
    /// Pop from stack        
    Pop,
    /// Get local value
    GetLoc,
    /// Get global value
    GetGlob,
    /// Define global value  
    DefGlob,
    /// Set local value
    SetLoc,
    /// Set global value
    SetGlob,
    /// Get upvalue value
    GetUVal,
    /// Set upvalue value
    SetUVal,
    /// Get property
    GetProp,
    /// Set property
    SetProp,
    /// Get parent class
    GetParen,
    /// Equality check
    Eq,
    /// Greater than check
    Gt,
    /// Less than check
    Lt,
    /// Arithmetic add with carry
    Add,
    /// Arithmetic sub with carry
    Sub,
    /// Arithmetic multiplication
    Mul,
    /// Arithmetic division
    Div,
    /// Boolean not (negation)
    Not,
    /// Arithmetic negation (*-1)
    Neg,
    /// Standard output
    Out,
    /// Jump control flow
    Jmp,
    /// Jump when false control flow
    JmpNz,
    /// Loop (any loop construct)
    Loop,
    /// Call instruction
    Call,
    /// Invoke method
    Inv,
    /// Invoke parent method
    ParenInv,
    /// Closure
    Clo,
    /// Close upvalue
    CloseUVal,
    /// Return
    Ret,
    /// Class definition
    Class,
    /// Inherit from class
    Inherit,
    /// Instance function (method)
    IFunc,
    /// Bitwise and
    BAnd,
    /// Bitwise or
    BOr,
    /// Bitwise xor
    BXor,
    /// Bitwise not (bit per bit negation)
    BNot,
}

/// Allows conversion from u8 to Instruction
/// using the Into keyword
impl Into<Instruction> for u8 {
    fn into(self) -> Instruction {
        unsafe { std::mem::transmute(self) }
    }
}