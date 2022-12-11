/// Expresses a Parsing precedence, which is most usually
/// directly related with mathematical precedence of
/// expressions
pub enum Precedence {
    None,
    Assignment,     // Applies to '=' operations
    Or,             // Applies to '||' operations
    And,            // Applies to '&&' operations
    Equality,       // Applies to '==' and '!=' operations
    Comparison,     // Applies to '<', '>', '<=' and '>=' operations
    Term,           // Applies to '+' and '-' operations
    Factor,         // Applies to '/' and '*' operations
    Unary,          // Applies to '.', '()' and '[]' operations
    Primary,        // Applies to raw, first level expressions
}

/// Expresses how a function should be treated:
/// it could either be a function, an initializer,
/// a member function or main-related script code
pub enum FunctionType {
    Function,
    Initializer,
    MemberFunc,
    Main,
}

/// Local value to store on the Virtual Machine
/// stack
pub struct Local {
    pub name: String,
    pub depth: i32,
    pub is_captured: bool,
}

impl Local {
    pub fn new(name: String, depth: i32) -> Local {
        Local { name, depth, is_captured: false }
    }
}

/// Virtual Machine and compiler related upvalue implementation
pub struct Upvalue {
    pub index: u8,
    pub is_local: bool,
}

impl Upvalue {
    pub fn new(index: u8, is_local: bool) -> Upvalue {
        Upvalue { index, is_local }
    }
}