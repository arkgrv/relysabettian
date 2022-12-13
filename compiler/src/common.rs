/// Describes a Parser precedence assignment.
/// Possible precedences are:
/// * `None` - describes no parsing precedence
/// * `Assignment` - precedence is given to assignment
/// * `Or` - precedence is given to or
/// * `And` - precedence is given to and
/// * `Equality` - precedence is given to equality
/// * `Comparison` - precedence is given to comparison
/// * `Term` - precedence is given to a term
/// * `Factor` - precedence is given to a factor
/// * `Unary` - precedence is given to unary operation
/// * `Call` - precedence is given to call
/// * `Primary` - primary expression has precedence
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

/// Expresses the kind (type) of referred function
/// Valid kinds are:
/// * `Function` - standard function
/// * `Initializer`- class initializer
/// * `Bound` - class bound function (method)
/// * `Main` - top-level caller code
#[derive(PartialEq, Clone)]
pub enum FunctionKind {
    Function,
    Initializer,
    Bound,
    Main,
}

/// Local value representation
#[derive(Clone)]
pub struct Local {
    pub name: String,
    pub depth: i64,
    pub is_captured: bool,
}

impl Local {
    /// Creates a new local value
    /// ### Arguments
    /// * `name` - name of local value
    /// * `is_captured` - status of local value
    pub fn new(name: String, depth: i64) -> Local {
        Local {
            name,
            depth,
            is_captured: false,
        }
    }
}

/// Function upvalue representation
#[derive(Clone)]
pub struct Upvalue {
    pub index: u8,
    pub is_local: bool,
}

impl Upvalue {
    /// Creates new upvalue
    /// ### Arguments
    /// * `index` - index of upvalue
    /// * `is_local` - locality of upvalue (true: local, false: global)
    pub fn new(index: u8, is_local: bool) -> Upvalue {
        Upvalue {
            index,
            is_local,
        }
    }
}
