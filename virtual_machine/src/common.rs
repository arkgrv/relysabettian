use compiler::value::{UINT8_COUNT, Closure};

/// Max number of stack frames
pub const FRAMES_MAX: u16 = 64;

/// Max size of stack
pub const STACK_MAX: usize = (FRAMES_MAX * UINT8_COUNT) as usize;

/// Result of runtime phase
pub enum InterpretResult {
    Ok,
    CompilationError,
    RuntimeError,
}

/// Represents a VM call stack frame
pub struct CallFrame {
    pub closure: Closure,
    pub ip: usize,
    pub stack_offset: usize,
}

impl CallFrame {
    /// Constructs a new CallFrame
    /// 
    /// Parameters:
    /// * `closure`: closure value
    /// * `ip`: instruction pointer value
    /// * `stack_offset`: stack offset value
    pub fn new(closure: Closure, ip: usize, stack_offset: usize) -> CallFrame {
        CallFrame {
            closure,
            ip,
            stack_offset,
        }
    }
}
