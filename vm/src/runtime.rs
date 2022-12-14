use std::collections::HashMap;

use compiler::value::{ValueType, UpvalueObj};

use crate::common::CallFrame;

/// Maximum number of stack frames
pub const FRAMES_MAX: usize = 64_usize;
/// Maximum size of system stack
pub const STACK_MAX: usize = 64_usize * (language::common::UINT8_COUNT as usize);

pub struct VirtualMachine {
    pub stack: Vec<Box<ValueType>>,
    pub frames: Vec<Box<CallFrame>>,
    pub globals: HashMap<String, Box<ValueType>>,
    pub open_upvalues: Box<UpvalueObj>,
}