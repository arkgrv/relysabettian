use compiler::value::{ClosureObj, ValueType, NativeFuncObj};

use crate::runtime::{VirtualMachine, STACK_MAX};

/// Describes the result of interpretation phase
/// ### Possible values:
/// * `Ok`: interpretation finished correctly
/// * `CompilationError`: error during compilation
/// * `RuntimeError`: error during runtime
pub enum InterpretResult {
    /// Finished correctly
    Ok,
    /// Error at compile time
    CompilationError,
    /// Error at runtime
    RuntimeError,
}

/// Describes a call stack frame related with a
/// specific function call.
pub struct CallFrame {
    pub closure: Box<ClosureObj>,
    pub ip: usize,
    pub stack_offset: usize,
}

/// Describes an implementation of a function call
/// visitor
pub struct CallVisitor {
    pub arg_count: usize,
    pub virtual_machine: Box<VirtualMachine>,
}

impl CallVisitor {
    /// Creates a new CallVisitor
    /// ### Arguments
    /// * `arg_count`: number of arguments
    /// * `virtual_machine`: virtual machine related to this visitor
    pub fn new(arg_count: usize, virtual_machine: Box<VirtualMachine>) -> CallVisitor {
        CallVisitor {
            arg_count,
            virtual_machine,
        }
    }

    /// Visit a value of defined type
    /// ### Arguments
    /// `value`: value to visit
    pub fn visit(&mut self, value: Box<ValueType>) -> bool {
        false
    }

    /// Visit a native function object
    /// ### Arguments
    /// * `func`: pointer to native function to visit
    fn visit_native_function(&mut self, func: Box<NativeFuncObj>) -> bool {
        let function = func.function;

        let end = self.virtual_machine.stack.len();
        let start = self.virtual_machine.stack.len() - self.arg_count - 1;
        let range = self.virtual_machine.stack[start..end].to_vec();

        let result = function(self.arg_count, range);
        
        let size = self.virtual_machine.stack.len() - self.arg_count - 1;
        self.virtual_machine.stack.resize(size, Box::new(ValueType::Null));
        self.virtual_machine.stack.reserve(STACK_MAX);

        true
    }
}