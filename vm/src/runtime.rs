use std::collections::HashMap;

use compiler::value::{ValueType, UpvalueObj, NativeFuncObj, InstanceObj, ClassObj, ClosureObj, BoundFuncObj};

use crate::common::{CallFrame, CallVisitor};

/// Maximum number of stack frames
pub const FRAMES_MAX: usize = 64_usize;
/// Maximum size of system stack
pub const STACK_MAX: usize = 64_usize * (language::common::UINT8_COUNT as usize);
/// Virtual machine init string
pub const INIT_STRING: &str = "init";

/// Virtual Machine implementation. This is what actually
/// executes the code
pub struct VirtualMachine {
    pub stack: Vec<Box<ValueType>>,
    pub frames: Vec<Box<CallFrame>>,
    pub globals: HashMap<String, Box<ValueType>>,
    pub open_upvalues: Option<Box<UpvalueObj>>,
}

impl VirtualMachine {
    /// Creates a new VirtualMachine
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            stack: Vec::<Box<ValueType>>::new(),
            frames: Vec::<Box<CallFrame>>::new(),
            globals: HashMap::<String, Box<ValueType>>::new(),
            open_upvalues: None,
        }
    }

    /// Resets the Virtual machine stack
    fn reset_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.stack.reserve(STACK_MAX);
        self.open_upvalues = None;
    }

    /// Calls a callable value
    /// ### Arguments
    /// * `callee`: object to be called
    /// * `arg_count`: number of arguments
    pub fn call_value(&mut self, callee: Box<ValueType>, arg_count: usize) -> bool {
        let vm = Box::new(self);
        let mut visitor = CallVisitor::new(arg_count, vm);
        return visitor.visit(callee);
    }

    /// Invokes a bound function
    /// ### Arguments
    /// * `name`: name of instance
    /// * `arg_count`: number of arguments
    pub fn invoke(&mut self, name: String, arg_count: usize) -> bool {
        let instance_val = self.peek(arg_count);
        let instance = ValueUtils::get_instance(instance_val);

        if instance.is_none() {
            self.runtime_error(format!("Only instances have bound functions."));
            return false
        }

        let binding = instance.unwrap();
        let found = binding.fields.get(&name);
        if found.is_none() {
            self.runtime_error(format!("Cannot find instance value with name {}.", name));
            return false
        }

        let value = found.unwrap();
        let offset = self.stack.len() - arg_count - 1;
        self.stack[offset] = value.clone();

        self.call_value(value.clone(), arg_count)
    }

    pub fn invoke_from_class(&mut self, class_value: Box<ClassObj>, name: String, arg_count: usize) -> bool {
        let found = class_value.methods.get(&name);
        if found.is_none() {
            self.runtime_error(format!("Undefined property '{}'.", name));
            return false
        }

        let callable = found.unwrap().clone();
        self.call(callable, arg_count)
    }

    /// Binds a function to a classs
    /// ### Arguments
    /// * `class_value`: class to bind with function
    /// * `name`: name of function
    /// * `arg_count`: number of arguments
    fn bind_func(&mut self, class_value: Box<ClassObj>, name: String, arg_count: usize) -> bool {
        let found = class_value.methods.get(&name);
        if found.is_none() {
            self.runtime_error(format!("Undefined property '{}'.", name));
            return false
        }

        let func = found.unwrap().clone();
        let instance_val = self.peek(0);
        let instance = ValueUtils::get_instance(instance_val);
        if instance.is_none() {
            self.runtime_error(format!("Only instances have bound functions."));
            return false;
        }

        let bound = Box::new(BoundFuncObj::new(instance.unwrap(), func.clone()));
        
        self.pop();
        self.push(Box::new(ValueType::BoundFunc(bound)));

        true
    }

    /// Calls a closure
    /// ### Arguments
    /// * `closure`: closure to call
    /// * `arg_count`: number of arguments
    pub fn call(&mut self, closure: Box<ClosureObj>, arg_count: usize) -> bool {
        if arg_count != closure.function.arity {
            self.runtime_error(format!("Expected {} arguments but got {}.",
                                closure.function.arity, arg_count));
            return false
        }

        if self.frames.len() + 1 == FRAMES_MAX {
            self.runtime_error(format!("Stack overflow."));
            return false
        }

        let ip = self.stack.len() - arg_count - 1;
        self.frames.push(Box::new(CallFrame::new(closure, 0_usize, ip)));

        true
    }

    /// Signals runtime error and stops execution
    /// ### Arguments
    /// * `message`: error message
    pub fn runtime_error(&self, message: String) {

    }

    /// Defines a new native function
    /// ### Arguments
    /// * `name`: name of the function
    /// * `function`: actual function instance
    pub fn define_native(&mut self, name: String, function: Box<NativeFuncObj>) {

    }

    /// Defines a new native constant value
    /// ### Arguments
    /// * `name`: name of the constant
    /// * `value`: value of the constant
    pub fn define_native_const(&mut self, name: String, value: Box<ValueType>) {

    }

    /// Pushes a new value to the VM stack
    /// ### Arguments
    /// * `value`: value to push
    pub fn push(&mut self, value: Box<ValueType>) {
        self.stack.push(value)
    }

    /// Pops a value from VM stack
    pub fn pop(&mut self) -> Box<ValueType> {
        let v = self.stack.last().unwrap().clone();
        self.stack.pop();
        v
    }

    /// Peeks at a value from the stack without removing it
    /// ### Arguments
    /// `distance`: distance of value to peek
    pub fn peek(&self, distance: usize) -> Box<ValueType> {
        let offset = self.stack.len() - 1 - distance;
        self.stack[offset].clone()
    }
}

pub struct ValueUtils;

impl ValueUtils {
    /// Converts a value type to an instance
    /// ### Arguments
    /// * `value`: value to convert
    pub fn get_instance(value: Box<ValueType>) -> Option<Box<InstanceObj>> {
        match *value {
            ValueType::Instance(v) => Some(v),
            _ => None,
        }
    }
}
