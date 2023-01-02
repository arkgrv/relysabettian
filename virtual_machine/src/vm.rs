use std::{collections::HashMap, rc::Rc, cell::RefCell};

use compiler::value::{Value, Upvalue, Class, Closure};

use crate::common::{CallFrame, STACK_MAX, InterpretResult};

/// Virtual Machine implementation
pub struct VirtualMachine {
    pub stack: Vec<Value>,
    pub frames: Vec<CallFrame>,
    pub globals: HashMap<String, Value>,
    pub open_upvalues: Rc<RefCell<Option<Upvalue>>>,
}

impl VirtualMachine {
    /// Constructs a new Virtual Machine
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
            open_upvalues: Rc::new(RefCell::new(None)),
        }
    }

    /// Resets the stack of the virtual machine
    pub fn reset_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.stack.reserve(STACK_MAX);
        self.open_upvalues = Rc::new(RefCell::new(None));
    }

    /// Pushes a new value onto the stack
    /// 
    /// Parameters:
    /// * `value`: value to push on the stack
    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    /// Removes and returns the last element of the stack
    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    /// Returns the item at the specified distance
    /// 
    /// Parameters:
    /// * `distance`: distance of the item
    pub fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - distance - 1]
    }

    pub fn call_value(&mut self, callee: Rc<RefCell<Value>>, arg_count: usize) -> bool {
        panic!("Not implemented!")
    }

    pub fn invoke(&mut self, name: String, arg_count: usize) -> bool {
        panic!("Not implemented!")
    }

    pub fn invoke_from_class(&mut self, class: Rc<RefCell<Class>>, name: String, arg_count: usize) -> bool {
        panic!("Not implemented!")
    }

    pub fn bind_method(&mut self, class_value: Rc<RefCell<Class>>, name: String) -> bool {
        panic!("Not implemented!")
    }

    pub fn capture_upvalue(&mut self, local: Rc<RefCell<Value>>) -> Upvalue {
        panic!("Not implemented!")
    }

    pub fn close_upvalues(&mut self, last: Rc<RefCell<Value>>) {
        panic!("Not implemented!")
    }

    pub fn define_method(&mut self, name: String) {
        panic!("Not implemented!")
    }

    pub fn call(&mut self, closure: Rc<Closure>, arg_count: usize) -> bool {
        panic!("Not implemented!")
    }

    pub fn interpret(&mut self, source: String) -> InterpretResult {
        panic!("Not implemented!")
    }

    pub fn runtime_error(&mut self, message: &str) {
        panic!("Not implemented!")
    }
}