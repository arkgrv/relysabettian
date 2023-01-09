use std::{collections::HashMap, rc::Rc, cell::RefCell, ops::Deref};

use compiler::value::{ClassRepr, MethodRepr, UpvalueRepr, Value, Class, Closure, Instance};

use crate::{
    call_visitor::CallVisitor,
    common::{CallFrame, InterpretResult, STACK_MAX}, rlinked_list::{RLinkedList, RNode},
};

/// Virtual Machine implementation
pub struct VirtualMachine {
    pub stack: Vec<Value>,
    pub frames: Vec<CallFrame>,
    pub globals: HashMap<String, Value>,
    pub open_upvalues: RLinkedList<UpvalueRepr>,
}

impl VirtualMachine {
    /// Constructs a new Virtual Machine
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
            open_upvalues: RNode::new_empty(),
        }
    }

    /// Resets the stack of the virtual machine
    pub fn reset_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.stack.reserve(STACK_MAX);
        self.open_upvalues = RNode::new_empty();
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

    /// Calls a callable value (might be a function, method or native function)
    ///
    /// Parameters:
    /// * `callee`: object being called
    /// * `arg_count`: number of arguments
    pub fn call_value(&mut self, callee: Rc<RefCell<Value>>, arg_count: usize) -> bool {
        let mut visitor = CallVisitor::new(arg_count, self);
        visitor.visit(callee)
    }

    /// Invokes a callable object
    ///
    /// Parameters:
    /// * `name`: name of the object
    /// * `arg_count`: number of arguments
    pub fn invoke(&mut self, name: String, arg_count: usize) -> bool {
        let found = self.peek(arg_count).clone();
        let value = match found {
            Value::Instance(i) => Some(i),
            _ => None,
        };

        if value.is_none() {
            self.runtime_error("This object is not an instance.");
            return false;
        }

        let instance = value.unwrap();
        let instance = instance.deref().borrow();
        let value = instance.fields.get(&name);

        let len = self.stack.len();
        if value.is_some() {
            self.stack[len - arg_count - 1] = value.unwrap().clone();
            return self.call_value(Rc::new(RefCell::new(value.unwrap().clone())), arg_count);
        }

        self.invoke_from_class(Rc::clone(&instance.deref().class), name, arg_count)
    }

    /// Invokes a method from a class
    ///
    /// * `class`: class to use
    /// * `name`: name of the method
    /// * `arg_count`: number of arguments
    pub fn invoke_from_class(
        &mut self,
        class: Class,
        name: String,
        arg_count: usize,
    ) -> bool {
        let binding = (*class).borrow();

        let found = binding.methods.get(&name);
        if found.is_none() {
            self.runtime_error(&format!("Undefined property '{}'.", name));
            return false;
        }

        let method = found.unwrap();
        self.call(Rc::clone(method), arg_count)
    }

    /// Binds a method to a class
    /// 
    /// Parameters:
    /// * `class_value`: class to bind to 
    /// * `name`: name of the method to bind
    pub fn bind_method(&mut self, class_value: Rc<RefCell<ClassRepr>>, name: String) -> bool {
        let class = (*class_value).borrow();
        let found = class.methods.get(&name);

        if found.is_none() {
            self.runtime_error(&format!("Undefined property '{}'.", name));
            return false;
        }

        let method = found.unwrap().clone();
        let instance = match self.peek(0) {
            Value::Instance(i) => Some(i),
            _ => None,
        };

        let bound = Rc::new(RefCell::new(MethodRepr::new(Rc::clone(instance.unwrap()), Rc::clone(&method))));

        self.pop();
        self.push(Value::Method(Rc::clone(&bound)));

        true
    }

    /// Captures an upvalue
    /// 
    /// Parameters:
    /// * `local`: local value to capture
    pub fn capture_upvalue(&mut self, local: Rc<RefCell<Value>>) -> Rc<RefCell<Option<UpvalueRepr>>> {
        panic!("Not implemented!");
    }

    pub fn close_upvalues(&mut self, last: Rc<RefCell<Value>>) {
        panic!("Not implemented!")
    }

    pub fn define_method(&mut self, name: String) {
        panic!("Not implemented!")
    }

    pub fn call(&mut self, closure: Closure, arg_count: usize) -> bool {
        panic!("Not implemented!")
    }

    pub fn interpret(&mut self, source: String) -> InterpretResult {
        panic!("Not implemented!")
    }

    pub fn runtime_error(&mut self, message: &str) {
        panic!("Not implemented!")
    }
}
