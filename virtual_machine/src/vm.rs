use std::{cell::RefCell, collections::HashMap, rc::Rc, ops::Deref};

use compiler::value::{Class, Closure, Instance, Method, Upvalue, Value};

use crate::{
    call_visitor::CallVisitor,
    common::{CallFrame, InterpretResult, STACK_MAX},
};

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

    /// Calls a callable value (might be a function, method or native function)
    ///
    /// Parameters:
    /// * `callee`: object being called
    /// * `arg_count`: number of arguments
    pub fn call_value(&mut self, callee: Rc<RefCell<Value>>, arg_count: usize) -> bool {
        let mut visitor = CallVisitor::new(arg_count, Rc::new(RefCell::new(self)));
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
        let value = instance.fields.get(&name);

        let len = self.stack.len();
        if value.is_some() {
            self.stack[len - arg_count - 1] = value.unwrap().clone();
            return self.call_value(Rc::new(RefCell::new(value.unwrap().clone())), arg_count);
        }

        self.invoke_from_class(Rc::clone(&instance.class), name, arg_count)
    }

    /// Invokes a method from a class
    ///
    /// * `class`: class to use
    /// * `name`: name of the method
    /// * `arg_count`: number of arguments
    pub fn invoke_from_class(
        &mut self,
        class: Rc<RefCell<Class>>,
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
        self.call(Rc::new(method.clone()), arg_count)
    }

    /// Binds a method to a class
    /// 
    /// Parameters:
    /// * `class_value`: class to bind to 
    /// * `name`: name of the method to bind
    pub fn bind_method(&mut self, class_value: Rc<RefCell<Class>>, name: String) -> bool {
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

        let bound = Rc::new(Method::new(instance.unwrap().clone(), Rc::new(method)));

        self.pop();
        self.push(Value::Method(Rc::clone(&bound)));

        true
    }

    /// Captures an upvalue
    /// 
    /// Parameters:
    /// * `local`: local value to capture
    pub fn capture_upvalue(&mut self, local: Rc<RefCell<Value>>) -> Rc<RefCell<Option<Upvalue>>> {
        let mut prev_upvalue = Rc::new(RefCell::<Option<Upvalue>>::new(None));
        let mut upvalue = self.open_upvalues;

        while !(*upvalue).borrow().is_none() && (*upvalue).borrow().unwrap().location != local {
            prev_upvalue = upvalue;
            upvalue = (*upvalue).borrow().unwrap().next;
        }

        if (*upvalue).borrow().is_none() && (*upvalue).borrow().unwrap().location == local {
            return None;
        }

        let mut new_upvalue = Upvalue::new(Rc::new(RefCell::new(Some((*local).borrow().clone()))));
        new_upvalue.next = Rc::new(RefCell::new(Some(Value::Upvalue(upvalue))));

        if (*prev_upvalue).borrow().is_none() {
            self.open_upvalues = new_upvalue;
        } else {
            (*prev_upvalue).borrow().unwrap().next = new_upvalue;
        }

        new_upvalue
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
