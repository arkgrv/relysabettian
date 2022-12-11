use core::ffi::NonZero_c_uint;
use std::collections::HashMap;
use language::common;
use compiler::value::{Value, Closure, Upvalue, Class, Instance, MemberFunc};

// Max number of frames
const FRAMES_MAX: usize = 64_usize;
// Max size of stack (in frames)
const STACK_MAX: usize = FRAMES_MAX * common::UINT8_COUNT as usize;
// Default initializer string
const INIT_STRING: String = "init".to_string();

/// Result of interpretation of byte codes
pub enum InterpretResult {
    Ok,
    CompilationError,
    RuntimeError,
}

/// Function call frame
pub struct CallFrame {
    pub closure: Closure,
    pub ip: usize,
    pub stack_offset: i64,
}

pub struct CallVisitor {
    pub arg_count: i32,
    pub vm: *mut VirtualMachine,
}

impl CallVisitor {
    pub fn new(arg_count: i32, vm: *mut VirtualMachine) -> CallVisitor {
        CallVisitor { arg_count, vm }
    }

    //pub fn visit_native(&mut self, native: )

    /// Visit a closure object
    pub fn visit_closure(self, closure: Closure) -> bool {
        self.vm.call(closure, self.arg_count)
    }

    pub fn visit_class(self, class_value: Class) -> bool {
        let val = class_value;
        let len = unsafe { (*self.vm).stack.len() };
        unsafe { (*self.vm).stack[len - (self.arg_count as usize) - 1] = 
            Value::Instance(Box::new(Instance::new(class_value))) };
        
        let funcs = val.memb_funcs;
        let found = funcs.get(&INIT_STRING);

        if found.is_none() {
            unsafe {*self.vm}
                .runtime_error(&format!("Expected 0 arguments but got {}.", self.arg_count));
            return false;
        }

        unsafe {(*self.vm).call(*found.unwrap(), self.arg_count)}
    }

    pub fn visit_member_func(&mut self, bound: MemberFunc) -> bool {
        let size = unsafe { *self.vm }.stack.len();
        unsafe {*self.vm}.stack[size - (self.arg_count as usize) - 1] = Value::Instance(Box::new(bound.receiver));
        unsafe {*self.vm}.call(bound.member_func, self.arg_count)
    }

    fn visit_other<T>(&mut self, value: T) -> bool {
        unsafe {*self.vm}.runtime_error("Only functions and classes can be called.");
        false
    }
}

pub struct VirtualMachine {
    pub stack: Vec<Value>,
    pub frames: Vec<CallFrame>,
    pub globals: HashMap<String, Value>,
    pub open_upvalues: *mut Upvalue,
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        let mut vm = VirtualMachine {
            stack: Vec::<Value>::new(),
            frames: Vec::<CallFrame>::new(),
            globals: HashMap::<String, Value>::new(),
            open_upvalues: std::ptr::null_mut(),
        };

        vm.stack.reserve(STACK_MAX);

        vm
    }

    pub fn runtime_error(&mut self, message: &str) {
        eprintln!("{}", message);

        for i in self.frames.len()..0 {
            let frame = &self.frames[i];
            let function = frame.closure.function;
            let line = function.chunk.lines[frame.ip];
            eprint!("[line {}] in ", line);
            if function.name.is_empty() {
                eprintln!("main code");
            } else {
                eprintln!("{}()", function.name);
            }
        }

        self.reset_stack();
    }

    pub fn interpret(&mut self, source: String) -> InterpretResult {

    }

    pub fn run(&mut self) -> InterpretResult {

    }

    fn reset_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.stack.reserve(STACK_MAX);
        self.open_upvalues = std::ptr::null_mut();
    }

    fn define_native(&mut self) {
        ()
    }

    fn define_native_const(&mut self, name: String, value: Value) {
        self.globals[&name] = value
    }

    fn binary_op(&mut self, op: fn(Value, Value) -> Value) -> bool {
        let b = Self::get_double(*self.peek(0));
        let a = Self::get_double(*self.peek(1));

        if b.is_none() || a.is_none() {
            self.runtime_error("Invalid argument");
            return false;
        }

        self.double_pop_and_push(op(Value::Double(a.unwrap()), Value::Double(a.unwrap())));
        true
    }

    fn double_pop_and_push(&mut self, value: Value) {
        self.pop();
        self.pop();
        self.push(value);
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        let v = *self.stack.last().unwrap();
        self.pop();
        v
    }

    fn peek(&mut self, distance: i32) -> &Value {
        &self.stack[self.stack.len() - 1 - (distance as usize)]
    }

    fn call_value(&mut self, callee: Value, arg_count: i32) -> bool {
        let mut visitor = CallVisitor::new(arg_count, self);
        match callee {
            Value::Closure(closure) => visitor.visit_closure(*closure),
            Value::Class(class) => visitor.visit_class(*class),
            Value::MemberFunc(member_func) => visitor.visit_member_func(*member_func),
            _ => visitor.visit_other(callee),
        }
    }

    fn invoke(&mut self, name: String, arg_count: i32) -> bool {
        let instance = Self::get_instance(*self.peek(arg_count));
        if instance.is_none() {
            self.runtime_error("Only instances have member functions.");
            return false;
        }

        let found = instance.unwrap().fields.get(&name);
        if found.is_some() {
            let value = found.unwrap();
            self.stack[self.stack.len() - (arg_count as usize) - 1] = *value;
            return self.call_value(*value, arg_count);
        }

        self.invoke_from_class(instance.unwrap().class, name, arg_count)
    }

    fn invoke_from_class(&mut self, class_value: Class, name: String, arg_count: i32) -> bool {
        let found = class_value.memb_funcs.get(&name);
        if found.is_none() {
            self.runtime_error(&format!("Undefined property '{}'.", name));
            return false;
        }

        let func = found.unwrap();
        self.call(*func, arg_count)
    }

    fn bind_member_func(&mut self, class_value: Class, name: String) -> bool {
        let found = class_value.memb_funcs.get(&name);
        if found.is_none() {
            self.runtime_error(&format!("Undefined property '{}'.", name));
            return false;
        }

        let func = found.unwrap();
        let instance = Self::get_instance(*self.peek(0));
        if instance.is_none() {
            self.runtime_error("Instance not found!");
            return false;
        }

        let bound = Box::new(MemberFunc::new(instance.unwrap(), *func));

        self.pop();
        self.push(Value::MemberFunc(bound));

        true
    }

    fn capture_upvalue(&mut self, local: *mut Value) -> *mut Upvalue {
        let mut prev_upvalue: *mut Upvalue = std::ptr::null_mut();
        let upvalue = self.open_upvalues;

        unsafe {
            while upvalue != std::ptr::null_mut() && (*upvalue).location > local {
                prev_upvalue = upvalue;
                upvalue = (*upvalue).next as *mut Upvalue;
            }

            if upvalue != std::ptr::null_mut() && (*upvalue).location == local {
                return upvalue;
            }

            let new_upvalue = Upvalue::new(local);
            new_upvalue.next = upvalue as *mut Value;

            if prev_upvalue != std::ptr::null_mut() {
                self.open_upvalues = &mut new_upvalue;
            } else {
                let as_ptr: *mut Upvalue = &mut new_upvalue;
                (*prev_upvalue).next = as_ptr as *mut Value;
            }

            &mut new_upvalue
        }
    }

    fn close_upvalues(&mut self, last: *mut Value) {
        unsafe {
            while self.open_upvalues != std::ptr::null_mut() && (*self.open_upvalues).location >= last {
                let upvalue = self.open_upvalues;
                (*upvalue).closed = *(*upvalue).location;
                (*upvalue).location = &mut (*upvalue).closed;
                self.open_upvalues = (*upvalue).next as *mut Upvalue;
            }
        }
    }

    fn define_member_func(&mut self, name: String) {
        let func = Self::get_closure(*self.peek(0));
        let class = Self::get_class(*self.peek(1));
        class.unwrap().memb_funcs[&name] = func.unwrap();
        self.pop();
    }

    fn call(&mut self, closure: Closure, arg_count: i32) -> bool {
        if arg_count as usize != closure.function.arity {
            self.runtime_error(&format!("Expected {} arguments but got {}.",
                closure.function.arity, arg_count));
            return false;
        }

        if self.frames.len() + 1 == FRAMES_MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }

        self.frames.push(CallFrame { 
            closure,
            ip: 0,
            stack_offset: (self.stack.len() - (arg_count as usize) - 1) as i64
        });

        true
    }

    fn get_instance(value: Value) -> Option<Instance> {
        match value {
            Value::Instance(instance) => Some(*instance),
            _ => None
        }
    }

    fn get_closure(value: Value) -> Option<Closure> {
        match value {
            Value::Closure(closure) => Some(*closure),
            _ => None,
        }
    }

    fn get_class(value: Value) -> Option<Class> {
        match value {
            Value::Class(class) => Some(*class),
            _ => None,
        }
    }

    fn get_double(value: Value) -> Option<f64> {
        match value {
            Value::Double(num) => Some(num),
            _ => None,
        }
    }
}