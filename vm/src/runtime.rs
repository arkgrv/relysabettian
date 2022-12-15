use std::collections::HashMap;

use compiler::{value::{ValueType, UpvalueObj, NativeFuncObj, InstanceObj, ClassObj, ClosureObj, BoundFuncObj}, codegen::Parser};

use crate::common::{CallFrame, CallVisitor, InterpretResult};

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

    pub fn interpret(&mut self, source: String) -> InterpretResult {
        let mut parser = Parser::new(source);
        let opt = parser.compile();
        if opt.is_none() {
            return InterpretResult::CompilationError;
        }

        let function = opt.unwrap();
        let closure = Box::new(ClosureObj::new(function));

        self.push(Box::new(ValueType::Closure(closure)));
        self.call(closure, 0);

        self.run();
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
    fn call_value(&mut self, callee: Box<ValueType>, arg_count: usize) -> bool {
        let vm = Box::new(self);
        let mut visitor = CallVisitor::new(arg_count, vm);
        return visitor.visit(callee);
    }

    /// Invokes a bound function
    /// ### Arguments
    /// * `name`: name of instance
    /// * `arg_count`: number of arguments
    fn invoke(&mut self, name: String, arg_count: usize) -> bool {
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

    fn invoke_from_class(&mut self, class_value: Box<ClassObj>, name: String, arg_count: usize) -> bool {
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

    /// Defines a new bound function
    /// ### Arguments
    /// * `name`: name of new bound function
    fn define_bound_func(&mut self, name: String) {
        let func_value = self.peek(0);
        let value = ValueUtils::get_closure(func_value);
        let class_value = self.peek(1);
        let class = ValueUtils::get_class(class_value);

        if value.is_none() {
            self.runtime_error(format!("Bound functions are valid only for instances."));
            return;
        }

        if class.is_none() {
            self.runtime_error(format!("Bound functions are valid only for instances."));
            return;
        }

        class.unwrap().methods.insert(name.clone(), value.unwrap());
        self.pop();
    }

    /// Captures a list of upvalues
    /// ### Arguments
    /// * `local`: local value to capture
    fn capture_upvalue(&mut self, local: Box<ValueType>) -> Box<UpvalueObj> {
        let mut prev_upvalue: Option<Box<UpvalueObj>> = None;
        let mut upvalue = self.open_upvalues.clone();

        while upvalue.is_some() && upvalue.as_ref().unwrap().location != local {
            prev_upvalue = upvalue.clone();
            upvalue = upvalue.unwrap().next;
        }

        if upvalue.is_some() && upvalue.as_ref().unwrap().location == local {
            return upvalue.unwrap().clone();
        }

        let mut new_upvalue = upvalue.clone().unwrap();
        new_upvalue.next = upvalue;

        if prev_upvalue.is_none() {
            self.open_upvalues = Some(new_upvalue.clone());
        } else {
            prev_upvalue.unwrap().next = Some(new_upvalue.clone());
        }

        new_upvalue.clone()
    }

    /// Closes all upvalues
    /// ### Arguments
    /// * `last`: last upvalue in the list
    fn close_upvalues(&mut self, last: Box<ValueType>) {
        while self.open_upvalues.is_some() && self.open_upvalues.as_ref().unwrap().location != last {
            let upvalue = &mut self.open_upvalues;
            upvalue.as_mut().unwrap().closed = upvalue.as_mut().unwrap().location.clone();
            upvalue.as_mut().unwrap().location = upvalue.as_mut().unwrap().closed.clone();
            self.open_upvalues = upvalue.as_mut().unwrap().next.clone();

        }
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

    fn run(&mut self) -> InterpretResult {

    }

    fn read_byte(&mut self) -> u8 {
        let offset = self.frames.last().unwrap().ip + 1;
        self.frames.last().unwrap().ip += 1;
        self.frames.last().unwrap().closure.function.get_code(offset)
    }

    fn read_const(&mut self) -> Box<ValueType> {
        let constant = self.read_byte();
        Box::new(*self.frames.last().unwrap().closure.function.get_const(constant.into()))
    }

    fn read_short(&mut self) -> u16 {
        self.frames.last().unwrap().ip += 2;
        let offset = 
            (self.frames.last().unwrap().ip - 2) << 8 |
            (self.frames.last().unwrap().closure.function.get_code(
                (self.frames.last().unwrap().ip - 1).into()
            ));
        let value = self.frames.last().unwrap().closure.function.get_code(offset)
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

    /// Converts a value type to a bound function
    /// ### Arguments
    /// * `value`: value to convert
    fn get_bound_func(value: Box<ValueType>) -> Option<Box<BoundFuncObj>> {
        match *value {
            ValueType::BoundFunc(bf) => Some(bf),
            _ => None,
        }
    }

    /// Converts a value type to a class object
    /// ### Arguments
    /// * `value`: value to convert
    fn get_class(value: Box<ValueType>) -> Option<Box<ClassObj>> {
        match *value {
            ValueType::Class(cl) => Some(cl),
            _ => None,
        }
    }

    fn get_closure(value: Box<ValueType>) -> Option<Box<ClosureObj>> {
        match *value {
            ValueType::Closure(cl) => Some(cl),
            _ => None,
        }
    }
}
