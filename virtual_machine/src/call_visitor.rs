use std::{rc::Rc, cell::RefCell};

use compiler::value::{NativeFunction, Value, Closure};

use crate::{vm::VirtualMachine, common::STACK_MAX};

/// Call visitor implementation
pub struct CallVisitor {
    pub arg_count: usize,
    pub vm: Rc<RefCell<VirtualMachine>>,
}

impl CallVisitor {
    /// Constructs a new Call visitor
    /// 
    /// Parameters:
    /// * `arg_count`: number of arguments
    /// * `vm`: virtual machine
    pub fn new(arg_count: usize, vm: Rc<RefCell<VirtualMachine>>) -> CallVisitor {
        CallVisitor {
            arg_count,
            vm: Rc::clone(&vm),
        }
    }

    pub fn visit(&mut self, value: Value) -> bool {
        match value {
            Value::NativeFunction(nf) => self.visit_native_fn(Rc::clone(&nf)),
            Value::Closure(cl) => self.visit_closure(cl),
            _ => false,
        }
    }

    fn visit_native_fn(&mut self, native: Rc<NativeFunction>) -> bool {
        let last = (*self.vm).borrow().frames.len();
        let new_size = (*self.vm).borrow().frames.len() - self.arg_count - 1;

        let args = &(*self.vm).borrow().stack[last - self.arg_count..=last];
        let result = native.function.clone()(self.arg_count, args.to_vec());
        
        (*self.vm).borrow_mut().stack.resize(new_size, Value::Null);
        (*self.vm).borrow_mut().stack.reserve(STACK_MAX);
        (*self.vm).borrow_mut().push(result);
        
        true
    }

    fn visit_closure(&mut self, closure: Rc<Closure>) -> bool {
        (*self.vm).borrow_mut().call(closure, self.arg_count)
    }
}