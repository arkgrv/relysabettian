use std::{cell::RefCell, ops::Deref, rc::Rc};

use compiler::value::{Closure, NativeFunction, Value};

use crate::{common::STACK_MAX, vm::VirtualMachine};

/// Call visitor implementation
pub struct CallVisitor<'a> {
    pub arg_count: usize,
    pub vm: Rc<RefCell<&'a mut VirtualMachine>>,
}

impl CallVisitor<'_> {
    /// Constructs a new Call visitor
    ///
    /// Parameters:
    /// * `arg_count`: number of arguments
    /// * `vm`: virtual machine
    pub fn new(arg_count: usize, vm: Rc<RefCell<&mut VirtualMachine>>) -> CallVisitor {
        CallVisitor {
            arg_count,
            vm: Rc::clone(&vm),
        }
    }

    pub fn visit(&mut self, value: Rc<RefCell<Value>>) -> bool {
        match (*value).borrow().deref() {
            Value::NativeFunction(nf) => self.visit_native_fn(Rc::clone(&nf)),
            Value::Closure(cl) => self.visit_closure(Rc::clone(&cl)),
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
