use std::{rc::Rc, cell::RefCell, ops::Deref};

use compiler::value::{Value, NativeFunction, Closure};

use crate::{common::STACK_MAX, vm::VirtualMachine};

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

    pub fn visit(&mut self, value: Rc<RefCell<Value>>) -> bool {
        match value.deref().borrow().deref() {
            Value::NativeFunction(nf) => self.visit_native_fn(Rc::clone(nf)),
            Value::Closure(cl) => self.visit_closure(Rc::clone(cl)),
            _ => false,
        }
    }

    fn visit_native_fn(&mut self, native: NativeFunction) -> bool {
        let last = self.vm.deref().borrow().frames.len() - 1;
        let new_size = self.vm.deref().borrow().frames.len() - self.arg_count - 1;

        let args = &self.vm.deref().borrow().stack[last - self.arg_count..=last];
        let result = native.deref().borrow().function.deref().borrow()(self.arg_count, args.to_vec());

        self.vm.deref().borrow_mut().stack.resize(new_size, Value::Null);
        self.vm.deref().borrow_mut().stack.reserve(STACK_MAX);
        self.vm.deref().borrow_mut().push(result);

        true
    }

    fn visit_closure(&mut self, closure: Closure) -> bool {
        self.vm.deref().borrow_mut().call(Rc::clone(&closure), self.arg_count)
    }
}
