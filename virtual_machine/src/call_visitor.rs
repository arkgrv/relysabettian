use std::{rc::Rc, cell::RefCell, ops::Deref};

use compiler::value::{Value, NativeFunction, Closure};

use crate::{common::STACK_MAX, vm::VirtualMachine};

/// Call visitor implementation
pub struct CallVisitor<'a> {
    pub arg_count: usize,
    pub vm: &'a mut VirtualMachine,
}

impl CallVisitor<'_> {
    /// Constructs a new Call visitor
    ///
    /// Parameters:
    /// * `arg_count`: number of arguments
    /// * `vm`: virtual machine
    pub fn new(arg_count: usize, vm: &mut VirtualMachine) -> CallVisitor<'_> {
        CallVisitor {
            arg_count,
            vm,
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
        let last = self.vm.frames.len() - 1;
        let new_size = self.vm.frames.len() - self.arg_count - 1;

        let args = &self.vm.stack[last - self.arg_count..=last];
        let result = native.deref().borrow().function.deref().borrow()(self.arg_count, args.to_vec());

        self.vm.stack.resize(new_size, Value::Null);
        self.vm.stack.reserve(STACK_MAX);
        self.vm.push(result);

        true
    }

    fn visit_closure(&mut self, closure: Closure) -> bool {
        self.vm.call(Rc::clone(&closure), self.arg_count)
    }
}
