use compiler::value::{Closure, NativeFunction, Value};

use crate::{common::STACK_MAX, vm::VirtualMachine};

/// Call visitor implementation
pub struct CallVisitor {
    pub arg_count: usize,
    pub vm: *mut VirtualMachine,
}

impl CallVisitor {
    /// Constructs a new Call visitor
    ///
    /// Parameters:
    /// * `arg_count`: number of arguments
    /// * `vm`: virtual machine
    pub fn new(arg_count: usize, vm: *mut VirtualMachine) -> CallVisitor {
        CallVisitor {
            arg_count,
            vm,
        }
    }

    pub fn visit(&mut self, value: *mut Value) -> bool {
        match unsafe { value.as_ref().unwrap() } {
            Value::NativeFunction(nf) => self.visit_native_fn(*nf),
            Value::Closure(cl) => self.visit_closure(*cl),
            _ => false,
        }
    }

    fn visit_native_fn(&mut self, native: *mut NativeFunction) -> bool {
        let last = unsafe { self.vm.as_ref() }.unwrap().frames.len() - 1;
        let new_size = unsafe { self.vm.as_ref() }.unwrap().frames.len() - self.arg_count - 1;

        let args = &unsafe { self.vm.as_ref() }.unwrap().stack[last - self.arg_count..=last];
        let result = unsafe { *(*native).function }(self.arg_count, args.to_vec());

        unsafe { self.vm.as_ref() }.unwrap().stack.resize(new_size, Value::Null);
        unsafe { self.vm.as_ref() }.unwrap().stack.reserve(STACK_MAX);
        unsafe { self.vm.as_ref() }.unwrap().push(result);

        true
    }

    fn visit_closure(&mut self, closure: *mut Closure) -> bool {
        unsafe { self.vm.as_ref() }.unwrap().call(closure, self.arg_count)
    }
}
