use std::{rc::Rc, collections::HashMap};

use crate::instruction::Opcode;

/// Maximum number of UINT8 variables
pub const UINT8_COUNT: u16 = u8::MAX as u16 + 1;

/// Represents any valid value in the language's
/// runtime environment.
#[derive(Clone)]
pub enum Value {
    Null,
    String(Rc<String>),
    Double(f64),
    Bool(bool),
    Function(Rc<Function>),
    NativeFunction(Rc<NativeFunction>),
    Closure(Rc<Closure>),
    Upvalue(Rc<Upvalue>),
    Class(Rc<Class>),
    Instance(Rc<Instance>),
    Method(Rc<Method>),
}

/// This structure holds a chunk of memory which is
/// directly related with a specific piece of code.
#[derive(Clone)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<i32>,
}

/// Default implementation for struct Chunk
impl Chunk {
    /// Constructs a new Chunk
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    /// Returns a value from the chunk's instruction memory
    /// 
    /// Parameters:
    /// * `offset`: offset of the value
    pub fn get_code(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    /// Sets a specific offset in the instruction memory
    /// to the given value.
    /// 
    /// Parameters:
    /// * `offset`: offset of the value
    /// * `value`: actual value to set
    pub fn set_code(&mut self, offset: usize, value: u8) {
        self.code[offset] = value
    }

    /// Returns a constant (value) from the data block of
    /// this chunk.
    /// 
    /// Parameters:
    /// * `offset`: offset of the constant
    pub fn get_constant(&self, offset: usize) -> Value {
        self.constants[offset].clone()
    }

    /// Writes a byte in the instruction memory.
    /// 
    /// Parameters:
    /// * `byte`: byte value to write
    /// * `line`: line bound to this particular instruction
    pub fn write_byte(&mut self, byte: u8, line: i32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    /// Writes an instruction in the instruction memory.
    /// 
    /// Parameters:
    /// * `instr`: the instruction to write
    /// * `line`: line bound to this particular instruction
    pub fn write_instr(&mut self, instr: Opcode, line: i32) {
        self.write_byte(instr.into(), line)
    }

    /// Adds a new constant to the memory data chunk.
    /// 
    /// Parameters:
    /// * `value`: new value to add
    pub fn add_constant(&mut self, value: &Value) -> usize {
        self.constants.push(value.clone());
        self.constants.len() - 1
    }

    /// Gets the line related to this instruction
    /// 
    /// Parameters:
    /// `instr`: instruction offset
    pub fn get_line(&self, instr: u8) -> i32 {
        self.lines[instr as usize]
    }

    /// Returns the size of the instruction memory
    /// in current chunk.
    pub fn count(&self) -> usize {
        self.code.len()
    }
}

/// Native function type alias
type NativeFn = fn(usize, Rc<Vec<Value>>);

/// Represents a native (runtime environment related)
/// function.
#[derive(Clone)]
pub struct NativeFunction {
    pub function: NativeFn,
}

/// Represents an upvalue in the language
#[derive(Clone)]
pub struct Upvalue {
    pub location: *mut Value,
    pub closed: Value,
    pub next: Option<Rc<Value>>,
}

impl Upvalue {
    /// Constructs a new Upvalue
    /// 
    /// Parameters:
    /// * `slot`: pointer to the value's location
    pub fn new(slot: *mut Value) -> Upvalue {
        Upvalue {
            location: slot,
            closed: Value::Null,
            next: None,
        }
    }
}

/// Represents a class in the language
#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Closure>,
}

impl Class {
    /// Constructs a new Class
    /// 
    /// Parameters:
    /// * `name`: name of the class
    pub fn new(name: String) -> Class {
        Class {
            name,
            methods: HashMap::new(),
        }
    }
}

/// Represents an instance of a class
#[derive(Clone)]
pub struct Instance {
    pub class: Rc<Class>,
    pub fields: HashMap<String, Value>,
}

impl Instance {
    /// Constructs a new Instance
    /// 
    /// Parameters:
    /// * `class`: instantiated class
    pub fn new(class: Rc<Class>) -> Instance {
        Instance {
            class: class.clone(),
            fields: HashMap::new(),
        }
    }
}

/// Represents a class' method
#[derive(Clone)]
pub struct Method {
    pub receiver: Rc<Instance>,
    pub method: Rc<Closure>,
}

impl Method {
    /// Constructs a new class method
    /// 
    /// Parameters:
    /// * `receiver`: instance bound to this method
    /// * `method`: closure related to this method
    pub fn new(receiver: Rc<Instance>, method: Rc<Closure>) -> Method {
        Method {
            receiver: receiver.clone(),
            method: method.clone(),
        }
    }
}

/// Represents a function in the language
#[derive(Clone)]
pub struct Function {
    pub arity: usize,
    pub upvalue_count: usize,
    pub name: String,
    pub chunk: Chunk,
}

impl Function {
    /// Constructs a new function
    /// 
    /// Parameters:
    /// * `arity`: number of arguments
    /// * `name`: name of the function 
    pub fn new(arity: usize, name: String) -> Function {
        Function {
            arity,
            upvalue_count: 0,
            name: name.clone(),
            chunk: Chunk::new(),
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

/// Represents a function closure
#[derive(Clone)]
pub struct Closure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Option<Rc<Upvalue>>>,
}

impl Closure {
    /// Constructs a new Closure
    /// 
    /// Parameters:
    /// * `function`: function bound to this closure
    pub fn new(function: Rc<Function>) -> Closure {
        let mut cl = Closure {
            function: function.clone(),
            upvalues: Vec::new(),
        };

        cl.upvalues.resize(function.upvalue_count, None);
        cl
    }
}

/// Visits a value printing on standard output
pub trait OutputVisitor {
    /// Visits the current value
    fn visit(&self);
}

/// Implementation of OutputVisitor for Value types
impl OutputVisitor for Value {
    fn visit(&self) {
        match self {
            Value::Null => print!("null"),
            Value::Double(d) => print!("{}", d),
            Value::Bool(b) => print!("{}", if *b { "true" } else { "false" }),
            Value::String(s) => print!("{}", s),
            Value::Function(f) => {
                if f.name.is_empty() {
                    print!("<main>");
                } else {
                    print!("<fn {}>", f.name);
                }
            },
            Value::NativeFunction(_) => print!("<native fn>"),
            Value::Closure(c) => Value::Function(c.function.clone()).visit(),
            Value::Upvalue(_) => print!("upvalue"),
            Value::Instance(i) => print!("{} instance", i.class.name),
            Value::Method(m) => Value::Function(m.method.function.clone()).visit(),
            _ => panic!("Unknown value!")
        }
    }
}

/// Visits a value checking its falsiness.
pub trait FalseVisitor {
    /// Visits the current value
    fn visit_false(&self) -> bool;
}

/// Implementation of FalseVisitor for boolean values
impl FalseVisitor for bool {
    fn visit_false(&self) -> bool {
        !*self
    }
}

impl FalseVisitor for Value {
    fn visit_false(&self) -> bool {
        matches!(self, Value::Null)
    }
}
