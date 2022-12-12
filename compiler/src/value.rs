use std::collections::HashMap;
use crate::bytecode::Instruction;

/// Represents a value in the language.
/// A value can be any of the following types:
/// * `Null` - neutral value
/// * `Number` - floating point 64 bit
/// * `Bool` - boolean value
/// * `String` - dynamic string
/// * `Func` - language function
/// * `NativeFunc` - VM function bound with language function
/// * `Closure` - closure in language
/// * `Upvalue` - free value captured by function
/// * `Class` - language class
/// * `Instance` - instance of a language class
/// * `BoundFunc` - language class instance function
#[derive(Clone)]
pub enum ValueType {
    Null,
    Number(f64),
    Bool(bool),
    String(String),
    Func(Box<FuncObj>),
    NativeFunc(Box<NativeFuncObj>),
    Closure(Box<ClosureObj>),
    Upvalue(Box<UpvalueObj>),
    Class(Box<ClassObj>),
    Instance(Box<InstanceObj>),
    BoundFunc(Box<BoundFuncObj>),
}

/// Represents a chunk of code (instruction + data)
/// of a given block of code in the language
#[derive(Clone)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Box<ValueType>>,
    lines: Vec<i32>,
}

impl Chunk {
    /// Create a new chunk on the stack
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::<u8>::new(),
            constants: Vec::<Box<ValueType>>::new(),
            lines: Vec::<i32>::new(),
        }
    }

    /// Returns a single byte of the code chunk
    /// ### Arguments
    /// * `offset` - A usize that holds the offset of the value
    pub fn get_code(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    /// Sets the value of a single byte of the code chunk
    /// ### Arguments
    /// * `offset` - A usize that holds the offset of the value
    /// * `value` - The value to write at the offset
    pub fn set_code(&mut self, offset: usize, value: u8) {
        self.code[offset] = value
    }

    /// Returns a constant from the chunk storage
    /// ### Arguments
    /// * `constant` - Index of the constant to get
    pub fn get_constant(&self, constant: usize) -> &ValueType {
        &self.constants[constant]
    }

    /// Writes a byte value into the chunk instruction
    /// memory
    /// ### Arguments
    /// * `value` - Value of the byte to write
    /// * `line` - Line bound with the specific value
    pub fn write_byte(&mut self, value: u8, line: i32) {
        self.code.push(value);
        self.lines.push(line)
    }

    /// Writes an instruction into the chunk instruction
    /// memory
    /// ### Arguments
    /// * `value` - Instruction opcode
    /// * `line` - Line bound with instruction
    pub fn write_instr(&mut self, value: Instruction, line: i32) {
        self.write_byte(value as u8, line)
    }

    /// Returns the line corresponding with the specified
    /// instruction
    /// ### Arguments
    /// * `instruction` - Instruction with corresponding line
    pub fn get_line(&self, instruction: usize) -> i32 {
        self.lines[instruction]
    }
}

/// Describes a native function, which is a language function
/// that is directly bound with a VM function
type NativeFunc = fn(usize, Vec::<ValueType>) -> ValueType;

/// Effective language representation of a native
/// function
#[derive(Clone)]
pub struct NativeFuncObj {
    pub function: Box<NativeFunc>,
}

impl NativeFuncObj {
    /// Creates a new Native Function object
    /// ### Arguments
    /// * `function` - A fn(usize, Vec\<ValueType\>) -> ValueType
    pub fn new(function: Box<NativeFunc>) -> NativeFuncObj {
        NativeFuncObj { function }
    }
}

/// Language implementation of a function upvalue
#[derive(Clone)]
pub struct UpvalueObj {
    pub location: Box<ValueType>,
    pub closed: Box<ValueType>,
    pub next: Option<Box<UpvalueObj>>,
}

impl UpvalueObj {
    /// Creates a new Upvalue
    /// ### Arguments
    /// * `location`: A Value type pointer (Box) referring to the location of this value
    /// * `closed`: the value of a corresponding closed upvalue
    /// * `
    pub fn new(location: Box<ValueType>) -> UpvalueObj {
        UpvalueObj {
            location,
            closed: Box::new(ValueType::Null),
            next: None
        }
    }
}

/// Language class object
#[derive(Clone)]
pub struct ClassObj {
    pub name: String,
    pub methods: HashMap<String, Box<ClosureObj>>,
}

impl ClassObj {
    /// Creates a new Class object
    /// ### Arguments
    /// * `name`: a string holding the name of the class
    pub fn new(name: String) -> ClassObj {
        ClassObj {
            name,
            methods: HashMap::<String, Box<ClosureObj>>::new()
        }
    }
}

/// Language class instance object
#[derive(Clone)]
pub struct InstanceObj {
    pub class: Box<ClassObj>,
    pub fields: HashMap<String, Box<ValueType>>,
}

impl InstanceObj {
    /// Creates a new instance object
    /// ### Arguments
    /// * `class`: holds a reference to the class this value instances
    pub fn new(class: Box<ClassObj>) -> InstanceObj {
        InstanceObj {
            class,
            fields: HashMap::<String, Box<ValueType>>::new()
        }
    }
}

/// Bound function (method) language object
#[derive(Clone)]
pub struct BoundFuncObj {
    pub receiver: Box<InstanceObj>,
    pub function: Box<ClosureObj>,
}

impl BoundFuncObj {
    /// Creates a new Bound function object
    /// ### Arguments
    /// * `receiver`: instance to which this function is bound to
    /// * `function`: function object reference
    pub fn new(receiver: Box<InstanceObj>, function: Box<ClosureObj>) -> BoundFuncObj {
        BoundFuncObj {
            receiver,
            function,
        }
    }
}

/// Language function object
#[derive(Clone)]
pub struct FuncObj {
    pub arity: usize,
    pub upvalue_count: usize,
    name: String,
    chunk: Chunk,
}

impl FuncObj {
    /// Creates a new Function object
    /// ### Arguments
    /// * `arity` - arity of the function (number of arguments)
    /// * `name` - name of the function
    pub fn new(arity: usize, name: String) -> FuncObj {
        FuncObj {
            arity,
            upvalue_count: 0,
            name,
            chunk: Chunk::new(),
        }
    }

    /// Returns the name of this function
    pub fn get_name(&self) -> &String {
        &self.name
    }

    /// Returns the chunk associated to this function
    pub fn get_chunk(&self) -> &Chunk {
        &self.chunk
    }

    /// Returns a piece of code from this function
    /// ### Arguments
    /// * `offset` - Offset of the code in function
    pub fn get_code(&self, offset: usize) -> u8 {
        self.chunk.code[offset]
    }

    /// Returns a constant value from this function's memory
    /// ### Arguments
    /// * `constant` - usize offset of the desired constant
    pub fn get_const(&self, constant: usize) -> &ValueType {
        self.chunk.get_constant(constant)
    }
}

/// Language closure object
#[derive(Clone)]
pub struct ClosureObj {
    pub function: Box<FuncObj>,
    pub upvalues: Vec<Box<UpvalueObj>>,
}

impl ClosureObj {
    /// Creates a new closure object
    /// ### Arguments
    /// `function`: a function reference to bind to this closure
    pub fn new(function: Box<FuncObj>) -> ClosureObj {
        let mut closure = ClosureObj {
            function: function.clone(),
            upvalues: Vec::<Box<UpvalueObj>>::new(),
        };
        closure.upvalues.reserve(function.upvalue_count);

        closure
    }
}

/// Stub struct for output management
pub struct OutputManager;
impl OutputManager {
    /// Prints values by their type
    /// ### Arguments
    /// * `value`: ValueType to be printed
    pub fn print_value(value: &ValueType) {
        match value {
            ValueType::Null => print!("null"),
            ValueType::Number(n) => print!("{}", n),
            ValueType::Bool(b) => print!("{}", b),
            ValueType::String(str) => print!("{}", str),
            ValueType::Func(func) => OutputManager::print_func(func),
            ValueType::NativeFunc(_) => print!("<native fn>"),
            ValueType::Closure(c) => OutputManager::print_func(&c.function),
            ValueType::Upvalue(u) => print!("upvalue"),
            ValueType::Class(cl) => print!("{}", cl.name),
            ValueType::Instance(inst) => 
                print!("{} instance", inst.class.name),
            ValueType::BoundFunc(bf) => OutputManager::print_func(&bf.function.function),
        }
    }

    /// Prints a function
    /// ### Arguments
    /// * `func`: reference to boxed function
    fn print_func(func: &Box<FuncObj>) {
        if func.get_name().is_empty() {
            print!("<main>")
        } else {
            print!("<fn {}>", func.get_name())
        }
    }
}

/// Stub FalseVisitor struct for visitor implementation
pub struct FalseVisitor;
impl FalseVisitor {
    /// Visits a value and returns its boolean 
    /// correspondant
    /// ### Arguments
    /// * `value`: reference to ValueType
    pub fn visit(value: &ValueType) -> bool {
        match value {
            ValueType::Bool(b) => !b,
            ValueType::Null => true,
            _ => false
        }
    }
}
