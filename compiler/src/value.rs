use std::collections::HashMap;
use crate::opcodes::OpCode;

/// Represents a variant value type, which can
/// hold all the valid types described inside
#[derive(Clone)]
pub enum ValueType {
    Double(f64),
    Bool(bool),
    Null,
    String(String),

}

/// Represents a memory chunk for a specific
/// instance or block of code that is being
/// referred
#[derive(Clone)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<ValueType>,
    lines: Vec<i32>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { 
            code: Vec::<u8>::new(),
            constants: Vec::<ValueType>::new(),
            lines: Vec::<i32>::new(),
        }
    }

    /// Returns an instruction located at offset
    pub fn get_code(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    /// Writes an instruction located at offset
    pub fn set_code(&mut self, offset: usize, value: u8) {
        self.code[offset] = value
    }

    /// Returns a value located at constant
    pub fn get_constant(&self, constant: usize) -> ValueType {
        self.constants[constant].clone()
    }

    /// Writes a byte to the end of the chunk
    pub fn write_byte(&mut self, byte: u8, line: i32) {
        self.code.push(byte);
        self.lines.push(line)
    }

    /// Writes an opcode to the end of the chunk
    pub fn write_op(&mut self, op: OpCode, line: i32) {
        self.write_byte(op as u8, line)
    }

    /// Writes a constant to the end of the chunk
    pub fn add_constant(&mut self, value: ValueType) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    /// Returns the line corresponding to the instruction
    pub fn get_line(&mut self, instruction: usize) -> i32 {
        self.lines[instruction]
    }

    /// Returns the number ot total instructions
    pub fn count(&self) -> usize {
        self.code.len()
    }
}

/// Native function type
pub type NativeFunctionType = fn(i32, Vec::<ValueType>);

/// Represents an upvalue and a possible list of upvalues,
/// which can be used to store temporary upvalues until 
/// closure on them is performed
pub struct UpvalueType<'lt> {
    pub location: Option<&'lt ValueType>,
    pub closed: ValueType,
    pub next: Option<&'lt UpvalueType<'lt>>,
}

impl <'lt> UpvalueType<'lt> {
    pub fn new(location: Option<&'lt ValueType>) -> UpvalueType {
        UpvalueType { location, closed: ValueType::Null, next: None }
    }
}

/// Represents a functional object usable
/// by the language
#[derive(Clone)]
pub struct FunctionType {
    pub arity: i32,
    pub upvalue_count: i32,
    name: String,
    chunk: Chunk,
}

impl FunctionType {
    pub fn new(arity: i32, name: String) -> FunctionType {
        FunctionType {
            arity,
            upvalue_count: 0_i32,
            name,
            chunk: Chunk::new(),
        }
    }

    /// Returns the function name
    pub fn get_name(&self) -> String {
        self.name.clone()
    }

    /// Returns the function's associated data chunk
    pub fn get_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    /// Returns the code located at offset
    pub fn get_code(&self, offset: usize) -> u8 {
        self.chunk.get_code(offset)
    }

    /// Returns the constant located at offset
    pub fn get_const(&self, constant: usize) -> ValueType {
        self.chunk.get_constant(constant)
    }
}

/// Represents a closure (function) for the
/// language
pub struct ClosureType<'lt> {
    pub function: FunctionType,
    pub upvalues: Vec<UpvalueType<'lt>>,
}

impl <'lt> ClosureType<'lt> {
    pub fn new(function: FunctionType) -> ClosureType<'lt> {
        let mut closure = ClosureType {
            function: function.clone(),
            upvalues: Vec::<UpvalueType<'lt>>::new(),
        };

        closure.upvalues.reserve(function.upvalue_count as usize);
        closure
    }
}

/// Represents a class used by language constructs
pub struct ClassType<'lt> {
    pub name: String,
    pub member_funcs: HashMap<String, ClosureType<'lt>>,
}

impl <'lt> ClassType<'lt> {
    pub fn new(name: String) -> ClassType<'lt> {
        ClassType {
            name,
            member_funcs: HashMap::<String, ClosureType<'lt>>::new()
        }
    }
}

/// Represents an instance of a class in the language
pub struct InstanceType<'lt> {
    pub class: ClassType<'lt>,
    pub fields: HashMap<String, ValueType>,
}

impl <'lt> InstanceType<'lt> {
    pub fn new(class: ClassType<'lt>) -> InstanceType<'lt> {
        InstanceType {
            class,
            fields: HashMap::<String, ValueType>::new()
        }
    }
}

/// Represents a member function (method) of a class
pub struct MemberFunctionType<'lt> {
    pub receiver: InstanceType<'lt>,
    pub function: ClosureType<'lt>,
}

impl <'lt> MemberFunctionType<'lt> {
    pub fn new(receiver: InstanceType<'lt>, function: ClosureType<'lt>) -> MemberFunctionType<'lt> {
        MemberFunctionType { receiver, function }
    }
}
