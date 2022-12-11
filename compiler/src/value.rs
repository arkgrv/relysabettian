use std::collections::HashMap;
use crate::opcodes::Opcode;

/// Variant type to store VM values
#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Null,
    Upvalue(Box<Upvalue>),
    Class(Box<Class>),
    Instance(Box<Instance>),
    MemberFunc(Box<MemberFunc>),
    Closure(Box<Closure>),
    Function(Box<Function>),
    Bool(bool),
    Double(f64),
    String(String),
}

/// Virtual Machine memory chunk
#[derive(Clone, PartialEq, Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub consts: Vec<Value>,
    pub lines: Vec<i32>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::<u8>::new(),
            consts: Vec::<Value>::new(),
            lines: Vec::<i32>::new(),
        }
    }

    /// Returns number of data in chunk
    pub fn count(&self) -> usize {
        self.code.len()
    }

    /// Writes a byte into VM memory
    pub fn write_byte(&mut self, byte: u8, line: i32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    /// Write an opcode into VM memory
    pub fn write_opcode(&mut self, opcode: Opcode, line: i32) {
        self.code.push(opcode as u8);
        self.lines.push(line);
    }

    /// Add constant to VM memory
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.consts.push(value);
        self.consts.len() - 1
    }

    pub fn disassemble(&mut self, name: String) {
        println!("== {} ==", name);
        let mut i = 0;
        while i < self.code.len() as i32 {
            i = self.disas_instruction(i);
        }
    }

    /// Disassemble and print (debug) instruction
    fn disas_instruction(&mut self, offset: i32) -> i32 {
        let mut offset = offset;

        print!("{:04x }", offset);
        if offset > 0i32 && self.lines[offset as usize] == self.lines[(offset - 1) as usize] {
            print!("  | ");
        } else {
            print!("{:4x}", self.lines[offset as usize]);
        }

        let instruction: Opcode = self.code[offset as usize].into();
        match instruction {
            Opcode::Constant => constant_instruction("OP_CONSTANT", self, offset),
            Opcode::Nop => simple_instruction("OP_NOP", offset),
            Opcode::True => simple_instruction("OP_TRUE", offset),
            Opcode::False => simple_instruction("OP_FALSE", offset),
            Opcode::Pop => simple_instruction("OP_POP", offset),
            Opcode::GetLocal => byte_instruction("OP_GET_LOCAL", self, offset),
            Opcode::GetGlobal => constant_instruction("OP_GET_GLOBAL", self, offset),
            Opcode::DefineGlobal => constant_instruction("OP_DEFINE_GLOBAL", self, offset),
            Opcode::SetLocal => byte_instruction("OP_SET_LOCAL", self, offset),
            Opcode::SetGlobal => constant_instruction("OP_SET_GLOBAL", self, offset),
            Opcode::GetUpvalue => byte_instruction("OP_GET_UPVALUE", self, offset),
            Opcode::SetUpvalue => byte_instruction("OP_SET_UPVALUE", self, offset),
            Opcode::GetProperty => constant_instruction("OP_GET_PROPERTY", self, offset),
            Opcode::SetProperty => constant_instruction("OP_SET_PROPERTY", self, offset),
            Opcode::GetSuper => constant_instruction("OP_GET_SUPER", self, offset),
            Opcode::Equal => simple_instruction("OP_EQUAL", offset),
            Opcode::Greater => simple_instruction("OP_GREATER", offset),
            Opcode::Less => simple_instruction("OP_LESS", offset),
            Opcode::Add => simple_instruction("OP_ADD", offset),
            Opcode::Sub => simple_instruction("OP_SUB", offset),
            Opcode::Mul => simple_instruction("OP_MUL", offset),
            Opcode::Div => simple_instruction("OP_DIV", offset),
            Opcode::BwAnd => simple_instruction("OP_BW_AND", offset),
            Opcode::BwOr => simple_instruction("OP_BW_OR", offset),
            Opcode::BwXor => simple_instruction("OP_BW_XOR", offset),
            Opcode::BwNot => simple_instruction("OP_BW_NOT", offset),
            Opcode::Not => simple_instruction("OP_NOT", offset),
            Opcode::Neg => simple_instruction("OP_NEG", offset),
            Opcode::Print => simple_instruction("OP_PRINT", offset),
            Opcode::Jmp => jump_instruction("OP_JMP", 1, self, offset),
            Opcode::JmpNz => jump_instruction("OP_JMP_NZ", 1, self, offset),
            Opcode::Loop => jump_instruction("OP_JMP", -1, self, offset),
            Opcode::Call => byte_instruction("OP_CALL", self, offset),
            Opcode::Invoke => invoke_instruction("OP_INVOKE", self, offset),
            Opcode::SuperInvoke => invoke_instruction("OP_SUPER_INVOKE", self, offset),
            Opcode::Closure => {
                offset += 1;
                let constant = self.code[offset as usize];
                let value = self.consts[constant as usize].clone();

                print!("{:<16} {:4x}", "OP_CLOSURE", constant);
                println!("{:?}", value);

                let function: Box<Function>;
                match value {
                    Value::Function(func) => function = func,
                    _ => panic!("Error: this is not a function!"),
                };
                
                for _j in 0..function.upvalue_count {
                    let is_local = self.code[offset as usize];
                    offset += 1;
                    let index = self.code[offset as usize];
                    offset += 1;

                    println!("{:04x}     |                {} {}",
                        offset - 2, if is_local == 1 { "local" } else { "upvalue" }, index);
                }

                return offset;
            },
            Opcode::CloseUpvalue => simple_instruction("OP_CLOSE_UPVALUE", offset),
            Opcode::Ret => simple_instruction("OP_RET", offset),
            Opcode::Class => constant_instruction("OP_CLASS", self, offset),
            Opcode::Inherit => simple_instruction("OP_INHERIT", offset),
            Opcode::MembFunc => constant_instruction("OP_MEMBER_FUNC", self, offset)
        }
    }
}

fn simple_instruction(name: &str, offset: i32) -> i32 {
    println!("{}", name);
    offset + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: i32) -> i32 {
    let offset = (offset + 1) as usize;
    let constant = chunk.code[offset] as usize;
    print!("{:<16} {:4x} '", name, constant);
    print!("{:?}", chunk.consts[constant]);
    println!("'");
    (offset + 2) as i32
}

fn invoke_instruction(name: &str, chunk: &Chunk, offset: i32) -> i32 {
    let offset = offset as usize;
    let constant = chunk.code[offset + 1];
    let arg_count = chunk.code[offset + 2];

    print!("{:<16} ({} args) {:4x} '", name, arg_count, constant);
    println!("{:?}'", chunk.consts[constant as usize]);

    (offset + 3) as i32
}

fn jump_instruction(name: &str, sign: i32, chunk: &Chunk, offset: i32) -> i32 {
    let offset = offset as usize;
    let jump = ((chunk.code[offset + 1] as u16) << 8u8) as u16;

    println!("{:<16} {:4x} -> {}", name, offset, offset + 3 + (sign * jump as i32) as usize);

    (offset + 3) as i32
}

fn byte_instruction(name: &str, chunk: &Chunk, offset: i32) -> i32 {
    let slot = chunk.code[(offset + 1) as usize];
    println!("{:<16} {:4x}", name, slot);

    offset + 2
}

/// Representation of an Upvalue
#[derive(Clone, PartialEq, Debug)]
pub struct Upvalue {
    pub location: *mut Value,
    pub closed: Value,
    pub next: *mut Value,
}

impl Upvalue {
    /// Create a new Upvalue struct
    pub fn new(slot: *mut Value) -> Upvalue {
        Upvalue {
            location: slot,
            closed: Value::Null,
            next: std::ptr::null_mut(),
        }
    }
}

/// VM class representation
#[derive(Clone, PartialEq, Debug)]
pub struct Class {
    pub name: String,
    pub memb_funcs: HashMap<String, Closure>,
}

impl Class {
    /// Create a new Class struct
    pub fn new(name: String) -> Class {
        Class {
            name: name.clone(),
            memb_funcs: HashMap::<String, Closure>::new(),
        }
    }
}

/// VM Instance representation
#[derive(Clone, PartialEq, Debug)]
pub struct Instance {
    pub class: Class,
    pub fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: Class) -> Instance {
        Instance {
            class: class.clone(),
            fields: HashMap::<String, Value>::new(),
        }
    }
}

/// VM Member function representation
#[derive(Clone, PartialEq, Debug)]
pub struct MemberFunc {
    pub receiver: Instance,
    pub member_func: Closure,
}

impl MemberFunc {
    pub fn new(receiver: Instance, member_func: Closure) -> MemberFunc {
        MemberFunc {
            receiver: receiver.clone(),
            member_func: member_func.clone(),
        }
    }
}

/// VM Closure representation
#[derive(Clone, PartialEq, Debug)]
pub struct Closure {
   pub function: Function,
   upvalues: Vec<Upvalue>,
}

impl Closure {
    pub fn new(function: Function) -> Closure {
        let mut c = Closure {
            function: function.clone(),
            upvalues: Vec::<Upvalue>::new()
        };
        c.upvalues.reserve(function.upvalue_count);

        c
    }
}

/// VM Function representation
#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub arity: usize,
    pub upvalue_count: usize,
    pub name: String,
    pub chunk: Chunk,
}

impl Function {
    pub fn new(arity: usize, name: String) -> Function {
        Function {
            arity,
            upvalue_count: 0usize,
            name: name.clone(),
            chunk: Chunk::new(),
        }
    }
}
