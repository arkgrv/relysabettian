use std::rc::Rc;

use crate::instruction::{self, Opcode};

/// Represents any valid value in the language's
/// runtime environment.
#[derive(Clone)]
pub enum Value {

}

/// This structure holds a chunk of memory which is
/// directly related with a specific piece of code.
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
    pub fn add_constant(&mut self, value: &Value) {

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