use crate::bytecode::Instruction;

/// Represents a value in the language.
/// A value can be any of the following types:
/// - Null (neutral value)
/// - Number (floating point 64 bit)
/// - Bool (boolean value)
/// - String (dynamic string)
/// 
pub enum ValueType {
    Number(f64),
    Bool(bool),
    String(String),
    Null,
    // Next values here
}

/// Represents a chunk of code (instruction + data)
/// of a given block of code in the language
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