use num::FromPrimitive as _;
use num_derive::{FromPrimitive, ToPrimitive};

use std::convert::TryFrom;

use crate::{trace_available, value::Value};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive)]
pub enum OpCode {
    Nil,
    True,
    False,
    Constant,
    Pop,
    Negate,
    Not,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Return,
    Print,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
}

#[derive(Default)]
pub struct Chunk {
    // invariant: code.len() == line.len()
    code: Vec<u8>,
    line: Vec<usize>,
    constants: Vec<Value>,
}

fn trace_simple_code(offset: usize, s: &str) -> usize {
    eprintln!("{}", s);
    offset + 1
}

/// SAFETY: constants in chunk must be valid
unsafe fn trace_constant_code(chunk: &Chunk, offset: usize, s: &str) -> usize {
    let constant_index = usize::from(chunk.code[offset + 1]);
    eprintln!("{:-16} {:4} '{}'", s, constant_index, unsafe {
        chunk.constants[constant_index].format_args()
    });
    offset + 2
}

impl Chunk {
    // I don't know why only this function triggers clippy warning about missing SAFETY section
    /// # Safety
    ///
    /// constants in this chunk must be valid
    pub unsafe fn trace_code(&self, offset: usize) -> usize {
        let code = self.code[offset];
        use OpCode::*;

        eprint!("{:04} ", offset);
        if offset > 0 && self.line[offset - 1] == self.line[offset] {
            eprint!("   | ");
        } else {
            eprint!("{:04} ", self.line[offset]);
        }
        match OpCode::from_u8(code) {
            None => trace_simple_code(offset, "OP_UNKNOWN"),
            Some(Nil) => trace_simple_code(offset, "OP_NIL"),
            Some(True) => trace_simple_code(offset, "OP_TRUE"),
            Some(False) => trace_simple_code(offset, "OP_FALSE"),
            Some(Constant) => {
                // SAFETY: constants in this chunk are valid
                unsafe { trace_constant_code(self, offset, "OP_CONSTANT") }
            }
            Some(Pop) => trace_simple_code(offset, "OP_POP"),
            Some(Negate) => trace_simple_code(offset, "OP_NEGATE"),
            Some(Not) => trace_simple_code(offset, "OP_NOT"),
            Some(Equal) => trace_simple_code(offset, "OP_EQUAL"),
            Some(Less) => trace_simple_code(offset, "OP_LESS"),
            Some(Greater) => trace_simple_code(offset, "OP_GREATER"),
            Some(Add) => trace_simple_code(offset, "OP_ADD"),
            Some(Subtract) => trace_simple_code(offset, "OP_SUB"),
            Some(Multiply) => trace_simple_code(offset, "OP_MUL"),
            Some(Divide) => trace_simple_code(offset, "OP_DIV"),
            Some(Return) => trace_simple_code(offset, "OP_RETURN"),
            Some(Print) => trace_simple_code(offset, "OP_PRINT"),
            Some(DefineGlobal) => {
                // SAFETY: constants in this chunk are valid
                unsafe { trace_constant_code(self, offset, "OP_DEFINE_GLOBAL") }
            }
            Some(GetGlobal) => {
                // SAFETY: constants in this chunk are valid
                unsafe { trace_constant_code(self, offset, "OP_GET_GLOBAL") }
            }
            Some(SetGlobal) => {
                // SAFETY: constants in this chunk are valid
                unsafe { trace_constant_code(self, offset, "OP_SET_GLOBAL") }
            }
        }
    }

    /// SAFETY: constants in this chunk must be valid
    pub unsafe fn trace_chunk(&self, name: &str) {
        if trace_available() {
            eprintln!("== {} ==", name);
            let mut offset = 0;
            while offset < self.code.len() {
                // SAFETY: constants in this chunk are valid
                unsafe {
                    offset = self.trace_code(offset);
                }
            }
        }
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn push_code(&mut self, data: u8, line: usize) {
        self.code.push(data);
        self.line.push(line);
    }

    pub fn push_constant(&mut self, value: Value) -> u8 {
        let index = self.constants.len();
        self.constants.push(value);
        u8::try_from(index).unwrap()
    }
}
