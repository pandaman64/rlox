use num::FromPrimitive as _;
use num_derive::{FromPrimitive, ToPrimitive};

use std::{convert::TryFrom, fmt, num::TryFromIntError};

use crate::{
    trace_available,
    value::Value,
    vm::object::{self, ObjectRef},
};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive)]
pub enum OpCode {
    Nil,
    True,
    False,
    Constant,
    Closure,
    Class,
    Pop,
    CloseUpvalue,
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
    GetLocal,
    SetLocal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    Jump,
    JumpIfFalse,
    Call,
}

#[derive(Default)]
pub struct Chunk {
    // invariant: code.len() == line.len()
    code: Box<[u8]>,
    line: Box<[usize]>,
    constants: Box<[Value]>,
}

fn trace_simple_code(offset: usize, s: &str) -> usize {
    eprintln!("{}", s);
    offset + 1
}

/// Safety
/// constants in chunk must be valid
unsafe fn trace_constant_code(chunk: &Chunk, offset: usize, s: &str) -> usize {
    let constant_index = usize::from(chunk.code[offset + 1]);
    eprintln!("{:-16} {:4} '{}'", s, constant_index, unsafe {
        chunk.constants[constant_index].format_args()
    });
    offset + 2
}

fn trace_byte_code(chunk: &Chunk, offset: usize, s: &str) -> usize {
    let byte = chunk.code[offset + 1];
    eprintln!("{:-16} {:4}", s, byte);
    offset + 2
}

fn trace_jump_code(chunk: &Chunk, offset: usize, s: &str) -> usize {
    let diff = chunk.read_jump_location(offset + 1);
    eprintln!(
        "{:-16} {:4} -> {}",
        s,
        offset,
        offset as isize + 1 + isize::from(diff)
    );
    offset + 3
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

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
            Some(Closure) => {
                let mut offset = offset + 1;
                let constant_index = usize::from(self.code[offset]);
                let constant = &self.constants[constant_index];
                eprintln!("{:-16} {:4} '{}'", "OP_CLOSURE", constant_index, unsafe {
                    constant.format_args()
                });

                // SAFETY: constants and generated code are valid
                unsafe {
                    match constant {
                        Value::Object(obj) => match object::as_ref(*obj) {
                            ObjectRef::Function(function) => {
                                for _ in 0..function.upvalues() {
                                    let is_local = if self.code[offset + 1] > 0 {
                                        "local"
                                    } else {
                                        "upvalue"
                                    };
                                    let index = self.code[offset + 2];
                                    eprintln!(
                                        "{:04}    |                     {} {}",
                                        offset, index, is_local,
                                    );
                                    offset += 2;
                                }
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    }
                }
                offset
            }
            Some(Class) => {
                // SAFETY: constants in this chunk are valid
                unsafe { trace_constant_code(self, offset, "OP_CLASS") }
            }
            Some(Pop) => trace_simple_code(offset, "OP_POP"),
            Some(CloseUpvalue) => trace_simple_code(offset, "OP_CLOSE_UPVALUE"),
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
            Some(GetLocal) => trace_byte_code(self, offset, "OP_GET_LOCAL"),
            Some(SetLocal) => trace_byte_code(self, offset, "OP_SET_LOCAL"),
            Some(GetUpvalue) => trace_byte_code(self, offset, "OP_GET_UPVALUE"),
            Some(SetUpvalue) => trace_byte_code(self, offset, "OP_SET_UPVALUE"),
            Some(GetProperty) => {
                // SAFETY: constants in this chunk are valid
                unsafe { trace_constant_code(self, offset, "OP_GET_PROPERTY") }
            }
            Some(SetProperty) => {
                // SAFETY: constants in this chunk are valid
                unsafe { trace_constant_code(self, offset, "OP_SET_PROPERTY") }
            }
            Some(Jump) => trace_jump_code(self, offset, "OP_JUMP"),
            Some(JumpIfFalse) => trace_jump_code(self, offset, "OP_JUMP_IF_FALSE"),
            Some(Call) => trace_byte_code(self, offset, "OP_CALL"),
        }
    }

    /// # Safety
    /// constants in this chunk must be valid
    pub unsafe fn trace_chunk<N: fmt::Display>(&self, name: N) {
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

    pub fn line(&self) -> &[usize] {
        &self.line
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn read_jump_location(&self, offset: usize) -> i16 {
        i16::from_le_bytes([self.code[offset], self.code[offset + 1]])
    }
}

#[derive(Default)]
pub struct ChunkBuilder {
    // invariant: code.len() == line.len()
    code: Vec<u8>,
    line: Vec<usize>,
    constants: Vec<Value>,
}

impl ChunkBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn take(&mut self) -> Chunk {
        use std::mem::take;
        Chunk {
            code: take(&mut self.code).into_boxed_slice(),
            line: take(&mut self.line).into_boxed_slice(),
            constants: take(&mut self.constants).into_boxed_slice(),
        }
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn line(&self) -> &[usize] {
        &self.line
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

    pub fn allocate_jump_location(&mut self, line: usize) -> usize {
        let ret = self.code.len();
        self.code.push(u8::MAX);
        self.code.push(u8::MAX);
        self.line.push(line);
        self.line.push(line);
        ret
    }

    pub fn fill_jump_location(&mut self, jump: usize, ip: usize) -> Result<(), TryFromIntError> {
        let diff = if ip >= jump {
            i16::try_from(ip - jump)?
        } else if jump - ip == usize::try_from(i16::MAX).unwrap() + 1 {
            i16::MIN
        } else {
            -i16::try_from(jump - ip)?
        };
        let bytes = diff.to_le_bytes();

        self.code[jump] = bytes[0];
        self.code[jump + 1] = bytes[1];

        Ok(())
    }

    pub fn fill_jump_location_with_current(&mut self, jump: usize) -> Result<(), TryFromIntError> {
        assert!(jump < self.code.len());
        self.fill_jump_location(jump, self.code.len())
    }

    pub fn read_jump_location(&self, offset: usize) -> i16 {
        i16::from_le_bytes([self.code[offset], self.code[offset + 1]])
    }
}
