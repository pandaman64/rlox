#![deny(rust_2018_idioms, unsafe_op_in_unsafe_fn)]
// #![warn(unreachable_pub)]

mod ast;
mod codegen;
mod object;
mod syntax;
mod table;
mod value;
mod vm;

use num::FromPrimitive as _;
use num_derive::{FromPrimitive, ToPrimitive};
use std::{
    convert::TryFrom,
    io::BufRead,
    sync::{
        atomic::{self, AtomicBool},
        Once,
    },
};
use value::Value;
use vm::Vm;

use crate::ast::Root;

pub fn trace_available() -> bool {
    static AVAILABLE: AtomicBool = AtomicBool::new(false);
    static ONCE: Once = Once::new();

    ONCE.call_once(|| {
        let config = matches!(std::env::var("RUST_LOG"), Ok(s) if s == "trace");
        AVAILABLE.store(config, atomic::Ordering::Relaxed);
    });
    AVAILABLE.load(atomic::Ordering::Relaxed)
}

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
}

#[derive(Default)]
pub struct Chunk {
    // invariant: code.len() == line.len()
    code: Vec<u8>,
    line: Vec<usize>,
    constants: Vec<value::Value>,
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
        }
    }

    /// SAFETY: constants in this chunk must be valid
    unsafe fn trace_chunk(&self, name: &str) {
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

// fn main() {
//     std::env::set_var("RUST_LOG", "trace");

//     let chunk = {
//         let mut chunk = Chunk::default();
//         let index1 = chunk.push_constant(3.1415);
//         let index2 = chunk.push_constant(2.7);
//         chunk.push_code(OpCode::Constant as u8, 13);
//         chunk.push_code(index1, 13);
//         chunk.push_code(OpCode::Constant as u8, 13);
//         chunk.push_code(index2, 13);
//         chunk.push_code(OpCode::Subtract as u8, 14);
//         chunk.push_code(OpCode::Return as u8, 15);
//         chunk
//     };

//     chunk.trace_chunk("test_chunk");

//     let mut vm = Vm::new(&chunk);
//     vm.run();
// }

fn main() -> std::io::Result<()> {
    use std::io::Write;

    std::env::set_var("RUST_LOG", "trace");

    let mut line = String::new();
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    loop {
        line.clear();
        write!(stdout, "> ")?;
        stdout.flush()?;
        if stdin.read_line(&mut line)? == 0 {
            break;
        }

        let mut vm = Vm::new();
        let node = syntax::parse(&line);
        eprintln!("{:#?}", node);
        let chunk = match Root::cast(node) {
            None => {
                eprintln!("syntax error");
                continue;
            }
            Some(root) => {
                let mut chunk = Chunk::default();
                for decl in root.decls() {
                    codegen::gen_decl(&mut vm, &mut chunk, decl);
                }
                chunk.push_code(OpCode::Return as _, 0);
                chunk
            }
        };
        // SAFETY: we construct a chunk with valid constants
        unsafe {
            chunk.trace_chunk(line.trim());
            vm.run(&chunk);
        }

        // SAFETY: we don't reuse vm and chunk, so no code can refer to deallocated objects.
        unsafe {
            vm.free_all_objects();
        }
    }

    Ok(())
}
