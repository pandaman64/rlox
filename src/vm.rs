use crate::{trace_available, value::Value, Chunk, OpCode};
use num::FromPrimitive as _;

macro_rules! try_pop {
    ($self:ident) => {{
        match $self.stack.pop() {
            Some(v) => v,
            None => return InterpetResult::RuntimeError,
        }
    }};
}

pub enum InterpetResult {
    Ok,
    CompileError,
    RuntimeError,
}

pub struct Vm<'c> {
    chunk: &'c Chunk,
    ip: usize,
    stack: Vec<Value>,
}

impl<'c> Vm<'c> {
    pub fn new(chunk: &'c Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: vec![],
        }
    }

    pub fn run(&mut self) -> InterpetResult {
        use OpCode::*;

        loop {
            if trace_available() {
                // print current stack
                eprint!("          ");
                for slot in self.stack.iter() {
                    eprint!("[ {} ]", slot);
                }
                eprintln!();

                // print current instruction
                self.chunk.trace_code(self.ip);
            }

            let instruction = self.chunk.code[self.ip];
            self.ip += 1;
            match OpCode::from_u8(instruction) {
                None => return InterpetResult::CompileError,
                Some(Return) => {
                    try_pop!(self);
                    return InterpetResult::Ok;
                }
                Some(Constant) => {
                    let index = usize::from(self.chunk.code[self.ip]);
                    self.ip += 1;
                    let constant = self.chunk.constants[index];
                    self.stack.push(constant);
                }
                Some(Negate) => {
                    let value = try_pop!(self);
                    self.stack.push(-value);
                }
                Some(Not) => {
                    let value = try_pop!(self).to_bits();
                    let not = u64::from(value != 0);
                    self.stack.push(f64::from_bits(not))
                }
                Some(Add) => {
                    let v2 = try_pop!(self);
                    let v1 = try_pop!(self);
                    self.stack.push(v1 + v2);
                }
                Some(Subtract) => {
                    let v2 = try_pop!(self);
                    let v1 = try_pop!(self);
                    self.stack.push(v1 - v2);
                }
                Some(Multiply) => {
                    let v2 = try_pop!(self);
                    let v1 = try_pop!(self);
                    self.stack.push(v1 * v2);
                }
                Some(Divide) => {
                    let v2 = try_pop!(self);
                    let v1 = try_pop!(self);
                    self.stack.push(v1 / v2);
                }
            }
        }
    }
}
