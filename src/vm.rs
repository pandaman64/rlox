use crate::{trace_available, value::Value, Chunk, OpCode};
use num::FromPrimitive as _;

macro_rules! try_pop {
    ($self:ident, $variant:ident) => {{
        match $self.stack.pop() {
            Some(Value::$variant(v)) => v,
            _ => return InterpetResult::RuntimeError,
        }
    }};
    ($self:ident) => {{
        match $self.stack.pop() {
            Some(v) => v,
            _ => return InterpetResult::RuntimeError,
        }
    }};
}

macro_rules! check_top {
    ($self:ident, $variant:ident) => {{
        match $self.stack.last() {
            Some(Value::$variant(_)) => {}
            None => {
                eprintln!("Expected {}, got empty stack", stringify!($variant));
                return InterpetResult::RuntimeError;
            }
            Some(v) => {
                eprintln!("Expected {}, got {}", stringify!($variant), v);
                return InterpetResult::RuntimeError;
            }
        }
    }};
}

macro_rules! binop {
    ($self:ident, $from_ty:ident, $op:tt, $to_ty:ident) => {{
        check_top!($self, $from_ty);
        let v2 = try_pop!($self, $from_ty);
        check_top!($self, $from_ty);
        let v1 = try_pop!($self, $from_ty);
        $self.stack.push($to_ty(v1 $op v2));
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
        use Value::*;

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
                    let constant = self.chunk.constants[index].clone();
                    self.stack.push(constant);
                }
                Some(Negate) => {
                    check_top!(self, Number);
                    let value = try_pop!(self, Number);
                    self.stack.push(Number(-value));
                }
                Some(Not) => {
                    check_top!(self, Number);
                    let value = try_pop!(self, Bool);
                    self.stack.push(Bool(!value))
                }
                Some(Add) => binop!(self, Number, +, Number),
                Some(Subtract) => binop!(self, Number, -, Number),
                Some(Multiply) => binop!(self, Number, *, Number),
                Some(Divide) => binop!(self, Number, /, Number),
            }
        }
    }
}
