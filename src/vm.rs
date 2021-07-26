use std::mem::discriminant;

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
        use crate::object;
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
                Some(OpCode::Nil) => {
                    self.stack.push(Value::Nil);
                }
                Some(True) => {
                    self.stack.push(Value::Bool(true));
                }
                Some(False) => {
                    self.stack.push(Value::Bool(false));
                }
                Some(Equal) => {
                    let v2 = try_pop!(self);
                    let v1 = try_pop!(self);

                    if discriminant(&v1) != discriminant(&v2) {
                        eprintln!("type mismatch between {} and {}", v1, v2);
                        return InterpetResult::RuntimeError;
                    }
                    self.stack.push(Value::Bool(v1 == v2));
                }
                Some(Less) => binop!(self, Number, <, Bool),
                Some(Greater) => binop!(self, Number, >, Bool),
                Some(Negate) => {
                    check_top!(self, Number);
                    let value = try_pop!(self, Number);
                    self.stack.push(Number(-value));
                }
                Some(Not) => {
                    check_top!(self, Bool);
                    let value = try_pop!(self, Bool);
                    self.stack.push(Bool(!value))
                }
                Some(Add) => {
                    let len = self.stack.len();
                    if len < 2 {
                        eprintln!("insufficient stack");
                        return InterpetResult::RuntimeError;
                    }
                    match &self.stack[len - 2..] {
                        [Object(object::Object::String(_)), Object(object::Object::String(_))] => {
                            // we will be adding other variants
                            #[allow(clippy::infallible_destructuring_match)]
                            let v2 = match try_pop!(self, Object) {
                                object::Object::String(s2) => s2,
                            };
                            #[allow(clippy::infallible_destructuring_match)]
                            let v1 = match try_pop!(self, Object) {
                                object::Object::String(s1) => s1,
                            };
                            self.stack.push(Object(object::Object::String(v1 + &v2)));
                        }
                        [Number(_), Number(_)] => {
                            binop!(self, Number, +, Number);
                        }
                        [v2, v1] => {
                            eprintln!("type mismatch between {} and {}", v2, v1);
                            return InterpetResult::RuntimeError;
                        }
                        _ => unreachable!(),
                    }
                }
                Some(Subtract) => binop!(self, Number, -, Number),
                Some(Multiply) => binop!(self, Number, *, Number),
                Some(Divide) => binop!(self, Number, /, Number),
            }
        }
    }
}
