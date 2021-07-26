use std::{mem::discriminant, ptr};

use crate::{
    object::{self, Header, ObjectKind, RawObject},
    trace_available,
    value::Value,
    Chunk, OpCode,
};
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
                eprintln!("Expected {}, got {:?}", stringify!($variant), v);
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

pub struct Vm {
    ip: usize,
    stack: Vec<Value>,
    objects: Option<RawObject>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: vec![],
            objects: None,
        }
    }

    /// SAFETY: `obj` must points to an initialized object.
    /// `obj.next` must not point to another valid object. Otherwise, the object can be leaked.
    unsafe fn add_object_head(&mut self, obj: RawObject) {
        // SAFETY: obj is initialized
        unsafe {
            let obj_ptr = obj.as_ptr();
            let next_ptr = ptr::addr_of_mut!((*obj_ptr).next);
            assert!(ptr::read(next_ptr).is_none());
            ptr::write(next_ptr, self.objects);

            self.objects = Some(obj);
        }
    }

    /// SAFETY: no chunks referring to objects managed by this vm can be run after calling free_objects
    pub unsafe fn free_all_objects(&mut self) {
        while let Some(obj) = { self.objects } {
            // SAFETY: self.objects points to an initialized object and covers the whole allocation
            // in an type-safe manner. this property must be guaranteed by allocation methods.
            unsafe {
                // since we carefully avoid using references in this implementation, all pointers
                // declared here should share the same permission for accessing the object `obj`
                // points to. moreover, obj_ptr should have the right provenance for the whole
                // object, not only for the header.
                let obj_ptr = obj.as_ptr();
                let kind_ptr = ptr::addr_of_mut!((*obj_ptr).kind);
                let next_ptr = ptr::addr_of_mut!((*obj_ptr).next);
                self.objects = ptr::read(next_ptr);

                // we assume that runtime type of the values are correct, so that we can cast back
                // to the original types and deallocate them here.
                match ptr::read(kind_ptr) {
                    ObjectKind::String => {
                        let str_ptr = obj_ptr as *mut object::Str;
                        drop(Box::from_raw(str_ptr));
                    }
                }
            }
        }
    }

    pub fn allocate_string(&mut self, content: String) -> RawObject {
        unsafe {
            let s = object::Str::new(content);
            let header_ptr = Box::leak(Box::new(s)) as *mut object::Str as *mut Header;
            // SAFETY: header_ptr is a non-null pointer pointing to an initialized string object.
            let obj = RawObject::new_unchecked(header_ptr);
            self.add_object_head(obj);
            obj
        }
    }

    // SAFETY: constants in chunk must be valid
    pub unsafe fn run(&mut self, chunk: &Chunk) -> InterpetResult {
        use OpCode::*;
        use Value::*;

        loop {
            if trace_available() {
                // SAFETY: constants in chunk are valid
                unsafe {
                    // print current stack
                    eprint!("          ");
                    for slot in self.stack.iter() {
                        eprint!("[ {} ]", slot.format_args());
                    }
                    eprintln!();

                    // print current instruction
                    chunk.trace_code(self.ip);
                }
            }

            let instruction = chunk.code[self.ip];
            self.ip += 1;
            match OpCode::from_u8(instruction) {
                None => return InterpetResult::CompileError,
                Some(Return) => {
                    try_pop!(self);
                    return InterpetResult::Ok;
                }
                Some(Constant) => {
                    let index = usize::from(chunk.code[self.ip]);
                    self.ip += 1;
                    let constant = chunk.constants[index].clone();
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
                        eprintln!("type mismatch in equality operation");
                        return InterpetResult::RuntimeError;
                    }
                    // SAFETY: v1 and v2 are valid object
                    let eq = unsafe { v1.eq(&v2) };
                    self.stack.push(Value::Bool(eq));
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
                        [Object(_), Object(_)] => {
                            // we will be adding other variants
                            let o2 = try_pop!(self, Object);
                            let o1 = try_pop!(self, Object);

                            // SAFETY: these objects are valid
                            match unsafe { (object::try_as_str(&o1), object::try_as_str(&o2)) } {
                                (Some(o1), Some(o2)) => {
                                    let result = o1.content.clone() + &o2.content;
                                    let obj = self.allocate_string(result);
                                    self.stack.push(Value::Object(obj));
                                }
                                _ => {
                                    eprintln!("type mismatch in addition");
                                    return InterpetResult::RuntimeError;
                                }
                            }
                        }
                        [Number(_), Number(_)] => {
                            binop!(self, Number, +, Number);
                        }
                        [_v2, _v1] => {
                            eprintln!("type mismatch in addition");
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
