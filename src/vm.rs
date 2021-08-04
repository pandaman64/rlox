use std::{
    collections::{HashMap, HashSet},
    mem::discriminant,
    ptr,
};

use crate::{
    object::{self, ObjectKind, RawObject, RawStr},
    opcode::{Chunk, OpCode},
    table::{InternedStr, Key},
    trace_available,
    value::Value,
};
use num::FromPrimitive as _;

macro_rules! try_pop {
    ($self:ident, $variant:ident) => {{
        match $self.stack.pop() {
            Some(Value::$variant(v)) => v,
            _ => return InterpretResult::RuntimeError,
        }
    }};
    ($self:ident) => {{
        match $self.stack.pop() {
            Some(v) => v,
            _ => return InterpretResult::RuntimeError,
        }
    }};
}

macro_rules! check_top {
    ($self:ident, $variant:ident) => {{
        match $self.stack.last() {
            Some(Value::$variant(_)) => {}
            None => {
                eprintln!("Expected {}, got empty stack", stringify!($variant));
                return InterpretResult::RuntimeError;
            }
            Some(v) => {
                eprintln!("Expected {}, got {:?}", stringify!($variant), v);
                return InterpretResult::RuntimeError;
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

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

pub struct Vm {
    ip: usize,
    stack: Vec<Value>,
    objects: Option<RawObject>,
    strings: HashSet<InternedStr>,
    globals: HashMap<Key, Value>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: vec![],
            objects: None,
            strings: HashSet::new(),
            globals: HashMap::new(),
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
        self.strings.clear();
        self.globals.clear();

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

    pub fn allocate_string(&mut self, content: String) -> InternedStr {
        unsafe {
            let s = object::Str::new(content);
            // SAFETY: the pointer passed to InternedStr::new is a valid pointer to the string
            let str_ptr = InternedStr::new(RawStr::from(Box::leak(Box::new(s))));

            match self.strings.get(&str_ptr) {
                None => {
                    self.add_object_head(str_ptr.clone().into_raw_obj());
                    self.strings.insert(str_ptr.clone());
                    str_ptr
                }
                Some(interned) => {
                    // free the constructed string since we return from the strings table
                    drop(Box::from_raw(str_ptr.into_inner().as_ptr()));
                    interned.clone()
                }
            }
        }
    }

    fn extract_constant(&mut self, chunk: &Chunk) -> Value {
        let index = usize::from(chunk.code()[self.ip]);
        self.ip += 1;
        chunk.constants()[index].clone()
    }

    fn extract_constant_key(&mut self, chunk: &Chunk) -> Option<Key> {
        let ident = self.extract_constant(chunk);
        match ident {
            Value::Object(obj) => {
                unsafe {
                    let obj_ptr = obj.as_ptr();
                    // SAFETY: obj points to a valid constant value
                    let kind_ptr = ptr::addr_of_mut!((*obj_ptr).kind);
                    // SAFETY: obj points to a valid constant value
                    let kind = ptr::read(kind_ptr);
                    match kind {
                        ObjectKind::String => {
                            let str_ptr = obj_ptr as *mut object::Str;
                            let str_ptr = RawStr::new(str_ptr).unwrap();
                            // SAFETY: we intern all string appears in the program execution
                            Some(Key::new(InternedStr::new(str_ptr)))
                        }
                    }
                }
            }
            _ => None,
        }
    }

    // SAFETY: constants in chunk must be valid
    pub unsafe fn run(&mut self, chunk: &Chunk) -> InterpretResult {
        use std::collections::hash_map::Entry;
        use OpCode::*;
        use Value::*;

        self.ip = 0;
        self.stack = vec![];

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

            let instruction = chunk.code()[self.ip];
            self.ip += 1;
            match OpCode::from_u8(instruction) {
                None => return InterpretResult::CompileError,
                Some(Return) => {
                    return InterpretResult::Ok;
                }
                Some(Print) => {
                    match self.stack.pop() {
                        None => {
                            eprintln!("no stack");
                            return InterpretResult::RuntimeError;
                        }
                        Some(v) => {
                            // SAFETY: v is a valid value
                            unsafe {
                                println!("{}", v.format_args());
                            }
                        }
                    }
                }
                Some(Constant) => {
                    let constant = self.extract_constant(chunk);
                    self.stack.push(constant);
                }
                Some(DefineGlobal) => {
                    let key = match self.extract_constant_key(chunk) {
                        Some(key) => key,
                        None => {
                            eprintln!("OP_DEFINE_GLOBAL takes a string constant");
                            return InterpretResult::CompileError;
                        }
                    };
                    match self.stack.last() {
                        None => {
                            eprintln!("no stack");
                            return InterpretResult::RuntimeError;
                        }
                        Some(value) => {
                            self.globals.insert(key, value.clone());
                        }
                    }
                    // we pop the value after inserting to globals table so that we can run GC
                    // during table insertion. however, since we rely on std's hash table, we don't
                    // think we can run GC in insertion in this implementation.
                    self.stack.pop();
                }
                Some(GetGlobal) => {
                    let key = match self.extract_constant_key(chunk) {
                        Some(key) => key,
                        None => {
                            eprintln!("OP_GET_GLOBAL takes a string constant");
                            return InterpretResult::CompileError;
                        }
                    };
                    let value = match self.globals.get(&key) {
                        Some(value) => value,
                        None => {
                            eprintln!("undefined variable: {}", key.display());
                            return InterpretResult::RuntimeError;
                        }
                    };
                    self.stack.push(value.clone());
                }
                Some(SetGlobal) => {
                    let key = match self.extract_constant_key(chunk) {
                        Some(key) => key,
                        None => {
                            eprintln!("OP_SET_GLOBAL takes a string constant");
                            return InterpretResult::CompileError;
                        }
                    };
                    // assignments leave the value untouched, so we don't pop here
                    match self.stack.last() {
                        None => {
                            eprintln!("no stack for OP_SET_GLOBAL");
                            return InterpretResult::RuntimeError;
                        }
                        Some(value) => match self.globals.entry(key) {
                            Entry::Vacant(v) => {
                                eprintln!("undefined variable: {}", v.key().display());
                                return InterpretResult::RuntimeError;
                            }
                            Entry::Occupied(mut o) => {
                                o.insert(value.clone());
                            }
                        },
                    }
                }
                Some(GetLocal) => {
                    let local = usize::from(chunk.code()[self.ip]);
                    self.ip += 1;
                    self.stack.push(self.stack[local].clone());
                }
                Some(SetLocal) => {
                    let local = usize::from(chunk.code()[self.ip]);
                    self.ip += 1;
                    match self.stack.last().cloned() {
                        None => {
                            eprintln!("empty stack for OP_SET_LOCAL");
                            return InterpretResult::RuntimeError;
                        }
                        Some(v) => {
                            self.stack[local] = v;
                        }
                    }
                }
                Some(Pop) => match self.stack.pop() {
                    None => {
                        eprintln!("no stack");
                        return InterpretResult::RuntimeError;
                    }
                    Some(_) => {}
                },
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
                        return InterpretResult::RuntimeError;
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
                Some(Not) => match self.stack.last() {
                    Some(v) => {
                        let falsy = v.is_falsy();
                        let last = self.stack.len();
                        self.stack[last - 1] = Bool(falsy);
                    }
                    None => {
                        eprintln!("Expected bool or nil, got empty stack");
                        return InterpretResult::RuntimeError;
                    }
                },
                Some(Add) => {
                    let len = self.stack.len();
                    if len < 2 {
                        eprintln!("insufficient stack");
                        return InterpretResult::RuntimeError;
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
                                    self.stack.push(Value::Object(obj.into_raw_obj()));
                                }
                                _ => {
                                    eprintln!("type mismatch in addition");
                                    return InterpretResult::RuntimeError;
                                }
                            }
                        }
                        [Number(_), Number(_)] => {
                            binop!(self, Number, +, Number);
                        }
                        [_v2, _v1] => {
                            eprintln!("type mismatch in addition");
                            return InterpretResult::RuntimeError;
                        }
                        _ => unreachable!(),
                    }
                }
                Some(Subtract) => binop!(self, Number, -, Number),
                Some(Multiply) => binop!(self, Number, *, Number),
                Some(Divide) => binop!(self, Number, /, Number),
                Some(Jump) => {
                    let diff = chunk.read_jump_location(self.ip);
                    self.ip = (self.ip as isize + isize::from(diff)) as usize;
                }
                Some(JumpIfFalse) => match self.stack.last() {
                    None => {
                        eprintln!("no stack for OP_JUMP_IF_FALSE");
                        return InterpretResult::RuntimeError;
                    }
                    Some(v) => {
                        if v.is_falsy() {
                            let diff = chunk.read_jump_location(self.ip);
                            self.ip = (self.ip as isize + isize::from(diff)) as usize;
                        } else {
                            self.ip += 2;
                        }
                    }
                },
            }
        }
    }
}
