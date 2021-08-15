use std::{
    collections::{HashMap, HashSet},
    mem::discriminant,
    ptr,
};

use crate::{
    object::{
        self, Closure, Function, NativeFunction, ObjectKind, ObjectRef, RawClosure, RawObject,
        RawStr, RawUpvalue, Upvalue,
    },
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

struct CallFrame {
    closure: RawClosure,
    // the ip for this function
    ip: usize,
    // the base pointer from which the local variables of this function start
    bp: usize,
}

#[must_use]
#[derive(PartialEq, Eq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

pub struct Objects {
    objects_head: Option<RawObject>,
    open_upvalues_head: Option<RawUpvalue>,
    strings: HashSet<InternedStr>,
    globals: HashMap<Key, Value>,
}

impl Objects {
    fn new() -> Self {
        Self {
            objects_head: None,
            open_upvalues_head: None,
            strings: HashSet::new(),
            globals: HashMap::new(),
        }
    }

    /// # Safety
    /// `obj` must points to an initialized object.
    /// `obj.next` must not point to another valid object. Otherwise, the object can be leaked.
    unsafe fn add_object_head(&mut self, obj: RawObject) {
        // SAFETY: obj is initialized
        unsafe {
            let obj_ptr = obj.as_ptr();
            let next_ptr = ptr::addr_of_mut!((*obj_ptr).next);
            assert!(ptr::read(next_ptr).is_none());
            ptr::write(next_ptr, self.objects_head);

            self.objects_head = Some(obj);
        }
    }

    /// # Safety
    /// no chunks referring to objects managed by this vm can be run after calling free_objects
    pub unsafe fn free_all_objects(&mut self) {
        self.strings.clear();
        self.globals.clear();

        while let Some(obj) = { self.objects_head } {
            // SAFETY: self.objects points to an initialized object and covers the whole allocation
            // in an type-safe manner. this property must be guaranteed by allocation methods.
            unsafe {
                self.objects_head = free_object(obj);
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
                    self.add_object_head(str_ptr.into_raw_obj());
                    self.strings.insert(str_ptr);
                    str_ptr
                }
                Some(interned) => {
                    // free the constructed string since we return from the strings table
                    drop(Box::from_raw(str_ptr.into_inner().as_ptr()));
                    *interned
                }
            }
        }
    }

    pub fn allocate_function(&mut self, function: Function) -> RawObject {
        let obj = allocate_object(function);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj
    }

    pub fn allocate_closure(&mut self, closure: Closure) -> RawClosure {
        let obj = allocate_object(closure);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj.cast()
    }

    pub fn allocate_upvalue(&mut self, upvalue: Upvalue) -> RawUpvalue {
        let obj = allocate_object(upvalue);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj.cast()
    }
}

pub struct Vm {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    objects: Objects,
}

fn extract_constant(ip: &mut usize, chunk: &Chunk) -> Value {
    let index = usize::from(chunk.code()[*ip]);
    *ip += 1;
    chunk.constants()[index].clone()
}

fn extract_constant_key(ip: &mut usize, chunk: &Chunk) -> Option<Key> {
    let ident = extract_constant(ip, chunk);
    match ident {
        Value::Object(obj) => {
            unsafe {
                let obj_ptr = obj.as_ptr();
                // SAFETY: obj points to a valid constant value
                let kind_ptr = ptr::addr_of_mut!((*obj_ptr).kind);
                // SAFETY: obj points to a valid constant value
                let kind = ptr::read(kind_ptr);
                match kind {
                    ObjectKind::Str => {
                        let str_ptr = obj_ptr as *mut object::Str;
                        let str_ptr = RawStr::new(str_ptr).unwrap();
                        // SAFETY: we intern all string appears in the program execution
                        Some(Key::new(InternedStr::new(str_ptr)))
                    }
                    _ => {
                        unreachable!("key must be string")
                    }
                }
            }
        }
        _ => None,
    }
}

fn allocate_object<T>(obj: T) -> RawObject {
    let obj_ptr = Box::into_raw(Box::new(obj));
    RawObject::new(obj_ptr as _).unwrap()
}

/// # Safety
/// obj must be valid and allocated by allocate_object
unsafe fn free_object(obj: RawObject) -> Option<RawObject> {
    unsafe {
        // SAFETY:
        // since we carefully avoid using references in this implementation, all pointers
        // declared here should share the same permission for accessing the object `obj`
        // points to. moreover, obj_ptr should have the right provenance for the whole
        // object, not only for the header.
        let obj_ptr = obj.as_ptr();
        let kind_ptr = ptr::addr_of_mut!((*obj_ptr).kind);
        let next_ptr = ptr::addr_of_mut!((*obj_ptr).next);
        let next = ptr::read(next_ptr);

        // SAFETY:
        // we assume that runtime type of the values are correct, so that we can cast back
        // to the original types and deallocate them here.
        macro_rules! free {
            ($ptr:expr, $ty:ty) => {{
                let ptr = $ptr as *mut $ty;
                drop(Box::from_raw(ptr));
            }};
        }
        match ptr::read(kind_ptr) {
            ObjectKind::Str => free!(obj_ptr, object::Str),
            ObjectKind::Function => free!(obj_ptr, object::Function),
            ObjectKind::NativeFunction => free!(obj_ptr, object::NativeFunction),
            ObjectKind::Closure => free!(obj_ptr, object::Closure),
            ObjectKind::Upvalue => free!(obj_ptr, object::Upvalue),
        }

        next
    }
}

/// # Safety
/// the objects and the frame must be valid
unsafe fn capture_upvalue(
    objects: &mut Objects,
    frame: &mut CallFrame,
    index: u8,
) -> Result<RawUpvalue, InterpretResult> {
    let stack_index = frame.bp.saturating_add(usize::from(index));
    if stack_index == usize::MAX {
        eprintln!("upvalue index cannot exceed usize::MAX");
        return Err(InterpretResult::RuntimeError);
    }

    let mut prev = None;
    let mut current = objects.open_upvalues_head;

    while let Some(cur) = current {
        // SAFETY: the objects must be valid
        unsafe {
            if cur.as_ref().stack_index() <= stack_index {
                break;
            }
            prev = current;
            current = cur.as_ref().next();
        }
    }

    if let Some(cur) = current {
        // SAFETY: the objects must be valid
        unsafe {
            if cur.as_ref().stack_index() == stack_index {
                return Ok(cur);
            }
        }
    }

    let upvalue = Upvalue::new_stack(stack_index, current);
    let upvalue_obj = objects.allocate_upvalue(upvalue);

    match prev {
        None => objects.open_upvalues_head = Some(upvalue_obj),
        // SAFETY: the objects must be valid
        Some(mut prev) => unsafe {
            *prev.as_mut().next_mut() = Some(upvalue_obj);
        },
    }

    Ok(upvalue_obj)
}

/// # Safety
/// the objects and stack must be valid
unsafe fn close_upvalue(objects: &mut Objects, stack: &[Value], stack_index: usize) {
    // SAFETY: the objects and stack are valid
    unsafe {
        while let Some(mut head) = objects.open_upvalues_head {
            if head.as_ref().stack_index() < stack_index {
                break;
            }

            let head = head.as_mut();
            let value = stack[head.stack_index()].clone();
            head.close(value);
            objects.open_upvalues_head = head.next();
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self {
            stack: vec![],
            frames: vec![],
            objects: Objects::new(),
        }
    }
}

impl Vm {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn objects_mut(&mut self) -> &mut Objects {
        &mut self.objects
    }

    pub fn define_native_function(&mut self, name: String, function: NativeFunction) {
        let name = self.objects.allocate_string(name);
        let fun_obj = {
            let fun_obj = allocate_object(function);
            // SAFETY: obj points to a valid object
            unsafe {
                self.objects.add_object_head(fun_obj);
            }
            fun_obj
        };

        // register objects as GC roots
        self.stack.push(Value::Object(name.into_raw_obj()));
        self.stack.push(Value::Object(fun_obj));

        self.objects
            .globals
            .insert(Key::new(name), Value::Object(fun_obj));

        // un-register objects
        self.stack.pop();
        self.stack.pop();
    }

    pub fn reset(&mut self, function: Function) {
        assert_eq!(function.upvalues(), 0);

        self.frames = vec![];

        let fun_obj = self.objects.allocate_function(function);
        self.stack = vec![Value::Object(fun_obj)];
        // SAFETY: function is valid
        let closure = unsafe { Closure::new(fun_obj.cast()) };
        let cls_obj = self.objects.allocate_closure(closure);
        self.stack.pop();
        self.stack.push(Value::Object(cls_obj.cast()));
    }

    pub fn call(&mut self, args: u8) -> bool {
        let bp = self.stack.len() - usize::from(args) - 1;
        let callee = *match self.stack.get(bp) {
            Some(Value::Object(callee)) => callee,
            Some(_) => {
                eprintln!("callee must be an object");
                return false;
            }
            None => {
                eprintln!("not enough stack for OP_CALL");
                return false;
            }
        };
        // SAFETY: values in the stack are valid
        match unsafe { object::as_ref(callee) } {
            ObjectRef::Closure(closure) => {
                // SAFETY: `function` is a reference to the function object derived by `closure.function`.
                // since we assume that our GC keeps values reachable from the stack valid, accessing
                // function is safe. moreover, the `function` and `closure` references dies before
                // the later use of callee, which satisfies Stacked Borrows.
                unsafe {
                    let function = closure.function().as_ref();
                    if function.arity() != args {
                        eprintln!("expected {} arguments but got {}", function.arity(), args);
                        return false;
                    }
                }
                self.frames.push(CallFrame {
                    // make sure that `closure` will share the same permission as `callee`.
                    closure: callee.cast(),
                    ip: 0,
                    bp,
                });
                true
            }
            ObjectRef::NativeFunction(function) => {
                let result = function.fun()(&self.stack[bp..]);
                self.stack.truncate(bp);
                self.stack.push(result);
                true
            }
            ObjectRef::Function(_function) => {
                eprintln!("cannot call bare function");
                false
            }
            ObjectRef::Str(_) | ObjectRef::Upvalue(_) => {
                eprintln!("callee must be a function");
                false
            }
        }
    }

    /// # Safety
    /// stack and frames must be valid
    pub unsafe fn print_stack_trace(&self) {
        for frame in self.frames.iter().rev() {
            // SAFETY: the frame contains valid pointer to a closure
            unsafe {
                let closure = frame.closure.as_ref();
                let function = closure.function().as_ref();

                // ip is incremented already
                eprint!(
                    "while evaluating line {}, ",
                    function.chunk().line()[frame.ip - 1]
                );
                match function.name() {
                    Some(name) => eprintln!("function {}", name.display()),
                    None => eprintln!("top-level script"),
                }
            }
        }
    }

    /// # Safety
    /// stack and frames must be valid
    pub unsafe fn run(&mut self) -> InterpretResult {
        use std::collections::hash_map::Entry;
        use OpCode::*;
        use Value::*;

        // TODO: instead of passing references to frame.ip, keep ip on a register
        // and update it on function call and return as a performance optimization
        loop {
            let frame = match self.frames.last_mut() {
                Some(frame) => frame,
                None => {
                    eprintln!("no call frame to execute");
                    return InterpretResult::RuntimeError;
                }
            };
            // SAFETY: given function is valid and we push to the call frame only valid closures
            let function = unsafe { frame.closure.as_ref().function().as_ref() };
            let chunk = function.chunk();
            let objects = &mut self.objects;

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
                    chunk.trace_code(frame.ip);
                }
            }

            let instruction = chunk.code()[frame.ip];
            frame.ip += 1;
            match OpCode::from_u8(instruction) {
                None => return InterpretResult::CompileError,
                Some(Return) => {
                    let ret = self.stack.pop().unwrap();
                    let frame = self.frames.pop().unwrap();

                    // SAFETY: the objects and stack are valid
                    unsafe {
                        close_upvalue(objects, &self.stack, frame.bp);
                    }
                    if self.frames.is_empty() {
                        // pop the top-level script function object
                        self.stack.pop();
                        return InterpretResult::Ok;
                    }

                    self.stack.truncate(frame.bp);
                    self.stack.push(ret);
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
                    let constant = extract_constant(&mut frame.ip, chunk);
                    self.stack.push(constant);
                }
                // SAFETY: the values reachable from the stack are valid
                Some(Closure) => unsafe {
                    use object::Closure;

                    let fun_obj = {
                        let obj = match extract_constant(&mut frame.ip, chunk) {
                            Value::Object(obj) => obj,
                            _ => {
                                eprintln!("not a function object for OP_CLOSURE");
                                return InterpretResult::RuntimeError;
                            }
                        };
                        match object::as_ref(obj) {
                            ObjectRef::Function(_) => obj.cast(),
                            _ => {
                                eprintln!("not a function object for OP_CLOSURE");
                                return InterpretResult::RuntimeError;
                            }
                        }
                    };
                    let closure = Closure::new(fun_obj);
                    let mut cls_obj = objects.allocate_closure(closure);
                    self.stack.push(Object(cls_obj.cast()));

                    // SAFETY: given function is valid and we push to the call frame only valid closures
                    let cls = cls_obj.as_mut();
                    let function = frame.closure.as_ref().function().as_ref();
                    let chunk = function.chunk();
                    for (i, upvalue) in cls.upvalues_mut().iter_mut().enumerate() {
                        let is_local = chunk.code()[frame.ip];
                        let index = chunk.code()[frame.ip + 1];
                        frame.ip += 2;
                        let upvalue_obj = if is_local > 0 {
                            // if `is_local == 1`, the upvalue is captured from the call-stack of
                            // the direct parent of closure, which is the current frame.

                            match capture_upvalue(objects, frame, index) {
                                Err(e) => return e,
                                Ok(upvalue) => Some(upvalue),
                            }
                        } else {
                            frame.closure.as_ref().upvalues()[i]
                        };
                        *upvalue = upvalue_obj;
                    }
                },
                Some(DefineGlobal) => {
                    let key = match extract_constant_key(&mut frame.ip, chunk) {
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
                            objects.globals.insert(key, value.clone());
                        }
                    }
                    // we pop the value after inserting to globals table so that we can run GC
                    // during table insertion. however, since we rely on std's hash table, we don't
                    // think we can run GC in insertion in this implementation.
                    self.stack.pop();
                }
                Some(GetGlobal) => {
                    let key = match extract_constant_key(&mut frame.ip, chunk) {
                        Some(key) => key,
                        None => {
                            eprintln!("OP_GET_GLOBAL takes a string constant");
                            return InterpretResult::CompileError;
                        }
                    };
                    let value = match objects.globals.get(&key) {
                        Some(value) => value,
                        None => {
                            eprintln!("undefined variable: {}", key.display());
                            return InterpretResult::RuntimeError;
                        }
                    };
                    self.stack.push(value.clone());
                }
                Some(SetGlobal) => {
                    let key = match extract_constant_key(&mut frame.ip, chunk) {
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
                        Some(value) => match objects.globals.entry(key) {
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
                    let local = frame.bp + usize::from(chunk.code()[frame.ip]);
                    frame.ip += 1;
                    self.stack.push(self.stack[local].clone());
                }
                Some(SetLocal) => {
                    let local = frame.bp + usize::from(chunk.code()[frame.ip]);
                    frame.ip += 1;
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
                Some(GetUpvalue) => {
                    let index = usize::from(chunk.code()[frame.ip]);
                    frame.ip += 1;
                    // SAFETY: the closure of the current frame and its upvalue are valid objects
                    unsafe {
                        let upvalue = frame.closure.as_ref().upvalues()[index].unwrap();
                        let stack_index = upvalue.as_ref().stack_index();
                        if stack_index < usize::MAX {
                            let value = self.stack[stack_index].clone();
                            self.stack.push(value);
                        } else {
                            let value = upvalue.as_ref().closed_value().clone();
                            self.stack.push(value);
                        }
                    }
                }
                Some(SetUpvalue) => {
                    let index = usize::from(chunk.code()[frame.ip]);
                    frame.ip += 1;
                    // SAFETY: the closure of the current frame and its upvalue are valid objects
                    unsafe {
                        let mut upvalue = frame.closure.as_ref().upvalues()[index].unwrap();
                        let stack_index = upvalue.as_ref().stack_index();
                        let top = match self.stack.last() {
                            Some(top) => top.clone(),
                            None => {
                                eprintln!("not enough stack for OP_SET_UPVALUE");
                                return InterpretResult::RuntimeError;
                            }
                        };
                        if stack_index < usize::MAX {
                            self.stack[stack_index] = top;
                        } else {
                            *upvalue.as_mut().closed_value_mut() = top;
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
                Some(CloseUpvalue) => {
                    // SAFETY: the objects and stack are valid
                    unsafe {
                        close_upvalue(objects, &self.stack, self.stack.len() - 1);
                    }
                    self.stack.pop();
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
                            match unsafe { (object::as_ref(o1), object::as_ref(o2)) } {
                                (ObjectRef::Str(o1), ObjectRef::Str(o2)) => {
                                    let result = o1.as_rust_str().to_string() + o2.as_rust_str();
                                    let obj = objects.allocate_string(result);
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
                    let diff = chunk.read_jump_location(frame.ip);
                    frame.ip = (frame.ip as isize + isize::from(diff)) as usize;
                }
                Some(JumpIfFalse) => match self.stack.last() {
                    None => {
                        eprintln!("no stack for OP_JUMP_IF_FALSE");
                        return InterpretResult::RuntimeError;
                    }
                    Some(v) => {
                        if v.is_falsy() {
                            let diff = chunk.read_jump_location(frame.ip);
                            frame.ip = (frame.ip as isize + isize::from(diff)) as usize;
                        } else {
                            frame.ip += 2;
                        }
                    }
                },
                Some(Call) => {
                    let args = function.chunk().code()[frame.ip];
                    frame.ip += 1;

                    if !self.call(args) {
                        return InterpretResult::RuntimeError;
                    }
                }
            }
        }
    }
}
