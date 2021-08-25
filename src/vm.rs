use std::{
    any::type_name,
    collections::HashSet,
    fmt,
    io::{self, Write},
    mem::size_of,
    ptr,
};

pub mod object;
use crate::{
    log_gc,
    opcode::{Chunk, OpCode},
    stress_gc,
    table::{InternedStr, Key, Table},
    trace_available,
    value::Value,
    HeapSize,
};
use num::FromPrimitive as _;
use object::{
    Closure, Function, NativeFunction, ObjectKind, ObjectRef, RawClosure, RawObject, RawStr,
    RawUpvalue, Str, Upvalue,
};

use self::object::{
    BoundMethod, Class, Instance, RawBoundMethod, RawClass, RawFunction, RawInstance,
};

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

macro_rules! binop {
    ($self:ident, $op:tt, $to_ty:ident) => {{
        match $self.stack.as_slice() {
            [.., v1, v2] => {
                let result = match (v1, v2) {
                    (Value::Number(n1), Value::Number(n2)) => n1 $op n2,
                    _ => {
                        eprintln!("Operands must be numbers.");
                        return InterpretResult::RuntimeError;
                    }
                };
                $self.stack.pop();
                $self.stack.pop();
                $self.stack.push(Value::$to_ty(result));
            },
            _ => return InterpretResult::CompileError,
        }
    }};
}

#[cfg(not(miri))]
const MAX_CALL_FRAME: usize = 65536;
#[cfg(miri)]
const MAX_CALL_FRAME: usize = 100;

const GC_HEAP_GLOW_FACTOR: usize = 2;
const GC_INITIAL_HEAP: usize = 1024 * 1024;

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

fn mark_nothing(_worklist: &mut Vec<RawObject>) {}

struct Objects {
    objects_head: Option<RawObject>,
    open_upvalues_head: Option<RawUpvalue>,
    strings: HashSet<InternedStr>,
    globals: Table,
    init_str: Option<InternedStr>,
    // in bytes
    allocated: usize,
    next_gc: usize,
}

impl Objects {
    fn new() -> Self {
        let mut this = Self {
            objects_head: None,
            open_upvalues_head: None,
            strings: HashSet::new(),
            globals: Table::new(),
            init_str: None,
            allocated: 0,
            next_gc: GC_INITIAL_HEAP,
        };
        this.init_str = Some(this.allocate_string("init".into(), &[], &[], mark_nothing));
        this
    }

    fn allocate_object<T: HeapSize, F: FnOnce(&mut Vec<RawObject>)>(
        &mut self,
        obj: T,
        stack: &[Value],
        frames: &[CallFrame],
        mark_compiler_roots: F,
    ) -> RawObject {
        let size = size_of::<T>() + obj.heap_size();
        self.allocated += size;

        if stress_gc() || self.allocated > self.next_gc {
            self.collect_garbage(stack, frames, mark_compiler_roots);
        }

        let obj_ptr = Box::into_raw(Box::new(obj));
        if log_gc() {
            eprintln!(
                "-- gc: {:?} allocate {:4} bytes for {}",
                obj_ptr,
                size,
                type_name::<T>()
            );
        }
        RawObject::new(obj_ptr as _).unwrap()
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
    /// obj must be valid and allocated by allocate_object
    unsafe fn free_object(&mut self, obj: RawObject) -> Option<RawObject> {
        let log = log_gc();
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
                ($ptr:expr, $ty:ident) => {{
                    let ptr = $ptr as *mut object::$ty;
                    let size =
                        std::mem::size_of::<object::$ty>() + ptr.as_ref().unwrap().heap_size();

                    if log {
                        eprintln!(
                            "-- gc: {:?}     free {:4} bytes of type {}",
                            ptr,
                            size,
                            stringify!($ty)
                        );
                    }

                    self.allocated -= size;
                    drop(Box::from_raw(ptr));
                }};
            }
            match ptr::read(kind_ptr) {
                ObjectKind::Str => free!(obj_ptr, Str),
                ObjectKind::Function => free!(obj_ptr, Function),
                ObjectKind::NativeFunction => free!(obj_ptr, NativeFunction),
                ObjectKind::Closure => free!(obj_ptr, Closure),
                ObjectKind::Upvalue => free!(obj_ptr, Upvalue),
                ObjectKind::Class => free!(obj_ptr, Class),
                ObjectKind::Instance => free!(obj_ptr, Instance),
                ObjectKind::BoundMethod => free!(obj_ptr, BoundMethod),
            }

            next
        }
    }

    /// # Safety
    /// no chunks referring to objects managed by this vm can be run after calling free_objects
    unsafe fn free_all_objects(&mut self) {
        self.strings.clear();
        self.globals.clear();
        self.init_str = None;

        while let Some(obj) = { self.objects_head } {
            // SAFETY: self.objects points to an initialized object and covers the whole allocation
            // in an type-safe manner. this property must be guaranteed by allocation methods.
            unsafe {
                self.objects_head = self.free_object(obj);
            }
        }
    }

    fn allocate_string<F: FnOnce(&mut Vec<RawObject>)>(
        &mut self,
        content: String,
        stack: &[Value],
        frames: &[CallFrame],
        mark_compiler_roots: F,
    ) -> InternedStr {
        unsafe {
            let s = Str::new(content);
            // SAFETY: the pointer passed to InternedStr::new is a valid pointer to the string
            let str_ptr = InternedStr::new(
                self.allocate_object(s, stack, frames, mark_compiler_roots)
                    .cast(),
            );

            match self.strings.get(&str_ptr) {
                None => {
                    self.add_object_head(str_ptr.into_raw_obj());
                    self.strings.insert(str_ptr);
                    str_ptr
                }
                Some(interned) => {
                    let interned = *interned;
                    // free the constructed string since we return from the strings table
                    self.free_object(str_ptr.into_raw_obj());
                    interned
                }
            }
        }
    }

    fn allocate_script<F: FnOnce(&mut Vec<RawObject>)>(
        &mut self,
        stack: &[Value],
        frames: &[CallFrame],
        mark_compiler_roots: F,
    ) -> RawFunction {
        let function = Function::new_script();
        let obj = self.allocate_object(function, stack, frames, mark_compiler_roots);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj.cast()
    }

    #[allow(clippy::too_many_arguments)]
    fn allocate_function<F: FnOnce(&mut Vec<RawObject>)>(
        &mut self,
        name: InternedStr,
        arity: u8,
        upvalues: u8,
        stack: &[Value],
        frames: &[CallFrame],
        mark_compiler_roots: F,
    ) -> RawFunction {
        let function = Function::new_function(name, arity, upvalues);
        let obj = self.allocate_object(function, stack, frames, mark_compiler_roots);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj.cast()
    }

    fn allocate_closure(
        &mut self,
        closure: Closure,
        stack: &[Value],
        frames: &[CallFrame],
    ) -> RawClosure {
        let obj = self.allocate_object(closure, stack, frames, mark_nothing);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj.cast()
    }

    fn allocate_bound_method(
        &mut self,
        bound_method: BoundMethod,
        stack: &[Value],
        frames: &[CallFrame],
    ) -> RawBoundMethod {
        let obj = self.allocate_object(bound_method, stack, frames, mark_nothing);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj.cast()
    }

    fn allocate_class(&mut self, class: Class, stack: &[Value], frames: &[CallFrame]) -> RawClass {
        let obj = self.allocate_object(class, stack, frames, mark_nothing);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj.cast()
    }

    fn allocate_instance(
        &mut self,
        instance: Instance,
        stack: &[Value],
        frames: &[CallFrame],
    ) -> RawInstance {
        let obj = self.allocate_object(instance, stack, frames, mark_nothing);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj.cast()
    }

    fn allocate_upvalue(
        &mut self,
        upvalue: Upvalue,
        stack: &[Value],
        frames: &[CallFrame],
    ) -> RawUpvalue {
        let obj = self.allocate_object(upvalue, stack, frames, mark_nothing);
        // SAFETY: obj points to a valid object
        unsafe {
            self.add_object_head(obj);
        }
        obj.cast()
    }

    /// # Safety
    /// the objects and the frame must be valid
    unsafe fn capture_upvalue(
        &mut self,
        frames: &mut [CallFrame],
        index: u8,
        stack: &[Value],
    ) -> Result<RawUpvalue, InterpretResult> {
        let frame = frames.last_mut().unwrap();
        let stack_index = frame.bp.saturating_add(usize::from(index));
        if stack_index == usize::MAX {
            eprintln!("upvalue index cannot exceed usize::MAX");
            return Err(InterpretResult::RuntimeError);
        }

        let mut prev = None;
        let mut current = self.open_upvalues_head;

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
        let upvalue_obj = self.allocate_upvalue(upvalue, stack, frames);

        match prev {
            None => self.open_upvalues_head = Some(upvalue_obj),
            // SAFETY: the objects must be valid
            Some(mut prev) => unsafe {
                *prev.as_mut().next_mut() = Some(upvalue_obj);
            },
        }

        Ok(upvalue_obj)
    }

    /// # Safety
    /// the objects and stack must be valid
    unsafe fn close_upvalue(&mut self, stack: &[Value], stack_index: usize) {
        // SAFETY: the objects and stack are valid
        unsafe {
            while let Some(mut head) = self.open_upvalues_head {
                if head.as_ref().stack_index() < stack_index {
                    break;
                }

                let head = head.as_mut();
                let value = stack[head.stack_index()].clone();
                head.close(value);
                self.open_upvalues_head = head.next();
            }
        }
    }

    /// # Safety
    /// function and chunk must be valid
    unsafe fn set_chunk(&mut self, mut function: RawFunction, chunk: Chunk) {
        // SAFETY: function is valid
        unsafe {
            let function = function.as_mut();
            let original_size = function.heap_size();
            function.set_chunk(chunk);
            let new_size = function.heap_size();

            self.allocated -= original_size;
            self.allocated += new_size;
        }
    }

    fn collect_garbage<F: FnOnce(&mut Vec<RawObject>)>(
        &mut self,
        stack: &[Value],
        frames: &[CallFrame],
        mark_compiler_roots: F,
    ) {
        // N.B. it's ok that these functions take shared references to values and objects because
        // we read a writable pointer to the allocation through the shared references.
        fn mark_roots(stack: &[Value], worklist: &mut Vec<RawObject>) {
            for value in stack.iter() {
                // SAFETY: GC keeps the objects in the stack valid
                unsafe {
                    value.mark(worklist);
                }
            }
        }
        fn mark_table(table: &Table, worklist: &mut Vec<RawObject>) {
            for (key, value) in table.iter() {
                let key = key.into_raw_obj();
                unsafe {
                    object::mark(key, worklist);
                    value.mark(worklist);
                }
            }
        }
        fn mark_call_frames(frames: &[CallFrame], worklist: &mut Vec<RawObject>) {
            for frame in frames.iter() {
                let obj = frame.closure.cast();
                unsafe {
                    object::mark(obj, worklist);
                }
            }
        }
        fn mark_upvalues(mut head: Option<RawUpvalue>, worklist: &mut Vec<RawObject>) {
            while let Some(upvalue) = head {
                unsafe {
                    object::mark(upvalue.cast(), worklist);
                    let upvalue = upvalue.as_ref();
                    head = upvalue.next();
                }
            }
        }
        fn trace_references(worklist: &mut Vec<RawObject>) {
            while let Some(obj) = worklist.pop() {
                unsafe {
                    object::blacken(obj, worklist);
                }
            }
        }
        fn sweep(this: &mut Objects) {
            unsafe {
                let mut prev = None;
                let mut current = this.objects_head;

                while let Some(obj) = current {
                    let marked_ptr = ptr::addr_of_mut!((*obj.as_ptr()).marked);
                    if ptr::read(marked_ptr) {
                        ptr::write(marked_ptr, false);
                        let next_ptr = ptr::addr_of!((*obj.as_ptr()).next);
                        prev = Some(obj);
                        current = ptr::read(next_ptr);
                    } else {
                        let unreachable_obj = obj;
                        let next_ptr = ptr::addr_of!((*obj.as_ptr()).next);
                        current = ptr::read(next_ptr);

                        if let Some(prev) = prev {
                            let next_ptr = ptr::addr_of_mut!((*prev.as_ptr()).next);
                            ptr::write(next_ptr, current);
                        } else {
                            this.objects_head = current;
                        }

                        this.free_object(unreachable_obj);
                    }
                }
            }
        }
        fn remove_white_interned_string(strings: &mut HashSet<InternedStr>) {
            // the strings table holds a weak pointer. in other words, if an interned string is
            // white (not marked), we remove it from the strings table and later collect it.
            strings.retain(|s| unsafe {
                let obj = s.into_raw_obj();
                let marked_ptr = ptr::addr_of!((*obj.as_ptr()).marked);
                ptr::read(marked_ptr)
            })
        }

        let before = self.allocated;
        let log = log_gc();
        if log {
            eprintln!("-- gc begin");
        }

        // TODO: tune initial capacity?
        let mut worklist = vec![];

        mark_roots(stack, &mut worklist);
        mark_table(&self.globals, &mut worklist);
        mark_call_frames(frames, &mut worklist);
        mark_upvalues(self.open_upvalues_head, &mut worklist);
        mark_compiler_roots(&mut worklist);
        if let Some(init_str) = self.init_str {
            unsafe {
                object::mark(init_str.into_raw_obj(), &mut worklist);
            }
        }

        trace_references(&mut worklist);
        remove_white_interned_string(&mut self.strings);
        sweep(self);

        self.next_gc = self.allocated * GC_HEAP_GLOW_FACTOR;

        if log {
            eprintln!("-- gc end");
            eprintln!(
                "   collected {:4} bytes (from {:4} to {:4}) next at {:4}",
                before - self.allocated,
                before,
                self.allocated,
                self.next_gc
            );
        }
    }
}

pub struct Vm<'w> {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    objects: Objects,
    stdout: &'w mut (dyn Write + 'w),
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

impl<'w> Vm<'w> {
    pub fn new(stdout: &'w mut (dyn Write + 'w)) -> Self {
        Self {
            stack: vec![],
            frames: vec![],
            objects: Objects::new(),
            stdout,
        }
    }

    pub fn allocate_string<F: FnOnce(&mut Vec<RawObject>)>(
        &mut self,
        content: String,
        mark_compiler_roots: F,
    ) -> InternedStr {
        self.objects
            .allocate_string(content, &self.stack, &self.frames, mark_compiler_roots)
    }

    pub fn allocate_script<F: FnOnce(&mut Vec<RawObject>)>(
        &mut self,
        mark_compiler_roots: F,
    ) -> RawFunction {
        self.objects
            .allocate_script(&self.stack, &self.frames, mark_compiler_roots)
    }

    pub fn allocate_function<F: FnOnce(&mut Vec<RawObject>)>(
        &mut self,
        name: InternedStr,
        arity: u8,
        upvalues: u8,
        mark_compiler_roots: F,
    ) -> RawFunction {
        self.objects.allocate_function(
            name,
            arity,
            upvalues,
            &self.stack,
            &self.frames,
            mark_compiler_roots,
        )
    }

    fn allocate_bound_method(&mut self, receiver: Value, method: RawClosure) -> RawBoundMethod {
        let bound_method = BoundMethod::new(receiver, method);
        self.objects
            .allocate_bound_method(bound_method, &self.stack, &self.frames)
    }

    fn allocate_class(&mut self, name: InternedStr) -> RawClass {
        let class = Class::new(name);
        self.objects
            .allocate_class(class, &self.stack, &self.frames)
    }

    fn allocate_instance(&mut self, class: RawClass) -> RawInstance {
        let instance = Instance::new(class);
        self.objects
            .allocate_instance(instance, &self.stack, &self.frames)
    }

    /// # Safety
    /// function and chunk must be valid
    pub unsafe fn set_chunk(&mut self, function: RawFunction, chunk: Chunk) {
        // SAFETY: function and chunk are valid
        unsafe {
            self.objects.set_chunk(function, chunk);
        }
    }

    /// # Safety
    /// no chunks referring to objects managed by this vm can be run after calling free_objects
    pub unsafe fn free_all_objects(&mut self) {
        // SAFETY: no chunks referring to objects managed by this vm can be run after calling free_objects
        unsafe {
            self.objects.free_all_objects();
        }
    }

    pub fn define_native_function(&mut self, name: String, fun: fn(args: &[Value]) -> Value) {
        let name = self
            .objects
            .allocate_string(name, &self.stack, &self.frames, mark_nothing);
        // register name as GC roots
        self.stack.push(Value::Object(name.into_raw_obj()));

        let fun_obj = {
            let function = NativeFunction::new(fun);
            let fun_obj =
                self.objects
                    .allocate_object(function, &self.stack, &self.frames, mark_nothing);
            // SAFETY: obj points to a valid object
            unsafe {
                self.objects.add_object_head(fun_obj);
            }
            fun_obj
        };
        // register fun as GC roots
        self.stack.push(Value::Object(fun_obj));

        self.objects.globals.insert(
            &mut self.objects.allocated,
            Key::new(name),
            Value::Object(fun_obj),
        );

        // un-register objects
        self.stack.pop();
        self.stack.pop();
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    pub fn init_str(&self) -> InternedStr {
        self.objects.init_str.unwrap()
    }

    /// # Safety
    /// The given function must be valid
    pub unsafe fn reset(&mut self, function: RawFunction) {
        // SAFETY: function is valid
        assert_eq!(unsafe { function.as_ref().upvalues() }, 0);

        self.frames = vec![];

        let fun_obj: RawObject = function.cast();
        self.stack = vec![Value::Object(fun_obj)];
        // SAFETY: function is valid
        let closure = unsafe { Closure::new(fun_obj.cast()) };
        let cls_obj = self
            .objects
            .allocate_closure(closure, &self.stack, &self.frames);
        self.stack.pop();
        self.stack.push(Value::Object(cls_obj.cast()));
    }

    fn push_frame(&mut self, closure: RawClosure, args: u8, bp: usize) -> bool {
        // SAFETY: `function` is a reference to the function object derived by `closure.function`.
        // since we assume that our GC keeps values reachable from the stack valid, accessing
        // function is safe. moreover, the `function` reference dies before the later use of
        // closure, which satisfies Stacked Borrows.
        unsafe {
            let function = closure.as_ref().function().as_ref();
            if function.arity() != args {
                eprintln!("Expected {} arguments but got {}.", function.arity(), args);
                return false;
            }
        }
        self.frames.push(CallFrame { closure, ip: 0, bp });
        true
    }

    pub fn call(&mut self, args: u8) -> bool {
        assert!(self.frames.len() < MAX_CALL_FRAME);
        let bp = self.stack.len() - usize::from(args) - 1;
        let callee = *match self.stack.get(bp) {
            Some(Value::Object(callee)) => callee,
            Some(_) => {
                eprintln!("Can only call functions and classes.");
                return false;
            }
            None => {
                eprintln!("not enough stack for OP_CALL");
                return false;
            }
        };
        // SAFETY: values in the stack are valid
        match unsafe { object::as_ref(callee) } {
            // call the given closure
            ObjectRef::Closure(_closure) => self.push_frame(callee.cast(), args, bp),
            // call the bound closure
            ObjectRef::BoundMethod(bound_method) => {
                self.stack[bp] = bound_method.receiver();
                self.push_frame(bound_method.method(), args, bp)
            }
            // call the given native function
            ObjectRef::NativeFunction(function) => {
                let result = function.fun()(&self.stack[bp..]);
                self.stack.truncate(bp);
                self.stack.push(result);
                true
            }
            // initialize an instance of the given class (akin to `new`)
            ObjectRef::Class(_class) => {
                // we must not use _class since we use its parent (callee) here
                let class = callee.cast();
                let instance = self.allocate_instance(class);
                self.stack[bp] = Value::Object(instance.cast());

                let init_str = Key::new(self.objects.init_str.unwrap());
                // SAFETY: values in the stack are valid
                unsafe {
                    if let Some(init) = class.as_ref().methods().get(init_str) {
                        // the class has init()
                        match init {
                            Value::Object(obj)
                                if matches!(object::as_ref(obj), ObjectRef::Closure(_)) =>
                            {
                                self.push_frame(obj.cast(), args, bp)
                            }
                            _ => unreachable!(),
                        }
                    } else if args != 0 {
                        // the class does not have init(), but non-zero arguments are supplied
                        eprintln!("Expected 0 arguments but got {}.", args);
                        false
                    } else {
                        // the class does not have init(), and zero arguments are supplied
                        true
                    }
                }
            }
            ObjectRef::Function(_function) => {
                eprintln!("Can only call functions and classes.");
                false
            }
            ObjectRef::Str(_) | ObjectRef::Upvalue(_) | ObjectRef::Instance(_) => {
                eprintln!("Can only call functions and classes.");
                false
            }
        }
    }

    fn invoke_from_class(
        &mut self,
        class: RawClass,
        method_name: Key,
        args: u8,
        bp: usize,
    ) -> bool {
        // SAFETY: given values are valid
        unsafe {
            match class.as_ref().methods().get(method_name) {
                None => {
                    eprintln!("Undefined property '{}'.", method_name.display());
                    false
                }
                Some(method) => match method {
                    Value::Object(obj) if matches!(object::as_ref(obj), ObjectRef::Closure(_)) => {
                        self.push_frame(obj.cast(), args, bp)
                    }
                    _ => {
                        eprintln!("method must be a closure");
                        false
                    }
                },
            }
        }
    }

    fn invoke(&mut self, method_name: Key, args: u8) -> bool {
        assert!(self.frames.len() < MAX_CALL_FRAME);
        let bp = self.stack.len() - usize::from(args) - 1;

        let receiver = self.stack[bp].clone();
        // SAFETY: values in the stack are valid
        unsafe {
            match receiver {
                Value::Object(obj) if matches!(object::as_ref(obj), ObjectRef::Instance(_)) => {
                    let instance: RawInstance = obj.cast();

                    if let Some(method) = instance.as_ref().fields().get(method_name) {
                        self.stack[bp] = method;
                        return self.call(args);
                    }

                    let class = instance.as_ref().class();
                    self.invoke_from_class(class, method_name, args, bp)
                }
                _ => {
                    eprintln!("Only instances have methods.");
                    false
                }
            }
        }
    }

    pub fn print<M: fmt::Display>(&mut self, message: M) -> io::Result<()> {
        write!(self.stdout, "{}", message)
    }

    pub fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }

    fn define_method(&mut self, method_name: Key) -> Result<(), InterpretResult> {
        match &self.stack[self.stack.len().saturating_sub(2)..] {
            [class, method] => match class {
                // SAFETY: values in the stack are valid
                Value::Object(obj)
                    if matches!(unsafe { object::as_ref(*obj) }, ObjectRef::Class(_)) =>
                {
                    unsafe {
                        let mut class: RawClass = obj.cast();
                        class.as_mut().methods_mut().insert(
                            &mut self.objects.allocated,
                            method_name,
                            method.clone(),
                        );
                        // pop method
                        self.stack.pop();
                        Ok(())
                    }
                }
                _ => {
                    eprintln!("OP_METHOD must take a class");
                    Err(InterpretResult::CompileError)
                }
            },
            _ => {
                eprintln!("not enough stack for OP_METHOD");
                Err(InterpretResult::CompileError)
            }
        }
    }

    fn bind_method(&self, class: RawClass, method_name: Key) -> Option<Value> {
        unsafe { class.as_ref().methods().get(method_name) }
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
                    "[line {}] in ",
                    function.chunk().line()[frame.ip.saturating_sub(1)]
                );
                match function.name() {
                    Some(name) => eprintln!("{}()", name.display()),
                    None => eprintln!("script"),
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
                        objects.close_upvalue(&self.stack, frame.bp);
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
                                if writeln!(self.stdout, "{}", v.format_args()).is_err() {
                                    return InterpretResult::RuntimeError;
                                }
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
                    let mut cls_obj = objects.allocate_closure(closure, &self.stack, &self.frames);
                    self.stack.push(Object(cls_obj.cast()));

                    // SAFETY: given function is valid and we push to the call frame only valid closures
                    let cls = cls_obj.as_mut();
                    let function = self
                        .frames
                        .last_mut()
                        .unwrap()
                        .closure
                        .as_ref()
                        .function()
                        .as_ref();
                    let chunk = function.chunk();
                    for (i, upvalue) in cls.upvalues_mut().iter_mut().enumerate() {
                        let frame = self.frames.last_mut().unwrap();
                        let is_local = chunk.code()[frame.ip];
                        let index = chunk.code()[frame.ip + 1];
                        frame.ip += 2;
                        let upvalue_obj = if is_local > 0 {
                            // if `is_local == 1`, the upvalue is captured from the call-stack of
                            // the direct parent of closure, which is the current frame.

                            match objects.capture_upvalue(&mut self.frames, index, &self.stack) {
                                Err(e) => return e,
                                Ok(upvalue) => Some(upvalue),
                            }
                        } else {
                            frame.closure.as_ref().upvalues()[i]
                        };
                        *upvalue = upvalue_obj;
                    }
                },
                Some(Class) => {
                    let name = match extract_constant_key(&mut frame.ip, chunk) {
                        Some(key) => key,
                        None => {
                            eprintln!("OP_CLASS takes a string constant");
                            return InterpretResult::CompileError;
                        }
                    };
                    let class = self.allocate_class(name.into_interned_str());
                    self.stack.push(Value::Object(class.cast()));
                }
                Some(Method) => {
                    let method_name = match extract_constant_key(&mut frame.ip, chunk) {
                        Some(key) => key,
                        None => {
                            eprintln!("OP_METHOD takes a string constant");
                            return InterpretResult::CompileError;
                        }
                    };
                    if let Err(e) = self.define_method(method_name) {
                        return e;
                    }
                }
                Some(Inherit) => {
                    match &self.stack[self.stack.len().saturating_sub(2)..] {
                        // SAFETY: values in the stack are valid
                        [super_class, subclass] => unsafe {
                            match super_class {
                                Value::Object(obj)
                                    if matches!(object::as_ref(*obj), ObjectRef::Class(_)) =>
                                {
                                    let super_class: RawClass = obj.cast();
                                    let mut subclass: RawClass = match subclass {
                                        Value::Object(obj)
                                            if matches!(
                                                object::as_ref(*obj),
                                                ObjectRef::Class(_)
                                            ) =>
                                        {
                                            obj.cast()
                                        }
                                        _ => {
                                            eprintln!("subclass is not a class");
                                            return InterpretResult::CompileError;
                                        }
                                    };

                                    for (name, method) in super_class.as_ref().methods().iter() {
                                        subclass.as_mut().methods_mut().insert(
                                            &mut self.objects.allocated,
                                            *name,
                                            method.clone(),
                                        );
                                    }
                                    // pop subclass
                                    self.stack.pop();
                                }
                                _ => {
                                    eprintln!("Superclass must be a class.");
                                    return InterpretResult::RuntimeError;
                                }
                            }
                        },
                        _ => {
                            eprintln!("OP_INHERIT takes super class and subclass");
                            return InterpretResult::CompileError;
                        }
                    }
                }
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
                            objects
                                .globals
                                .insert(&mut objects.allocated, key, value.clone());
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
                    let value = match objects.globals.get(key) {
                        Some(value) => value,
                        None => {
                            eprintln!("Undefined variable '{}'.", key.display());
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
                        Some(value) => {
                            let old_size = objects.globals.heap_size();
                            match objects.globals.entry(key) {
                                Entry::Vacant(v) => {
                                    eprintln!("Undefined variable '{}'.", v.key().display());
                                    return InterpretResult::RuntimeError;
                                }
                                Entry::Occupied(mut o) => {
                                    o.insert(value.clone());
                                }
                            }
                            let new_size = objects.globals.heap_size();
                            objects.allocated += new_size - old_size;
                        }
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
                Some(GetProperty) => {
                    let name = match extract_constant_key(&mut frame.ip, chunk) {
                        Some(key) => key,
                        None => {
                            eprintln!("OP_GET_PROPERTY takes a string constant");
                            return InterpretResult::CompileError;
                        }
                    };
                    // SAFETY: values in the stack are valid
                    unsafe {
                        let instance: RawInstance = match self.stack.last() {
                            Some(value) => match value {
                                Value::Object(obj)
                                    if matches!(object::as_ref(*obj), ObjectRef::Instance(_)) =>
                                {
                                    obj.cast()
                                }
                                _ => {
                                    eprintln!("Only instances have properties.");
                                    return InterpretResult::RuntimeError;
                                }
                            },
                            None => {
                                eprintln!("not enough stack for OP_GET_PROPERTY");
                                return InterpretResult::RuntimeError;
                            }
                        };

                        if let Some(value) = instance.as_ref().fields().get(name) {
                            let value = value.clone();
                            // pop the instance
                            self.stack.pop();
                            self.stack.push(value);
                        } else if let Some(method) =
                            self.bind_method(instance.as_ref().class(), name)
                        {
                            match method {
                                Value::Object(obj)
                                    if matches!(object::as_ref(obj), ObjectRef::Closure(_)) =>
                                {
                                    let bound_method = self.allocate_bound_method(
                                        Value::Object(instance.cast()),
                                        obj.cast(),
                                    );
                                    // pop the instance
                                    self.stack.pop();
                                    self.stack.push(Value::Object(bound_method.cast()));
                                }
                                _ => {
                                    eprintln!("an instance must have closure as method");
                                    return InterpretResult::CompileError;
                                }
                            }
                        } else {
                            eprintln!("Undefined property '{}'.", name.display());
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                Some(SetProperty) => {
                    let name = match extract_constant_key(&mut frame.ip, chunk) {
                        Some(key) => key,
                        None => {
                            eprintln!("OP_SET_PROPERTY takes a string constant");
                            return InterpretResult::CompileError;
                        }
                    };
                    let len = self.stack.len();
                    if len < 2 {
                        eprintln!("insufficient stack for OP_SET_PROPERTY");
                        return InterpretResult::CompileError;
                    }
                    match &mut self.stack[len - 2..] {
                        [instance, value] => {
                            // SAFETY: values in the stack are valid
                            unsafe {
                                let mut instance: RawInstance = match instance {
                                    Value::Object(obj)
                                        if matches!(
                                            object::as_ref(*obj),
                                            ObjectRef::Instance(_)
                                        ) =>
                                    {
                                        obj.cast()
                                    }
                                    _ => {
                                        eprintln!("Only instances have fields.");
                                        return InterpretResult::RuntimeError;
                                    }
                                };
                                instance.as_mut().fields_mut().insert(
                                    &mut objects.allocated,
                                    name,
                                    value.clone(),
                                );

                                let value = self.stack.pop().unwrap();
                                self.stack.pop();
                                self.stack.push(value);
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                Some(GetSuper) => {
                    match &self.stack[self.stack.len().saturating_sub(2)..] {
                        // SAFETY: values in the stack are valid
                        [this, super_class] => unsafe {
                            // the compiler ensures that `this` is the receiver
                            let this = this.clone();
                            let method_name = match extract_constant_key(&mut frame.ip, chunk) {
                                Some(key) => key,
                                None => {
                                    eprintln!("OP_SET_PROPERTY takes a string constant");
                                    return InterpretResult::CompileError;
                                }
                            };

                            let super_class: RawClass = match super_class {
                                Value::Object(obj)
                                    if matches!(object::as_ref(*obj), ObjectRef::Class(_)) =>
                                {
                                    obj.cast()
                                }
                                _ => {
                                    eprintln!("OP_SET_PROPERTY requires a super class");
                                    return InterpretResult::CompileError;
                                }
                            };

                            if let Some(method) = self.bind_method(super_class, method_name) {
                                match method {
                                    Value::Object(obj)
                                        if matches!(object::as_ref(obj), ObjectRef::Closure(_)) =>
                                    {
                                        let bound_method =
                                            self.allocate_bound_method(this, obj.cast());
                                        // pop the instance
                                        self.stack.pop();
                                        self.stack.push(Value::Object(bound_method.cast()));
                                    }
                                    _ => {
                                        eprintln!("an instance must have closure as method");
                                        return InterpretResult::CompileError;
                                    }
                                }
                            } else {
                                eprintln!("Undefined property '{}'.", method_name.display());
                                return InterpretResult::RuntimeError;
                            }
                        },
                        _ => {
                            eprintln!("not enough stack for OP_GET_SUPER");
                            return InterpretResult::CompileError;
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
                        objects.close_upvalue(&self.stack, self.stack.len() - 1);
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

                    // SAFETY: v1 and v2 are valid object
                    let eq = unsafe { v1.eq(&v2) };
                    self.stack.push(Value::Bool(eq));
                }
                Some(Less) => binop!(self, <, Bool),
                Some(Greater) => binop!(self, >, Bool),
                Some(Negate) => match self.stack.last() {
                    None => {
                        eprintln!("Expected number, got empty stack");
                        return InterpretResult::CompileError;
                    }
                    Some(Number(n)) => {
                        let value = Number(-n);
                        self.stack.pop();
                        self.stack.push(value);
                    }
                    Some(_) => {
                        eprintln!("Operand must be a number.");
                        return InterpretResult::RuntimeError;
                    }
                },
                Some(Not) => match self.stack.last() {
                    Some(v) => {
                        let falsy = v.is_falsy();
                        let last = self.stack.len();
                        self.stack[last - 1] = Bool(falsy);
                    }
                    None => {
                        eprintln!("Expected bool or nil, got empty stack");
                        return InterpretResult::CompileError;
                    }
                },
                Some(Add) => {
                    let len = self.stack.len();
                    if len < 2 {
                        eprintln!("insufficient stack");
                        return InterpretResult::CompileError;
                    }
                    match &self.stack[len - 2..] {
                        [Object(o1), Object(o2)] => {
                            // SAFETY: these objects are valid
                            match unsafe { (object::as_ref(*o1), object::as_ref(*o2)) } {
                                (ObjectRef::Str(o1), ObjectRef::Str(o2)) => {
                                    let result = o1.as_rust_str().to_string() + o2.as_rust_str();
                                    let obj = objects.allocate_string(
                                        result,
                                        &self.stack,
                                        &self.frames,
                                        mark_nothing,
                                    );
                                    self.stack.pop();
                                    self.stack.pop();
                                    self.stack.push(Value::Object(obj.into_raw_obj()));
                                }
                                _ => {
                                    eprintln!("Operands must be two numbers or two strings.");
                                    return InterpretResult::RuntimeError;
                                }
                            }
                        }
                        [Number(_), Number(_)] => {
                            binop!(self, +, Number);
                        }
                        [_v2, _v1] => {
                            eprintln!("Operands must be two numbers or two strings.");
                            return InterpretResult::RuntimeError;
                        }
                        _ => unreachable!(),
                    }
                }
                Some(Subtract) => binop!(self, -, Number),
                Some(Multiply) => binop!(self, *, Number),
                Some(Divide) => binop!(self, /, Number),
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

                    if self.frames.len() == MAX_CALL_FRAME {
                        eprintln!("Stack overflow.");
                        return InterpretResult::RuntimeError;
                    }
                    if !self.call(args) {
                        return InterpretResult::RuntimeError;
                    }
                }
                Some(Invoke) => {
                    let method_name = match extract_constant_key(&mut frame.ip, chunk) {
                        Some(key) => key,
                        None => {
                            eprintln!("OP_INVOKE takes a string constant");
                            return InterpretResult::CompileError;
                        }
                    };
                    let args = function.chunk().code()[frame.ip];
                    frame.ip += 1;

                    if self.frames.len() == MAX_CALL_FRAME {
                        eprintln!("Stack overflow.");
                        return InterpretResult::RuntimeError;
                    }
                    if !self.invoke(method_name, args) {
                        return InterpretResult::RuntimeError;
                    }
                }
            }
        }
    }
}
