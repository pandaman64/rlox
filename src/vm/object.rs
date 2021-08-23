use std::ptr::{self, NonNull};

use crate::{
    log_gc,
    opcode::Chunk,
    table::InternedStr,
    value::{self, Value},
};

// the pointer must have valid provenance not only for the header but the whole object
// pub type RawValue = NonNull<Value>;
pub type RawObject = NonNull<Header>;
pub type RawStr = NonNull<Str>;
pub type RawFunction = NonNull<Function>;
pub type RawClosure = NonNull<Closure>;
pub type RawUpvalue = NonNull<Upvalue>;

#[repr(u8)]
pub enum ObjectKind {
    Str,
    Function,
    NativeFunction,
    Closure,
    Upvalue,
    Class,
}

#[repr(C)]
pub struct Header {
    pub kind: ObjectKind,
    pub marked: bool,
    // prev: RawObject,
    pub next: Option<RawObject>,
}

impl Header {
    fn new(kind: ObjectKind) -> Self {
        Self {
            kind,
            marked: false,
            next: None,
        }
    }
}

pub enum ObjectRef<'a> {
    Str(&'a Str),
    Function(&'a Function),
    NativeFunction(&'a NativeFunction),
    Closure(&'a Closure),
    // upvalues might contain self-referential pointer, so we do not provide a reference
    Upvalue(*const Upvalue),
    Class(&'a Class),
}

/// # Safety
/// the object must be initialized and its kind must match the runtime representation.
pub unsafe fn as_ref<'a>(obj: RawObject) -> ObjectRef<'a> {
    unsafe {
        let obj: *const Header = obj.as_ptr();
        let kind_ptr = ptr::addr_of!((*obj).kind);
        match ptr::read(kind_ptr) {
            ObjectKind::Str => {
                let str_ptr: *const Str = obj.cast();
                ObjectRef::Str(str_ptr.as_ref().unwrap())
            }
            ObjectKind::Function => {
                let fun_ptr: *const Function = obj.cast();
                ObjectRef::Function(fun_ptr.as_ref().unwrap())
            }
            ObjectKind::NativeFunction => {
                let fun_ptr: *const NativeFunction = obj.cast();
                ObjectRef::NativeFunction(fun_ptr.as_ref().unwrap())
            }
            ObjectKind::Closure => {
                let cls_ptr: *const Closure = obj.cast();
                ObjectRef::Closure(cls_ptr.as_ref().unwrap())
            }
            ObjectKind::Upvalue => {
                let upvalue_ptr: *const Upvalue = obj.cast();
                ObjectRef::Upvalue(upvalue_ptr)
            }
            ObjectKind::Class => {
                let class_ptr: *const Class = obj.cast();
                ObjectRef::Class(class_ptr.as_ref().unwrap())
            }
        }
    }
}

/// # Safety
/// The object must be valid
pub unsafe fn mark(obj: RawObject, worklist: &mut Vec<RawObject>) {
    unsafe {
        let ptr = obj.as_ptr();
        let marked_ptr = ptr::addr_of_mut!((*ptr).marked);

        if ptr::read(marked_ptr) {
            return;
        }

        worklist.push(obj);
        ptr::write(marked_ptr, true);
    }
}

/// # Safety
/// The object must be valid
pub unsafe fn blacken(obj: RawObject, worklist: &mut Vec<RawObject>) {
    // SAFETY: object is valid
    unsafe {
        let ptr = obj.as_ptr();
        let kind_ptr = ptr::addr_of!((*ptr).kind);

        if log_gc() {
            eprintln!(
                "-- gc: {:?} blacken {}",
                obj,
                Value::Object(obj).format_args()
            );
        }

        match ptr::read(kind_ptr) {
            ObjectKind::Str | ObjectKind::NativeFunction => {}
            ObjectKind::Function => {
                let function: *mut Function = ptr.cast();
                if let Some(name) = (*function).name {
                    mark(name.into_raw_obj(), worklist);
                }
                for constant in (*function).chunk.constants() {
                    constant.mark(worklist);
                }
            }
            ObjectKind::Closure => {
                let closure: *mut Closure = ptr.cast();
                mark((*closure).function.cast(), worklist);
                for upvalue in (*closure).upvalues.iter().flatten() {
                    mark(upvalue.cast(), worklist);
                }
            }
            ObjectKind::Upvalue => {
                let upvalue: *mut Upvalue = ptr.cast();
                (*upvalue).closed_value.mark(worklist);
            }
            ObjectKind::Class => {
                let class: *mut Class = ptr.cast();
                mark((*class).name.into_raw_obj().cast(), worklist);
            }
        }
    }
}

// TODO: inline string contents to shave off one allocation
#[repr(C)]
pub struct Str {
    header: Header,
    content: String,
}

impl Str {
    pub(in crate::vm) fn new(content: String) -> Self {
        Self {
            header: Header::new(ObjectKind::Str),
            content,
        }
    }

    pub fn as_rust_str(&self) -> &String {
        &self.content
    }
}

#[repr(C)]
pub struct Function {
    header: Header,
    arity: u8,
    upvalues: u8,
    chunk: Chunk,
    name: Option<InternedStr>,
}

impl Function {
    pub(in crate::vm) fn new_script() -> Self {
        Self {
            header: Header::new(ObjectKind::Function),
            arity: 0,
            upvalues: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }

    pub(in crate::vm) fn new_function(name: InternedStr, arity: u8, upvalues: u8) -> Self {
        Self {
            header: Header::new(ObjectKind::Function),
            arity,
            upvalues,
            chunk: Chunk::new(),
            name: Some(name),
        }
    }

    pub fn name(&self) -> Option<InternedStr> {
        self.name
    }

    pub fn arity(&self) -> u8 {
        self.arity
    }

    pub fn upvalues(&self) -> u8 {
        self.upvalues
    }

    pub fn upvalues_mut(&mut self) -> &mut u8 {
        &mut self.upvalues
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    /// # Safety
    /// self must be a valid function
    pub unsafe fn trace(&self) {
        let chunk = &self.chunk;
        // SAFETY: self is a valid function, so the chunk and name is valid too
        unsafe {
            match self.name {
                None => chunk.trace_chunk("<top-level script>"),
                Some(name) => chunk.trace_chunk(value::format_obj(name.into_raw_obj())),
            }
        }
    }
}

#[repr(C)]
pub struct NativeFunction {
    header: Header,
    // fun takes [fun_obj, arg1, arg2, ..., argN]
    fun: fn(args: &[Value]) -> Value,
}

impl NativeFunction {
    pub(in crate::vm) fn new(fun: fn(args: &[Value]) -> Value) -> Self {
        Self {
            header: Header::new(ObjectKind::NativeFunction),
            fun,
        }
    }

    pub fn fun(&self) -> fn(args: &[Value]) -> Value {
        self.fun
    }
}

#[repr(C)]
pub struct Closure {
    header: Header,
    // This must point to a Function
    function: RawFunction,
    upvalues: Vec<Option<RawUpvalue>>,
}

impl Closure {
    /// # Safety
    /// function must point to valid function
    pub(in crate::vm) unsafe fn new(function: RawFunction) -> Self {
        // SAFETY: given function is valid
        let num_upvalues = unsafe { function.as_ref().upvalues() };
        Self {
            header: Header::new(ObjectKind::Closure),
            function,
            upvalues: vec![None; usize::from(num_upvalues)],
        }
    }

    pub fn function(&self) -> RawFunction {
        self.function
    }

    pub fn upvalues(&self) -> &Vec<Option<RawUpvalue>> {
        &self.upvalues
    }

    pub fn upvalues_mut(&mut self) -> &mut Vec<Option<RawUpvalue>> {
        &mut self.upvalues
    }
}

#[repr(C)]
pub struct Upvalue {
    header: Header,
    // pointer to the next upvalue
    next: Option<RawUpvalue>,
    // the index of the open variable in the stack
    stack_index: usize,
    // when stack_index == usize::MAX, the variable is stored inline
    closed_value: Value,
}

impl Upvalue {
    pub(in crate::vm) fn new_stack(stack_index: usize, next: Option<RawUpvalue>) -> Self {
        Self {
            header: Header::new(ObjectKind::Upvalue),
            next,
            stack_index,
            closed_value: Value::Nil,
        }
    }

    pub fn next(&self) -> Option<RawUpvalue> {
        self.next
    }

    pub fn next_mut(&mut self) -> &mut Option<RawUpvalue> {
        &mut self.next
    }

    pub fn stack_index(&self) -> usize {
        self.stack_index
    }

    pub fn closed_value(&self) -> &Value {
        &self.closed_value
    }

    pub fn closed_value_mut(&mut self) -> &mut Value {
        &mut self.closed_value
    }

    pub fn close(&mut self, value: Value) {
        self.stack_index = usize::MAX;
        self.closed_value = value;
    }
}

#[repr(C)]
pub struct Class {
    header: Header,
    name: InternedStr,
}

impl Class {
    pub fn name(&self) -> InternedStr {
        self.name
    }
}
