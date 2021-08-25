use std::ptr::{self, NonNull};

use crate::{
    log_gc,
    opcode::Chunk,
    table::{InternedStr, Table},
    value::{self, Value},
    HeapSize,
};

// the pointer must have valid provenance not only for the header but the whole object
// pub type RawValue = NonNull<Value>;
pub type RawObject = NonNull<Header>;
pub type RawStr = NonNull<Str>;
pub type RawFunction = NonNull<Function>;
pub type RawClosure = NonNull<Closure>;
pub type RawUpvalue = NonNull<Upvalue>;
pub type RawClass = NonNull<Class>;
pub type RawInstance = NonNull<Instance>;
pub type RawBoundMethod = NonNull<BoundMethod>;

#[derive(PartialEq, Eq)]
#[repr(u8)]
pub enum ObjectKind {
    Str,
    Function,
    NativeFunction,
    Closure,
    Upvalue,
    Class,
    Instance,
    BoundMethod,
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
    // upvalues does not contain self-referencial pointer, so it's safe to return a reference
    Upvalue(&'a Upvalue),
    Class(&'a Class),
    Instance(&'a Instance),
    BoundMethod(&'a BoundMethod),
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
                ObjectRef::Upvalue(upvalue_ptr.as_ref().unwrap())
            }
            ObjectKind::Class => {
                let class_ptr: *const Class = obj.cast();
                ObjectRef::Class(class_ptr.as_ref().unwrap())
            }
            ObjectKind::Instance => {
                let instance_ptr: *const Instance = obj.cast();
                ObjectRef::Instance(instance_ptr.as_ref().unwrap())
            }
            ObjectKind::BoundMethod => {
                let bound_method_ptr: *const BoundMethod = obj.cast();
                ObjectRef::BoundMethod(bound_method_ptr.as_ref().unwrap())
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
                for (key, value) in (*class).methods.iter() {
                    mark(key.into_raw_obj(), worklist);
                    value.mark(worklist);
                }
            }
            ObjectKind::Instance => {
                let instance: *mut Instance = ptr.cast();
                mark((*instance).class.cast(), worklist);
                for (key, value) in (*instance).fields.iter() {
                    mark(key.into_raw_obj(), worklist);
                    value.mark(worklist);
                }
            }
            ObjectKind::BoundMethod => {
                let bound_method: *mut BoundMethod = ptr.cast();
                (*bound_method).receiver.mark(worklist);
                mark((*bound_method).method.cast(), worklist);
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

impl HeapSize for Str {
    fn heap_size(&self) -> usize {
        self.content.heap_size()
    }
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

impl HeapSize for Function {
    fn heap_size(&self) -> usize {
        self.chunk.heap_size()
    }
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

    pub(in crate::vm) fn set_chunk(&mut self, chunk: Chunk) {
        self.chunk = chunk;
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

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
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

impl HeapSize for NativeFunction {
    fn heap_size(&self) -> usize {
        0
    }
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
    upvalues: Box<[Option<RawUpvalue>]>,
}

impl HeapSize for Closure {
    fn heap_size(&self) -> usize {
        self.upvalues.heap_size()
    }
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
            upvalues: vec![None; usize::from(num_upvalues)].into_boxed_slice(),
        }
    }

    pub fn function(&self) -> RawFunction {
        self.function
    }

    pub fn upvalues(&self) -> &[Option<RawUpvalue>] {
        &self.upvalues
    }

    pub fn upvalues_mut(&mut self) -> &mut [Option<RawUpvalue>] {
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

impl HeapSize for Upvalue {
    fn heap_size(&self) -> usize {
        0
    }
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
    methods: Table,
}

impl HeapSize for Class {
    fn heap_size(&self) -> usize {
        0
    }
}

impl Class {
    pub(in crate::vm) fn new(name: InternedStr) -> Self {
        Self {
            header: Header::new(ObjectKind::Class),
            name,
            methods: Table::new(),
        }
    }

    pub fn name(&self) -> InternedStr {
        self.name
    }

    pub fn methods(&self) -> &Table {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Table {
        &mut self.methods
    }
}

#[repr(C)]
pub struct Instance {
    header: Header,
    class: RawClass,
    fields: Table,
}

impl HeapSize for Instance {
    fn heap_size(&self) -> usize {
        self.fields.heap_size()
    }
}

impl Instance {
    pub(in crate::vm) fn new(class: RawClass) -> Self {
        Self {
            header: Header::new(ObjectKind::Instance),
            class,
            fields: Table::new(),
        }
    }

    pub fn class(&self) -> RawClass {
        self.class
    }

    pub fn fields(&self) -> &Table {
        &self.fields
    }

    pub fn fields_mut(&mut self) -> &mut Table {
        &mut self.fields
    }
}

#[repr(C)]
pub struct BoundMethod {
    header: Header,
    receiver: Value,
    method: RawClosure,
}

impl HeapSize for BoundMethod {
    fn heap_size(&self) -> usize {
        0
    }
}

impl BoundMethod {
    pub(in crate::vm) fn new(receiver: Value, method: RawClosure) -> Self {
        Self {
            header: Header::new(ObjectKind::BoundMethod),
            receiver,
            method,
        }
    }

    pub fn receiver(&self) -> Value {
        self.receiver
    }

    pub fn method(&self) -> RawClosure {
        self.method
    }
}
