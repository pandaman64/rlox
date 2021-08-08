use std::ptr::{self, NonNull};

use crate::{opcode::Chunk, value};

// the pointer must have valid provenance not only for the header but the whole object
pub type RawObject = NonNull<Header>;
pub type RawStr = NonNull<Str>;

#[repr(u8)]
pub enum ObjectKind {
    Str,
    Function,
}

#[repr(C)]
pub struct Header {
    pub kind: ObjectKind,
    // prev: RawObject,
    pub next: Option<RawObject>,
}

pub enum ObjectRef<'a> {
    Str(&'a Str),
    Function(&'a Function),
}

/// SAFETY: the object must be initialized and its kind must match the runtime representation.
pub unsafe fn as_ref<'a>(obj: RawObject) -> ObjectRef<'a> {
    unsafe {
        let obj = obj.as_ptr();
        let kind_ptr = ptr::addr_of_mut!((*obj).kind);
        match ptr::read(kind_ptr) {
            ObjectKind::Str => {
                let str_ptr = obj as *mut Str;
                ObjectRef::Str(str_ptr.as_ref().unwrap())
            }
            ObjectKind::Function => {
                let fun_ptr = obj as *mut Function;
                ObjectRef::Function(fun_ptr.as_ref().unwrap())
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
    pub fn new(content: String) -> Self {
        Self {
            header: Header {
                kind: ObjectKind::Str,
                next: None,
            },
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
    arity: u32,
    chunk: Chunk,
    name: Option<RawObject>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            header: Header {
                kind: ObjectKind::Function,
                next: None,
            },
            arity: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }

    pub fn name(&self) -> Option<RawObject> {
        self.name
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    /// SAFETY: self must be a valid function
    pub unsafe fn trace(&self) {
        let chunk = &self.chunk;
        // SAFETY: self is a valid function, so the chunk and name is valid too
        unsafe {
            match self.name {
                None => chunk.trace_chunk("<top-level script>"),
                Some(name) => chunk.trace_chunk(value::format_obj(name)),
            }
        }
    }
}
