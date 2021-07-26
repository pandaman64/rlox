use std::ptr::{self, NonNull};

// the pointer must have valid provenance not only for the header but the whole object
pub type RawObject = NonNull<Header>;
pub type RawStr = NonNull<Str>;

#[repr(u8)]
pub enum ObjectKind {
    String,
}

#[repr(C)]
pub struct Header {
    pub kind: ObjectKind,
    // prev: RawObject,
    pub next: Option<RawObject>,
}

// TODO: inline string contents to shave off one allocation
#[repr(C)]
pub struct Str {
    pub header: Header,
    pub content: String,
}

impl Str {
    pub fn new(content: String) -> Self {
        Self {
            header: Header {
                kind: ObjectKind::String,
                next: None,
            },
            content,
        }
    }
}

/// SAFETY: the object must be initialized and its kind must match the runtime representation.
pub unsafe fn try_as_str(obj: &RawObject) -> Option<&Str> {
    unsafe {
        let obj = obj.as_ptr();
        let kind_ptr = ptr::addr_of_mut!((*obj).kind);
        match ptr::read(kind_ptr) {
            ObjectKind::String => {
                let str_ptr = obj as *mut Str;
                Some(str_ptr.as_ref().unwrap())
            }
        }
    }
}
