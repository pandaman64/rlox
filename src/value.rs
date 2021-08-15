use core::fmt;
use std::ptr;

use crate::object::{self, ObjectRef, RawObject};

// we intentionally omit Copy so that cloning values is more explicit
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Object(RawObject),
}

pub enum ValueDisplay<'v> {
    Nil,
    Bool(bool),
    Number(f64),
    Str(&'v str),
    Script,
    Function(Value),
    NativeFunction,
    Upvalue,
}

impl fmt::Display for ValueDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ValueDisplay::*;

        match self {
            Nil => write!(f, "nil"),
            Bool(b) => write!(f, "{}", b),
            Number(num) => write!(f, "{}", num),
            Str(s) => write!(f, "{}", s),
            Script => write!(f, "<top-level script>"),
            // SAFETY: construction of ValueDisplay guarantees the validity of the object
            Function(name) => write!(f, "<fn {}>", unsafe { name.format_args() }),
            NativeFunction => write!(f, "<native fn>"),
            Upvalue => write!(f, "<upvalue>"),
        }
    }
}

/// # Safety
/// `obj` must be a (recursively) valid object.
pub unsafe fn format_obj<'obj>(obj: RawObject) -> ValueDisplay<'obj> {
    // SAFETY: this object is valid, so the type must match the runtime representation
    match unsafe { object::as_ref(obj) } {
        ObjectRef::Str(str) => ValueDisplay::Str(str.as_rust_str()),
        ObjectRef::Function(fun) => match fun.name() {
            None => ValueDisplay::Script,
            // SAFETY: the object must be recursively valid.
            Some(name) => ValueDisplay::Function(Value::Object(name.into_raw_obj())),
        },
        ObjectRef::NativeFunction(_) => ValueDisplay::NativeFunction,
        // SAFETY: the object must be recursively valid.
        ObjectRef::Closure(cls) => unsafe { format_obj(cls.function().cast()) },
        ObjectRef::Upvalue(_) => ValueDisplay::Upvalue,
    }
}

impl Value {
    /// # Safety
    /// `self` must be a valid object (initialized and not freed).
    pub unsafe fn format_args(&self) -> ValueDisplay<'_> {
        use Value::*;

        match self {
            Nil => ValueDisplay::Nil,
            Bool(b) => ValueDisplay::Bool(*b),
            Number(num) => ValueDisplay::Number(*num),
            Object(obj) => unsafe {
                // SAFETY: this object is valid
                format_obj(*obj)
            },
        }
    }

    /// # Safety
    /// `self` and `other` must be valid objects (initialized and not freed).
    pub unsafe fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (self, other) {
            (Nil, Nil) => true,
            (Bool(b1), Bool(b2)) => b1 == b2,
            (Number(n1), Number(n2)) => n1 == n2,
            (Object(o1), Object(o2)) => {
                // SAFETY: these objects are valid
                match unsafe { (object::as_ref(*o1), object::as_ref(*o2)) } {
                    // strings are interned
                    (ObjectRef::Str(_), ObjectRef::Str(_)) => ptr::eq(o1.as_ptr(), o2.as_ptr()),
                    // functions are equal iff they point to the same function object
                    (ObjectRef::Function(_), ObjectRef::Function(_)) => {
                        ptr::eq(o1.as_ptr(), o2.as_ptr())
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }

    pub fn is_falsy(&self) -> bool {
        use Value::*;

        matches!(self, Nil | Bool(false))
    }
}
