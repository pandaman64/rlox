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

enum ValueDisplay<'v> {
    Nil,
    Bool(bool),
    Number(f64),
    Object(&'v (dyn fmt::Display + 'v)),
}

impl fmt::Display for ValueDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ValueDisplay::*;

        match self {
            Nil => write!(f, "nil"),
            Bool(b) => write!(f, "{}", b),
            Number(num) => write!(f, "{}", num),
            Object(obj) => write!(f, "{}", obj),
        }
    }
}

impl Value {
    /// SAFETY: `self` must be a valid object (initialized and not freed).
    pub unsafe fn format_args(&self) -> impl fmt::Display + '_ {
        use Value::*;

        match self {
            Nil => ValueDisplay::Nil,
            Bool(b) => ValueDisplay::Bool(*b),
            Number(num) => ValueDisplay::Number(*num),
            Object(obj) => {
                // SAFETY: this object is valid, so the type must match the runtime representation
                match unsafe { object::as_ref(*obj) } {
                    ObjectRef::Str(str) => ValueDisplay::Object(str.as_rust_str()),
                }
            }
        }
    }

    /// SAFETY: `self` and `other` must be valid objects (initialized and not freed).
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
