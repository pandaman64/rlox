use core::fmt;

use crate::object::{self, RawObject};

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
                unsafe { ValueDisplay::Object(&object::try_as_str(obj).unwrap().content) }
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
                match unsafe { (object::try_as_str(o1), object::try_as_str(o2)) } {
                    (Some(o1), Some(o2)) => o1.content == o2.content,
                    _ => false,
                }
            }
            _ => false,
        }
    }
}
