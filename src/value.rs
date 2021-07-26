use core::fmt;

use crate::object;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Object(object::Object),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;

        match self {
            Nil => write!(f, "nil"),
            Bool(b) => write!(f, "{}", b),
            Number(num) => write!(f, "{}", num),
            Object(object::Object::String(s)) => write!(f, r#""{}""#, s),
        }
    }
}
