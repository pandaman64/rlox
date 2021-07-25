use core::fmt;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;

        match self {
            Nil => write!(f, "nil"),
            Bool(b) => write!(f, "{}", b),
            Number(num) => write!(f, "{}", num),
        }
    }
}
