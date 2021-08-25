use core::fmt;

use crate::vm::object::{
    self, ObjectKind, ObjectRef, RawClass, RawClosure, RawFunction, RawInstance, RawObject, RawStr,
};

#[derive(Debug, Clone, Copy)]
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
    Instance(Value),
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
            Instance(class) => write!(f, "{} instance", unsafe { class.format_args() }),
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
        // SAFETY: the object must be recursively valid.
        ObjectRef::Class(class) => unsafe { format_obj(class.name().into_raw_obj()) },
        ObjectRef::Instance(instance) => {
            ValueDisplay::Instance(Value::Object(instance.class().cast()))
        }
        // SAFETY: the object must be recursively valid.
        ObjectRef::BoundMethod(method) => unsafe { format_obj(method.method().cast()) },
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
            (Object(o1), Object(o2)) => o1 == o2,
            _ => false,
        }
    }

    pub fn is_falsy(&self) -> bool {
        use Value::*;

        matches!(self, Nil | Bool(false))
    }

    /// # Safety
    /// this object must be valid
    pub unsafe fn mark(&self, worklist: &mut Vec<RawObject>) {
        if let Value::Object(obj) = self {
            // SAFETY: the object is valid
            unsafe {
                object::mark(*obj, worklist);
            }
        }
    }

    /// # Safety
    /// this object must be valid
    pub unsafe fn as_str(&self) -> Option<RawStr> {
        unsafe {
            match self {
                Self::Object(obj) if obj.as_ref().kind == ObjectKind::Str => Some(obj.cast()),
                _ => None,
            }
        }
    }

    /// # Safety
    /// this object must be valid
    pub unsafe fn as_function(&self) -> Option<RawFunction> {
        unsafe {
            match self {
                Self::Object(obj) if obj.as_ref().kind == ObjectKind::Function => Some(obj.cast()),
                _ => None,
            }
        }
    }

    /// # Safety
    /// this object must be valid
    pub unsafe fn as_closure(&self) -> Option<RawClosure> {
        unsafe {
            match self {
                Self::Object(obj) if obj.as_ref().kind == ObjectKind::Closure => Some(obj.cast()),
                _ => None,
            }
        }
    }

    /// # Safety
    /// this object must be valid
    pub unsafe fn as_class(&self) -> Option<RawClass> {
        unsafe {
            match self {
                Self::Object(obj) if obj.as_ref().kind == ObjectKind::Class => Some(obj.cast()),
                _ => None,
            }
        }
    }

    /// # Safety
    /// this object must be valid
    pub unsafe fn as_instance(&self) -> Option<RawInstance> {
        unsafe {
            match self {
                Self::Object(obj) if obj.as_ref().kind == ObjectKind::Instance => Some(obj.cast()),
                _ => None,
            }
        }
    }
}
