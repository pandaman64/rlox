use std::{
    fmt,
    hash::{Hash, Hasher},
};

use crate::vm::object::{RawObject, RawStr};

#[derive(Clone, Copy)]
pub struct InternedStr(RawStr);

impl PartialEq for InternedStr {
    fn eq(&self, other: &Self) -> bool {
        // SAFETY: the construction of InternedStr guarantees the validity of pointee
        unsafe {
            let s1 = &self.0.as_ref().as_rust_str();
            let s2 = &other.0.as_ref().as_rust_str();

            s1.eq(s2)
        }
    }
}
impl Eq for InternedStr {}

impl Hash for InternedStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // SAFETY: the construction of InternedStr guarantees the validity of pointee
        unsafe {
            let s = &self.0.as_ref().as_rust_str();
            s.hash(state);
        }
    }
}

impl InternedStr {
    /// # Safety
    /// s must point to a valid string
    pub unsafe fn new(s: RawStr) -> Self {
        Self(s)
    }

    pub fn into_inner(self) -> RawStr {
        self.0
    }

    pub fn into_raw_obj(self) -> RawObject {
        self.0.cast()
    }

    pub fn display(&self) -> impl fmt::Display + '_ {
        // SAFETY: the construction of InternedStr guarantees the validity
        unsafe { self.0.as_ref().as_rust_str() }
    }
}

// interned keys that can be compared with pointer values
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Key(RawStr);

impl Key {
    pub fn new(s: InternedStr) -> Self {
        Self(s.0)
    }

    pub fn into_raw_obj(self) -> RawObject {
        self.0.cast()
    }

    pub fn display(&self) -> impl fmt::Display + '_ {
        // SAFETY: the construction of InternedStr guarantees the validity
        unsafe { self.0.as_ref().as_rust_str() }
    }
}
