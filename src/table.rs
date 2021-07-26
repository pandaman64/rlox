use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

use crate::object::{RawObject, RawStr};

#[derive(Clone)]
pub struct InternedStr(RawStr);

impl PartialEq for InternedStr {
    fn eq(&self, other: &Self) -> bool {
        // SAFETY: the construction of InternedStr guarantees the validity of pointee
        unsafe {
            let s1 = &self.0.as_ref().content;
            let s2 = &other.0.as_ref().content;

            s1.eq(s2)
        }
    }
}
impl Eq for InternedStr {}

impl Hash for InternedStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // SAFETY: the construction of InternedStr guarantees the validity of pointee
        unsafe {
            let s = &self.0.as_ref().content;
            s.hash(state);
        }
    }
}

impl InternedStr {
    /// SAFETY: s must point to a valid string
    pub unsafe fn new(s: RawStr) -> Self {
        Self(s)
    }

    pub fn into_inner(self) -> RawStr {
        self.0
    }

    pub fn into_raw_obj(self) -> RawObject {
        self.0.cast()
    }
}

// interned keys that can be compared with pointer values
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Key(RawStr);

impl Key {
    pub fn new(s: InternedStr) -> Self {
        Self(s.0)
    }
}
