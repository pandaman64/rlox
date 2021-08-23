use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    hash::{Hash, Hasher},
};

use crate::{
    value::Value,
    vm::object::{RawObject, RawStr},
};

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

#[repr(transparent)]
pub struct Table {
    map: HashMap<Key, Value>,
}

impl Default for Table {
    fn default() -> Self {
        Self {
            map: HashMap::with_capacity(0),
        }
    }
}

impl Table {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn capacity(&self) -> usize {
        self.map.capacity()
    }

    pub fn get(&self, key: Key) -> Option<Value> {
        self.map.get(&key).cloned()
    }

    pub fn insert(&mut self, allocated: &mut usize, key: Key, value: Value) {
        let old_cap = self.map.capacity();
        self.map.insert(key, value);
        *allocated += self.map.capacity() - old_cap;
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Key, &Value)> {
        self.map.iter()
    }

    // clear does not change capacity
    pub fn clear(&mut self) {
        self.map.clear();
    }

    pub fn entry(&mut self, key: Key) -> Entry<'_, Key, Value> {
        self.map.entry(key)
    }
}
