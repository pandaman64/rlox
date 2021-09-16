#![deny(rust_2018_idioms, unsafe_op_in_unsafe_fn)]
// #![warn(unreachable_pub)]

pub mod ast;
pub mod codegen;
pub mod line_map;
pub mod opcode;
pub mod syntax;
pub mod table;
pub mod value;
pub mod vm;

use std::{
    collections::HashMap,
    convert::TryInto,
    io::{self, Write},
    sync::{
        atomic::{self, AtomicBool},
        Once,
    },
};

use ast::Root;
use codegen::{CodegenError, Compiler};
use line_map::LineMap;
use regex::Regex;
use syntax::{SyntaxError, SyntaxNode};
use vm::Vm;

use crate::vm::InterpretResult;

pub(crate) trait HeapSize {
    fn heap_size(&self) -> usize;
}

impl<T> HeapSize for Box<[T]> {
    fn heap_size(&self) -> usize {
        self.len() * std::mem::size_of::<T>()
    }
}

impl HeapSize for String {
    fn heap_size(&self) -> usize {
        self.capacity()
    }
}

impl<K, V> HeapSize for HashMap<K, V> {
    fn heap_size(&self) -> usize {
        let size = std::mem::size_of::<(K, V)>();
        self.capacity() * size
    }
}

#[derive(Debug)]
pub enum Error {
    Input(io::Error),
    Output(io::Error),
    Syntax,
    Codegen,
    Runtime,
}

pub fn trace_available() -> bool {
    static AVAILABLE: AtomicBool = AtomicBool::new(false);
    static ONCE: Once = Once::new();

    ONCE.call_once(|| {
        let config = matches!(std::env::var("RUST_LOG"), Ok(s) if s == "trace");
        AVAILABLE.store(config, atomic::Ordering::Relaxed);
    });
    AVAILABLE.load(atomic::Ordering::Relaxed)
}

pub fn stress_gc() -> bool {
    static AVAILABLE: AtomicBool = AtomicBool::new(false);
    static ONCE: Once = Once::new();

    ONCE.call_once(|| {
        let config = matches!(std::env::var("RLOX_STRESS_GC"), Ok(s) if s == "1");
        AVAILABLE.store(config, atomic::Ordering::Relaxed);
    });
    AVAILABLE.load(atomic::Ordering::Relaxed)
}

pub fn log_gc() -> bool {
    static AVAILABLE: AtomicBool = AtomicBool::new(false);
    static ONCE: Once = Once::new();

    ONCE.call_once(|| {
        let config = matches!(std::env::var("RLOX_LOG_GC"), Ok(s) if s == "1");
        AVAILABLE.store(config, atomic::Ordering::Relaxed);
    });
    AVAILABLE.load(atomic::Ordering::Relaxed)
}

pub fn print_syntax_error(error: &SyntaxError, root: &SyntaxNode, line_map: &LineMap) {
    use SyntaxError::*;

    match error {
        UnterminatedStringLiteral { position } => {
            let line = line_map.resolve(*position);
            eprintln!("[line {}] Error: Unterminated string.", line);
        }
        ExpectNode { name, position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(c) => eprint!("'{}'", c.text()),
                None => eprint!("end"),
            }
            eprintln!(": Expect {}.", name);
        }
        ExpectIdentifier { got, position } => {
            let line = line_map.resolve(*position);
            eprintln!(
                "[line {}] Error at '{}': Expect variable name.",
                line,
                got.to_keyword_str().unwrap()
            );
        }
        InvalidAssignment { position } => {
            let line = line_map.resolve(*position);
            eprintln!("[line {}] Error at '=': Invalid assignment target.", line);
        }
        UnrecognizedToken { position } => {
            let line = line_map.resolve(*position);
            eprintln!("[line {}] Error: Unexpected character.", line);
        }
        ExpectSuperClassMethodName { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(c) => eprint!("'{}'", c.text()),
                None => eprint!("end"),
            }
            eprintln!(": Expect superclass method name.");
        }
        ExpectDotAfterSuper { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(c) => eprint!("'{}'", c.text()),
                None => eprint!("end"),
            }
            eprintln!(": Expect '.' after 'super'.");
        }
        _error => {
            // TODO: adjust error message
            // eprintln!("{:?}", error);
        }
    }
}

pub fn print_codegen_error(error: &CodegenError, root: &SyntaxNode, line_map: &LineMap) {
    use CodegenError::*;

    match error {
        ShadowingInSameScope { ident, position } => {
            let line = line_map.resolve(*position);
            eprintln!(
                "[line {}] Error at '{}': Already a variable with this name in this scope.",
                line, ident
            );
        }
        UnassignedLocal { ident, position } => {
            let line = line_map.resolve(*position);
            eprintln!(
                "[line {}] Error at '{}': Can't read local variable in its own initializer.",
                line, ident,
            );
        }
        LoopTooLarge { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.left_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Loop body too large.");
        }
        ReturnFromTopLevel { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.left_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Can't return from top-level code.");
        }
        DeferFromGlobal { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Can't defer in global scope.");
        }
        TooManyLocalVariables { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Too many local variables in function.");
        }
        TooManyConstants { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Too many constants in one chunk.");
        }
        TooManyParameters { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Can't have more than 255 parameters.");
        }
        TooManyArguments { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Can't have more than 255 arguments.");
        }
        TooManyUpvalues { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Too many closure variables in function.");
        }
        ThisOutsideClass { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Can't use 'this' outside of a class.");
        }
        ReturnFromInit { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Can't return a value from an initializer.");
        }
        InheritFromItself { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": A class can't inherit from itself.");
        }
        SuperOutsideClass { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Can't use 'super' outside of a class.");
        }
        SuperWithoutSuperClass { position } => {
            let position = *position;
            let line = line_map.resolve(position);
            let token = root.token_at_offset(position.try_into().unwrap());
            eprint!("[line {}] Error at ", line);
            match token.right_biased() {
                Some(token) => eprint!("'{}'", token.text()),
                None => eprint!("end"),
            }
            eprintln!(": Can't use 'super' in a class with no superclass.");
        }
    }
}

pub fn run<W: Write>(input: &str, mut stdout: W) -> Result<(), Error> {
    let mut vm = Vm::new(&mut stdout);
    vm.define_native_function("clock".into(), |_args| {
        use std::time::*;
        use value::Value;

        let now = SystemTime::now();
        let duration = now.duration_since(UNIX_EPOCH).unwrap();

        Value::Number(duration.as_secs_f64())
    });

    let line_map = {
        let mut line_map = LineMap::new();
        line_map.push(1, 0);
        let re = Regex::new(r"\r\n|\n").unwrap();
        for (i, newline) in re.find_iter(input).enumerate() {
            line_map.push(i + 2, newline.end());
        }
        line_map
    };
    let (node, errors) = syntax::parse(input);
    if trace_available() {
        eprintln!("{:#?}", node);
    }
    if !errors.is_empty() {
        for error in errors {
            print_syntax_error(&error, &node, &line_map);
        }
        return Err(Error::Syntax);
    }

    let mut compiler = Compiler::new_script(&line_map);
    let root = Root::cast(node.clone()).unwrap();
    for decl in root.decls() {
        compiler.gen_decl(&mut vm, decl);
    }

    // SAFETY: we construct a chunk with valid constants
    let result = unsafe {
        let (function, upvalues, errors) = match compiler.finish(&mut vm) {
            Some(v) => v,
            None => return Ok(()),
        };
        if !errors.is_empty() {
            for error in errors {
                print_codegen_error(&error, &node, &line_map);
            }
            return Err(Error::Codegen);
        }
        assert!(upvalues.is_empty());

        vm.reset(function);
        vm.call(0);
        if vm.run() != InterpretResult::Ok {
            vm.print_stack_trace();
            Err(Error::Runtime)
        } else {
            Ok(())
        }
    };

    // SAFETY: we don't reuse vm and chunk, so no code can refer to deallocated objects.
    unsafe {
        vm.free_all_objects();
    }

    result
}
