#![deny(rust_2018_idioms, unsafe_op_in_unsafe_fn)]
// #![warn(unreachable_pub)]

pub mod ast;
pub mod codegen;
pub mod line_map;
pub mod object;
pub mod opcode;
pub mod syntax;
pub mod table;
pub mod value;
pub mod vm;

use std::{
    io::{self, Write},
    sync::{
        atomic::{self, AtomicBool},
        Once,
    },
};

use ast::Root;
use codegen::Compiler;
use line_map::LineMap;
use object::NativeFunction;
use regex::Regex;
use syntax::SyntaxError;
use vm::Vm;

use crate::vm::InterpretResult;

#[derive(Debug)]
pub enum Error {
    Input(io::Error),
    Output(io::Error),
    Syntax,
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

pub fn print_syntax_error(error: &SyntaxError, input: &str, line_map: &LineMap) {
    use SyntaxError::*;

    match error {
        UnterminatedStringLiteral { position } => {
            let line = line_map.resolve(*position);
            eprintln!("[line {}] Error: Unterminated string.", line);
        }
        ExpectNode { name, position } => {
            let line = line_map.resolve(*position);
            eprint!("[line {}] Error at ", line);
            match input[*position..].chars().next() {
                Some(c) => eprint!("'{}'", c),
                None => eprint!("end"),
            }
            eprintln!(": Expect {}.", name);
        }
        _ => {
            // TODO: adjust error message
            // eprintln!("{:?}", error);
        }
    }
}

pub fn run<W: Write>(input: &str, mut stdout: W) -> Result<(), Error> {
    let mut vm = Vm::new(&mut stdout);
    vm.define_native_function(
        "clock".into(),
        NativeFunction::new(|_args| {
            use std::time::*;
            use value::Value;

            let now = SystemTime::now();
            let duration = now.duration_since(UNIX_EPOCH).unwrap();

            Value::Number(duration.as_secs_f64())
        }),
    );

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
            print_syntax_error(&error, input, &line_map);
        }
        return Err(Error::Syntax);
    }

    let mut compiler = Compiler::new_script(&line_map);
    let root = Root::cast(node).unwrap();
    for decl in root.decls() {
        compiler.gen_decl(&mut vm, decl);
    }

    // SAFETY: we construct a chunk with valid constants
    let result = unsafe {
        let (function, upvalues) = match compiler.finish() {
            Some(v) => v,
            None => return Ok(()),
        };
        assert!(upvalues.is_empty());
        function.trace();

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
        vm.objects_mut().free_all_objects();
    }

    result
}
