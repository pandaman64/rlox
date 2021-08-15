#![deny(rust_2018_idioms, unsafe_op_in_unsafe_fn)]
// #![warn(unreachable_pub)]

pub mod ast;
pub mod codegen;
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
use object::NativeFunction;
use vm::Vm;

use crate::vm::InterpretResult;

pub fn trace_available() -> bool {
    static AVAILABLE: AtomicBool = AtomicBool::new(false);
    static ONCE: Once = Once::new();

    ONCE.call_once(|| {
        let config = matches!(std::env::var("RUST_LOG"), Ok(s) if s == "trace");
        AVAILABLE.store(config, atomic::Ordering::Relaxed);
    });
    AVAILABLE.load(atomic::Ordering::Relaxed)
}

pub fn run<W: Write>(input: &str, mut stdout: W) -> io::Result<()> {
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

    let (node, errors) = syntax::parse(input);
    if trace_available() {
        eprintln!("{:#?}", node);
    }
    if !errors.is_empty() {
        for error in errors {
            eprintln!("{:?}", error);
        }
        return Err(io::Error::new(io::ErrorKind::Other, "syntax error"));
    }

    let mut compiler = Compiler::new_script();
    let root = Root::cast(node).unwrap();
    for decl in root.decls() {
        compiler.gen_decl(&mut vm, decl);
    }

    // SAFETY: we construct a chunk with valid constants
    let result = unsafe {
        let (function, upvalues) = compiler.finish();
        assert!(upvalues.is_empty());
        function.trace();

        vm.reset(function);
        vm.call(0);
        if vm.run() != InterpretResult::Ok {
            vm.print_stack_trace();
            Err(io::Error::new(io::ErrorKind::Other, "vm terminated"))
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
