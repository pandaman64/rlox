#![deny(rust_2018_idioms, unsafe_op_in_unsafe_fn)]
// #![warn(unreachable_pub)]

mod ast;
mod codegen;
mod object;
mod opcode;
mod syntax;
mod table;
mod value;
mod vm;

use std::{
    io::BufRead,
    sync::{
        atomic::{self, AtomicBool},
        Once,
    },
};
use vm::Vm;

use crate::{ast::Root, codegen::Compiler};

pub fn trace_available() -> bool {
    static AVAILABLE: AtomicBool = AtomicBool::new(false);
    static ONCE: Once = Once::new();

    ONCE.call_once(|| {
        let config = matches!(std::env::var("RUST_LOG"), Ok(s) if s == "trace");
        AVAILABLE.store(config, atomic::Ordering::Relaxed);
    });
    AVAILABLE.load(atomic::Ordering::Relaxed)
}

fn main() -> std::io::Result<()> {
    use std::io::Write;

    // std::env::set_var("RUST_LOG", "trace");

    let mut line = String::new();
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    let mut vm = Vm::new();

    loop {
        line.clear();
        write!(stdout, "> ")?;
        stdout.flush()?;
        if stdin.read_line(&mut line)? == 0 {
            break;
        }

        let node = syntax::parse(&line);
        if trace_available() {
            eprintln!("{:#?}", node);
        }

        let mut compiler = Compiler::new_script();
        match Root::cast(node) {
            None => {
                eprintln!("syntax error");
                continue;
            }
            Some(root) => {
                for decl in root.decls() {
                    compiler.gen_decl(&mut vm, decl);
                }
            }
        };
        // SAFETY: we construct a chunk with valid constants
        unsafe {
            let function = compiler.finish();
            function.trace();

            vm.reset(function);
            vm.call(0);
            vm.run();
        }
    }

    // SAFETY: we don't reuse vm and chunk, so no code can refer to deallocated objects.
    unsafe {
        vm.free_all_objects();
    }

    Ok(())
}
