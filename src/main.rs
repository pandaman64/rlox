#![deny(rust_2018_idioms, unsafe_op_in_unsafe_fn)]
// #![warn(unreachable_pub)]

use std::io::{BufRead, Write};

use rlox::{
    ast::Root,
    codegen::Compiler,
    object::NativeFunction,
    trace_available,
    vm::{InterpretResult, Vm},
};

fn repl<R: BufRead>(mut input: R) -> std::io::Result<()> {
    // std::env::set_var("RUST_LOG", "trace");

    let mut vm = Vm::new();
    vm.define_native_function(
        "clock".into(),
        NativeFunction::new(|_args| {
            use rlox::value::Value;
            use std::time::*;

            let now = SystemTime::now();
            let duration = now.duration_since(UNIX_EPOCH).unwrap();

            Value::Number(duration.as_secs_f64())
        }),
    );

    let mut line = String::new();
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    loop {
        line.clear();
        write!(stdout, "> ")?;
        stdout.flush()?;
        if input.read_line(&mut line)? == 0 {
            break;
        }

        let node = rlox::syntax::parse(&line);
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
            let (function, upvalues) = compiler.finish();
            assert!(upvalues.is_empty());
            function.trace();

            vm.reset(function);
            vm.call(0);
            if vm.run() != InterpretResult::Ok {
                vm.print_stack_trace();
            }
        }
    }

    // SAFETY: we don't reuse vm and chunk, so no code can refer to deallocated objects.
    unsafe {
        vm.objects_mut().free_all_objects();
    }

    Ok(())
}

fn main() -> std::io::Result<()> {
    let stdin = std::io::stdin();
    let stdin = stdin.lock();
    repl(stdin)
}

#[test]
fn test_closure() {
    let input: &[u8] = br#""
fun greet(name) { fun hi() { print name; } return hi; }
var john = greet("John");
var bob = greet("Bob");
john();
bob();
""#;
    repl(input).unwrap();
}
