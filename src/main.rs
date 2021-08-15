#![deny(rust_2018_idioms, unsafe_op_in_unsafe_fn)]
// #![warn(unreachable_pub)]

use clap::Clap;
use std::{fs, io::BufRead, path::PathBuf, process};

use rlox::{
    ast::Root,
    codegen::Compiler,
    line_map::LineMap,
    object::NativeFunction,
    run, trace_available,
    vm::{InterpretResult, Vm},
};

fn repl<R: BufRead>(mut input: R) -> Result<(), rlox::Error> {
    // std::env::set_var("RUST_LOG", "trace");

    let mut line = String::new();
    let mut line_num = 1;
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    let mut vm = Vm::new(&mut stdout);
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

    loop {
        line.clear();
        vm.print("> ").map_err(rlox::Error::Output)?;
        vm.flush().map_err(rlox::Error::Output)?;
        if input.read_line(&mut line).map_err(rlox::Error::Input)? == 0 {
            break;
        }

        let line_map = {
            let mut line_map = LineMap::new();
            line_map.push(line_num, 0);
            line_num += 1;
            line_map
        };
        let (node, errors) = rlox::syntax::parse(&line);
        if trace_available() {
            eprintln!("{:#?}", node);
        }
        if !errors.is_empty() {
            for error in errors {
                rlox::print_syntax_error(&error, &line, &line_map);
            }
            continue;
        }

        let mut compiler = Compiler::new_script(&line_map);
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
            let (function, upvalues) = match compiler.finish() {
                Some(v) => v,
                None => continue,
            };
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

#[derive(Clap)]
struct Opts {
    file_name: Option<PathBuf>,
}

fn main() {
    let opts = Opts::parse();

    let result = match opts.file_name {
        Some(file_name) => {
            let input = fs::read_to_string(file_name).unwrap();
            let stdout = std::io::stdout();
            let stdout = stdout.lock();
            run(&input, stdout)
        }
        None => {
            let stdin = std::io::stdin();
            let stdin = stdin.lock();
            repl(stdin)
        }
    };

    match result {
        Ok(()) => process::exit(0),
        Err(rlox::Error::Syntax) => process::exit(65),
        Err(rlox::Error::Runtime) => process::exit(70),
        Err(_) => process::exit(999),
    }
}
