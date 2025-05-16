use lc1::{codegen::Codegen, interpreter::Interpreter, parser, tokenizer::Lexer};
use std::{env, fs, process};

fn main() {
    let mut args = env::args();
    let prog_name = args.next().unwrap_or_else(|| "lc1".into());

    let mode = match args.next() {
        Some(m) if m == "--interpreter" || m == "--asm" => m,
        Some(other) => {
            eprintln!("Unknown mode `{}`. Use --interpreter or --asm.", other);
            eprintln!("Usage: {} [--interpreter | --asm] <input_file>", prog_name);
            process::exit(1);
        }
        None => {
            eprintln!("Missing mode. Use --interpreter or --asm.");
            eprintln!("Usage: {} [--interpreter | --asm] <input_file>", prog_name);
            process::exit(1);
        }
    };

    let filename = match args.next() {
        Some(f) => f,
        None => {
            eprintln!("Missing input file.");
            eprintln!("Usage: {} [--interpreter | --asm] <input_file>", prog_name);
            process::exit(1);
        }
    };

    let src = match fs::read_to_string(&filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading `{}`: {}", filename, e);
            process::exit(1);
        }
    };

    let mut lexer = Lexer::new(&src);
    let tokens = lexer.lex();
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse_program();

    match mode.as_str() {
        "--interpreter" => {
            let mut interpreter = Interpreter::default();
            interpreter.run(&ast);
        }
        "--asm" => {
            let asm = Codegen::default().run(&ast);
            println!("{}", asm);
        }
        _ => unreachable!(), // we've already validated
    }
}
