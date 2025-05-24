use lc1::{codegen::Codegen, interpreter::Interpreter, parser, tokenizer::Lexer};
use std::{env, fs, process};

fn usage_and_exit(prog_name: &str, msg: &str) -> ! {
    eprintln!("{}", msg);
    eprintln!("Usage: {} [--interpreter | --asm] <input_file>", prog_name);
    process::exit(1);
}

fn main() {
    let mut args = env::args();
    let prog_name = args.next().unwrap_or("lc1".to_string());

    let mode = args
        .next()
        .unwrap_or_else(|| usage_and_exit(&prog_name, "Missing mode."));
    if mode != "--interpreter" && mode != "--asm" {
        usage_and_exit(&prog_name, &format!("Unknown mode `{}`.", mode));
    }

    let filename = args
        .next()
        .unwrap_or_else(|| usage_and_exit(&prog_name, "Missing input file."));

    let src = fs::read_to_string(&filename).unwrap_or_else(|e| {
        eprintln!("Error reading `{}`: {}", filename, e);
        process::exit(1);
    });

    let tokens = Lexer::new(&src).lex();
    let ast = parser::Parser::new(tokens).parse_program();

    match mode.as_str() {
        "--interpreter" => Interpreter::default().run(&ast),
        "--asm" => println!("{}", Codegen::default().run(&ast)),
        _ => unreachable!(),
    }
}
