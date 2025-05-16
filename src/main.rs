use lc1::{codegen::Codegen, parser, tokenizer::Lexer};

use std::{env, fs, process};

fn main() {
    let mut args = env::args();
    let prog_name = args.next().unwrap_or_else(|| "lc1 compiler".into());
    let filename = match args.next() {
        Some(f) => f,
        None => {
            eprintln!("Usage: {} <input_file>", prog_name);
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

    let asm = Codegen::default().run(&ast);

    println!("{}", asm);
}
