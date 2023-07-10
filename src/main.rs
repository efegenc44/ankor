mod engine;
mod expr;
mod lexer;
mod parser;
mod token;
mod value;
mod prelude;

use std::{io::{self, Write}, env::args, fs};

use engine::Engine;
use lexer::Lexer;

use crate::{parser::Parser, prelude::get_prelude};

fn main() -> io::Result<()> {
    match &args().collect::<Vec<_>>()[..] {
        [] => unreachable!(),
        [_] => repl(),
        [_, file_path] => from_file(file_path),
        _ => {
            println!("usage: ./ankor [file]");
            Ok(())
        }
    }
}

fn from_file(file_path: &str) -> io::Result<()> {
    let file = fs::read_to_string(file_path)?;
    let tokens = Lexer::new(&file).collect();
    let astree = Parser::new(tokens).parse_module();
    let result = Engine::run_from_entry(&astree);
    println!("= {result}");
    Ok(())
}

fn repl() -> io::Result<()> {
    let mut engine = Engine::new();
    let prelude = get_prelude();

    let mut stdout = io::stdout();
    let stdin = io::stdin();
    loop {
        print!("> ");
        stdout.flush()?;

        let mut input = String::new();
        stdin.read_line(&mut input)?;
        let input = input.trim_end();

        match input {
            ".exit" => break,
            "" => continue,
            _ => (),
        }

        let tokens = Lexer::new(input).collect();
        let astree = Parser::new(tokens).parse();
        let result = engine.evaluate(&astree, &prelude);

        println!("= {result}");
    }

    Ok(())
}
