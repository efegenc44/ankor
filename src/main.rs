mod engine;
mod expr;
mod lexer;
mod parser;
mod token;
mod value;
mod prelude;
mod span;
mod reporter;
mod error;

use std::{io::{self, Write}, env::args, fs};

use engine::{Engine, Exception};
use lexer::Lexer;
use parser::Parser;
use prelude::get_prelude;
use reporter::Reporter;

#[macro_export]
macro_rules! handle_error {
    ($expr:expr, $stage:expr, $reporter:expr, $source_name:expr) => {
        match $expr {
            Ok(v)  => v,
            Err(err) => {
                $reporter.report($source_name, err, $stage);
                std::process::exit(1);
            }
        } 
    };
}

macro_rules! handle_error_repl {
    ($expr:expr, $stage:expr, $input:expr, $reporter:expr) => {
        match $expr {
            Ok(v)  => v,
            Err(err) => {
                // TODO
                $reporter.lineses.insert("REPL".to_string(), $input.to_string());
                $reporter.report("REPL", err, $stage);
                continue
            }
        } 
    };
}

fn main() -> io::Result<()> {
    match &args().collect::<Vec<_>>()[1..] {
        [] => repl(),
        [file, cli_args @ ..] => {
            from_file(file, cli_args);
            Ok(())
        },
    }
}

fn from_file(file_path: &str, cli_args: &[String]) {
    let file = fs::read_to_string(file_path)
        .map_err(|err| Reporter::report_file_error(err, file_path)).unwrap();

    let mut reporter = Reporter::new();
    let tokens = handle_error!(Lexer::new(&file).collect(), "tokenizing", reporter, file_path);
    let astree = handle_error!(Parser::new(tokens).parse_module(), "parsing", reporter, file_path);
    let result = Engine::run_from_entry(file_path, &astree, cli_args);
    match result {
        Ok(result) => println!("= {result}"),
        Err(Exception::Exception(exc)) => reporter.report(&exc.1, exc.0, "runtime"),
        _ => unreachable!()
    }
}

fn repl() -> io::Result<()> {
    let mut engine = Engine::new();
    let mut reporter = Reporter::new();
    let prelude = get_prelude("REPL");

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
        
        let tokens = handle_error_repl!(Lexer::new(input).collect(), "tokenizing", input, reporter);
        let astree = handle_error_repl!(Parser::new(tokens).parse(), "parsing", input, reporter);
        let result = engine.evaluate(&astree, &prelude);
        
        match result {
            Ok(result) => println!("= {result}"),
            Err(Exception::Exception(exc)) => {
                reporter.lineses.insert("REPL".to_string(), input.to_string());
                reporter.report(&exc.1, exc.0, "runtime")
            },
            _ => unreachable!()
        }
    }

    Ok(())
}
