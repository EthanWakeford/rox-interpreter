use std::{
    error::Error,
    fs,
    io::{self, Write},
};

mod grammar;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
use grammar::{Evaluate, AST};
use parser::Parser;
use scanner::Scanner;

pub fn print_error(line: u32, message: String) {
    eprintln!("Error: [Line {}]>> {}", line, message);
}

pub fn run_file(filename: &String) -> Result<(), Box<dyn Error>> {
    println!("im a runnin {}", filename);
    let input = fs::read_to_string(filename)?;

    let mut scanner = Scanner::new(&input);

    let tokens = scanner.scan_tokens()?;

    let program = AST::new(tokens)?;

    program.run()?;

    Ok(())
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    println!("im a promptin");
    loop {
        print!("  ===]> ");

        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        let mut scanner = Scanner::new(&input);

        let tokens = scanner.scan_tokens()?;

        let program = AST::new(tokens)?;

        program.run()?;
    }
}
