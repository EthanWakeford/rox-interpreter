use std::{
    error::Error,
    fs,
    io::{self, Write},
};

mod scanner;
use scanner::Scanner;

pub fn print_error(line: u32, message: String) {
    eprintln!("Error: [Line {}]>> {}", line, message);
}

pub fn run_file(filename: &String) -> Result<(), Box<dyn Error>> {
    println!("im a runnin {}", filename);
    let input = fs::read_to_string(filename)?;

    let mut scanner = Scanner::new(&input);

    let output = scanner.scan_tokens()?;

    println!("{:?}", output);
    Ok(())
}

pub fn run_proompt() -> Result<(), Box<dyn Error>> {
    println!("im a proomptin");
    loop {
        print!("  ===]> ");

        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        let mut scanner = Scanner::new(&input);

        let output = scanner.scan_tokens()?;

        println!("{:?}", output);
    }
}
