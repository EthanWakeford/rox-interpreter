use std::env;

use rox::{run_file, run_prompt};

fn main() {
    let args: Vec<String> = env::args().collect();
    println!();

    if args.len() > 2 {
        println!("Usage: rox [script]")
    } else if args.len() == 2 {
        let filename = args.get(1).expect("should be filename to unwrap");

        if let Err(e) = run_file(filename) {
            eprintln!("Error: {}", e);
        }
    } else {
        if let Err(e) = run_prompt() {
            eprintln!("Error: {}", e);
        };
    }
}
