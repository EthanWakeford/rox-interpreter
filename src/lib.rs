use std::{
    fmt, fs,
    io::{self, Write},
    path::Display,
};

enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Let,
    While,

    EOF,
}

struct Token {
    tokentype: TokenType,
    lexeme: String,
    // No idea what this should actually be
    // book has it as object
    literal: String,
    line: i32,
}

// impl fmt::Display for Token {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(
//             f,
//             "({}, {}, {}, {})",
//             self.tokentype, self.lexeme, self.literal, self.line
//         )
//     }
// }

fn scan(input: &String) {
    let had_error = false;

    println!()
}

pub fn run_file(filename: &String) -> Result<(), io::Error> {
    println!("im a runnin {}", filename);
    let contents = fs::read_to_string(filename)?;

    scan(&contents);
    Ok(())
}

pub fn run_proompt() -> Result<(), io::Error> {
    println!("im a proomptin");
    loop {
        print!("  ===]> ");

        std::io::stdout().flush().unwrap();
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;

        let mut inputs = buffer.split_whitespace();

        let command = match inputs.next() {
            Some(s) => s,
            None => continue,
        };
    }
}
