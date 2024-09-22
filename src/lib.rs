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
    line: u32,
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

fn is_at_end(current: &u32, input: &String) {
    return;
}

fn scan_tokens(input: &String, start: u32) {
    let had_error = false;
    let mut tokens: Vec<Token> = Vec::new();
    let mut line: u32 = 1;
    let mut current = start;
    let length: u32 = input
        .len()
        .try_into()
        .expect("Length of input shouldbe able to be converted into u32");

    while current < length {
        scan_one();
        break;
    }

    tokens.push(Token {
        tokentype: TokenType::EOF,
        lexeme: "".to_string(),
        literal: "".to_string(),
        line: line,
    });
}

fn scan_one() {}

pub fn run_file(filename: &String) -> Result<(), io::Error> {
    println!("im a runnin {}", filename);
    let contents = fs::read_to_string(filename)?;

    scan_tokens(&contents, 0);
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
    }
}
