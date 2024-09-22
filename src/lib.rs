use std::{
    fs,
    io::{self, Write},
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

struct Token<'a> {
    tokentype: TokenType,
    lexeme: &'a str,
    // No idea what this should actually be
    // book has it as object
    literal: &'a str,
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

fn print_error(line: u32, message: String) {
    eprintln!("Error: [Line {}]>> {}", line, message);
}

fn scan_tokens(input: &String, start: u32) {
    let mut had_error = false;
    let mut tokens: Vec<Token> = Vec::new();
    let mut line: u32 = 1;
    let mut current = start;
    let length: u32 = input
        .len()
        .try_into()
        .expect("Length of input shouldbe able to be converted into u32");
    let mut chars = input.chars();

    while let Some(char) = chars.next() {
        let token_type = match char {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            _ => {
                print_error(line, format!("Unexpected character: {}", char));
                had_error = true;
                continue;
            }
        };

        tokens.push(Token {
            tokentype: token_type,
            lexeme: "",
            literal: "",
            line: line,
        });
        break;
    }

    tokens.push(Token {
        tokentype: TokenType::EOF,
        lexeme: "",
        literal: "",
        line: line,
    });
}

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
