use std::{
    fs,
    io::{self, Write},
    iter::Peekable,
    str::Chars,
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

struct Scanner<'a> {
    tokens: Vec<Token<'a>>,
    chars: Peekable<Chars<'a>>,
    start: u32,
    current: u32,
    line: u32,
    length: u32,
}

impl Scanner<'_> {
    fn scan_tokens(mut self) {
        let mut had_error = false;

        while let Some(char) = self.chars.next() {
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
                    print_error(self.line, format!("Unexpected character: {}", char));
                    had_error = true;
                    continue;
                }
            };

            self.tokens.push(Token {
                tokentype: token_type,
                lexeme: "",
                literal: "",
                line: self.line,
            });
            break;
        }

        self.tokens.push(Token {
            tokentype: TokenType::EOF,
            lexeme: "",
            literal: "",
            line: self.line,
        });
    }

    fn scan_token(self) {}
}

fn print_error(line: u32, message: String) {
    eprintln!("Error: [Line {}]>> {}", line, message);
}

fn scan_tokens(input: &String, start: u32) {
    let mut had_error = false;
    // let mut tokens: Vec<Token> = Vec::new();
    // let mut line: u32 = 1;
    // let mut current = start;
    // let length: u32 = input
    // .len()
    // .try_into()
    // .expect("Length of input shouldbe able to be converted into u32");
    // let mut chars = input.chars().peekable();
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
