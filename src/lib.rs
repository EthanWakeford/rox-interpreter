use std::{
    error::Error,
    fmt, fs,
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

enum Literal {
    Number(f32),
    String(String),
    Symbol,
}

struct Token {
    tokentype: TokenType,
    // lexeme: String,
    // No idea what this should actually be
    // book has it as object
    literal: Literal,
    line: u32,
}

#[derive(Debug)]
struct ScanError {
    message: String,
}

impl ScanError {
    fn new(message: &str) -> ScanError {
        ScanError {
            message: message.to_string(),
        }
    }
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for ScanError {}

struct Scanner<'a> {
    tokens: Vec<Token>,
    chars: Peekable<Chars<'a>>,
    start: u32,
    current: u32,
    line: u32,
    length: u32,
}

impl Scanner<'_> {
    fn scan_tokens(&mut self) -> Result<&Vec<Token>, ScanError> {
        let mut had_error = false;

        while let Some(char) = self.chars.next() {
            let (token_type, literal) = match self.scan_token(char)? {
                Some((token_type, literal)) => (token_type, literal),
                None => continue,
            };

            self.tokens.push(Token {
                tokentype: token_type,
                // lexeme: literal,
                literal: literal,
                line: self.line,
            });
        }

        // End with EOF
        self.tokens.push(Token {
            tokentype: TokenType::EOF,
            // lexeme: "".to_string(),
            literal: Literal::Symbol,
            line: self.line,
        });

        Ok(&self.tokens)
    }

    fn scan_token(&mut self, char: char) -> Result<Option<(TokenType, Literal)>, ScanError> {
        match char {
            '(' => Ok(Some((TokenType::LeftParen, Literal::Symbol))),
            ')' => Ok(Some((TokenType::RightParen, Literal::Symbol))),
            '{' => Ok(Some((TokenType::LeftBrace, Literal::Symbol))),
            '}' => Ok(Some((TokenType::RightBrace, Literal::Symbol))),
            ',' => Ok(Some((TokenType::Comma, Literal::Symbol))),
            '.' => Ok(Some((TokenType::Dot, Literal::Symbol))),
            '-' => Ok(Some((TokenType::Minus, Literal::Symbol))),
            '+' => Ok(Some((TokenType::Plus, Literal::Symbol))),
            ';' => Ok(Some((TokenType::Semicolon, Literal::Symbol))),
            '*' => Ok(Some((TokenType::Star, Literal::Symbol))),
            '!' => match self.chars.peek().unwrap_or(&' ') {
                '=' => {
                    self.chars.next();
                    Ok(Some((TokenType::BangEqual, Literal::Symbol)))
                }
                _ => Ok(Some((TokenType::Bang, Literal::Symbol))),
            },
            '=' => match self.chars.peek().unwrap_or(&' ') {
                '=' => {
                    self.chars.next();
                    Ok(Some((TokenType::EqualEqual, Literal::Symbol)))
                }
                _ => Ok(Some((TokenType::Equal, Literal::Symbol))),
            },
            '<' => match self.chars.peek().unwrap_or(&' ') {
                '=' => {
                    self.chars.next();
                    Ok(Some((TokenType::LessEqual, Literal::Symbol)))
                }
                _ => Ok(Some((TokenType::Less, Literal::Symbol))),
            },
            '>' => match self.chars.peek().unwrap_or(&' ') {
                '=' => {
                    self.chars.next();
                    Ok(Some((TokenType::GreaterEqual, Literal::Symbol)))
                }
                _ => Ok(Some((TokenType::Greater, Literal::Symbol))),
            },
            '/' => match self.chars.peek().unwrap_or(&' ') {
                // comment found
                '/' => {
                    while self.chars.peek().unwrap_or(&' ') != &'\n' {
                        self.chars.next();
                    }
                    Ok(None)
                }
                _ => Ok(Some((TokenType::Slash, Literal::Symbol))),
            },
            ' ' => Ok(None),

            '\r' => Ok(None),

            '\t' => Ok(None),

            '\n' => {
                self.line += 1;
                Ok(None)
            }
            '"' => {
                let mut literal = String::new();
                // Iteraties through string and stores literal
                loop {
                    let c = match self.chars.peek() {
                        Some(c) => c,
                        // None would mean EOF here
                        None => {
                            return Err(ScanError::new("Unterminated String"));
                        }
                    };
                    // let c = self.chars.peek();
                    // if let None = c {
                    //     Err("Unterminated String")
                    // }

                    if c == &'"' {
                        // Consume closing parenthesis
                        self.chars.next();

                        // String is finished
                        break;
                    }

                    // Newlines ARE allowed
                    if c == &'\n' {
                        self.line += 1;
                    }

                    literal.push(*c);

                    // Consume next value as we've already used it
                    self.chars.next();
                }
                Ok(Some((TokenType::String, Literal::Symbol)))
            }
            _ => Err(ScanError {
                message: "Unkown Character Found".to_string(),
            }),
        }
    }
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
