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

struct Token {
    tokentype: TokenType,
    // lexeme: String,
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
//         )x
//     }
// }

struct Scanner<'a> {
    tokens: Vec<Token>,
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
            // Literal used for tokens that need it
            // String, number, etc.
            let mut literal = String::new();

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
                '!' => match self.chars.peek().unwrap_or(&' ') {
                    '=' => {
                        self.chars.next();
                        TokenType::BangEqual
                    }
                    _ => TokenType::Bang,
                },
                '=' => match self.chars.peek().unwrap_or(&' ') {
                    '=' => {
                        self.chars.next();
                        TokenType::EqualEqual
                    }
                    _ => TokenType::Equal,
                },
                '<' => match self.chars.peek().unwrap_or(&' ') {
                    '=' => {
                        self.chars.next();
                        TokenType::LessEqual
                    }
                    _ => TokenType::Less,
                },
                '>' => match self.chars.peek().unwrap_or(&' ') {
                    '=' => {
                        self.chars.next();
                        TokenType::GreaterEqual
                    }
                    _ => TokenType::Greater,
                },
                '/' => match self.chars.peek().unwrap_or(&' ') {
                    // comment found
                    '/' => {
                        while self.chars.peek().unwrap_or(&' ') != &'\n' {
                            self.chars.next();
                        }
                        continue;
                    }
                    _ => TokenType::Slash,
                },
                ' ' => {
                    continue;
                }
                '\r' => {
                    continue;
                }
                '\t' => {
                    continue;
                }
                '\n' => {
                    self.line += 1;
                    continue;
                }
                '"' => {
                    // Iteraties through string and stores literal
                    loop {
                        let c = match self.chars.peek() {
                            Some(c) => c,
                            // None would mean EOF here
                            None => {
                                print_error(self.line, "Unterminated String".to_string());
                                return;
                            }
                        };

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
                    TokenType::String
                }
                _ => {
                    print_error(self.line, format!("Unexpected character: {}", char));
                    had_error = true;
                    continue;
                }
            };

            self.tokens.push(Token {
                tokentype: token_type,
                // lexeme: literal,
                literal: "".to_string(),
                line: self.line,
            });
            break;
        }

        // End with EOF
        self.tokens.push(Token {
            tokentype: TokenType::EOF,
            // lexeme: "".to_string(),
            literal: "".to_string(),
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
