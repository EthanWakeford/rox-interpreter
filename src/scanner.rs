use std::{error::Error, fmt, iter::Peekable, str::Chars};

#[derive(Debug)]
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

#[derive(Debug)]
enum Literal {
    Number(f64),
    String(String),
    Symbol,
    Keyword,
}

#[derive(Debug)]
pub struct Token {
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
    fn new<T: Into<String>>(message: T) -> ScanError {
        ScanError {
            message: message.into(),
        }
    }
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for ScanError {}

pub struct Scanner<'a> {
    tokens: Vec<Token>,
    chars: Peekable<Chars<'a>>,
    // start: u32,
    // current: u32,
    line: u32,
    // length: u32,
}

impl Scanner<'_> {
    pub fn new<'a>(input: &'a String) -> Scanner<'a> {
        let tokens: Vec<Token> = vec![];
        let chars = input.chars().peekable();

        Scanner {
            tokens,
            chars,
            // start: 0,
            // current: 0,
            line: 1,
            // length: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, Box<dyn Error>> {
        // let mut had_error = false;

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

    fn scan_token(&mut self, char: char) -> Result<Option<(TokenType, Literal)>, Box<dyn Error>> {
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
                // Iterates through string and stores literal
                loop {
                    let c = match self.chars.peek() {
                        Some(c) => c,
                        // None would mean EOF here
                        None => {
                            return Err(Box::new(ScanError::new("Unterminated String")));
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

                Ok(Some((TokenType::String, Literal::String(literal))))
            }
            num @ '0'..='9' => {
                let mut literal = String::from(num);
                let mut found_dot = false;

                loop {
                    let c = match self.chars.peek() {
                        Some(c) => c,
                        // EOF
                        None => {
                            break;
                        }
                    };

                    match c {
                        '0'..='9' => {
                            literal.push(*c);
                            self.chars.next();
                        }
                        '.' => {
                            if found_dot {
                                // Only one dot allowed in num
                                return Err(Box::new(ScanError::new("Unexpected Character '.'")));
                            }

                            found_dot = true;
                            literal.push(*c);
                            self.chars.next();
                        }
                        _ => {
                            break;
                        }
                    }
                }

                // Numbers are not allowd to end with dot
                if literal.ends_with('.') {
                    return Err(Box::new(ScanError::new("Unexpected Character '.'")));
                };

                let literal = literal.parse::<f64>()?;

                Ok(Some((TokenType::Number, Literal::Number(literal))))
            }
            c @ 'A'..='z' => {
                let mut literal = String::from(c);
                loop {
                    match self.chars.peek() {
                        Some(c) => match c {
                            // Push alpha chars
                            c @ 'A'..='z' => {
                                literal.push(*c);
                                self.chars.next();
                            }
                            _ => break,
                        },
                        // None would mean EOF here
                        None => {
                            break;
                        }
                    };
                }

                // Keyword matching
                match literal.as_str() {
                    "and" => {
                        return Ok(Some((TokenType::And, Literal::Keyword)));
                    }
                    "class" => {
                        return Ok(Some((TokenType::Class, Literal::Keyword)));
                    }
                    "else" => {
                        return Ok(Some((TokenType::Else, Literal::Keyword)));
                    }
                    "false" => {
                        return Ok(Some((TokenType::False, Literal::Keyword)));
                    }
                    "for" => {
                        return Ok(Some((TokenType::For, Literal::Keyword)));
                    }
                    "fun" => {
                        return Ok(Some((TokenType::Fun, Literal::Keyword)));
                    }
                    "if" => {
                        return Ok(Some((TokenType::If, Literal::Keyword)));
                    }
                    "nil" => {
                        return Ok(Some((TokenType::Nil, Literal::Keyword)));
                    }
                    "or" => {
                        return Ok(Some((TokenType::Or, Literal::Keyword)));
                    }
                    "print" => {
                        return Ok(Some((TokenType::Print, Literal::Keyword)));
                    }
                    "return" => {
                        return Ok(Some((TokenType::Return, Literal::Keyword)));
                    }
                    "super" => {
                        return Ok(Some((TokenType::Super, Literal::Keyword)));
                    }
                    "this" => {
                        return Ok(Some((TokenType::This, Literal::Keyword)));
                    }
                    "true" => {
                        return Ok(Some((TokenType::True, Literal::Keyword)));
                    }
                    "let" => {
                        return Ok(Some((TokenType::Let, Literal::Keyword)));
                    }
                    "while" => {
                        return Ok(Some((TokenType::While, Literal::Keyword)));
                    }
                    _ => {
                        let message = format!("Unexpected Keyword '{}'", literal);
                        return Err(Box::new(ScanError::new(message)));
                    }
                }
            }
            _ => Err(Box::new(ScanError {
                message: "Unkown Character Found".to_string(),
            })),
        }
    }
}
