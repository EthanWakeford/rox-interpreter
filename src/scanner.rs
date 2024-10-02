use std::{error::Error, fmt, iter::Peekable, str::Chars};

#[derive(Debug)]
pub enum TokenType {
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
    // Identifier,
    String(String),
    Number(f64),

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
    // EOF,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    // lexeme: String,
    // pub line: u64,
}

#[derive(Debug)]
pub struct ScanError {
    message: String,
}

impl ScanError {
    pub fn new<T: Into<String>>(message: T) -> ScanError {
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
    line: u64,
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
            let token_type = match self.scan_token(char)? {
                Some(token_type) => token_type,
                None => continue,
            };

            self.tokens.push(Token {
                token_type,
                // lexeme: literal,
                // line: self.line,
            });
        }

        // Remove for now unless I need it later
        // // End with EOF
        // self.tokens.push(Token {
        //     token_type: TokenType::EOF,
        //     // lexeme: "".to_string(),
        //     line: self.line,
        // });

        Ok(&self.tokens)
    }

    fn scan_token(&mut self, char: char) -> Result<Option<TokenType>, Box<dyn Error>> {
        match char {
            '(' => Ok(Some(TokenType::LeftParen)),
            ')' => Ok(Some(TokenType::RightParen)),
            '{' => Ok(Some(TokenType::LeftBrace)),
            '}' => Ok(Some(TokenType::RightBrace)),
            ',' => Ok(Some(TokenType::Comma)),
            '.' => Ok(Some(TokenType::Dot)),
            '-' => Ok(Some(TokenType::Minus)),
            '+' => Ok(Some(TokenType::Plus)),
            ';' => Ok(Some(TokenType::Semicolon)),
            '*' => Ok(Some(TokenType::Star)),
            '!' => match self.chars.peek().unwrap_or(&' ') {
                '=' => {
                    self.chars.next();
                    Ok(Some(TokenType::BangEqual))
                }
                _ => Ok(Some(TokenType::Bang)),
            },
            '=' => match self.chars.peek().unwrap_or(&' ') {
                '=' => {
                    self.chars.next();
                    Ok(Some(TokenType::EqualEqual))
                }
                _ => Ok(Some(TokenType::Equal)),
            },
            '<' => match self.chars.peek().unwrap_or(&' ') {
                '=' => {
                    self.chars.next();
                    Ok(Some(TokenType::LessEqual))
                }
                _ => Ok(Some(TokenType::Less)),
            },
            '>' => match self.chars.peek().unwrap_or(&' ') {
                '=' => {
                    self.chars.next();
                    Ok(Some(TokenType::GreaterEqual))
                }
                _ => Ok(Some(TokenType::Greater)),
            },
            '/' => match self.chars.peek().unwrap_or(&' ') {
                // comment found
                '/' => {
                    while self.chars.peek().unwrap_or(&' ') != &'\n' {
                        self.chars.next();
                    }
                    Ok(None)
                }
                _ => Ok(Some(TokenType::Slash)),
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

                Ok(Some(TokenType::String(literal)))
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

                Ok(Some(TokenType::Number(literal)))
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
                        return Ok(Some(TokenType::And));
                    }
                    "class" => {
                        return Ok(Some(TokenType::Class));
                    }
                    "else" => {
                        return Ok(Some(TokenType::Else));
                    }
                    "false" => {
                        return Ok(Some(TokenType::False));
                    }
                    "for" => {
                        return Ok(Some(TokenType::For));
                    }
                    "fun" => {
                        return Ok(Some(TokenType::Fun));
                    }
                    "if" => {
                        return Ok(Some(TokenType::If));
                    }
                    "nil" => {
                        return Ok(Some(TokenType::Nil));
                    }
                    "or" => {
                        return Ok(Some(TokenType::Or));
                    }
                    "print" => {
                        return Ok(Some(TokenType::Print));
                    }
                    "return" => {
                        return Ok(Some(TokenType::Return));
                    }
                    "super" => {
                        return Ok(Some(TokenType::Super));
                    }
                    "this" => {
                        return Ok(Some(TokenType::This));
                    }
                    "true" => {
                        return Ok(Some(TokenType::True));
                    }
                    "let" => {
                        return Ok(Some(TokenType::Let));
                    }
                    "while" => {
                        return Ok(Some(TokenType::While));
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
