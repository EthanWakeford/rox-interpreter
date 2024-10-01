use crate::{grammar::*, scanner::Token};

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    line: i64,
}

impl Parser<'_> {
    pub fn new(tokens: &Vec<Token>) -> Parser {
        Parser {
            tokens: &tokens,
            line: 1,
        }
    }

    pub fn parse(&mut self) {
        let mut tok_slice = self.tokens.as_slice();

        while !tok_slice.is_empty() {
            let expr = Expr::new(tok_slice);

            match expr {
                Err(e) => {
                    eprintln!("[Line: {}] {}", self.line, e);
                }
                Ok((expr, rest_tokens)) => {
                    println!("[Line: {}] {:?}", self.line, expr);

                    // Update tok_slice to the remaining tokens
                    tok_slice = rest_tokens;
                }
            }
            self.line += 1;
        }
    }
}
