use crate::{grammar::*, scanner::Token};

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: i64,
}

impl Parser<'_> {
    pub fn new(tokens: &Vec<Token>) -> Parser {
        Parser {
            tokens: &tokens,
            current: 0,
        }
    }

    pub fn parse(&self) {
        let expr = Expr::new(&self.tokens);

        match expr {
            Err(e) => {
                eprintln!("{}", e);
            }
            Ok(expr) => {
                println!("{:?}", expr);
            }
        }

        //     for t in &self.tokens {
        //         print!("asdf")
        //     }
        // }

        // fn expr(&self) {
        //     self.equality()
        // }

        // fn equality(&self) {
        //     let expr = self.comparison();
    }
}
