use crate::{grammar::*, scanner::Token};

pub struct Parser {
    tokens: Vec<Token>,
    current: i64,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            current: 0,
        }
    }

    fn parse(&self) {
        let mut expr= Expr::new(&self.tokens);

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
