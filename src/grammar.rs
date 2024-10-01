use std::error::Error;

use crate::scanner::{ScanError, Token, TokenType};

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    // Grouping(Grouping),
    Unary(Unary),
    Binary(Binary),
    Operator(Operator),
    // Equality(Equality),
    Primary(Primary),
}

impl Expr {
    pub fn new(tokens: &[Token]) -> Result<Expr, Box<dyn Error>> {
        // What is the best way to figure out what is the correct
        // Expression to look for

        // let token = match tokens.get(0) {
        //     Some(l) => l,
        //     None => {
        //         return Err(Box::new(ScanError::new("Expected Token")));
        //     }
        // };

        // let literal = Literal::new(token)?;

        println!("making an expr");

        // let (primary, _) = Primary::new(tokens)?;
        // Ok(Expr::Primary(primary))

        // let (unary, _) = Unary::new(tokens)?;
        // Ok(Expr::Unary(unary))

        let (binary, _) = Binary::new(tokens)?;
        Ok(Expr::Binary(binary))
    }
}

#[derive(Debug)]
pub enum Primary {
    NUMBER(f64),
    STRING(String),
    True,
    False,
    Nil,
    Expr(Box<Expr>),
}

impl Primary {
    pub fn new(tokens: &[Token]) -> Result<(Primary, &[Token]), Box<dyn Error>> {
        println!("making a primary");

        let token = match tokens.get(0) {
            Some(t) => t,
            None => {
                return Err(Box::new(ScanError::new("Token Expected But Not Found")));
            }
        };

        let primary = match &token.token_type {
            TokenType::Number(num) => Primary::NUMBER(*num),
            TokenType::String(s) => Primary::STRING(s.to_string()),
            TokenType::True => Primary::True,
            TokenType::False => Primary::False,
            TokenType::Nil => Primary::Nil,
            TokenType::LeftParen => {
                let expr = Expr::new(&tokens[1..])?;
                let primary = Primary::Expr(Box::new(expr));

                return Ok((primary, &tokens[..1]));
            }
            token => {
                let message = format!("Unexpected Token: {:?}", token);
                return Err(Box::new(ScanError::new(message)));
            }
        };

        Ok((primary, &tokens[1..]))
    }
}

#[derive(Debug)]
enum UnaryOp {
    Minus,
    Bang,
}

#[derive(Debug)]
pub enum Unary {
    UnaryExpr(UnaryOp, Box<Unary>),
    Primary(Primary),
}

impl Unary {
    pub fn new(tokens: &[Token]) -> Result<(Unary, &[Token]), Box<dyn Error>> {
        let un_op = match tokens.get(0) {
            Some(op) => match op.token_type {
                TokenType::Minus => UnaryOp::Minus,
                TokenType::Bang => UnaryOp::Bang,
                _ => {
                    let (primary, sliced_tokens) = Primary::new(tokens)?;

                    return Ok((Unary::Primary(primary), sliced_tokens));
                }
            },
            None => {
                println!("found none");
                return Err(Box::new(ScanError::new("Expected Token")));
            }
        };

        let (unary, sliced_tokens) = Unary::new(&tokens[1..])?;
        let unary = Unary::UnaryExpr(un_op, Box::new(unary));

        Ok((unary, sliced_tokens))
    }
}

#[derive(Debug)]
enum FactorOp {
    Slash,
    Star,
}

#[derive(Debug)]
pub enum Factor {
    Unary(Unary),
    FactorExpr(FactorOp, Unary),
}

impl Factor {
    pub fn new(tokens: &[Token]) -> Result<(Factor, &[Token]), Box<dyn Error>> {
        let (unary, sliced_tokens) = Unary::new(tokens)?;

        let mut binary: Binary;

        loop {
            let factor_op = match sliced_tokens.get(0) {
                Some(op) => match op.token_type {
                    TokenType::Slash => FactorOp::Slash,
                    TokenType::Star => FactorOp::Star,
                    _ => {
                        //   Done with loop
                        let factor = Factor::Unary(unary);

                        return Ok((factor, sliced_tokens));
                    }
                },
                None => {
                    println!("found none");
                    return Err(Box::new(ScanError::new(
                        "No Token Found, Exprected Unary Operator",
                    )));
                }
            };
        }

        let factor = Factor::Unary(unary);

        Ok((factor, sliced_tokens))
    }
}

#[derive(Debug)]
enum TermOp {
    Plus,
    Minus,
}

#[derive(Debug)]
enum FactorOrTermOp {
    Factor(Factor),
    TermOp(TermOp),
}

#[derive(Debug)]
pub struct Term(Factor, FactorOrTermOp);

#[derive(Debug)]
enum ComparisonOp {
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug)]
enum TermOrComparisonOp {
    Term(Term),
    ComparisionOp(ComparisonOp),
}

#[derive(Debug)]
pub struct Comparison(Term, TermOrComparisonOp);

#[derive(Debug)]
enum EqualityOp {
    BangEqual,
    EqualEqual,
}

#[derive(Debug)]
pub struct Equality(Comparison, (EqualityOp, Comparison));

impl Equality {
    // pub fn new(tokens: &[Token]) -> Result<(Equality, &[Token]), Box<dyn Error>> {
    //     let (left, sliced_tokens) = Comparison::New(tokens);

    //     let eq_op = match tokens.get(0) {
    //         Some(op) => match op.token_type {
    //             TokenType::EqualEqual => EqualityOp::EqualEqual,
    //             TokenType::BangEqual => EqualityOp::BangEqual,
    //             _ => {
    //                 println!("unexepcred");
    //                 let message = format!(
    //                     "Unexpected Token {:?} Found, Exprected Unary Operator",
    //                     op.token_type
    //                 );
    //                 return Err(Box::new(ScanError::new(message)));
    //             }
    //         },
    //         None => {
    //             println!("found none");
    //             return Err(Box::new(ScanError::new(
    //                 "No Token Found, Exprected Unary Operator",
    //             )));
    //         }
    //     };
    // }
}

impl Equality {
    // pub fn new()
}

#[derive(Debug)]
pub enum Literal {
    NUMBER(f64),
    STRING(String),
    True,
    False,
    Nil,
}

impl Literal {
    pub fn new(token: &Token) -> Result<Literal, Box<dyn Error>> {
        let literal = match &token.token_type {
            TokenType::Number(num) => Literal::NUMBER(*num),
            TokenType::String(s) => Literal::STRING(s.to_string()),
            TokenType::True => Literal::True,
            TokenType::False => Literal::False,
            TokenType::Nil => Literal::Nil,
            _ => {
                return Err(Box::new(ScanError::new("Invalid Literal")));
            }
        };

        Ok(literal)
    }
}

// // Not sure about this one but we'll leave for nows
#[derive(Debug)]
pub struct Grouping(Box<Expr>);

#[derive(Debug)]
pub enum Binary {
    BinaryExpr(Unary, Operator, Box<Expr>),
    Unary(Unary),
}

impl Binary {
    pub fn new(tokens: &[Token]) -> Result<(Binary, &[Token]), Box<dyn Error>> {
        println!("making a binary");
        let (unary, sliced_tokens) = Unary::new(tokens)?;

        println!("made binary");
        dbg!(sliced_tokens);

        if sliced_tokens.len() == 0 {
            return Ok((Binary::Unary(unary), &sliced_tokens[..]));
        }

        let bin_op = match sliced_tokens.get(0) {
            Some(op) => op,
            None => {
                return Err(Box::new(ScanError::new(
                    "Expected Binary Token But Found None",
                )));
            }
        };

        println!("here is my bin op {:?}", bin_op);

        let bin_op = match bin_op.token_type {
            TokenType::EqualEqual => Operator::BangEqual,
            TokenType::BangEqual => Operator::BangEqual,
            TokenType::Greater => Operator::Greater,
            TokenType::GreaterEqual => Operator::GreaterEqual,
            TokenType::Less => Operator::Less,
            TokenType::LessEqual => Operator::LessEqual,
            TokenType::Minus => Operator::Minus,
            TokenType::Plus => Operator::Plus,
            TokenType::Slash => Operator::Slash,
            TokenType::Star => Operator::Star,
            _ => {
                return Err(Box::new(ScanError::new(
                    "Unexpected Token, Expected Binary Operator",
                )));
            }
        };
        // first item was consumed
        let sliced_tokens = &sliced_tokens[1..];

        let expr = Expr::new(sliced_tokens)?;

        Ok((
            Binary::BinaryExpr(unary, bin_op, Box::new(expr)),
            sliced_tokens,
        ))
    }
}

#[derive(Debug)]
pub enum Operator {
    Minus,
    Plus,
    Slash,
    Star,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Operator {
    pub fn new(token: &Token) -> Result<Operator, Box<dyn Error>> {
        let operator = match &token.token_type {
            TokenType::Minus => Operator::Minus,
            TokenType::Plus => Operator::Plus,
            TokenType::Slash => Operator::Slash,
            TokenType::Star => Operator::Star,
            TokenType::BangEqual => Operator::BangEqual,
            TokenType::EqualEqual => Operator::EqualEqual,
            TokenType::Greater => Operator::Greater,
            TokenType::GreaterEqual => Operator::GreaterEqual,
            TokenType::Less => Operator::Less,
            TokenType::LessEqual => Operator::LessEqual,
            _ => {
                return Err(Box::new(ScanError::new("Invalid Operator")));
            }
        };

        Ok(operator)
    }
}
