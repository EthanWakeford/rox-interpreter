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

        // let (primary, _) = Primary::new(tokens)?;
        // Ok(Expr::Primary(primary))

        // let (unary, _) = Unary::new(tokens)?;
        // Ok(Expr::Unary(unary))

        let (binary, _) = Binary::new(tokens)?;

        Ok(match binary {
            Binary::Primary(p) => Expr::Primary(p),
            Binary::Unary(u) => Expr::Unary(u),
            Binary::BinaryExpr(_, _, _) => Expr::Binary(binary),
        })
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
pub enum UnaryOp {
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
                return Err(Box::new(ScanError::new("Expected Token")));
            }
        };

        let (unary, sliced_tokens) = Unary::new(&tokens[1..])?;
        let unary = Unary::UnaryExpr(un_op, Box::new(unary));

        Ok((unary, sliced_tokens))
    }
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
    BinaryExpr(Box<Binary>, Operator, Box<Binary>),
    Unary(Unary),
    Primary(Primary),
}

impl Binary {
    pub fn new(tokens: &[Token]) -> Result<(Binary, &[Token]), Box<dyn Error>> {
        Binary::equality(tokens)
    }

    fn equality(tokens: &[Token]) -> Result<(Binary, &[Token]), Box<dyn Error>> {
        let (mut expr, mut sliced_tokens) = Binary::comparison(tokens)?;

        while let Some(token) = sliced_tokens.get(0) {
            let operator = match token.token_type {
                TokenType::EqualEqual => Operator::EqualEqual,
                TokenType::BangEqual => Operator::BangEqual,
                _ => break,
            };
            sliced_tokens = &sliced_tokens[1..]; // Consume the operator

            let (right, rest_tokens) = Binary::comparison(sliced_tokens)?;
            expr = Binary::BinaryExpr(Box::new(expr), operator, Box::new(right));
            sliced_tokens = rest_tokens;
        }

        Ok((expr, sliced_tokens))
    }

    fn comparison(tokens: &[Token]) -> Result<(Binary, &[Token]), Box<dyn Error>> {
        let (mut expr, mut sliced_tokens) = Binary::term(tokens)?;

        while let Some(token) = sliced_tokens.get(0) {
            let operator = match token.token_type {
                TokenType::Greater => Operator::Greater,
                TokenType::GreaterEqual => Operator::GreaterEqual,
                TokenType::LessEqual => Operator::LessEqual,
                TokenType::Less => Operator::Less,
                _ => break,
            };
            sliced_tokens = &sliced_tokens[1..]; // Consume the operator

            let (right, rest_tokens) = Binary::term(sliced_tokens)?;
            expr = Binary::BinaryExpr(Box::new(expr), operator, Box::new(right));
            sliced_tokens = rest_tokens;
        }

        Ok((expr, sliced_tokens))
    }

    fn term(tokens: &[Token]) -> Result<(Binary, &[Token]), Box<dyn Error>> {
        let (mut expr, mut sliced_tokens) = Binary::factor(tokens)?;

        while let Some(token) = sliced_tokens.get(0) {
            let operator = match token.token_type {
                TokenType::Plus => Operator::Plus,
                TokenType::Minus => Operator::Minus,
                _ => break,
            };
            sliced_tokens = &sliced_tokens[1..]; // Consume the operator

            let (right, rest_tokens) = Binary::factor(sliced_tokens)?;
            expr = Binary::BinaryExpr(Box::new(expr), operator, Box::new(right));
            sliced_tokens = rest_tokens;
        }

        Ok((expr, sliced_tokens))
    }

    fn factor(tokens: &[Token]) -> Result<(Binary, &[Token]), Box<dyn Error>> {
        let (left, mut sliced_tokens) = Unary::new(tokens)?;
        let mut expr = match left {
            Unary::Primary(p) => Binary::Primary(p),
            Unary::UnaryExpr(_, _) => Binary::Unary(left),
        };

        while let Some(token) = sliced_tokens.get(0) {
            let operator = match token.token_type {
                TokenType::Star => Operator::Star,
                TokenType::Slash => Operator::Slash,
                _ => break,
            };
            sliced_tokens = &sliced_tokens[1..]; // Consume the operator

            let (right, rest_tokens) = Unary::new(sliced_tokens)?;
            let rightexpr = match right {
                Unary::Primary(p) => Binary::Primary(p),
                Unary::UnaryExpr(_, _) => Binary::Unary(right),
            };

            sliced_tokens = rest_tokens;
            expr = Binary::BinaryExpr(Box::new(expr), operator, Box::new(rightexpr));
        }

        Ok((expr, sliced_tokens))
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
