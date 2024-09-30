use std::error::Error;

use crate::scanner::{ScanError, Token, TokenType};

pub enum Expr {
    Literal(Literal),
    // Grouping(Grouping),
    // Unary(Unary),
    Binary(Binary),
    Operator(Operator),
    // Equality(Equality),
}

impl Expr {
    pub fn new(tokens: &Vec<Token>) -> Result<Expr, Box<dyn Error>> {
        // What is the best way to figure out what is the correct
        // Expression to look for

        let token = match tokens.get(0) {
            Some(l) => l,
            None => {
                return Err(Box::new(ScanError::new("Expected Token")));
            }
        };

        let literal = Literal::new(token)?;

        Ok(Expr::Literal(literal))
    }
}

pub enum Primary {
    NUMBER(f64),
    STRING(String),
    True,
    False,
    Nil,
    Expr(Box<Expr>),
}

impl Primary {
    pub fn new(tokens: &Vec<Token>) -> Result<Primary, Box<dyn Error>> {
        if tokens.len() > 1 {
            let expr = Expr::new(tokens)?;

            return Ok(Primary::Expr(Box::new(expr)));
        }

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
            _ => {
                return Err(Box::new(ScanError::new("Invalid Literal")));
            }
        };

        Ok(primary)
    }
}

enum UnaryOp {
    Minus,
    Bang,
}
enum UnaryOrExpr {
    Unary(Box<Unary>),
    Expr(Box<Expr>),
}
pub struct Unary(UnaryOp, UnaryOrExpr);

enum FactorOp {
    Slash,
    Star,
}

enum UnaryOrFactorOp {
    FactorOp(FactorOp),
    Unary(Unary),
}

pub struct Factor(Unary, UnaryOrFactorOp);

enum TermOp {
    Plus,
    Minus,
}

enum FactorOrTermOp {
    Factor(Factor),
    TermOp(TermOp),
}

pub struct Term(Factor, FactorOrTermOp);

enum ComparisonOp {
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

enum TermOrComparisonOp {
    Term(Term),
    ComparisionOp(ComparisonOp),
}

pub struct Comparison(Term, TermOrComparisonOp);

enum EqualityOp {
    BangEqual,
    EqualEqual,
}

enum ComparisonOrEqualityOp {
    Comparison,
    EqualityOp,
}

pub struct Equality(Comparison, ComparisonOrEqualityOp);

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
pub struct Grouping(Box<Expr>);

pub struct Binary(Box<Expr>, Operator, Box<Expr>);

impl Binary {
    pub fn new(tokens: &Vec<Token>) -> Result<Binary, Box<dyn Error>> {
        let [left, op, right] = (match tokens.get(0..=2) {
            Some(slice) => slice,
            None => {
                return Err(Box::new(ScanError::new("Invalid Literal")));
            }
        }) else {
            return Err(Box::new(ScanError::new("Invalid Literal")));
        };

        let left_expr = match &left.token_type {
            TokenType::String(s) => Box::new(Expr::Literal(Literal::STRING(s.to_string()))),
            _ => {
                let message = format!(
                    "Operation {:?} Not Allowed on Types {:?},{:?}",
                    op.token_type, left.token_type, right.token_type
                );
                return Err(Box::new(ScanError::new(message)));
            }
        };

        let right_expr = match &right.token_type {
            TokenType::String(s) => Box::new(Expr::Literal(Literal::STRING(s.to_string()))),
            _ => {
                let message = format!(
                    "Operation {:?} Not Allowed on Types {:?},{:?}",
                    op.token_type, left.token_type, right.token_type
                );
                return Err(Box::new(ScanError::new(message)));
            }
        };

        let op_expr = Operator::new(op)?;

        Ok(Binary(left_expr, op_expr, right_expr))
    }
}

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
