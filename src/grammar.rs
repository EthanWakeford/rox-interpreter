use std::{any::Any, error::Error};

use crate::scanner::{ScanError, Token, TokenType};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

pub trait Evaluate {
    fn eval(&self) -> Result<Value, Box<dyn Error>>;
}

#[derive(Debug)]
pub enum Expr {
    // Literal(Literal),
    // Grouping(Grouping),
    Unary(Unary),
    Binary(Binary),
    // Operator(Operator),
    // Equality(Equality),
    Primary(Primary),
}

impl Expr {
    pub fn new(tokens: &[Token]) -> Result<(Expr, &[Token]), Box<dyn Error>> {
        // Everything is a binary expr or something derived from one

        let (binary, rest_tokens) = Binary::new(tokens)?;

        let expr = match binary {
            Binary::Primary(p) => Expr::Primary(p),
            Binary::Unary(u) => Expr::Unary(u),
            Binary::BinaryExpr(_, _, _) => Expr::Binary(binary),
        };

        Ok((expr, rest_tokens))
    }
}

impl Evaluate for Expr {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Expr::Primary(p) => p.eval()?,
            Expr::Unary(u) => u.eval()?,
            Expr::Binary(b) => b.eval()?,
        };
        Ok(value)
    }
}

// // Not sure about this one but we'll leave for nows
#[derive(Debug)]
pub struct Grouping(Box<Expr>);

impl Grouping {
    pub fn new(tokens: &[Token]) -> Result<(Grouping, &[Token]), Box<dyn Error>> {
        let mut index = 1;
        while let Some(token) = tokens.get(index) {
            match &token.token_type {
                TokenType::RightParen => {
                    let (expr, rest_tokens) = Expr::new(&tokens[..index])?;
                    // Rest_tokens should not be allowed here???
                    if rest_tokens.len() > 0 {
                        return Err(Box::new(ScanError::new(
                            "Unexpected Tokens In Group Expression",
                        )));
                    }

                    // let primary = Primary::Grouping(Grouping(Box::new(expr)));
                    let grouping = Grouping(Box::new(expr));

                    // Rest of tokens start immediately after right paren
                    return Ok((grouping, &tokens[(index + 1)..]));
                }
                _ => {
                    index += 1;
                    continue;
                }
            }
        }

        Err(Box::new(ScanError::new("Expected Closing Parenthesis")))
    }
}

impl Evaluate for Grouping {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        self.0.eval()
    }
}

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

impl Evaluate for Binary {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Binary::Primary(p) => p.eval()?,
            Binary::Unary(u) => u.eval()?,
            Binary::BinaryExpr(left, op, right) => {
                let left = left.eval()?;
                let right = right.eval()?;

                match op {
                    // No equality type coercion
                    // Equality between different types allowed but will always be false
                    Operator::EqualEqual => match (left, right) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Bool(leftnum == rightnum)
                        }
                        (Value::String(leftstr), Value::String(rightstr)) => {
                            Value::Bool(leftstr == rightstr)
                        }
                        (Value::Bool(boolright), Value::Bool(boolleft)) => {
                            Value::Bool(boolleft == boolright)
                        }
                        (Value::Nil, Value::Nil) => Value::Bool(true),
                        _ => Value::Bool(false),
                    },
                    Operator::BangEqual => match (left, right) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Bool(leftnum != rightnum)
                        }
                        (Value::String(leftstr), Value::String(rightstr)) => {
                            Value::Bool(leftstr != rightstr)
                        }
                        (Value::Bool(boolright), Value::Bool(boolleft)) => {
                            Value::Bool(boolleft != boolright)
                        }
                        (Value::Nil, Value::Nil) => Value::Bool(false),
                        _ => Value::Bool(true),
                    },
                    // Comparison only allowed between numbers otherwise throw error
                    Operator::Less => match (left.clone(), right.clone()) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Bool(leftnum < rightnum)
                        }
                        _ => {
                            let message =
                                format!("Invalid Comparison {:?}, Requires 2 Numbers", op);
                            return Err(Box::new(ScanError::new(message)));
                        }
                    },
                    Operator::LessEqual => match (left, right) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Bool(leftnum <= rightnum)
                        }
                        _ => {
                            let message =
                                format!("Invalid Comparison {:?}, Requires 2 Numbers", op);
                            return Err(Box::new(ScanError::new(message)));
                        }
                    },
                    Operator::Greater => match (left, right) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Bool(leftnum > rightnum)
                        }
                        _ => {
                            let message =
                                format!("Invalid Comparison {:?}, Requires 2 Numbers", op);
                            return Err(Box::new(ScanError::new(message)));
                        }
                    },
                    Operator::GreaterEqual => match (left, right) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Bool(leftnum >= rightnum)
                        }
                        _ => {
                            let message =
                                format!("Invalid Comparison {:?}, Requires 2 Numbers", op);
                            return Err(Box::new(ScanError::new(message)));
                        }
                    },
                    // Plus can also Concatenate Strings
                    Operator::Plus => match (left, right) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Number(leftnum + rightnum)
                        }
                        (Value::String(mut leftstr), Value::String(rightstr)) => {
                            leftstr.push_str(&rightstr);
                            Value::String(leftstr)
                        }
                        _ => {
                            let message = format!(
                                "Invalid Operation {:?}, Requires 2 Numbers Or 2 Strings",
                                op
                            );
                            return Err(Box::new(ScanError::new(message)));
                        }
                    },
                    // subtract, divide, multiply only on numbers
                    Operator::Minus => match (left, right) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Number(leftnum - rightnum)
                        }
                        _ => {
                            let message = format!("Invalid Operation {:?}, Requires 2 Numbers", op);
                            return Err(Box::new(ScanError::new(message)));
                        }
                    },
                    Operator::Star => match (left, right) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Number(leftnum * rightnum)
                        }
                        _ => {
                            let message = format!("Invalid Operation {:?}, Requires 2 Numbers", op);
                            return Err(Box::new(ScanError::new(message)));
                        }
                    },
                    Operator::Slash => match (left, right) {
                        (Value::Number(leftnum), Value::Number(rightnum)) => {
                            Value::Number(leftnum / rightnum)
                        }
                        _ => {
                            let message = format!("Invalid Operation {:?}, Requires 2 Numbers", op);
                            return Err(Box::new(ScanError::new(message)));
                        }
                    },
                }
            }
        };
        Ok(value)
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

impl Evaluate for Unary {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Unary::Primary(p) => p.eval()?,
            Unary::UnaryExpr(op, primary) => {
                let prim_val = primary.eval()?;
                match (op, prim_val) {
                    (UnaryOp::Bang, Value::Bool(b)) => Value::Bool(!b),
                    (UnaryOp::Minus, Value::Number(num)) => Value::Number(num * -1.0),
                    _ => {
                        let message = format!("Invalid Operation {:?} on type {:?}", op, primary);
                        return Err(Box::new(ScanError::new(message)));
                    }
                }
            }
        };
        Ok(value)
    }
}

#[derive(Debug)]
pub enum Primary {
    NUMBER(f64),
    STRING(String),
    True,
    False,
    Nil,
    Grouping(Grouping),
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
                let (grouping, rest_tokens) = Grouping::new(&tokens[1..])?;

                return Ok((Primary::Grouping(grouping), rest_tokens));
            }
            token => {
                let message = format!("Unexpected Token: {:?}", token);
                return Err(Box::new(ScanError::new(message)));
            }
        };

        Ok((primary, &tokens[1..]))
    }
}

impl Evaluate for Primary {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let val = match self {
            Primary::NUMBER(num) => Value::Number(*num),
            Primary::STRING(s) => Value::String(s.to_string()),
            Primary::True => Value::Bool(true),
            Primary::False => Value::Bool(false),
            Primary::Nil => Value::Nil,
            Primary::Grouping(g) => g.eval()?,
        };
        Ok(val)
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
