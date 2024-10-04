use std::{error::Error, thread::Scope};

use crate::{
    resolver::Environment,
    scanner::{ScanError, Token, TokenType},
};

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
pub struct AST<'a> {
    pub decls: Vec<Declaration<'a>>,
}

impl AST<'_> {
    pub fn new(mut tokens: &[Token]) -> Result<AST, Box<dyn Error>> {
        let mut decls = Vec::new();

        while !tokens.is_empty() {
            let decl = Declaration::new(tokens);

            match decl {
                Err(e) => {
                    // TODO: How do I figure out where to run again??
                    eprintln!("{e}");
                    panic!("panic")
                }
                Ok((expr, rest_tokens)) => {
                    decls.push(expr);

                    // Update tok_slice to the remaining tokens
                    tokens = rest_tokens;
                }
            }
        }

        let ast = AST { decls };

        Ok(ast)
    }

    pub fn run(&self) -> Result<(), Box<dyn Error>> {
        for stmt in &self.decls {
            let value = stmt.eval()?;

            dbg!(value);
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Declaration<'a> {
    VarDecl(VarDecl<'a>),
    Statement(Statement<'a>),
}

impl Declaration<'_> {
    pub fn new(tokens: &[Token]) -> Result<(Declaration, &[Token]), Box<dyn Error>> {
        if let Some(token) = tokens.get(0) {
            match token.token_type {
                // Look for "let" keyword
                TokenType::Let => {
                    let (decl, rest_tokens) = VarDecl::new(&tokens[1..])?;
                    let decl = Declaration::VarDecl(decl);

                    return Ok((decl, rest_tokens));
                }
                _ => (),
            }
        }

        let (stmt, rest_tokens) = Statement::new(tokens)?;

        let decl = Declaration::Statement(stmt);

        Ok((decl, rest_tokens))
    }
}

impl Evaluate for Declaration<'_> {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Declaration::Statement(stmt) => stmt.eval()?,
            Declaration::VarDecl(vd) => vd.eval()?,
        };

        Ok(value)
    }
}

#[derive(Debug)]
pub struct VarDecl<'a>(pub Identifier<'a>, pub Expr<'a>);

impl VarDecl<'_> {
    pub fn new(tokens: &[Token]) -> Result<(VarDecl, &[Token]), Box<dyn Error>> {
        let identifier = match tokens.get(..=1) {
            Some([iden_token, eq_token]) => match (&iden_token.token_type, &eq_token.token_type) {
                (TokenType::Identifier(identifier), TokenType::Equal) => identifier,
                _ => {
                    return Err(Box::new(ScanError::new(
                        "Expected Identifier And Assignment Operator After Let Declaration",
                    )));
                }
            },
            None => {
                return Err(Box::new(ScanError::new("Expected Identifier After Let")));
            }
            _ => {
                return Err(Box::new(ScanError::new(
                    "Expected Identifier And Assignment Operator After Let Declaration",
                )));
            }
        };

        let identifier = Identifier::Unresolved(identifier.to_string());
        let rest_tokens = &tokens[2..];
        let (expr, rest_tokens) = Expr::new(rest_tokens)?;

        Ok((VarDecl(identifier, expr), rest_tokens))
    }
}

impl Evaluate for VarDecl<'_> {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let (name, scope) = match &self.0 {
            Identifier::Unresolved(_name) => {
                return Err(Box::new(ScanError::new(
                    "Somehow identifier is not resolved here during var decl",
                )));
            }
            Identifier::Resolved { name, env: scope } => (name, scope),
        };

        let val = self.1.eval()?;
        scope
            .values
            .borrow_mut()
            .insert(name.to_string(), Some(val));

        Ok(Value::Nil)
    }
}

#[derive(Debug)]
pub enum Statement<'a> {
    ExprStatement(ExprStatement<'a>),
    PrintStatement(PrintStatement<'a>),
}

impl Statement<'_> {
    pub fn new(tokens: &[Token]) -> Result<(Statement, &[Token]), Box<dyn Error>> {
        if let Some(p) = tokens.get(0) {
            match p.token_type {
                TokenType::Print => {
                    let (stmt, rest_tokens) = PrintStatement::new(&tokens[1..])?;
                    let stmt = Statement::PrintStatement(stmt);
                    return Ok((stmt, rest_tokens));
                }
                _ => (),
            };
        }

        let (stmt, rest_tokens) = ExprStatement::new(tokens)?;
        let stmt = Statement::ExprStatement(stmt);
        return Ok((stmt, rest_tokens));
    }
}

impl Evaluate for Statement<'_> {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Self::ExprStatement(e) => e.eval()?,
            Self::PrintStatement(p) => p.eval()?,
        };

        Ok(value)
    }
}

#[derive(Debug)]
pub struct ExprStatement<'a>(pub Expr<'a>);

impl ExprStatement<'_> {
    pub fn new(tokens: &[Token]) -> Result<(ExprStatement, &[Token]), Box<dyn Error>> {
        let (expr, rest_tokens) = Expr::new(tokens)?;

        Ok((ExprStatement(expr), rest_tokens))
    }
}

impl Evaluate for ExprStatement<'_> {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = self.0.eval()?;

        Ok(value)
    }
}

#[derive(Debug)]
pub struct PrintStatement<'a>(pub Expr<'a>);

impl PrintStatement<'_> {
    pub fn new(tokens: &[Token]) -> Result<(PrintStatement, &[Token]), Box<dyn Error>> {
        let (expr, rest_tokens) = Expr::new(tokens)?;

        Ok((PrintStatement(expr), rest_tokens))
    }
}

impl Evaluate for PrintStatement<'_> {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = self.0.eval()?;

        // Do I print here????

        match value.clone() {
            Value::String(str) => {
                println!("{str}");
            }
            Value::Bool(b) => {
                println!("{b}");
            }
            Value::Number(num) => {
                println!("{num}");
            }
            Value::Nil => {
                println!("nil");
            }
        };

        Ok(value)
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    // Literal(Literal),
    // Grouping(Grouping),
    Unary(Unary<'a>),
    Binary(Binary<'a>),
    // Operator(Operator),
    // Equality(Equality),
    Primary(Primary<'a>),
}

impl Expr<'_> {
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

impl Evaluate for Expr<'_> {
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
pub struct Grouping<'a>(Box<Expr<'a>>);

impl Grouping<'_> {
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

        // FIXME: doesn't actually end parsing
        Err(Box::new(ScanError::new("Expected Closing Parenthesis")))
    }
}

impl Evaluate for Grouping<'_> {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        self.0.eval()
    }
}

#[derive(Debug)]
pub enum Binary<'a> {
    BinaryExpr(Box<Binary<'a>>, Operator, Box<Binary<'a>>),
    Unary(Unary<'a>),
    Primary(Primary<'a>),
}

impl Binary<'_> {
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

impl Evaluate for Binary<'_> {
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
                    // Nil is NOT falsy for binary "equal"/"not equal"
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
pub enum Unary<'a> {
    UnaryExpr(UnaryOp, Box<Unary<'a>>),
    Primary(Primary<'a>),
}

impl Unary<'_> {
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

impl Evaluate for Unary<'_> {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Unary::Primary(p) => p.eval()?,
            Unary::UnaryExpr(op, primary) => {
                let prim_val = primary.eval()?;
                match (op, prim_val) {
                    (UnaryOp::Bang, Value::Bool(b)) => Value::Bool(!b),
                    // Nil evaluated as falsy only for unary "NOT"
                    (UnaryOp::Bang, Value::Nil) => Value::Bool(true),
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
pub enum Primary<'a> {
    NUMBER(f64),
    STRING(String),
    Identifier(Identifier<'a>),
    True,
    False,
    Nil,
    Grouping(Grouping<'a>),
}

impl Primary<'_> {
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
            TokenType::Identifier(str) => {
                Primary::Identifier(Identifier::Unresolved(str.to_string()))
            }
            token => {
                let message = format!("Unexpected Token: {:?}", token);
                return Err(Box::new(ScanError::new(message)));
            }
        };

        Ok((primary, &tokens[1..]))
    }
}

impl Evaluate for Primary<'_> {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let val = match self {
            Primary::NUMBER(num) => Value::Number(*num),
            Primary::STRING(s) => Value::String(s.to_string()),
            Primary::True => Value::Bool(true),
            Primary::False => Value::Bool(false),
            Primary::Nil => Value::Nil,
            Primary::Identifier(i) => i.eval()?,
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

#[derive(Debug, Clone)]
pub enum Identifier<'a> {
    Unresolved(String),
    Resolved { name: String, env: &'a Environment },
}

impl Evaluate for Identifier<'_> {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Identifier::Unresolved(str) => {
                let message = format!("This Identifier was Never Resolved {}", str);
                return Err(Box::new(ScanError::new(message)));
            }
            Identifier::Resolved { name, env: scope } => {
                let values = scope.values.borrow_mut();
                let value = values.get(name).cloned();

                // if empty here not in scope
                // TODO: look in parent scope
                match value {
                    None => {
                        let message = format!("This Identifier Is not present, Should have been caught during semantic analysis {}", name);
                        return Err(Box::new(ScanError::new(message)));
                    }
                    Some(val) => match val {
                        None => {
                            let message = format!("This Identifier was never initialized, also should have been caught before this {}", name);
                            return Err(Box::new(ScanError::new(message)));
                        }
                        Some(val) => val,
                    },
                }
            }
        };

        Ok(value.clone())
    }
}
