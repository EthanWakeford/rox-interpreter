use std::{cell::RefCell, error::Error, fmt::Debug, rc::Rc};

use crate::{
    resolver::Environment,
    scanner::{ScanError, Token, TokenType},
};

pub trait Callable: Debug {
    fn call(&self, args: Option<Vec<Value>>) -> Result<Value, Box<dyn Error>>;
    fn print(&self) -> String;
    fn check_arity(&self, args: &Option<Vec<Value>>) -> Result<(), Box<dyn Error>>;
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    signature: Option<Vec<Identifier>>,
    body: Statement,
}

impl Callable for Function {
    fn call(&self, args: Option<Vec<Value>>) -> Result<Value, Box<dyn Error>> {
        // Each call clones the function
        let function_call = self.clone();

        function_call.check_arity(&args)?;

        if let Some((args, signature)) = args.zip(function_call.signature.clone()) {
            for (arg_iden, arg_value) in signature.iter().zip(args.iter()) {
                match arg_iden {
                    Identifier::Unresolved(name) => {
                        let message = format!(
                            "Identifier: ({}) not resolved during call to function {}",
                            name,
                            function_call.print()
                        );
                        return Err(Box::new(ScanError::new(message)));
                    }
                    // Add arguments to call environment
                    Identifier::Resolved { name, env } => {
                        env.borrow_mut().declare(name, Some(arg_value.clone()));
                    }
                }
            }
        }

        function_call.body.eval()
    }

    fn print(&self) -> String {
        format!("fun {}()", self.name)
    }

    fn check_arity(&self, args: &Option<Vec<Value>>) -> Result<(), Box<dyn Error>> {
        let expected_arity = self
            .signature
            .as_ref()
            .map_or(0, |signature| signature.len());
        let passed_arity = args.as_ref().map_or(0, |args| args.len());

        if expected_arity != passed_arity {
            let message = format!(
                "Wrong amount of arguments for function {}, expected {}, Found {}",
                self.print(),
                expected_arity,
                passed_arity
            );
            Err(Box::new(ScanError::new(message)))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Function(Rc<RefCell<dyn Callable>>),
    Nil,
}

pub trait Evaluate {
    fn eval(&self) -> Result<Value, Box<dyn Error>>;
}

#[derive(Debug, Clone)]
pub struct AST {
    pub decls: Vec<Declaration>,
}

impl AST {
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

#[derive(Debug, Clone)]
pub enum Declaration {
    VarDecl(VarDecl),
    FunDecl(FunDecl),
    Statement(Statement),
}

impl Declaration {
    pub fn new(tokens: &[Token]) -> Result<(Declaration, &[Token]), Box<dyn Error>> {
        if let Some(token) = tokens.get(0) {
            match token.token_type {
                // Look for "let" keyword
                TokenType::Let => {
                    let (decl, rest_tokens) = VarDecl::new(&tokens[1..])?;
                    let decl = Declaration::VarDecl(decl);

                    return Ok((decl, rest_tokens));
                }
                TokenType::Fun => {
                    let (decl, rest_tokens) = FunDecl::new(&tokens[1..])?;
                    let decl = Declaration::FunDecl(decl);

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

impl Evaluate for Declaration {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Declaration::Statement(stmt) => stmt.eval()?,
            Declaration::VarDecl(vd) => vd.eval()?,
            Declaration::FunDecl(fd) => fd.eval()?,
        };

        Ok(value)
    }
}

// TODO: allow for declaration but not assignment
#[derive(Debug, Clone)]
pub struct VarDecl(pub Identifier, pub Expr);

impl VarDecl {
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

impl Evaluate for VarDecl {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let (name, env) = match &self.0 {
            Identifier::Unresolved(_name) => {
                return Err(Box::new(ScanError::new(
                    "Somehow identifier is not resolved here during var decl",
                )));
            }
            Identifier::Resolved { name, env } => (name, env),
        };

        let value = self.1.eval()?;

        // Nil values not set to none rn, just Value::nil
        env.borrow_mut().declare(name, Some(value));

        Ok(Value::Nil)
    }
}

#[derive(Debug, Clone)]
pub struct FunDecl(pub Identifier, pub Option<Vec<Identifier>>, pub Statement);

impl FunDecl {
    pub fn new(tokens: &[Token]) -> Result<(FunDecl, &[Token]), Box<dyn Error>> {
        let (iden, rest_tokens) = match tokens.get(..=1) {
            None => {
                return Err(Box::new(ScanError::new("Expected Function Declaration")));
            }
            Some([iden, open_paren]) => {
                match (iden.token_type.clone(), open_paren.token_type.clone()) {
                    (TokenType::Identifier(name), TokenType::LeftParen) => (name, &tokens[2..]),
                    _ => {
                        return Err(Box::new(ScanError::new("Invalid Function Declaration")));
                    }
                }
            }
            _ => {
                return Err(Box::new(ScanError::new("Invalid Function Declaration")));
            }
        };

        let mut signature: Vec<Identifier> = Vec::new();
        let mut rest_tokens = rest_tokens;

        // Does NOT need to be comma separated
        while let Some(token) = rest_tokens.get(0) {
            match &token.token_type {
                TokenType::RightParen => break,
                TokenType::Identifier(name) => {
                    if signature.len() >= 255 {
                        return Err(Box::new(ScanError::new(
                            "No more than 255 arguments to function allowed",
                        )));
                    }

                    let iden = Identifier::Unresolved(name.to_string());
                    signature.push(iden);
                    rest_tokens = &rest_tokens[1..];
                }
                _ => {
                    return Err(Box::new(ScanError::new("Invalid Function Declaration")));
                }
            }
        }
        rest_tokens = &rest_tokens[1..];

        let signature = match signature.len() {
            0 => None,
            _ => Some(signature),
        };

        let (stmt, rest_tokens) = Statement::new(rest_tokens)?;
        let iden = Identifier::Unresolved(iden);
        let fd = FunDecl(iden, signature, stmt);

        Ok((fd, rest_tokens))
    }
}

impl Evaluate for FunDecl {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let (name, env) = match &self.0 {
            Identifier::Unresolved(_name) => {
                return Err(Box::new(ScanError::new(
                    "Somehow identifier is not resolved here during var decl",
                )));
            }
            // This scope is the scope for the function identifier and not function body
            Identifier::Resolved { name, env } => (name, env),
        };

        let stmt = &self.2;
        let signature = &self.1;

        // Add identifier of function to enclosing scope
        let fun = Function {
            name: name.to_string(),
            signature: signature.clone(),
            body: stmt.clone(),
        };
        let fun = Rc::new(RefCell::new(fun));
        let value = Value::Function(fun);

        env.borrow_mut().declare(name, Some(value.clone()));

        Ok(value)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExprStatement(ExprStatement),
    PrintStatement(PrintStatement),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    ForStatement(ForStatement),
    Block(Block),
}

impl Statement {
    pub fn new(tokens: &[Token]) -> Result<(Statement, &[Token]), Box<dyn Error>> {
        if let Some(p) = tokens.get(0) {
            match p.token_type {
                TokenType::Print => {
                    let (stmt, rest_tokens) = PrintStatement::new(&tokens[1..])?;
                    let stmt = Statement::PrintStatement(stmt);
                    return Ok((stmt, rest_tokens));
                }
                TokenType::LeftBrace => {
                    let (stmt, rest_tokens) = Block::new(&tokens[1..])?;
                    let stmt = Statement::Block(stmt);
                    return Ok((stmt, rest_tokens));
                }
                TokenType::If => {
                    let (stmt, rest_tokens) = IfStatement::new(&tokens[1..])?;
                    let stmt = Statement::IfStatement(stmt);
                    return Ok((stmt, rest_tokens));
                }
                TokenType::While => {
                    let (stmt, rest_tokens) = WhileStatement::new(&tokens[1..])?;
                    let stmt = Statement::WhileStatement(stmt);
                    return Ok((stmt, rest_tokens));
                }
                TokenType::For => {
                    let (stmt, rest_tokens) = ForStatement::new(&tokens[1..])?;
                    let stmt = Statement::ForStatement(stmt);
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

impl Evaluate for Statement {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Self::ExprStatement(e) => e.eval()?,
            Self::PrintStatement(p) => p.eval()?,
            Self::IfStatement(i) => i.eval()?,
            Self::WhileStatement(w) => w.eval()?,
            Self::ForStatement(f) => f.eval()?,
            Self::Block(b) => b.eval()?,
        };

        Ok(value)
    }
}

#[derive(Debug, Clone)]
pub struct ExprStatement(pub Expr);

impl ExprStatement {
    pub fn new(tokens: &[Token]) -> Result<(ExprStatement, &[Token]), Box<dyn Error>> {
        let (expr, rest_tokens) = Expr::new(tokens)?;

        Ok((ExprStatement(expr), rest_tokens))
    }
}

impl Evaluate for ExprStatement {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = self.0.eval()?;

        Ok(value)
    }
}

#[derive(Debug, Clone)]
pub struct PrintStatement(pub Expr);

impl PrintStatement {
    pub fn new(tokens: &[Token]) -> Result<(PrintStatement, &[Token]), Box<dyn Error>> {
        let (expr, rest_tokens) = Expr::new(tokens)?;

        Ok((PrintStatement(expr), rest_tokens))
    }
}

impl Evaluate for PrintStatement {
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
            Value::Function(f) => {
                println!("{}", f.borrow().print());
            }
            Value::Nil => {
                println!("nil");
            }
        };

        Ok(value)
    }
}

#[derive(Debug, Clone)]
pub struct IfStatement(pub Expr, pub Box<Statement>, pub Option<Box<Statement>>);

impl IfStatement {
    pub fn new(tokens: &[Token]) -> Result<(IfStatement, &[Token]), Box<dyn Error>> {
        let (expr, rest_tokens) = Expr::new(tokens)?;
        let (stmt, rest_tokens) = Statement::new(rest_tokens)?;

        if let Some(token) = rest_tokens.get(0) {
            match token.token_type {
                TokenType::Else => {
                    let (else_stmt, rest_tokens) = Statement::new(&rest_tokens[1..])?;

                    let if_stmt = IfStatement(expr, Box::new(stmt), Some(Box::new(else_stmt)));
                    return Ok((if_stmt, rest_tokens));
                }
                _ => (),
            }
        }

        let if_stmt = IfStatement(expr, Box::new(stmt), None);
        Ok((if_stmt, rest_tokens))
    }
}

impl Evaluate for IfStatement {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let expr = self.0.eval()?;

        // Requires expression to resolve to boolean value
        // Or else throws runtime error
        match expr {
            Value::Bool(b) => {
                if b {
                    return Ok(self.1.eval()?);
                } else {
                    match &self.2 {
                        Some(else_stmt) => {
                            return Ok(else_stmt.eval()?);
                        }
                        None => {
                            // No else block to enter, nothing to eval(), keep it moving
                            return Ok(Value::Nil);
                        }
                    }
                }
            }
            _ => (),
        }
        Err(Box::new(ScanError::new("Expected a Boolean Value")))
    }
}

#[derive(Debug, Clone)]
pub struct WhileStatement(pub Expr, pub Box<Statement>);

impl WhileStatement {
    pub fn new(tokens: &[Token]) -> Result<(WhileStatement, &[Token]), Box<dyn Error>> {
        let (expr, rest_tokens) = Expr::new(tokens)?;

        let (stmt, rest_tokens) = Statement::new(rest_tokens)?;

        let wstmt = WhileStatement(expr, Box::new(stmt));

        Ok((wstmt, rest_tokens))
    }
}

impl Evaluate for WhileStatement {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        loop {
            let condition = self.0.eval()?;

            match condition {
                Value::Bool(b) => {
                    if b == true {
                        self.1.eval()?;
                    } else {
                        break;
                    }
                }
                _ => {
                    return Err(Box::new(ScanError::new(
                        "Expected a Boolean Value As The Condition",
                    )));
                }
            }
        }

        Ok(Value::Nil)
    }
}

#[derive(Debug, Clone)]
pub enum ForStatementInitializer {
    VarDecl(VarDecl),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub initializer: Option<ForStatementInitializer>,
    pub condition: Option<Expr>,
    pub increment: Option<Expr>,
    pub body: Box<Statement>,
}

impl ForStatement {
    pub fn new(tokens: &[Token]) -> Result<(ForStatement, &[Token]), Box<dyn Error>> {
        let rest_tokens = tokens;

        let (initializer, rest_tokens) = match rest_tokens.get(0) {
            None => {
                return Err(Box::new(ScanError::new("Expected a For Loop Definition")));
            }
            Some(token) => match token.token_type {
                TokenType::Let => {
                    let (vd, rest_tokens) = VarDecl::new(&rest_tokens[1..])?;
                    // Expects a comma at end of initializer
                    let rest_tokens = Self::expect_comma(rest_tokens)?;
                    (Some(ForStatementInitializer::VarDecl(vd)), rest_tokens)
                }
                TokenType::Comma => (None, &rest_tokens[1..]),
                _ => {
                    let (expr, rest_tokens) = Expr::new(rest_tokens)?;
                    let rest_tokens = Self::expect_comma(rest_tokens)?;
                    (Some(ForStatementInitializer::Expr(expr)), rest_tokens)
                }
            },
        };

        let (condition, rest_tokens) = match rest_tokens.get(0) {
            None => {
                return Err(Box::new(ScanError::new("Expected a For Loop Definition")));
            }
            Some(token) => match token.token_type {
                TokenType::Comma => (None, &rest_tokens[1..]),
                _ => {
                    let (expr, rest_tokens) = Expr::new(rest_tokens)?;
                    let rest_tokens = Self::expect_comma(rest_tokens)?;
                    (Some(expr), rest_tokens)
                }
            },
        };

        let (increment, rest_tokens) = match rest_tokens.get(0) {
            None => {
                return Err(Box::new(ScanError::new("Expected a For Loop Definition")));
            }
            Some(token) => match token.token_type {
                TokenType::Comma => (None, &rest_tokens[1..]),
                _ => {
                    let (expr, rest_tokens) = Expr::new(rest_tokens)?;
                    // No comma at end of increment expected if there is an increment
                    (Some(expr), rest_tokens)
                }
            },
        };

        let (body, rest_tokens) = Statement::new(rest_tokens)?;
        let stmt = ForStatement {
            initializer,
            condition,
            increment,
            body: Box::new(body),
        };

        Ok((stmt, rest_tokens))
    }

    fn expect_comma(tokens: &[Token]) -> Result<&[Token], Box<dyn Error>> {
        match tokens.get(0) {
            None => {
                return Err(Box::new(ScanError::new("Invalid For Loop Definition")));
            }
            Some(token) => match token.token_type {
                TokenType::Comma => Ok(&tokens[1..]),
                _ => Err(Box::new(ScanError::new(
                    "Expected a Comma in For Loop Definition",
                ))),
            },
        }
    }
}

impl Evaluate for ForStatement {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        if let Some(init) = &self.initializer {
            match init {
                ForStatementInitializer::Expr(e) => e.eval()?,
                ForStatementInitializer::VarDecl(vd) => vd.eval()?,
            };
        }

        loop {
            let condition = &self.condition;

            match condition {
                // No condition, always true
                // Why would you do this????
                None => (),
                Some(condition) => match condition.eval()? {
                    Value::Bool(b) => {
                        if b == true {
                            self.body.eval()?;
                        } else {
                            break;
                        }
                    }
                    _ => {
                        return Err(Box::new(ScanError::new("Expected a Boolean Value")));
                    }
                },
            }

            if let Some(increment) = &self.increment {
                increment.eval()?;
            }
        }

        Ok(Value::Nil)
    }
}

// Currently does not hold its own environment
// Environment is only owned by identifiers right now
#[derive(Debug, Clone)]
pub struct Block(pub Vec<Declaration>);

impl Block {
    pub fn new(mut tokens: &[Token]) -> Result<(Block, &[Token]), Box<dyn Error>> {
        let mut decls: Vec<Declaration> = Vec::new();

        while let Some(token) = tokens.get(0) {
            match token.token_type {
                TokenType::RightBrace => {
                    let block = Block(decls);

                    return Ok((block, &tokens[1..]));
                }
                _ => {
                    let (decl, rest_tokens) = Declaration::new(tokens)?;
                    decls.push(decl);
                    tokens = rest_tokens;
                }
            }
        }

        Err(Box::new(ScanError::new("Expected a Closing Curly Brace")))
    }
}

impl Evaluate for Block {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        // Returns whatever final expression
        let mut last_value = Value::Nil;

        for stmt in &self.0 {
            last_value = stmt.eval()?;
        }

        Ok(last_value)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assignment(Assignment),
    Binary(Binary),
    Unary(Unary),
    Call(Call),
    Primary(Primary),
}

impl Expr {
    pub fn new(tokens: &[Token]) -> Result<(Expr, &[Token]), Box<dyn Error>> {
        if let Some([identifier, assign_op]) = tokens.get(0..=1) {
            match (identifier.token_type.clone(), assign_op.token_type.clone()) {
                (TokenType::Identifier(_), TokenType::Equal) => {
                    let (assign, rest_tokens) = Assignment::new(tokens)?;

                    let expr = Expr::Assignment(assign);

                    return Ok((expr, rest_tokens));
                }
                _ => (),
            }
        }

        // All other expression types fallthrough to the other types

        let (binary, rest_tokens) = Binary::new(tokens)?;

        let expr = match binary {
            Binary::BinaryExpr(_, _, _) => Expr::Binary(binary),
            Binary::Unary(u) => Expr::Unary(u),
            Binary::Call(c) => Expr::Call(c),
            Binary::Primary(p) => Expr::Primary(p),
        };

        Ok((expr, rest_tokens))
    }
}

impl Evaluate for Expr {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Expr::Assignment(e) => e.eval()?,
            Expr::Binary(b) => b.eval()?,
            Expr::Unary(u) => u.eval()?,
            Expr::Call(c) => c.eval()?,
            Expr::Primary(p) => p.eval()?,
        };
        Ok(value)
    }
}

#[derive(Debug, Clone)]
pub struct Assignment(pub Identifier, pub Box<Expr>);

impl Assignment {
    pub fn new(tokens: &[Token]) -> Result<(Assignment, &[Token]), Box<dyn Error>> {
        if let Some([identifier, assign_op]) = tokens.get(0..=1) {
            match (identifier.token_type.clone(), assign_op.token_type.clone()) {
                (TokenType::Identifier(name), TokenType::Equal) => {
                    let (expr, rest_tokens) = Expr::new(&tokens[2..])?;
                    let id = Identifier::Unresolved(name);

                    let assign = Assignment(id, Box::new(expr));

                    return Ok((assign, rest_tokens));
                }
                _ => (),
            }
        }
        Err(Box::new(ScanError::new("Invalid Assignment")))
    }
}

impl Evaluate for Assignment {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let (name, env) = match &self.0 {
            Identifier::Unresolved(_name) => {
                return Err(Box::new(ScanError::new(
                    "Somehow identifier is not resolved here during var decl",
                )));
            }
            Identifier::Resolved { name, env } => (name, env),
        };

        // Currently no type checks happening
        // So the langauge is dynamically typed

        let val = self.1.eval()?;

        env.borrow_mut().assign(name, val);

        Ok(Value::Nil)
    }
}

// // Not sure about this one but we'll leave for nows
#[derive(Debug, Clone)]
pub struct Grouping(pub Box<Expr>);

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

        // FIXME: doesn't actually end parsing
        Err(Box::new(ScanError::new("Expected Closing Parenthesis")))
    }
}

impl Evaluate for Grouping {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        self.0.eval()
    }
}

#[derive(Debug, Clone)]
pub enum Binary {
    BinaryExpr(Box<Binary>, Operator, Box<Binary>),
    Unary(Unary),
    Call(Call),
    Primary(Primary),
}

impl Binary {
    pub fn new(tokens: &[Token]) -> Result<(Binary, &[Token]), Box<dyn Error>> {
        Binary::logic_or(tokens)
    }

    fn logic_or(tokens: &[Token]) -> Result<(Binary, &[Token]), Box<dyn Error>> {
        let (mut expr, mut sliced_tokens) = Binary::logic_and(tokens)?;

        while let Some(token) = sliced_tokens.get(0) {
            let operator = match token.token_type {
                TokenType::Or => Operator::Or,
                _ => break,
            };
            sliced_tokens = &sliced_tokens[1..]; // Consume the operator

            let (right, rest_tokens) = Binary::logic_and(sliced_tokens)?;
            expr = Binary::BinaryExpr(Box::new(expr), operator, Box::new(right));
            sliced_tokens = rest_tokens;
        }

        Ok((expr, sliced_tokens))
    }

    fn logic_and(tokens: &[Token]) -> Result<(Binary, &[Token]), Box<dyn Error>> {
        let (mut expr, mut sliced_tokens) = Binary::equality(tokens)?;

        while let Some(token) = sliced_tokens.get(0) {
            let operator = match token.token_type {
                TokenType::And => Operator::And,
                _ => break,
            };
            sliced_tokens = &sliced_tokens[1..]; // Consume the operator

            let (right, rest_tokens) = Binary::equality(sliced_tokens)?;
            expr = Binary::BinaryExpr(Box::new(expr), operator, Box::new(right));
            sliced_tokens = rest_tokens;
        }

        Ok((expr, sliced_tokens))
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
            Unary::UnaryExpr(_, _) => Binary::Unary(left),
            Unary::Call(c) => Binary::Call(c),
            Unary::Primary(p) => Binary::Primary(p),
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
                Unary::UnaryExpr(_, _) => Binary::Unary(right),
                Unary::Call(c) => Binary::Call(c),
                Unary::Primary(p) => Binary::Primary(p),
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
            Binary::Call(c) => c.eval()?,
            Binary::Unary(u) => u.eval()?,
            Binary::BinaryExpr(left, op, right) => {
                let left = left.eval()?;
                let right = right.eval()?;

                match op {
                    Operator::Or => match (left, right) {
                        (Value::Bool(boolright), Value::Bool(boolleft)) => {
                            Value::Bool(boolleft == true || boolright == true)
                        }
                        _ => Value::Bool(false),
                    },
                    Operator::And => match (left, right) {
                        (Value::Bool(boolright), Value::Bool(boolleft)) => {
                            Value::Bool(boolleft == true && boolright == true)
                        }
                        _ => Value::Bool(false),
                    },
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

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Minus,
    Bang,
}

#[derive(Debug, Clone)]
pub enum Unary {
    UnaryExpr(UnaryOp, Box<Unary>),
    Call(Call),
    Primary(Primary),
}

impl Unary {
    pub fn new(tokens: &[Token]) -> Result<(Unary, &[Token]), Box<dyn Error>> {
        let un_op = match tokens.get(0) {
            Some(op) => match op.token_type {
                TokenType::Minus => UnaryOp::Minus,
                TokenType::Bang => UnaryOp::Bang,
                _ => {
                    let (call, rest_tokens) = Call::new(tokens)?;

                    return Ok((Unary::Call(call), rest_tokens));
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
            Unary::Call(c) => c.eval()?,
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

#[derive(Debug, Clone)]
pub enum Call {
    Call(Identifier, Option<Vec<Expr>>),
    Primary(Primary),
}

impl Call {
    pub fn new(tokens: &[Token]) -> Result<(Call, &[Token]), Box<dyn Error>> {
        let (iden, rest_tokens) = match tokens.get(..=1) {
            None => {
                let (primary, rest_tokens) = Primary::new(tokens)?;
                let call = Call::Primary(primary);

                return Ok((call, rest_tokens));
            }
            Some([iden, open_paren]) => {
                match (iden.token_type.clone(), open_paren.token_type.clone()) {
                    (TokenType::Identifier(name), TokenType::LeftParen) => (name, &tokens[2..]),
                    _ => {
                        let (primary, rest_tokens) = Primary::new(tokens)?;
                        let call = Call::Primary(primary);

                        return Ok((call, rest_tokens));
                    }
                }
            }
            _ => {
                let (primary, rest_tokens) = Primary::new(tokens)?;
                let call = Call::Primary(primary);

                return Ok((call, rest_tokens));
            }
        };

        // parse through args until right paren
        let mut args_len: u8 = 0;
        let mut args: Vec<Expr> = Vec::new();
        let mut rest_tokens = rest_tokens;

        // Does NOT need to be comma separated
        while let Some(token) = rest_tokens.get(0) {
            match token.token_type {
                TokenType::RightParen => break,
                _ => {
                    if args_len == 255 {
                        return Err(Box::new(ScanError::new(
                            "No more than 255 arguments to function allowed",
                        )));
                    }

                    let (expr, temp_rest_tokens) = Expr::new(rest_tokens)?;
                    args.push(expr);
                    rest_tokens = temp_rest_tokens;
                    args_len += 1;
                }
            }
        }
        rest_tokens = &rest_tokens[1..];

        let iden = Identifier::Unresolved(iden);
        let args = match args.len() {
            0 => None,
            _ => Some(args),
        };
        let call = Call::Call(iden, args);

        Ok((call, rest_tokens))
    }
}

impl Evaluate for Call {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let (iden, args) = match self {
            Call::Primary(p) => {
                return p.eval();
            }
            Call::Call(iden, args) => (iden, args),
        };

        // Make sure value is resolved and in scope
        let value = match iden {
            Identifier::Unresolved(str) => {
                let message = format!("This Identifier was Never Resolved {}", str);
                return Err(Box::new(ScanError::new(message)));
            }
            Identifier::Resolved { name, env } => {
                let value = env.borrow().get(name);

                // if empty here not in scope
                match value {
                    None => {
                        let message = format!("This Identifier Is not present, Should have been caught during semantic analysis {}", name);
                        return Err(Box::new(ScanError::new(message)));
                    }
                    Some(val) => match val {
                        None => {
                            let message = format!("This Identifier inside call was never initialized, also should have been caught before this {}", name);
                            return Err(Box::new(ScanError::new(message)));
                        }
                        Some(val) => val.clone(),
                    },
                }
            }
        };

        match value {
            Value::Function(fun) => {
                let args = match args {
                    None => None,
                    Some(args) => {
                        let mut args_values: Vec<Value> = Vec::new();

                        for arg in args {
                            args_values.push(arg.eval()?);
                        }

                        Some(args_values)
                    }
                };
                let fun = fun.borrow();
                let value = fun.call(args)?;
                Ok(value)
            }
            _ => {
                let message = format!("Identifier: {:?} Not Callable", value);
                return Err(Box::new(ScanError::new(message)));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Primary {
    NUMBER(f64),
    STRING(String),
    Identifier(Identifier),
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

impl Evaluate for Primary {
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

#[derive(Debug, Clone)]
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
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum Identifier {
    Unresolved(String),
    Resolved {
        name: String,
        env: Rc<RefCell<Environment>>,
    },
}

impl Evaluate for Identifier {
    fn eval(&self) -> Result<Value, Box<dyn Error>> {
        let value = match self {
            Identifier::Unresolved(str) => {
                let message = format!("This Identifier was Never Resolved {}", str);
                return Err(Box::new(ScanError::new(message)));
            }
            Identifier::Resolved { name, env } => {
                let value = env.borrow().get(name);

                // if empty here not in scope
                match value {
                    None => {
                        let message = format!("This Identifier Is not present, Should have been caught during semantic analysis {}", name);
                        return Err(Box::new(ScanError::new(message)));
                    }
                    Some(val) => match val {
                        None => {
                            let message = format!("This Identifier: ({}) was never initialized, also should have been caught before this", name);
                            return Err(Box::new(ScanError::new(message)));
                        }
                        Some(val) => val.clone(),
                    },
                }
            }
        };

        Ok(value)
    }
}
