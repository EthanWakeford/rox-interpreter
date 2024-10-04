use std::{collections::HashMap, error::Error};

use crate::{grammar::*, scanner::ScanError};

// 1. Name resolution
// 2. Create Scopes/Environments

// What value need to be able to refer to their environment???
// a. Functions
// b. Variables
// Both of which are variants of "Primary" enum

// We return a new Program that has names resovled, and Environments created
pub fn analyze(ast: AST) -> Result<(), Box<dyn Error>> {
    let mut global = Environment::new();

    // Resolve
    for decl in ast.decls {
        match decl {
            Declaration::VarDecl(vd) => {
                let iden = vd.0;
                match iden {
                    // We can overwrite variables
                    Identifier::Resolved { name, scope } => {}
                    // Everything starts unresolved
                    Identifier::Unresolved(name) => {
                        // We dont' worry about the expression right now, only adding key to map
                        global.values.insert(name, None);
                    }
                }
            }
            Declaration::Statement(stmt) => {
                match stmt {
                    // Check if value exists here
                    Statement::PrintStatement(pstmt) => resolve_expr(pstmt.0, &global)?,

                    Statement::ExprStatement(estmt) => resolve_expr(estmt.0, &global)?,
                };
            }
        }
    }

    Ok(())
}

fn resolve_expr(expr: Expr, env: &Environment) -> Result<(), Box<dyn Error>> {
    match expr {
        Expr::Primary(p) => resolve_primary(p, env)?,
        Expr::Unary(u) => resolve_unary(u, env)?,
        Expr::Binary(b) => resolve_binary(b, env)?,
    }
    Ok(())
}

pub fn resolve_primary(primary: Primary, env: &Environment) -> Result<(), Box<dyn Error>> {
    match primary {
        Primary::Identifier(iden) => {
            match iden {
                Identifier::Unresolved(name) => {
                    let value = env.values.get(&name);
                    match value {
                        // If exists, resolve
                        Some(val) => {
                            let resolved = Identifier::Resolved { name, scope: env };
                            // TODO: return this here somewhere
                        }
                        // If not, Error
                        None => {
                            let message =
                                format!("Value referenced before initialization: {}", name,);
                            return Err(Box::new(ScanError::new(message)));
                        }
                    }
                }
                Identifier::Resolved { name, scope } => {
                    let message = format!(
                        "Somehow trying to resolved an already resolved value: {}",
                        name
                    );
                    return Err(Box::new(ScanError::new(message)));
                }
            }
        }
        // TODO: do something when its a grouping
        Primary::Grouping(g) => (),
        // Do nothing if anything else
        _ => (),
    };
    // TODO: hardcode return
    Ok(())
}

pub fn resolve_unary(unary: Unary, env: &Environment) -> Result<(), Box<dyn Error>> {
    match unary {
        Unary::Primary(p) => resolve_primary(p, env)?,
        Unary::UnaryExpr(__, u) => resolve_unary(*u, env)?,
    }

    Ok(())
}

pub fn resolve_binary(binary: Binary, env: &Environment) -> Result<(), Box<dyn Error>> {
    match binary {
        Binary::Primary(p) => resolve_primary(p, env)?,
        Binary::Unary(u) => resolve_unary(u, env)?,
        Binary::BinaryExpr(left, _, right) => {
            resolve_binary(*left, &env)?;
            resolve_binary(*right, &env)?;
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub nested: Vec<Box<Environment>>,
    pub values: HashMap<String, Option<Value>>,
}

impl Environment {
    pub fn new() -> Environment {
        let nested: Vec<Box<Environment>> = Vec::new();
        let variables: HashMap<String, Option<Value>> = HashMap::new();

        Environment {
            nested,
            values: variables,
        }
    }

    pub fn decl(&mut self, name: String) {
        self.values.insert(name, None);
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.values.insert(name, Some(value));
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.values.get(name) {
            None => None,
            Some(val) => val.clone(),
        }
    }
}

pub struct Scope<'a>(Declaration<'a>, Environment);

impl Scope<'_> {
    pub fn new(decl: Declaration, env: Environment) -> Result<Scope, Box<dyn Error>> {
        Ok(Scope(decl, env))

        // match stmt {
        //     Statement::ExprStatement()
        // }
    }
}

pub struct ResolvedAST<'a>(Scope<'a>);

impl ResolvedAST<'_> {
    pub fn new(scope: Scope) -> Result<ResolvedAST, Box<dyn Error>> {
        Ok(ResolvedAST(scope))
    }
}
