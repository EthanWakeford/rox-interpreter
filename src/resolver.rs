use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, error::Error, rc::Rc};

use crate::{grammar::*, scanner::ScanError};

fn resolve_identifier(
    identifier: &mut Identifier,
    env: Rc<RefCell<Environment>>,
) -> Result<(), Box<dyn Error>> {
    match identifier {
        Identifier::Unresolved(name) => {
            // Check to see if value has been declared to hashmap
            let value = env.borrow().get(name);

            match value {
                // If exists, resolve
                Some(_) => {
                    let name = name.to_string();
                    // Update identifier
                    *identifier = Identifier::Resolved {
                        name,
                        env: env.clone(),
                    };
                }
                // If not, Error
                None => {
                    let message = format!("Value referenced before initialization: {}", name,);
                    return Err(Box::new(ScanError::new(message)));
                }
            }
        }
        Identifier::Resolved { name, env: _ } => {
            let message = format!(
                "Somehow trying to resolved an already resolved value: {}",
                name
            );
            return Err(Box::new(ScanError::new(message)));
        }
    };

    Ok(())
}

fn resolve_var_decl(vd: &mut VarDecl, env: Rc<RefCell<Environment>>) -> Result<(), Box<dyn Error>> {
    let expr = &mut vd.1;
    resolve_expr(expr, env.clone())?;

    let iden = &mut vd.0;
    match iden {
        Identifier::Resolved { name, env: _ } => {
            let message = format!(
                "Somehow trying to resolved an already resolved value: {}",
                name
            );
            return Err(Box::new(ScanError::new(message)));
        }
        Identifier::Unresolved(name) => {
            // We dont' worry about the expression right now, only adding key to map
            env.borrow().declare(name);

            *iden = Identifier::Resolved {
                name: name.clone(),
                env: Rc::clone(&env),
            };
        }
    }
    Ok(())
}

fn resolve_assignment(
    assign: &mut Assignment,
    env: Rc<RefCell<Environment>>,
) -> Result<(), Box<dyn Error>> {
    let expr = &mut assign.1;
    resolve_expr(expr, env.clone())?;

    let iden = &mut assign.0;
    resolve_identifier(iden, env)?;

    Ok(())
}

fn resolve_expr(expr: &mut Expr, env: Rc<RefCell<Environment>>) -> Result<(), Box<dyn Error>> {
    match expr {
        Expr::Assignment(e) => resolve_assignment(e, env)?,
        Expr::Primary(p) => resolve_primary(p, env)?,
        Expr::Unary(u) => resolve_unary(u, env)?,
        Expr::Binary(b) => resolve_binary(b, env)?,
    }
    Ok(())
}

pub fn resolve_primary(
    primary: &mut Primary,
    env: Rc<RefCell<Environment>>,
) -> Result<(), Box<dyn Error>> {
    match primary {
        Primary::Identifier(ref mut iden) => {
            resolve_identifier(iden, env)?;
        }
        // TODO: do something when its a grouping
        Primary::Grouping(_g) => (),
        // Do nothing if anything else
        _ => (),
    };

    Ok(())
}

pub fn resolve_unary(
    unary: &mut Unary,
    env: Rc<RefCell<Environment>>,
) -> Result<(), Box<dyn Error>> {
    match unary {
        Unary::Primary(p) => resolve_primary(p, env)?,
        Unary::UnaryExpr(__, u) => resolve_unary(u, env)?,
    }

    Ok(())
}

pub fn resolve_binary(
    binary: &mut Binary,
    env: Rc<RefCell<Environment>>,
) -> Result<(), Box<dyn Error>> {
    match binary {
        Binary::Primary(p) => resolve_primary(p, env)?,
        Binary::Unary(u) => resolve_unary(u, env)?,
        Binary::BinaryExpr(left, _, right) => {
            resolve_binary(left, env.clone())?;
            resolve_binary(right, env.clone())?;
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>,
    pub values: RefCell<HashMap<String, Option<Value>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Environment {
        let variables: HashMap<String, Option<Value>> = HashMap::new();

        Environment {
            enclosing,
            values: RefCell::new(variables),
        }
    }

    fn next(&self) -> Option<Rc<RefCell<Environment>>> {
        self.enclosing.clone()
    }

    pub fn get(&self, name: &String) -> Option<Option<Value>> {
        if let Some(value) = self.values.borrow().get(name).cloned() {
            return Some(value);
        }

        // Search through enclosings
        match &self.enclosing {
            Some(enclosing) => enclosing.borrow().get(name),
            None => None,
        }
    }

    pub fn declare(&self, name: &String) {
        self.values.borrow_mut().insert(name.to_string(), None);
    }

    pub fn assign(&self, name: &String, value: Option<Value>) {
        self.values.borrow_mut().insert(name.to_string(), value);
    }
}

pub struct Scope {
    pub decls: Vec<Declaration>,
    pub env: Rc<RefCell<Environment>>,
}

impl Scope {
    pub fn new(mut decls: Vec<Declaration>) -> Result<Scope, Box<dyn Error>> {
        let global = Rc::new(RefCell::new(Environment::new(None)));

        // Resolve declarations
        for decl in decls.as_mut_slice() {
            match decl {
                Declaration::VarDecl(ref mut vd) => {
                    resolve_var_decl(vd, global.clone())?;
                }
                Declaration::Statement(ref mut stmt) => match stmt {
                    Statement::PrintStatement(ref mut pstmt) => {
                        let expr = &mut pstmt.0;
                        resolve_expr(expr, global.clone())?;
                    }
                    Statement::ExprStatement(ref mut estmt) => {
                        let expr = &mut estmt.0;
                        resolve_expr(expr, global.clone())?;
                    }
                },
            }
        }

        // Create scope after resolving declarations
        let scope = Scope { decls, env: global };

        Ok(scope)
    }
}

pub struct ResolvedAST {
    pub scope: Scope,
}

impl ResolvedAST {
    pub fn new(ast: AST) -> Result<ResolvedAST, Box<dyn Error>> {
        let decls = ast.decls;
        let scope = Scope::new(decls)?;

        Ok(ResolvedAST { scope })
    }
}
