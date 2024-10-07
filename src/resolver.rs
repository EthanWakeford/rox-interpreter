use std::{cell::RefCell, collections::HashMap, error::Error, rc::Rc};

use crate::{grammar::*, scanner::ScanError};

fn resolve_identifier(
    identifier: &mut Identifier,
    env: Rc<RefCell<Environment>>,
) -> Result<(), Box<dyn Error>> {
    match identifier {
        Identifier::Unresolved(name) => {
            // Check to see if value has been declared to hashmap
            let value = env.borrow().get(name)?;

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
            env.borrow().declare(name, None);

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
        Primary::Grouping(ref mut g) => {
            resolve_expr(&mut g.0, env.clone())?;
        }
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

fn resolve_stmt(stmt: &mut Statement, env: Rc<RefCell<Environment>>) -> Result<(), Box<dyn Error>> {
    match stmt {
        Statement::Block(b) => {
            // New Scope and Environment made
            let enclosed_env = Environment::new(Some(env.clone()));
            let enclosed_env = Rc::new(RefCell::new(enclosed_env));

            resolve_block(b, enclosed_env)?;
        }
        Statement::PrintStatement(ref mut pstmt) => {
            let expr = &mut pstmt.0;
            resolve_expr(expr, env.clone())?;
        }
        Statement::ExprStatement(ref mut estmt) => {
            let expr = &mut estmt.0;
            resolve_expr(expr, env.clone())?;
        }
        Statement::IfStatement(ref mut if_stmt) => {
            let expr = &mut if_stmt.0;
            resolve_expr(expr, env.clone())?;

            let stmt = &mut if_stmt.1;
            resolve_stmt(stmt, env.clone())?;

            match &mut if_stmt.2 {
                None => (),
                Some(stmt) => {
                    resolve_stmt(stmt, env.clone())?;
                }
            }
        }
        Statement::WhileStatement(ref mut w_stmt) => {
            let expr = &mut w_stmt.0;
            resolve_expr(expr, env.clone())?;

            let stmt = &mut w_stmt.1;
            resolve_stmt(stmt, env.clone())?;
        }
        Statement::ForStatement(ref mut f_stmt) => {
            // New Scope and Environment made
            let enclosed_env = Environment::new(Some(env.clone()));
            let enclosed_env = Rc::new(RefCell::new(enclosed_env));

            if let Some(init) = &mut f_stmt.initializer {
                match init {
                    ForStatementInitializer::Expr(expr) => {
                        resolve_expr(expr, enclosed_env.clone())?
                    }
                    ForStatementInitializer::VarDecl(vd) => {
                        resolve_var_decl(vd, enclosed_env.clone())?
                    }
                }
            }

            if let Some(condition) = &mut f_stmt.condition {
                resolve_expr(condition, enclosed_env.clone())?
            }

            if let Some(increment) = &mut f_stmt.increment {
                resolve_expr(increment, enclosed_env.clone())?
            }

            resolve_stmt(&mut f_stmt.body, enclosed_env.clone())?
        }
    }
    Ok(())
}

fn resolve_block(block: &mut Block, env: Rc<RefCell<Environment>>) -> Result<(), Box<dyn Error>> {
    // let mut decls = ast.decls;
    // let global = Rc::new(RefCell::new(Environment::new(None)));

    // Resolve declarations
    for decl in &mut block.0 {
        match decl {
            Declaration::VarDecl(ref mut vd) => {
                resolve_var_decl(vd, env.clone())?;
            }
            Declaration::Statement(ref mut stmt) => {
                resolve_stmt(stmt, env.clone())?;
            }
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

    /// Retrives variable value, reads through enclosing scopes
    pub fn get(&self, name: &String) -> Result<Option<Option<Value>>, Box<dyn Error>> {
        if let Some(value) = self.values.borrow().get(name).cloned() {
            return Ok(Some(value));
        }

        // Search through enclosings
        match &self.enclosing {
            Some(enclosing) => enclosing.borrow().get(name),

            None => {
                let message = format!("Attempting to Reference Variable");
                return Err(Box::new(ScanError::new(message)));
            }
        }
    }

    /// Creates new variable after "let" stmt in nearest scope
    ///
    /// Declares with  None(Value), instantiates with Some(Value)
    pub fn declare(&self, name: &String, value: Option<Value>) {
        self.values.borrow_mut().insert(name.to_string(), value);
    }

    /// Updates value to variable, can reach back in to previous scopes
    ///
    /// Cannot reassign no Value
    pub fn assign(&self, name: &String, value: Value) -> Result<(), Box<dyn Error>> {
        let mut values_map = self.values.borrow_mut();

        if values_map.contains_key(name) {
            values_map.insert(name.to_string(), Some(value.clone()));

            return Ok(());
        }

        // Not found so Search through enclosings
        match &self.enclosing {
            Some(enclosing) => enclosing.borrow().assign(name, value.clone()),
            None => Err(Box::new(ScanError::new(
                "Attempting to reassign Variable not in Scope",
            ))),
        }
    }
}

pub struct ResolvedAST {
    pub decls: Vec<Declaration>,
    pub env: Rc<RefCell<Environment>>,
}

impl ResolvedAST {
    pub fn new(ast: AST) -> Result<ResolvedAST, Box<dyn Error>> {
        let mut decls = ast.decls;
        let global = Rc::new(RefCell::new(Environment::new(None)));

        // Resolve declarations
        for decl in decls.as_mut_slice() {
            match decl {
                Declaration::VarDecl(ref mut vd) => {
                    resolve_var_decl(vd, global.clone())?;
                }
                Declaration::Statement(ref mut stmt) => {
                    resolve_stmt(stmt, global.clone())?;
                }
            }
        }

        let ast = ResolvedAST { decls, env: global };
        Ok(ast)
    }
}
