use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{grammar::*, scanner::ScanError};

pub(crate) fn resolve_identifier(
    identifier: &mut Identifier,
    scope: Rc<RefCell<Scope>>,
) -> Result<(), Box<dyn Error>> {
    match identifier {
        Identifier::Unresolved(name) => {
            // Check to see if value has been declared to hashmap
            let env = scope.borrow().get_env_clone();
            let resolved = env.borrow().contains_key(name);
            dbg!(scope.clone());
            match resolved {
                // If exists, resolve
                true => {
                    let name = name.to_string();
                    // Update identifier
                    *identifier = Identifier::Resolved { name, env };
                }
                // If not, Error
                false => {
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

fn resolve_var_decl(vd: &mut VarDecl, scope: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    let expr = &mut vd.1;
    resolve_expr(expr, scope.clone())?;

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

            let env = scope.borrow().get_env_clone();
            env.borrow_mut().declare(name, None);

            *iden = Identifier::Resolved {
                name: name.clone(),
                env,
            };
        }
    }
    Ok(())
}

fn resolve_fun_decl(fd: &mut FunDecl, scope: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    let iden = &mut fd.0;

    match iden {
        Identifier::Resolved { name, .. } => {
            let message = format!(
                "Somehow trying to resolved an already resolved value: {}",
                name
            );
            return Err(Box::new(ScanError::new(message)));
        }
        Identifier::Unresolved(name) => {
            // func iden is now owned by scope its defined in

            let env = scope.borrow().get_env_clone();
            env.borrow_mut().declare(name, None);

            *iden = Identifier::Resolved {
                name: name.clone(),
                env,
            };
        }
    }

    // Name resolution for function body has to happen at runtime now because of how I built my code

    // // Create new env for function body
    // let enclosed_env = Scope::new(Some(scope));
    // let enclosed_env = Rc::new(RefCell::new(enclosed_env));

    // // Resolve all args into function body env if any
    // if let Some(signature) = &mut fd.1 {
    //     let _ = signature
    //         .iter_mut()
    //         .map(|arg| resolve_identifier(arg, enclosed_env.clone()));
    // }

    // let stmt = &mut fd.2;
    // resolve_stmt(stmt, enclosed_env.clone())?;

    Ok(())
}

fn resolve_assignment(
    assign: &mut Assignment,
    env: Rc<RefCell<Scope>>,
) -> Result<(), Box<dyn Error>> {
    let expr = &mut assign.1;
    resolve_expr(expr, env.clone())?;

    let iden = &mut assign.0;
    resolve_identifier(iden, env)?;

    Ok(())
}

fn resolve_expr(expr: &mut Expr, env: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    match expr {
        Expr::Assignment(e) => resolve_assignment(e, env)?,
        Expr::Binary(b) => resolve_binary(b, env)?,
        Expr::Unary(u) => resolve_unary(u, env)?,
        Expr::Call(c) => resolve_call(c, env)?,
        Expr::Primary(p) => resolve_primary(p, env)?,
    }
    Ok(())
}

fn resolve_call(call: &mut Call, env: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    match call {
        Call::Primary(p) => resolve_primary(p, env)?,
        Call::Call(iden, args) => {
            resolve_identifier(iden, env.clone())?;

            if let Some(args) = args {
                for expr in args.as_mut_slice() {
                    resolve_expr(expr, env.clone())?;
                }
            }
        }
    }
    Ok(())
}

pub fn resolve_primary(
    primary: &mut Primary,
    env: Rc<RefCell<Scope>>,
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

pub fn resolve_unary(unary: &mut Unary, env: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    match unary {
        Unary::Primary(p) => resolve_primary(p, env)?,
        Unary::Call(c) => resolve_call(c, env)?,
        Unary::UnaryExpr(__, u) => resolve_unary(u, env)?,
    }

    Ok(())
}

pub fn resolve_binary(binary: &mut Binary, env: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    match binary {
        Binary::Primary(p) => resolve_primary(p, env)?,
        Binary::Call(c) => resolve_call(c, env)?,
        Binary::Unary(u) => resolve_unary(u, env)?,
        Binary::BinaryExpr(left, _, right) => {
            resolve_binary(left, env.clone())?;
            resolve_binary(right, env.clone())?;
        }
    }

    Ok(())
}

pub(crate) fn resolve_stmt(
    stmt: &mut Statement,
    env: Rc<RefCell<Scope>>,
) -> Result<(), Box<dyn Error>> {
    match stmt {
        Statement::Block(b) => {
            // New Scope and Environment made
            let enclosed_env = Scope::new(Some(env.clone()));
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
            let enclosed_env = Scope::new(Some(env.clone()));
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

fn resolve_block(block: &mut Block, env: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    // let mut decls = ast.decls;
    // let global = Rc::new(RefCell::new(Environment::new(None)));

    // Resolve declarations
    for decl in &mut block.0 {
        match decl {
            Declaration::VarDecl(ref mut vd) => {
                resolve_var_decl(vd, env.clone())?;
            }
            Declaration::FunDecl(ref mut fd) => {
                resolve_fun_decl(fd, env.clone())?;
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
    values: HashMap<String, Option<Value>>,
}

// Environment can only look at itself
impl Environment {
    /// Retrives variable value, reads through enclosing scopes
    pub fn get(&self, name: &String) -> Option<Option<Value>> {
        // Runtime only, identifier should be resolved already to correct env
        self.values.get(name).cloned()

        // if let Some(value) = self.values.get(name).cloned() {
        //     return Ok(Some(value));
        // }

        // // Search through enclosings
        // match &self.enclosing {
        //     Some(enclosing) => enclosing.borrow().get(name),

        //     None => {
        //         let message = format!("Attempting to Reference Variable");
        //         return Err(Box::new(ScanError::new(message)));
        //     }
        // }
    }

    /// Creates new variable after "let" stmt in nearest scope
    ///
    /// Declares with  None(Value), instantiates with Some(Value)
    pub fn declare(&mut self, name: &String, value: Option<Value>) {
        self.values.insert(name.to_string(), value);
    }

    /// Updates value to variable, can reach back in to previous scopes
    ///
    /// Cannot reassign no Value
    /// Should Not be able to fail as semantic analysis made sure of it
    pub fn assign(&mut self, name: &String, value: Value) -> () {
        self.values.insert(name.to_string(), Some(value));
    }

    /// Checks if contains key
    pub fn contains_key(&self, name: &String) -> bool {
        self.values.contains_key(name)
    }

    // Creates a new independent copy of the values hashmop for closures/function scopes NOT a copy to the same environment
    pub fn deep_copy(&self) -> Environment {
        Environment {
            values: self.values.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub enclosing: Option<Rc<RefCell<Scope>>>,
    pub environment: Rc<RefCell<Environment>>,
}

// Scope can look through enclosing scopes
impl Scope {
    pub fn new(enclosing: Option<Rc<RefCell<Scope>>>) -> Scope {
        let global_env: HashMap<String, Option<Value>> = HashMap::new();
        let global_env = Environment { values: global_env };

        Scope {
            enclosing,
            environment: Rc::new(RefCell::new(global_env)),
        }
    }

    /// Retrieves clone of wrapped environment
    pub fn get_env_clone(&self) -> Rc<RefCell<Environment>> {
        self.environment.clone()
    }

    /// Adds variable to this scopes env
    pub fn declare_to_env(&self, name: &String) {
        self.environment.borrow_mut().declare(name, None);
    }

    /// Given an identifier name it returns the environment that the identifier belongs to
    ///
    /// Returns error if not found in any scope
    ///
    pub fn resolve_environment(
        &self,
        name: &String,
    ) -> Result<Rc<RefCell<Environment>>, Box<dyn Error>> {
        let env = self.environment.clone();

        if env.borrow().contains_key(name) {
            return Ok(env);
        }

        // Not found so Search through enclosings
        match &self.enclosing {
            Some(enclosing) => enclosing.borrow().resolve_environment(name),
            None => {
                let message = format!("Reference to variable not in scope: {}", name);
                Err(Box::new(ScanError::new(message)))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedAST {
    pub decls: Vec<Declaration>,
    pub env: Rc<RefCell<Scope>>,
}

impl ResolvedAST {
    pub fn new(ast: AST) -> Result<ResolvedAST, Box<dyn Error>> {
        let mut decls = ast.decls;
        let global = Rc::new(RefCell::new(Scope::new(None)));

        // Resolve declarations
        for decl in decls.as_mut_slice() {
            match decl {
                Declaration::VarDecl(ref mut vd) => {
                    resolve_var_decl(vd, global.clone())?;
                }
                Declaration::FunDecl(ref mut fd) => {
                    resolve_fun_decl(fd, global.clone())?;
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

// Native function declarations

#[derive(Debug, Clone)]
pub struct GetTime();

impl Callable for GetTime {
    fn call(&mut self, _args: Option<Vec<Value>>) -> Result<Value, Box<dyn Error>> {
        let time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_secs_f64();

        Ok(Value::Number(time))
    }
    fn print(&self) -> String {
        format!("fun GetTime()")
    }
    fn check_arity(&self, args: &Option<Vec<Value>>) -> Result<(), Box<dyn Error>> {
        if !args.is_none() {
            let message = format!(
                "Wrong amount of arguments for function {}, expected 0",
                self.print(),
            );
            return Err(Box::new(ScanError::new(message)));
        };

        Ok(())
    }
}
