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
            let env = scope.borrow().resolve_environment(name)?;
            let name = name.to_string();
            // Update identifier
            *identifier = Identifier::Resolved { name, env };
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

            let signature = &mut fd.1;
            let body = &mut fd.2;

            let body_scope = Scope::new(Some(scope));
            let body_scope = Rc::new(RefCell::new(body_scope));

            // add signature to body scope
            if let Some(signature) = signature {
                for iden in signature.iter_mut() {
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

                            let env = body_scope.borrow().get_env_clone();
                            env.borrow_mut().declare(name, None);

                            *iden = Identifier::Resolved {
                                name: name.clone(),
                                env,
                            };
                        }
                    }
                }
            }

            // resolve body
            resolve_stmt(body, body_scope)?;
        }
    }

    Ok(())
}

fn resolve_assignment(
    assign: &mut Assignment,
    scope: Rc<RefCell<Scope>>,
) -> Result<(), Box<dyn Error>> {
    let expr = &mut assign.1;
    resolve_expr(expr, scope.clone())?;

    let iden = &mut assign.0;
    resolve_identifier(iden, scope)?;

    Ok(())
}

fn resolve_expr(expr: &mut Expr, scope: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    match expr {
        Expr::Assignment(e) => resolve_assignment(e, scope)?,
        Expr::Binary(b) => resolve_binary(b, scope)?,
        Expr::Unary(u) => resolve_unary(u, scope)?,
        Expr::Call(c) => resolve_call(c, scope)?,
        Expr::Primary(p) => resolve_primary(p, scope)?,
    }
    Ok(())
}

fn resolve_call(call: &mut Call, scope: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    match call {
        Call::Primary(p) => resolve_primary(p, scope)?,
        Call::Call(iden, args) => {
            resolve_identifier(iden, scope.clone())?;

            if let Some(args) = args {
                for expr in args.as_mut_slice() {
                    resolve_expr(expr, scope.clone())?;
                }
            }
        }
    }
    Ok(())
}

pub fn resolve_primary(
    primary: &mut Primary,
    scope: Rc<RefCell<Scope>>,
) -> Result<(), Box<dyn Error>> {
    match primary {
        Primary::Identifier(ref mut iden) => {
            resolve_identifier(iden, scope)?;
        }
        Primary::Grouping(ref mut g) => {
            resolve_expr(&mut g.0, scope.clone())?;
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
    scope: Rc<RefCell<Scope>>,
) -> Result<(), Box<dyn Error>> {
    match stmt {
        Statement::Block(b) => {
            // New Scope and Environment made
            let enclosed_scope = Scope::new(Some(scope.clone()));
            let enclosed_env = Rc::new(RefCell::new(enclosed_scope));

            resolve_block(b, enclosed_env)?;
        }
        Statement::PrintStatement(ref mut pstmt) => {
            let expr = &mut pstmt.0;
            resolve_expr(expr, scope.clone())?;
        }
        Statement::ExprStatement(ref mut estmt) => {
            let expr = &mut estmt.0;
            resolve_expr(expr, scope.clone())?;
        }
        Statement::IfStatement(ref mut if_stmt) => {
            let expr = &mut if_stmt.0;
            resolve_expr(expr, scope.clone())?;

            let stmt = &mut if_stmt.1;
            resolve_stmt(stmt, scope.clone())?;

            match &mut if_stmt.2 {
                None => (),
                Some(stmt) => {
                    resolve_stmt(stmt, scope.clone())?;
                }
            }
        }
        Statement::WhileStatement(ref mut w_stmt) => {
            let expr = &mut w_stmt.0;
            resolve_expr(expr, scope.clone())?;

            let stmt = &mut w_stmt.1;
            resolve_stmt(stmt, scope.clone())?;
        }
        Statement::ForStatement(ref mut f_stmt) => {
            // New Scope and Environment made
            let enclosed_env = Scope::new(Some(scope.clone()));
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

fn resolve_block(block: &mut Block, scope: Rc<RefCell<Scope>>) -> Result<(), Box<dyn Error>> {
    // Resolve declarations
    for decl in &mut block.0 {
        match decl {
            Declaration::VarDecl(ref mut vd) => {
                resolve_var_decl(vd, scope.clone())?;
            }
            Declaration::FunDecl(ref mut fd) => {
                resolve_fun_decl(fd, scope.clone())?;
            }
            Declaration::Statement(ref mut stmt) => {
                resolve_stmt(stmt, scope.clone())?;
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

        let get_time_native_func = Rc::new(RefCell::new(GetTime()));

        let global_env = global.borrow().get_env_clone();
        global_env.borrow_mut().declare(
            &"getTime".to_string(),
            Some(Value::Function(get_time_native_func)),
        );

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
    fn call(&self, _args: Option<Vec<Value>>) -> Result<Value, Box<dyn Error>> {
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
