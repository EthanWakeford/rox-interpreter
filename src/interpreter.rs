use crate::grammar::{Evaluate, Expr};

pub fn interpret(expr: Expr) {
    let val = expr.eval().unwrap();
}
