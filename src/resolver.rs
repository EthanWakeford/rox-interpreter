use std::error::Error;

use crate::grammar::Program;

// 1. Name resolution
// 2. Create Scopes/Environments

// What value need to be able to refer to their environment???
// a. Functions
// b. Variables
// Both of which are variants of "Primary" enum

// We return a new Program that has names resovled, and Environments created
// pub fn analyze(program: Program) -> Result<Program, Box<dyn Error>> {}
