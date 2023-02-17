mod tests;

use crate::ast::*;
use crate::object::*;

pub fn eval(program: &Program) -> Object {
    program.eval()
}
