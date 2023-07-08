use std::rc::Rc;

use crate::expr::Expr;

pub struct FunctionValue {
    pub args: Vec<String>,
    pub expr: Box<Expr>,
    pub clos: Option<Vec<(String, Value)>>
}

#[derive(Clone)]
pub enum Value {
    Integer(isize),
    Function(Rc<FunctionValue>),
    Native(fn(&[Value]) -> Value),
    Unit
}

impl Value {
    pub fn to_integer(&self) -> isize {
        match self {
            Self::Integer(int) => *int,
            _ => todo!("Error handling"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        match self {
            Integer(int) => write!(f, "{int}"),
            Function(_) => write!(f, "<function>"),
            Native(_) => write!(f, "<native function>"),
            Unit => write!(f, "()"),
        }
    }
}
