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
    Bool(bool),
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
            Bool(bool) => write!(f, "{bool}"),
            Function(_) => write!(f, "<function>"),
            Native(_) => write!(f, "<native function>"),
            Unit => write!(f, "()"),
        }
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (self, other) {
            (Integer(lint), Integer(rint)) => lint == rint,
            (Bool(lbool), Bool(rbool)) => lbool == rbool,
            (Function(_), Function(_)) => false,
            (Native(_), Native(_)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
} 