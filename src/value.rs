
use std::{rc::Rc, collections::HashMap, cell::RefCell};

use crate::expr::Expr;

pub type Module = Rc<RefCell<HashMap<String, Value>>>;

pub struct FunctionValue {
    pub args: Vec<String>,
    pub expr: Box<Expr>,
    pub clos: Option<Vec<(String, Value)>>,
    pub modl: Module
}

#[derive(Clone)]
pub enum Value {
    Integer(isize),
    Bool(bool),
    Function(Rc<FunctionValue>),
    Native(fn(&[Value]) -> Value),
    Module(Module),
    List(Rc<Vec<Value>>),
    Unit
}

impl Value {
    pub fn to_integer(&self) -> isize {
        match self {
            Self::Integer(int) => *int,
            _ => todo!("Error handling"),
        }
    }

    pub fn to_bool(&self) -> bool {
        match self {
            Self::Bool(bool) => *bool,
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
            Module(_) => write!(f, "<module>"),
            List(list) => {
                match &list[..] {
                    [] => writeln!(f, "[]"),
                    [x] => writeln!(f, "[{x}]"),
                    [x, xs @ .., l] => {
                        write!(f, "[{x}")?;
                        for value in xs {
                            write!(f, ", {value}")?;
                        }
                        write!(f, ", {l}]")
                    }
                }
            },
            Unit => write!(f, "()"),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (self, other) {
            (Integer(lint), Integer(rint)) => lint == rint,
            (Bool(lbool), Bool(rbool)) => lbool == rbool,
            (List(llist), List(rlist)) => llist == rlist,
            (Function(_), Function(_)) => false,
            (Native(_), Native(_)) => false,
            (Module(_), Module(_)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
} 