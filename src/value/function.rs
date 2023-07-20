use std::rc::Rc;

use crate::{expr::{Pattern, Expr}, span::Spanned};

use super::{Value, Module};

pub type Native = fn(&[Value]) -> Result<Value, String>;

#[derive(Clone)]
pub struct FunctionValue {
    pub args: Vec<Spanned<Pattern>>,
    pub expr: Box<Spanned<Expr>>,
    pub clos: Option<Vec<(String, Value)>>,
    pub modl: Module
}

#[derive(Clone)]
pub enum Function {
    Native(Native),
    Partial(Box<Function>, Box<Value>),
    Composed(Box<Function>, Box<Function>),
    Standart(Rc<FunctionValue>),        
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Function::*;

        match self {
            Native(_) => write!(f, "<native function>"),
            Partial(_, _) => write!(f, "<partial function>"),
            Composed(_, _) => write!(f, "<composed function>"),
            Standart(_) => write!(f, "<function>"),
        }
    }
}