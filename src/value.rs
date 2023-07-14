use std::{rc::Rc, collections::HashMap, cell::RefCell};

use crate::expr::{Expr, Pattern};

pub type Module = Rc<RefCell<HashMap<String, Value>>>;
pub type List = Rc<Vec<Value>>;
pub type Native = fn(&[Value]) -> Value;
pub type Function = Rc<FunctionValue>; 
pub type Structure = Rc<RefCell<HashMap<String, Value>>>;

pub struct FunctionValue {
    pub args: Vec<Pattern>,
    pub expr: Box<Expr>,
    pub clos: Option<Vec<(String, Value)>>,
    pub modl: Module
}

#[derive(Clone)]
pub enum Value {
    Integer(isize),
    String(String),
    Bool(bool),
    Function(Function),
    Native(Native),
    Module(Module),
    List(List),
    Structure(Structure),
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
            _ => todo!("Error handling")
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        match self {
            Integer(int) => write!(f, "{int}"),
            String(string) => write!(f, "{string}"),
            Bool(bool) => write!(f, "{bool}"),
            Function(_) => write!(f, "<function>"),
            Native(_) => write!(f, "<native function>"),
            Module(_) => write!(f, "<module>"),
            List(list) => match &list[..] {
                [] => write!(f, "[]"),
                [x] => write!(f, "[{x}]"),
                [x, xs @ .., l] => {
                    write!(f, "[{x}")?;
                    for value in xs {
                        write!(f, ", {value}")?;
                    }
                    write!(f, ", {l}]")
                }
            }
            Structure(structure) => {
                let structure = structure.borrow();

                if structure.is_empty() {
                    return write!(f, "{{}}")
                }

                let mut first = true;

                write!(f, "{{")?;
                for (field_name, value) in structure.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ",")?;
                    }
                    
                    write!(f, " {field_name}: {value}")?;
                }
                write!(f, " }}")
            }
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
            (Structure(ls), Structure(rs)) => ls == rs,
            (Unit, Unit) => true,
            (Function(_), Function(_)) => false,
            (Native(_), Native(_)) => false,
            (Module(_), Module(_)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
} 