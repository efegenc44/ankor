pub mod integer;
pub mod range;

use std::{rc::Rc, collections::HashMap, cell::RefCell};

use crate::{expr::{Expr, Pattern}, span::Spanned};

use self::{integer::Integer, range::Range};

pub type List = Rc<RefCell<Vec<Value>>>;
pub type Native = fn(&[Value]) -> Result<Value, String>;
pub type Function = Rc<FunctionValue>; 
pub type Structure = Rc<RefCell<HashMap<String, Value>>>;
pub type Float = f64;

#[derive(Clone)]
pub struct ModuleValue {
    pub source: String,
    pub map: Rc<RefCell<HashMap<String, Value>>>
}

pub struct FunctionValue {
    pub args: Vec<Spanned<Pattern>>,
    pub expr: Box<Spanned<Expr>>,
    pub clos: Option<Vec<(String, Value)>>,
    pub modl: ModuleValue
}

#[derive(Clone)]
pub enum Value {
    Integer(Integer),
    Float(Float),
    String(String),
    Bool(bool),
    Function(Function),
    ComposedFunctions(Box<Value>, Box<Value>),
    ParitalFunction(Box<Value>, Box<Value>),
    Native(Native),
    Module(ModuleValue),
    List(List),
    Structure(Structure),
    Range(Range),
    Unit
}

impl Value {
    pub fn as_integer(&self) -> Result<Integer, String> {
        Ok(match self {
            Self::Integer(int) => int.clone(),
            // TOOD: Report type here
            _ => return Err("Expected `Integer`".to_string())
        })
    }

    pub fn as_bool(&self) -> Result<bool, String> {
        Ok(match self {
            Self::Bool(bool) => *bool,
            // TOOD: Report type here
            _ => return Err("Expected `Bool`".to_string())
        })
    }

    pub fn as_list(&self) -> Result<List, String> {
        Ok(match self {
            Self::List(list) => list.clone(),
            // TOOD: Report type here
            _ => return Err("Expected `List`".to_string())
        })
    }

    pub fn as_str(&self) -> Result<&str, String> {
        Ok(match self {
            Self::String(string) => string,
            // TOOD: Report type here
            _ => return Err("Expected `String`".to_string())
        })
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        match self {
            Integer(int) => write!(f, "{int}"),
            Float(float) => write!(f, "{float}"),
            String(string) => write!(f, "{string}"),
            Bool(bool) => write!(f, "{bool}"),
            Function(_) => write!(f, "<function>"),
            ComposedFunctions(..) => write!(f, "<function>"),
            ParitalFunction(..) => write!(f, "<function>"),
            Native(_) => write!(f, "<native function>"),
            Module(_) => write!(f, "<module>"),
            List(list) => match &list.borrow()[..] {
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
            Range(range) => write!(f, "{range}"),
            Unit => write!(f, "()"),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::cmp::Eq for Value {}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (self, other) {
            (Integer(lint), Integer(rint)) => lint == rint,
            (Float(lfloat), Float(rfloat)) => lfloat == rfloat,
            (String(lstring), String(rstring)) => lstring == rstring,
            (Bool(lbool), Bool(rbool)) => lbool == rbool,
            (List(llist), List(rlist)) => llist == rlist,
            (Structure(ls), Structure(rs)) => ls == rs,
            (Range(lrange), Range(rrange)) => lrange == rrange,
            (Unit, Unit) => true,
            (Function(_), Function(_)) => false,
            (ComposedFunctions(..), ComposedFunctions(..)) => false,
            (ParitalFunction(..), ParitalFunction(..)) => false,
            (Native(_), Native(_)) => false,
            (Module(_), Module(_)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Value::*;

        Some(match (self, other) {
            (Integer(lint), Integer(rint)) => lint.cmp(rint),
            (Float(lfloat), Float(rfloat)) => lfloat.total_cmp(rfloat),
            _ => return None
        })
    }
}

impl std::ops::Add for &Value {
    type Output = Option<Value>;

    fn add(self, rhs: Self) -> Self::Output {
        use Value::*;

        Some(match (self, rhs) {
            (Integer(lint), Integer(rint)) => Integer(lint + rint),
            (Float(lfloat), Float(rfloat)) => Float(lfloat + rfloat),
            _ => return None 
        })
    }
}

impl std::ops::Sub for &Value {
    type Output = Option<Value>;

    fn sub(self, rhs: Self) -> Self::Output {
        use Value::*;

        Some(match (self, rhs) {
            (Integer(lint), Integer(rint)) => Integer(lint - rint),
            (Float(lfloat), Float(rfloat)) => Float(lfloat - rfloat),
            _ => return None
        })
    }
}

impl std::ops::Mul for &Value {
    type Output = Option<Value>;

    fn mul(self, rhs: Self) -> Self::Output {
        use Value::*;

        Some(match (self, rhs) {
            (Integer(lint), Integer(rint)) => Integer(lint * rint),
            (Float(lfloat), Float(rfloat)) => Float(lfloat * rfloat),
            _ => return None
        })
    }
}

impl std::ops::Div for &Value {
    type Output = Option<Value>;

    fn div(self, rhs: Self) -> Self::Output {
        use Value::*;

        Some(match (self, rhs) {
            (Integer(lint), Integer(rint)) => Integer(lint / rint),
            (Float(lfloat), Float(rfloat)) => Float(lfloat / rfloat),
            _ => return None
        })
    }
}

impl std::ops::BitAnd for &Value {
    type Output = Option<Value>;

    fn bitand(self, rhs: Self) -> Self::Output {
        use Value::*;

        Some(match (self, rhs) {
            (Bool(lbool), Bool(rbool)) => Bool(lbool & rbool),
            _ => return None
        })
    }
}

impl std::ops::BitOr for &Value {
    type Output = Option<Value>;

    fn bitor(self, rhs: Self) -> Self::Output {
        use Value::*;

        Some(match (self, rhs) {
            (Bool(lbool), Bool(rbool)) => Bool(lbool | rbool),
            _ => return None
        })
    }
}

impl std::ops::Neg for &Value {
    type Output = Option<Value>;

    fn neg(self) -> Self::Output {
        use Value::*;

        Some(match self {
            Integer(int) => Integer(-int),
            Float(float) => Float(-float),
            _ => return None
        })
    }
}

impl std::ops::Not for &Value {
    type Output = Option<Value>;

    fn not(self) -> Self::Output {
        use Value::*;

        Some(match self {
            Bool(bool) => Bool(!bool),
            _ => return None
        })
    }
}
