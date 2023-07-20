pub mod integer;
pub mod range;
pub mod function;

use std::{rc::Rc, collections::HashMap, cell::RefCell};

use self::{integer::Integer, range::Range, function::Function};

pub type List = Rc<RefCell<Vec<Value>>>;
pub type Structure = Rc<RefCell<HashMap<String, Value>>>;
pub type Float = f64;

#[derive(Clone)]
pub struct Module {
    pub source: String,
    pub map: Rc<RefCell<HashMap<String, Value>>>
}

#[derive(Clone)]
pub enum Value {
    Integer(Integer),
    Float(Float),
    String(String),
    Bool(bool),
    Function(Function),
    Module(Module),
    List(List),
    Structure(Structure),
    Range(Range),
    Unit
}

impl Value {
    pub fn as_function(&self) -> Result<Function, String> {
        Ok(match self {
            Self::Function(func) => func.clone(),
            _ => return Err(format!("Expected `Function` instead found `{}`", self.type_name()))
        })
    }

    pub fn as_integer(&self) -> Result<Integer, String> {
        Ok(match self {
            Self::Integer(int) => int.clone(),
            _ => return Err(format!("Expected `Integer` instead found `{}`", self.type_name()))
        })
    }

    pub fn as_bool(&self) -> Result<bool, String> {
        Ok(match self {
            Self::Bool(bool) => *bool,
            _ => return Err(format!("Expected `Bool` instead found `{}`", self.type_name()))
        })
    }

    pub fn as_list(&self) -> Result<List, String> {
        Ok(match self {
            Self::List(list) => list.clone(),
            _ => return Err(format!("Expected `List` instead found `{}`", self.type_name()))
        })
    }

    pub fn as_str(&self) -> Result<&str, String> {
        Ok(match self {
            Self::String(string) => string,
            _ => return Err(format!("Expected `String` instead found `{}`", self.type_name()))
        })
    }

    pub fn type_name(&self) -> &str {
        use Value::*;

        match self {
            Integer(_) => "Integer",
            Float(_) => "Float",
            String(_) => "String",
            Bool(_) => "Bool",
            Function(_) => "Function",
            Module(_) => "Module",
            List(_) => "List",
            Structure(_) => "Structure",
            Range(_) => "Range",
            Unit => "Unit",
        }
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
            Function(func) => write!(f, "{func}"),
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
    type Output = Result<Value, String>;

    fn add(self, rhs: Self) -> Self::Output {
        use Value::*;

        Ok(match (self, rhs) {
            (Integer(lint), Integer(rint)) => Integer(lint + rint),
            (Float(lfloat), Float(rfloat)) => Float(lfloat + rfloat),
            _ => return Err(format!("Type Error at Binary Operation +: {}, {}", self.type_name(), rhs.type_name()))
        })
    }
}

impl std::ops::Sub for &Value {
    type Output = Result<Value, String>;

    fn sub(self, rhs: Self) -> Self::Output {
        use Value::*;

        Ok(match (self, rhs) {
            (Integer(lint), Integer(rint)) => Integer(lint - rint),
            (Float(lfloat), Float(rfloat)) => Float(lfloat - rfloat),
            _ => return Err(format!("Type Error at Binary Operation -: {}, {}", self.type_name(), rhs.type_name()))
        })
    }
}

impl std::ops::Mul for &Value {
    type Output = Result<Value, String>;

    fn mul(self, rhs: Self) -> Self::Output {
        use Value::*;

        Ok(match (self, rhs) {
            (Integer(lint), Integer(rint)) => Integer(lint * rint),
            (Float(lfloat), Float(rfloat)) => Float(lfloat * rfloat),
            _ => return Err(format!("Type Error at Binary Operation *: {}, {}", self.type_name(), rhs.type_name()))
        })
    }
}

impl std::ops::Div for &Value {
    type Output = Result<Value, String>;

    fn div(self, rhs: Self) -> Self::Output {
        use Value::*;

        Ok(match (self, rhs) {
            (Integer(lint), Integer(rint)) => {
                if rint == &integer::Integer::Small(0) {
                    return Err("Attempt to Divide by Zero".to_string())
                }
                Integer(lint / rint)
            },
            (Float(lfloat), Float(rfloat)) => {
                if rfloat == &0. {
                    return Err("Attempt to Divide by Zero".to_string())
                }
                Float(lfloat / rfloat)
            },
            _ => return Err(format!("Type Error at Binary Operation /: {}, {}", self.type_name(), rhs.type_name()))
        })
    }
}

impl std::ops::BitAnd for &Value {
    type Output = Result<Value, String>;

    fn bitand(self, rhs: Self) -> Self::Output {
        use Value::*;

        Ok(match (self, rhs) {
            (Bool(lbool), Bool(rbool)) => Bool(lbool & rbool),
            _ => return Err(format!("Type Error at Binary Operation and: {}, {}", self.type_name(), rhs.type_name()))
        })
    }
}

impl std::ops::BitOr for &Value {
    type Output = Result<Value, String>;

    fn bitor(self, rhs: Self) -> Self::Output {
        use Value::*;

        Ok(match (self, rhs) {
            (Bool(lbool), Bool(rbool)) => Bool(lbool | rbool),
            _ => return Err(format!("Type Error at Binary Operation or: {}, {}", self.type_name(), rhs.type_name()))
        })
    }
}

impl std::ops::Neg for &Value {
    type Output = Result<Value, String>;

    fn neg(self) -> Self::Output {
        use Value::*;

        Ok(match self {
            Integer(int) => Integer(-int),
            Float(float) => Float(-float),
            _ => return Err(format!("Type Error at Unary Operation -: {}", self.type_name()))
        })
    }
}

impl std::ops::Not for &Value {
    type Output = Result<Value, String>;

    fn not(self) -> Self::Output {
        use Value::*;

        Ok(match self {
            Bool(bool) => Bool(!bool),
            _ => return Err(format!("Type Error at Unary Operation !: {}", self.type_name()))
        })
    }
}
