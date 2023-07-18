use std::{rc::Rc, collections::HashMap, cell::RefCell};

use apnum::BigInt;

use crate::{expr::{Expr, Pattern}, span::Spanned};

pub type List = Rc<Vec<Value>>;
pub type Native = fn(&[Value]) -> Result<Value, String>;
pub type Function = Rc<FunctionValue>; 
pub type Structure = Rc<RefCell<HashMap<String, Value>>>;
pub type Float = f64;
pub type Integer = i32;

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
    BigInteger(BigInt),
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
    Unit
}

impl Value {
    pub fn as_bool(&self) -> Result<bool, String> {
        Ok(match self {
            Self::Bool(bool) => *bool,
            // TOOD: Report type here
            _ => return Err("Expected `Bool`".to_string())
        })
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        match self {
            Integer(int) => write!(f, "{int}"),
            BigInteger(bigint) => write!(f, "{bigint}"),
            Float(float) => write!(f, "{float}"),
            String(string) => write!(f, "{string}"),
            Bool(bool) => write!(f, "{bool}"),
            Function(_) => write!(f, "<function>"),
            ComposedFunctions(..) => write!(f, "<function>"),
            ParitalFunction(..) => write!(f, "<function>"),
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

impl std::cmp::Eq for Value {}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (self, other) {
            (Integer(lint), Integer(rint)) => lint == rint,
            (BigInteger(lbigint), BigInteger(rbigint)) => lbigint == rbigint,
            (BigInteger(bigint), Integer(int)) => bigint.eq_i32(*int),
            (Integer(int), BigInteger(bigint)) => bigint.eq_i32(*int),
            (Float(lfloat), Float(rfloat)) => lfloat == rfloat,
            (String(lstring), String(rstring)) => lstring == rstring,
            (Bool(lbool), Bool(rbool)) => lbool == rbool,
            (List(llist), List(rlist)) => llist == rlist,
            (Structure(ls), Structure(rs)) => ls == rs,
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
            (BigInteger(lbigint), BigInteger(rbigint)) => lbigint.cmp(rbigint),
            (BigInteger(bigint), Integer(int)) => bigint.cmp_i32(*int),
            (Integer(int), BigInteger(bigint)) => bigint.cmp_i32(*int).reverse(),
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
            (Integer(lint), Integer(rint)) => match lint.checked_add(*rint) {
                Some(int) => Integer(int),
                None => BigInteger(BigInt::from(*lint) + *rint),
            },
            (BigInteger(lbigint), BigInteger(rbigint)) => BigInteger(lbigint + rbigint),
            (BigInteger(bigint), Integer(int)) => BigInteger(bigint + *int),
            (Integer(int), BigInteger(bigint)) => BigInteger(*int + bigint),
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
            (Integer(lint), Integer(rint)) => match lint.checked_sub(*rint) {
                Some(int) => Integer(int),
                None => BigInteger(BigInt::from(*lint) - *rint),
            },
            (BigInteger(lbigint), BigInteger(rbigint)) => BigInteger(lbigint - rbigint),
            (BigInteger(bigint), Integer(int)) => BigInteger(bigint - *int),
            (Integer(int), BigInteger(bigint)) => BigInteger(*int - bigint),
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
            (Integer(lint), Integer(rint)) => match lint.checked_mul(*rint) {
                Some(int) => Integer(int),
                None => BigInteger(BigInt::from(*lint) * *rint),
            },
            (BigInteger(lbigint), BigInteger(rbigint)) => BigInteger(lbigint * rbigint),
            (BigInteger(bigint), Integer(int)) => BigInteger(bigint * *int),
            (Integer(int), BigInteger(bigint)) => BigInteger(*int * bigint),
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
            (Integer(lint), Integer(rint)) => match lint.checked_div(*rint) {
                Some(int) => Integer(int),
                None => BigInteger((BigInt::from(*lint) / *rint).0),
            },
            (BigInteger(lbigint), BigInteger(rbigint)) => BigInteger((lbigint / rbigint).0),
            (BigInteger(bigint), Integer(int)) => BigInteger((bigint / *int).0),
            (Integer(int), BigInteger(bigint)) => BigInteger((&BigInt::from(*int) / bigint).0),
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
            BigInteger(bigint) => BigInteger(-bigint),
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
